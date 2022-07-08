new_misvm <- function(x = list(), method = c("heuristic", "mip", "qp-heuristic")) {
  stopifnot(is.list(x))
  method <- match.arg(method)
  structure(
    x,
    class = "misvm",
    method = method
  )
}

validate_misvm <- function(x) {
  message("No validations currently in place for object of class 'misvm'.")
  x
}

#' Fit MI-SVM model to the data
#'
#' This function fits the MI-SVM model, first proposed by Andrews et al. (2003).
#' It is a variation on the traditional SVM framework that carefully treats data
#' from the multiple instance learning paradigm, where instances are grouped
#' into bags, and a label is only available for each bag.
#'
#' Several choices of fitting algorithm are available, including a version of
#' the heuristic algorithm proposed by Andrews et al. (2003) and a novel
#' algorithm that explicitly solves the mixed-integer programming (MIP) problem
#' using the gurobi package optimization back-end.
#'
#' @param x A data.frame, matrix, or similar object of covariates, where each
#'   row represents an instance. If a `mi_df` object is passed, `y, bags` are
#'   automatically extracted, and all other columns will be used as predictors.
#'   If a `mild_df` object is passed, `y, bags, instances` are automatically
#'   extracted, and all other columns will be used as predictors.
#' @param y A numeric, character, or factor vector of bag labels for each
#'   instance.  Must satisfy `length(y) == nrow(x)`. Suggest that one of the
#'   levels is 1, '1', or TRUE, which becomes the positive class; otherwise, a
#'   positive class is chosen and a message will be supplied.
#' @param bags A vector specifying which instance belongs to each bag.  Can be a
#'   string, numeric, of factor.
#' @param formula a formula with specification `mi(y, bags) ~ x` which uses the
#'   `mi` function to create the bag-instance structure. This argument is an
#'   alternative to the `x, y, bags` arguments, but requires the `data`
#'   argument. See examples.
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted.
#' @param cost The cost parameter in SVM. If `method = 'heuristic'`, this will
#'   be fed to `kernlab::ksvm()`, otherwise it is similarly in internal
#'   functions.
#' @param method The algorithm to use in fitting (default  `'heuristic'`).  When
#'   `method = 'heuristic'`, which employs an algorithm similar to Andrews et
#'   al. (2003). When `method = 'mip'`, the novel MIP method will be used.  When
#'   `method = 'qp-heuristic`, the heuristic algorithm is computed using the
#'   dual SVM.  See details.
#' @param weights named vector, or `TRUE`, to control the weight of the cost
#'   parameter for each possible y value.  Weights multiply against the cost
#'   vector. If `TRUE`, weights are calculated based on inverse counts of
#'   instances with given label, where we only count one positive instance per
#'   bag. Otherwise, names must match the levels of `y`.
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   * `kernel` either a character the describes the kernel ('linear' or
#'   'radial') or a kernel matrix at the instance level.
#'   * `sigma` argument needed for radial basis kernel.
#'   * `nystrom_args` a list of parameters to pass to [kfm_nystrom()]. This is
#'   used when `method = 'mip'` and `kernel = 'radial'` to generate a Nystrom
#'   approximation of the kernel features.
#'   * `max_step` argument used when `method = 'heuristic'`. Maximum steps of
#'   iteration for the heuristic algorithm.
#'   * `type`: argument used when `method = 'heuristic'`. The `type` argument is
#'   passed to `e1071::svm()`.
#'   * `scale` argument used for all methods. A logical for whether to rescale
#'   the input before fitting.
#'   * `verbose` argument used when `method = 'mip'`. Whether to message output
#'   to the console.
#'   * `time_limit` argument used when `method = 'mip'`. `FALSE`, or a time
#'   limit (in seconds) passed to `gurobi()` parameters.  If `FALSE`, no time
#'   limit is given.
#'   * `start` argument used when `method = 'mip'`.  If `TRUE`, the mip program
#'   will be warm_started with the solution from `method = 'qp-heuristic'` to
#'   potentially improve speed.
#' @param .fns (argument for `misvm.mild_df()` method) list of functions to
#'   summarize instances over.
#' @param cor (argument for `misvm.mild_df()` method) logical, whether to
#'   include correlations between all features in the summarization.
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `misvm.`  The object contains at least the
#'   following components:
#'   * `*_fit`: A fit object depending on the `method` parameter.  If `method =
#'   'heuristic'`, this will be an `svm` fit from the e1071 package.  If
#'   `method = 'mip', 'qp-heuristic'` this will be `gurobi_fit` from a model
#'   optimization.
#'   * `call_type`: A character indicating which method `misvm()` was called
#'   with.
#'   * `features`: The names of features used in training.
#'   * `levels`: The levels of `y` that are recorded for future prediction.
#'   * `cost`: The cost parameter from function inputs.
#'   * `weights`: The calculated weights on the `cost` parameter.
#'   * `repr_inst`: The instances from positive bags that are selected to be
#'   most representative of the positive instances.
#'   * `n_step`: If `method %in% c('heuristic', 'qp-heuristic')`, the total
#'   steps used in the heuristic algorithm.
#'   * `x_scale`: If `scale = TRUE`, the scaling parameters for new predictions.
#'
#' @references Andrews, S., Tsochantaridis, I., & Hofmann, T. (2002). Support
#'   vector machines for multiple-instance learning. *Advances in neural
#'   information processing systems*, *15*.
#'
#'   Kent, S., & Yu, M. (2022). Non-convex SVM for cancer diagnosis based on
#'   morphologic features of tumor microenvironment *arXiv preprint*
#'   [arXiv:2206.14704](https://arxiv.org/abs/2206.14704)
#'
#' @seealso
#' * [predict.misvm()] for prediction on new data.
#' * [cv_misvm()] for cross-validation fitting.
#'
#' @examples
#' set.seed(8)
#' mil_data <- generate_mild_df(nbag = 20,
#'                              positive_prob = 0.15,
#'                              sd_of_mean = rep(0.1, 3))
#' df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
#'
#' # Heuristic method
#' mdl1 <- misvm(x = df[, 4:123], y = df$bag_label,
#'               bags = df$bag_name, method = "heuristic")
#' mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)
#'
#' # MIP method
#' if (require(gurobi)) {
#'   mdl3 <- misvm(x = df[, 4:123], y = df$bag_label,
#'                 bags = df$bag_name, method = "mip")
#' }
#'
#' predict(mdl1, new_data = df, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df %>%
#'   bind_cols(predict(mdl2, df, type = "class")) %>%
#'   bind_cols(predict(mdl2, df, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#'
#' @author Sean Kent, Yifei Liu
#' @name misvm
NULL

#' @export
misvm <- function(x, ...) {
  UseMethod("misvm")
}

#' @describeIn misvm Method for data.frame-like objects
#' @export
misvm.default <- function(
    x,
    y,
    bags,
    cost = 1,
    method = c("heuristic", "mip", "qp-heuristic"),
    weights = TRUE,
    control = list(kernel = "linear",
                   sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                   nystrom_args = list(m = nrow(x),
                                       r = nrow(x),
                                       sampling = "random"),
                   max_step = 500,
                   type = "C-classification",
                   scale = TRUE,
                   verbose = FALSE,
                   time_limit = 60,
                   start = FALSE),
    ...) {

  method <- match.arg(method, c("heuristic", "mip", "qp-heuristic"))

  defaults <- list(
    kernel = "linear",
    sigma = if (is.vector(x)) 1 else 1 / ncol(x),
    nystrom_args = list(m = nrow(x), r = nrow(x), sampling = "random"),
    max_step = 500,
    type = "C-classification",
    verbose = FALSE,
    time_limit = 60,
    start = FALSE
  )
  control <- .set_default(control, defaults)
  control <- .set_scale(control)
  control <- .set_start(control)

  # store the levels of y and convert to 0,1 numeric format.
  y_info <- convert_y(y)
  y <- y_info$y
  lev <- y_info$lev

  # remove NaN columns and columns with no variance, store col names
  x_info <- .convert_x(x, scale = FALSE) # scaling done internally
  x <- x_info$x
  col_x <- x_info$col_x

  weights <- .set_weights(weights, y_info, bags)

  # Nystrom approximation to x for mip and  methods
  if (method %in% c("mip") && control$kernel == "radial") {
    control$nystrom_args$df <- as.matrix(x)
    control$nystrom_args$kernel <- control$kernel
    control$nystrom_args$sigma <- control$sigma
    kfm_fit <- do.call(kfm_nystrom, args = control$nystrom_args)

    x <- build_fm(kfm_fit, x)
  }

  if (method == "heuristic") {
    y <- 2*y - 1 # convert {0,1} to {-1, 1}
    res <- misvm_heuristic_fit(y, bags, x,
                               c = cost,
                               rescale = control$scale,
                               weights = weights,
                               kernel = control$kernel,
                               sigma = control$sigma,
                               max_step = control$max_step,
                               type = control$type,
                               scale = control$scale)
  } else if (method == "mip") {
    y <- 2*y - 1 # convert {0,1} to {-1, 1}
    res <- misvm_mip_fit(y, bags, x,
                         c = cost,
                         rescale = control$scale,
                         weights = weights,
                         verbose = control$verbose,
                         time_limit = control$time_limit,
                         start = control$start)
  } else if (method == "qp-heuristic") {
    y <- 2*y - 1 # convert {0,1} to {-1, 1}
    res <- misvm_dualqpheuristic_fit(y, bags, x,
                                     c = cost,
                                     rescale = control$scale,
                                     weights = weights,
                                     kernel = control$kernel,
                                     sigma = control$sigma,
                                     verbose = control$verbose,
                                     time_limit = control$time_limit,
                                     max_step = control$max_step)
  } else {
    stop("misvm requires method = 'heuristic', 'mip', or 'qp-heuristic'.")
  }

  out <- res[1]
  if (method %in% c("mip") && control$kernel == "radial") {
    out$kfm_fit <- kfm_fit
  }
  out$call_type <- "misvm.default"
  out$x <- res$x
  out$features <- col_x
  out$levels <- lev
  out$cost <- cost
  out$weights <- weights
  out$kernel <- control$kernel
  out$kernel_param <- switch(
    out$kernel,
    "radial" = list("sigma" = control$sigma),
    "linear" = NULL,
  )
  out$repr_inst <- res$repr_inst
  out$n_step <- res$n_step
  out$x_scale <- res$x_scale
  new_misvm(out, method = method)
}

#' @describeIn misvm Method for passing formula
#' @export
misvm.formula <- function(formula, data, ...) {
  # NOTE: other 'professional' functions use a different type of call that I
  #   couldn't get to work. See https://github.com/therneau/survival/blob/master/R/survfit.R
  #   or https://github.com/cran/e1071/blob/master/R/svm.R
  #   right now we're using something that should work for most generic formulas

  mi_names <- as.character(stats::terms(formula, data = data)[[2]])
  bag_name <- mi_names[[3]]

  x <- x_from_mi_formula(formula, data)
  response <- stats::get_all_vars(formula, data = data)
  y <- response[, 1]
  bags <- response[, 2]

  res <- misvm.default(x, y, bags, ...)

  res$call_type <- "misvm.formula"
  res$formula <- formula
  res$bag_name <- bag_name
  return(res)
}

#' @describeIn misvm Method for `mi_df` objects, automatically handling bag
#'   names, labels, and all covariates.
#' @export
misvm.mi_df <- function(x, ...) {
  x <- as.data.frame(validate_mi_df(x))
  y <- x$bag_label
  bags <- x$bag_name
  x$bag_label <- x$bag_name <- NULL

  res <- misvm.default(x, y, bags, ...)
  res$call_type <- "misvm.mi_df"
  res$bag_name <- "bag_name"
  return(res)
}

#' @describeIn misvm Method for `mild_df` objects. Summarize samples to the
#'   instance level based on specified functions, then perform `misvm()` on
#'   instance level data.
#' @export
misvm.mild_df <- function(x, .fns = list(mean = mean, sd = stats::sd), cor = FALSE, ...) {
  x <- summarize_samples(x, .fns, cor) # instance level data
  y <- x$bag_label
  bags <- x$bag_name
  x$bag_label <- x$bag_name <- x$instance_name <- NULL

  res <- misvm.default(x, y, bags, ...)
  res$call_type <- "misvm.mild_df"
  res$bag_name <- "bag_name"
  res$instance_name <- "instance_name"
  res$summary_fns <- .fns
  res$summary_cor <- cor
  return(res)
}

#' Predict method for `misvm` object
#'
#' @details
#' When the object was fitted using the `formula` method, then the parameters
#' `new_bags` and `new_instances` are not necessary, as long as the names match
#' the original function call.
#'
#' @param object An object of class `misvm`.
#' @param new_data A data frame to predict from. This needs to have all of the
#'   features that the data was originally fitted with.
#' @param type If `'class'`, return predicted values with threshold of 0 as
#'   -1 or +1.  If `'raw'`, return the raw predicted scores.
#' @param layer If `'bag'`, return predictions at the bag level.  If
#'   `'instance'`, return predictions at the instance level.
#' @param new_bags A character or character vector.  Can specify a singular
#'   character that provides the column name for the bag names in `new_data`
#'   (default `'bag_name'`).  Can also specify a vector of length
#'   `nrow(new_data)` that has bag name for each row.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column `.pred_class`.  If `type = 'raw'`, the tibble will have
#'   a column `.pred`.
#'
#' @seealso
#' * [misvm()] for fitting the `misvm` object.
#' * [cv_misvm()] for fitting the `misvm` object with cross-validation.
#'
#' @examples
#' mil_data <- generate_mild_df(nbag = 20,
#'                              positive_prob = 0.15,
#'                              sd_of_mean = rep(0.1, 3))
#' df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
#' mdl1 <- misvm(x = df1[, 4:63], y = df1$bag_label,
#'               bags = df1$bag_name, method = "heuristic")
#'
#' predict(mdl1, new_data = df1, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df1 %>%
#'   bind_cols(predict(mdl1, df1, type = "class")) %>%
#'   bind_cols(predict(mdl1, df1, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#' @export
#' @author Sean Kent
predict.misvm <- function(object,
                          new_data,
                          type = c("class", "raw"),
                          layer = c("bag", "instance"),
                          new_bags = "bag_name",
                          ...) {
  type <- match.arg(type, c("class", "raw"))
  layer <- match.arg(layer, c("bag", "instance"))
  method <- attr(object, "method")

  if (object$call_type == "misvm.mild_df") {
    mil_cols <- c("bag_label", "bag_name", "instance_name")
    mil_info <- new_data[, mil_cols, drop = FALSE]
    new_data <- summarize_samples(new_data,
                                  group_cols = mil_cols,
                                  .fns = object$summary_fns,
                                  cor = object$summary_cor)
  }

  new_x <- .get_new_x(object, new_data)

  if (method == "qp-heuristic") {
    kernel <- compute_kernel(as.matrix(new_x),
                             object$gurobi_fit$xmatrix,
                             type = object$gurobi_fit$kernel,
                             sigma = object$gurobi_fit$sigma)
  }

  scores <- switch(
    method,
    "heuristic" = {
        pos <- predict(object = object$svm_fit, newdata = new_x, decision.values = TRUE)
        as.numeric(attr(pos, "decision.values"))
    },
    "mip" = as.matrix(new_x) %*% object$gurobi_fit$w + object$gurobi_fit$b,
    "qp-heuristic" = kernel %*% object$gurobi_fit$ay + object$gurobi_fit$b
  )

  if (layer == "bag") {
    bags <- .get_bags(object, new_data, new_bags)
    scores <- classify_bags(scores, bags, condense = FALSE)
  }

  pos <- .to_plus_minus(scores)
  pos <- factor(pos, levels = c(-1, 1), labels = object$levels)

  res <- .pred_output(type, scores, pos)
  if (object$call_type == "misvm.mild_df") {
    # bring back the predictions from instance level to the sample level
    ind <- match(mil_info$instance_name, new_data$instance_name)
    res <- res[ind, , drop = FALSE]
  }
  attr(res, "layer") <- layer
  return(res)
}

#' @export
print.misvm <- function(x, digits = getOption("digits"), ...) {
  method <- attr(x, "method")
  kernel_param <- .get_kernel_param_str(x, digits)
  weights <- .get_weights_str(x)

  cat("An misvm object called with", x$call_type, "\n")
  cat("", "\n")
  cat("Parameters:", "\n")
  cat("  method:", method, "\n")
  cat("  kernel:", x$kernel, kernel_param, "\n")
  cat("  cost:", x$cost, "\n")
  cat("  scale:", !is.null(x$x_scale), "\n")
  cat("  weights:", weights, "\n")
  cat("", "\n")
  cat("Model info:", "\n")
  cat("  Features:")
  utils::str(x$features, width = getOption("width")-14)
  if (method == "heuristic" || method == "qp-heuristic") {
    cat("  Number of iterations:", x$n_step, "\n")
  }
  if (method == "mip") {
    cat("  Gap to optimality:", x$gurobi_fit$mipgap, "\n")
  }
  cat("\n")
}

# Specific implementation methods below ----------------------------------------


#' INTERNAL Fit MI-SVM model based on full MIP problem
#'
#' Function to train an MI-SVM classifier based on the full specification of the
#' Mixed Integer Programming (MIP) problem.  The optimization problem is solved
#' using the `gurobi` R package through the Gurobi backend.
#'
#' @param y a nx1 numeric vector of bag labels with length equal to the number
#'   of instances (not the number of bags). Must have -1 for negative bags and
#'   +1 for positive bags
#' @param bags a nx1 vector specifying which instance belongs to each bag.  Can
#'   be a string, numeric, of factor
#' @param X an nxp data.frame of covariates.  Can also supply a matrix.
#' @param c scalar indicating the penalty term on the sum of Xi in the objective
#'   function
#' @param rescale logical; whether to rescale the input before fitting
#' @param weights named vector to control the weight of the cost parameter for
#'   each possible y value.  Weights multiply against the cost vector.
#' @param verbose whether to message output to the console; default is FALSE.
#' @param time_limit FALSE, or a time limit (in seconds) passed to gurobi
#'   parameters. If FALSE, no time limit is given.
#' @param start logical; whether to warm start the MIP program with the
#'   heuristic fit.  For large problems, it's expected that this will speed up
#'   the algorithm.
#'
#' @return `misvm_mip_fit()` returns an object of class `"misvm"`.
#'   An object of class "misvm" is a list containing at least the following
#'   components:
#'   * `model`: a list with components:
#'     * `w`: weights to apply to the predictors to make the classifier
#'     * `b`: intercept term used to make the classifier
#'     * `xi`: slack variables returned from the optimization
#'     * `z`: integer variables returned from the optimization that determine
#'     selected instances
#'     `status`: solution status from `gurobi::gurobi`
#'     `itercount`: itercount from `gurobi::gurobi`
#'     `baritercount`: baritercount from `gurobi::gurobi`
#'     `objval`: value of the objective at the solution
#'     `c`: value of the cost parameter used in solving the optimization
#'   * `representative_inst`: NULL, TODO: mesh with other misvm method
#'   * `traindata`: NULL, TODO: mesh with other misvm method
#'   * `useful_inst_idx`: NULL, TODO: mesh with other misvm method
#'
#' @author Sean Kent
#' @noRd
misvm_mip_fit <- function(y,
                          bags,
                          x,
                          c,
                          rescale = TRUE,
                          weights = NULL,
                          verbose = FALSE,
                          time_limit = FALSE,
                          start = FALSE) {
  # TODO: maybe change function call to y, X, bags?
  if (rescale) x <- scale(x)
  bags <- as.numeric(factor(bags, levels = unique(bags)))

  warm_start <- NULL
  if (start) {
    qp_fit <- misvm_qpheuristic_fit(y, bags, x, c, rescale, weights, verbose = FALSE, time_limit)
    warm_start <- list(
      opt = c(qp_fit$model$w, qp_fit$model$b, rep(NA, length(qp_fit$model$xi))),
      selected = qp_fit$representative_inst
    )
  }

  model <- misvm_mip_model(y, bags, x, c, weights, warm_start)
  params <- .gurobi_params(verbose, time_limit)
  gurobi_result <- gurobi::gurobi(model, params = params)

  w <- gurobi_result$x[grepl("w", model$varnames)]
  b_ <- gurobi_result$x[grepl("b", model$varnames)]
  if (rescale) {
    b_ <- b_ - sum(attr(x, "scaled:center") * w / attr(x, "scaled:scale"))
    w <- w / attr(x, "scaled:scale")
  }

  res <- list(
    gurobi_fit = list(
      w = w,
      b = b_,
      xi = gurobi_result$x[grepl("xi", model$varnames)],
      z = gurobi_result$x[grepl("z", model$varnames)],
      status = gurobi_result$status,
      itercount = gurobi_result$itercount,
      baritercount = gurobi_result$baritercount,
      objval = gurobi_result$objval,
      mipgap = gurobi_result$mipgap,
      c = c
    ),
    x = NULL,
    repr_inst = NULL
  )
  if (rescale) {
    res$x_scale <- list(
      "center" = attr(x, "scaled:center"),
      "scale" = attr(x, "scaled:scale")
    )
  }
  names(res$gurobi_fit$w) <- colnames(x)
  return(res)
}

#' INTERNAL Create optimization model for MI-SVM problem
#'
#' Internal function to build an optimization model (that can be passed to
#' `gurobi::gurobi`) based on the MI-SVM problem.
#'
#' @inheritParams misvm_mip_fit
#' @param warm_start NULL, or a list with components 'opt' and 'selected' which
#'   provide the warm start values to use for the (w, b, xi) and (z)
#'   constraints, respectively.
#' @return a model that can be passed to `gurobi::gurobi()` that contains the
#'   MIQP problem defined by MI-SVM in Andrews et al. (2003)
#'
#' @author Sean Kent
#' @noRd
misvm_mip_model <- function(y, bags, x, c, weights = NULL, warm_start = NULL) {
  lim <- 1e2 * sum(abs(x) / nrow(x))
  # TODO: check that y has only -1 and 1
  r <- .reorder(y, bags, x)
  y <- r$y
  bags <- r$b
  x <- r$X

  # Build constraint matrix
  # order of variables is [w, b, xi, z]
  n_w <- ncol(x)
  n_b <- 1
  n_xi <- length(unique(bags))
  n_z <- sum(y == 1)

  # constraint1 is related to the data
  w_constraint <- y*x
  b_constraint <- y*1
  xi_col <- function(b, n_xi) {
    # puts a 1 in the `b`th entry of a n_xi-length 0 vector
    vec <- rep(0, n_xi)
    vec[b] <- 1
    return(vec)
  }
  xi_constraint <- t(sapply(bags, xi_col, n_xi = n_xi))
  z_constraint <- rbind(matrix(0, nrow = sum(y == -1), ncol = n_z),
                        lim*diag(nrow = n_z, ncol = n_z))

  constraint1 <- as.matrix(cbind(w_constraint, b_constraint, xi_constraint, z_constraint))
  rhs1 <- rep(1, nrow(x))

  # constraint2 is related to how many z can be non-zero
  pos_bags <- unique(bags[y == 1])
  pos_bag_counts <- as.data.frame(table(bags[y==1]))$Freq
  z_constraint <- matrix(NA, nrow = length(pos_bags), ncol = n_z)
  for (bag in pos_bags) {
    row <- unlist(mapply(rep, x = 1 * (bag == pos_bags), times = pos_bag_counts))
    z_constraint[bag == pos_bags, ] <- row
  }
  wbxi_constraint <- matrix(0, nrow = length(pos_bags), ncol = n_w + n_b + n_xi)
  constraint2 <- cbind(wbxi_constraint, z_constraint)
  colnames(constraint2) <- NULL
  rhs2 <- pos_bag_counts - 1

  if (is.null(weights)) {
    c_vec <- rep(c, n_xi)
  } else {
    c_vec <- numeric(n_xi)
    c_vec[which(unique(bags) %in% pos_bags)] <- weights[["1"]] * c
    c_vec[which(unique(bags) %ni% pos_bags)] <- weights[["-1"]] * c
  }

  model <- list()
  # Objective
  model[["modelsense"]] <- "min"
  model[["obj"]] <- c(rep(0, n_w + n_b), c_vec, rep(0, n_z)) # linear portion of objective
  model[["Q"]] <- diag(c(rep(1, n_w), rep(0, n_b + n_xi + n_z))) # quadratic portion of objective
  # Constraints
  model[["varnames"]] <- c(paste0("w", 1:n_w), "b", paste0("xi", 1:n_xi), paste0("z", 1:n_z))
  model[["A"]] <- rbind(constraint1, constraint2)
  model[["sense"]] <- c(rep(">=", length(rhs1)), rep("<=", length(rhs2)))
  model[["rhs"]] <- c(rhs1, rhs2)
  model[["vtype"]] <- c(rep("C", n_w + n_b + n_xi), rep("B", n_z))
  model[["lb"]] <- c(rep(-Inf, n_w + n_b), rep(0, n_xi + n_z))
  # Advanced
  if (!is.null(warm_start)) model$start <- c(warm_start[["opt"]], warm_start[["selected"]][r$order][y == 1])
  return(model)
}

#' INTERNAL MI-SVM algorithm implementation in R
#'
#' This function implements the MI-SVM algorithm proposed by Andrews et al (2003)
#' @param data A data.frame whose first three columns are `bag_label`, `bag_name` and `instance_name`.
#' @param cost The cost parameter to be fed to `e1071::svm`.
#' @param kernel The kernel function to be used for `e1071::svm`.
#' @param max_step Maximum steps of iteration for the iterative SVM methods.
#' @param type type that to be used for `e1071::svm`.
#' @return An object of class 'MI_SVM'
#' @examples
#' mild_df1 <- generate_mild_df(positive_dist = 'mvt',
#'                              negative_dist = 'mvnormal',
#'                              remainder_dist = 'mvnormal',
#'                              nbag = 50,
#'                              nsample = 20,
#'                              positive_degree = 3,
#'                              positive_prob = 0.15,
#'                              positive_mean = rep(0, 5))
#' df1 <- build_instance_feature(mild_df1, seq(0.05, 0.95, length.out = 10))
#' mdl <- MI_SVM(data = df1, cost = 1, kernel = 'radial')
#' @author Yifei Liu, Sean Kent
#' @noRd
misvm_heuristic_fit <- function(y,
                                bags,
                                x,
                                c,
                                rescale = TRUE,
                                weights = NULL,
                                kernel = "radial",
                                sigma = 1,
                                max_step = 500,
                                type = "C-classification",
                                scale = TRUE) {
  r <- .reorder(y, bags, x)
  y <- r$y
  bags <- r$b
  x <- r$X

  # compute initial selection variables for the positively labeled bags as mean within each bag
  pos_bags <- unique(bags[y==1])
  if (ncol(x) == 1) {
    x_selected <- sapply(pos_bags, function(bag) {
      mean(x[bags == bag, ])
    })
    x_selected <- as.matrix(x_selected)
  } else {
    x_selected <- t(sapply(pos_bags,
                           function(bag) {
                             apply(x[bags == bag, , drop = FALSE], 2, mean)
                           }))
  }

  past_selection <- matrix(NA, length(pos_bags), max_step+1)
  past_selection[, 1] <- rep(0, length(pos_bags))
  selection_changed <- TRUE
  n_selections <- 0

  while (selection_changed && n_selections < max_step) {
    y_model <- c(rep(1, nrow(x_selected)), y[y == -1])
    b_model <- c(pos_bags, bags[y == -1])
    x_model <- rbind(x_selected,
                     x[y == -1, , drop = FALSE])

    svm_fit <- e1071::svm(x = x_model,
                          y = y_model,
                          class.weights = weights,
                          cost = c,
                          kernel = kernel,
                          gamma = sigma,
                          scale = rescale,
                          type = type)

    pred_all_inst <- predict(object = svm_fit, newdata = x, decision.values = TRUE)
    pred_all_score <- attr(pred_all_inst, "decision.values")

    # update selections and check for repeats
    selected <- sapply(pos_bags, function(bag) {
      which(pred_all_score == max(pred_all_score[bags == bag]))[1]
    })
    n_selections <- n_selections + 1
    for (i in 1:n_selections) {
      selection_changed <- !all(selected == past_selection[, i])
      if (!selection_changed) {
        break
      }
    }
    if (selection_changed) {
      x_selected <- x[selected, , drop = FALSE]
      past_selection[, (n_selections+1)] <- selected
    }
  }
  if (n_selections == max_step) {
    msg <- paste0("Number of iterations of heuristic algorithm reached threshold",
                  "of ", max_step, ". Stopping with current selection.")
    warning(msg)
  }

  # vector representing selected positive instances
  selected_vec <- rep(0, length(y))
  selected_vec[selected] <- 1
  selected_vec <- selected_vec[order(r$order)] # un-order the vector

  res <- list(
    svm_fit = svm_fit,
    n_step = n_selections,
    repr_inst = selected_vec
  )
  if (rescale) {
    res$x_scale <- stats::setNames(svm_fit$x.scale, c("center", "scale"))
  }
  return(res)
}

#' INTERNAL Fit MI-SVM model based on heuristic method and QP optimization
#'
#' Function to train an MI-SVM classifier based on the heuristic method proposed
#' by Andrews et al. (2003) where each step is solved with the Quadratic
#' Programming (QP) problem with correct constraints.  This can only be solved
#' with a linear space (no kernel methods are available).  The optimization
#' problem is solved using the `gurobi` R package through the Gurobi backend.
#'
#' @inheritParams misvm_mip_fit
#'
#' @return `misvm_qpheuristic_fit()` returns an object of class `"misvm"`.
#'   An object of class "misvm" is a list containing at least the following
#'   components:
#'   * `model`: a list with components:
#'     * `w`: weights to apply to the predictors to make the classifier
#'     * `b`: intercept term used to make the classifier
#'     * `xi`: slack variables returned from the optimization
#'     * `status`: solution status from `gurobi::gurobi`
#'     * `itercount`: itercount from `gurobi::gurobi`
#'     * `baritercount`: baritercount from `gurobi::gurobi`
#'     * `objval`: value of the objective at the solution
#'     * `c`: value of the cost parameter used in solving the optimization
#'     * `n_selections`: the number of selections used in fitting
#'   * `representative_inst`: NULL, TODO: mesh with other misvm method
#'   * `traindata`: NULL, TODO: mesh with other misvm method
#'   * `useful_inst_idx`: NULL, TODO: mesh with other misvm method
#'
#' @author Sean Kent
#' @noRd
misvm_qpheuristic_fit <- function(y,
                                  bags,
                                  x,
                                  c,
                                  rescale = TRUE,
                                  weights = NULL,
                                  verbose = FALSE,
                                  time_limit = FALSE,
                                  max_step = 500) {
  r <- .reorder(y, bags, x)
  y <- r$y
  bags <- r$b
  x <- r$X
  if (rescale) x <- scale(x)

  # compute initial selection variables for the positively labeled bags as mean within each bag
  pos_bags <- unique(bags[y==1])
  if (ncol(x) == 1) {
    x_selected <- sapply(pos_bags, function(bag) {
      mean(x[bags == bag, ])
    })
    x_selected <- as.matrix(x_selected)
  } else {
    x_selected <- t(sapply(pos_bags,
                           function(bag) {
                             apply(x[bags == bag, , drop = FALSE], 2, mean)
                           }))
  }

  selection_changed <- TRUE
  itercount <- 0
  baritercount <- 0
  n_selections <- 0

  while (selection_changed && n_selections < max_step) {
    y_model <- c(y[y == -1], rep(1, nrow(x_selected)))
    b_model <- c(bags[y == -1], pos_bags)
    x_model <- rbind(x[y == -1, , drop = FALSE],
                     x_selected)
    model <- misvm_qpheuristic_model(y_model, b_model, x_model, c, weights)
    gurobi_result <- gurobi::gurobi(model, .gurobi_params(verbose, time_limit))

    w <- gurobi_result$x[grepl("w", model$varnames)]
    b_ <- gurobi_result$x[grepl("b", model$varnames)]
    f <- as.matrix(x) %*% w + b_
    itercount <- itercount + gurobi_result$itercount
    baritercount <- baritercount + gurobi_result$baritercount

    selected <- sapply(pos_bags, function(bag) {
      which(f == max(f[bags == bag]))[1]
    })
    selection_changed <- !identical(x_selected, x[selected, , drop = FALSE])
    if (selection_changed) {
      x_selected <- x[selected, , drop = FALSE]
      n_selections <- n_selections + 1
    }
  }
  if (n_selections == max_step) {
    msg <- paste0("Number of iterations of heuristic algorithm reached threshold",
                  "of ", max_step, ". Stopping with current selection.")
    warning(msg)
  }

  if (rescale) {
    b_ <- b_ - sum(attr(x, "scaled:center") * w / attr(x, "scaled:scale"))
    w <- w / attr(x, "scaled:scale")
  }
  # vector representing selected positive instances
  selected_vec <- rep(0, length(y))
  selected_vec[selected] <- 1
  selected_vec <- selected_vec[order(r$order)] # un-order the vector

  res <- list(
    gurobi_fit = list(
      w = w,
      b = b_,
      xi = gurobi_result$x[grepl("xi", model$varnames)],
      status = gurobi_result$status,
      itercount = itercount,
      baritercount = baritercount,
      objval = gurobi_result$objval,
      c = c,
      n_selections = n_selections
    ),
    repr_inst = selected_vec,
    x = NULL,
    x_scale = list(
      "center" = attr(x, "scaled:center"),
      "scale" = attr(x, "scaled:scale")
    )
  )
  names(res$gurobi_fit$w) <- colnames(x)
  return(res)
}


#' INTERNAL Fit MI-SVM model based on heuristic method and dual QP optimization
#'
#' Function to train an MI-SVM classifier based on the heuristic method proposed
#' by Andrews et al. (2003) where each step is solved with the Quadratic
#' Programming (QP) problem with correct constraints.  This is solved in the
#' dual and so it offers kernel options.  The optimization problem is solved
#' using the `gurobi` R package through the Gurobi backend.
#'
#' @inheritParams misvm_mip_fit
#'
#' @return `misvm_qpheuristic_fit()` returns an object of class `"misvm"`.
#'   An object of class "misvm" is a list containing at least the following
#'   components:
#'   * `model`: a list with components:
#'     * `b`: intercept term used to make the classifier
#'     * `xmatrix`: data used in future kernel calculations
#'     * `ay`: alpha vectors multiplied by y
#'     * `status`: solution status from `gurobi::gurobi`
#'     * `itercount`: itercount from `gurobi::gurobi`
#'     * `baritercount`: baritercount from `gurobi::gurobi`
#'     * `objval`: value of the objective at the solution
#'     * `c`: value of the cost parameter used in solving the optimization
#'     * `n_selections`: the number of selections used in fitting
#'   * `representative_inst`: NULL, TODO: mesh with other misvm method
#'   * `traindata`: NULL, TODO: mesh with other misvm method
#'   * `useful_inst_idx`: NULL, TODO: mesh with other misvm method
#'   * `center`: vector of centering means, if `rescale`
#'   * `scale`: vector of scaling sds, if `rescale`
#'
#' @author Sean Kent
#' @noRd
misvm_dualqpheuristic_fit <- function(y,
                                      bags,
                                      x,
                                      c,
                                      rescale = TRUE,
                                      weights = NULL,
                                      kernel = "linear",
                                      sigma = NULL,
                                      verbose = FALSE,
                                      time_limit = FALSE,
                                      max_step = 500) {
  r <- .reorder(y, bags, x)
  y <- r$y
  bags <- r$b
  x <- r$X
  if (is.matrix(kernel)) {
    kernel <- kernel[r$order, r$order]
  }
  if (rescale) x <- scale(x)

  # randomly select initial representative instances
  pos_bags <- unique(bags[y==1])
  selected <- sapply(pos_bags, function(bag) {
    .resample(which(bags == bag), size = 1)
  })

  kern_mat <- .convert_kernel(x, kernel, sigma = sigma)

  selection_changed <- TRUE
  itercount <- 0
  baritercount <- 0
  n_selections <- 0

  while (selection_changed && n_selections < max_step) {
    ind <- c(which(y == -1), selected) # effective set for this iteration
    kern_ind <- kern_mat[ind, ind, drop = FALSE]

    opt_model <- misvm_dualqpheuristic_model(y[ind], bags[ind], kern_ind, c, weights)

    gurobi_result <-
      tryCatch({
        gurobi::gurobi(opt_model, .gurobi_params(verbose, time_limit))
      },
      error = function(e) {
        rlang::warn(paste0("Warning from gurobi: ", conditionMessage(e)))
        rlang::inform("Trying NonConvex version")
        params <- .gurobi_params(verbose, time_limit)
        params[["NonConvex"]] <- 2
        return(gurobi::gurobi(opt_model, params = params))
      })

    a <- gurobi_result$x
    # TODO: need a better way to figure out which a to compute b_ over
    # want to exclude those where sum a_i = C in each bag

    b_ <- as.vector(1 / y[ind] - kern_ind %*% (a * y[ind]))
    b_ <- mean(b_[(a > 1e-9) & (a < c)])
    f <- kern_mat[, ind, drop = FALSE] %*% (a * y[ind]) + b_

    itercount <- itercount + gurobi_result$itercount
    baritercount <- baritercount + gurobi_result$baritercount

    new_selection <- sapply(pos_bags, function(bag) {
      b_ind <- bags == bag
      which(f == max(f[b_ind]) & b_ind)[1]
    })

    selection_changed <- !identical(selected, new_selection)
    if (selection_changed) {
      selected <- new_selection
      n_selections <- n_selections + 1
    }
  }
  if (n_selections == max_step) {
    msg <- paste0("Number of iterations of heuristic algorithm reached threshold",
                  "of ", max_step, ". Stopping with current selection.")
    warning(msg)
  }

  # vector representing selected positive instances
  selected_vec <- rep(0, length(y))
  selected_vec[selected] <- 1
  selected_vec[which(y == -1)] <- 1
  selected_vec <- selected_vec[order(r$order)] # un-order the vector

  res <- list(
    gurobi_fit = list(
      # w = w,
      b = b_,
      xmatrix = x[ind, , drop = FALSE],
      ay = a * y[ind],
      kernel = kernel,
      sigma = sigma,
      # xi = gurobi_result$x[grepl("xi", model$varnames)],
      status = gurobi_result$status,
      itercount = itercount,
      baritercount = baritercount,
      objval = gurobi_result$objval,
      c = c,
      n_selections = n_selections
    ),
    n_step = n_selections,
    repr_inst = selected_vec,
    x = NULL
  )
  if (rescale) {
    res$x_scale <- list(
      "center" = attr(x, "scaled:center"),
      "scale" = attr(x, "scaled:scale")
    )
  }
  return(res)
}

#' INTERNAL Create optimization model for MI-SVM problem
#'
#' Internal function to build an optimization model (that can be passed to
#' `gurobi::gurobi`) based on the MI-SVM quadratic problem for the
#' representative instances.
#'
#' @inheritParams misvm_mip_fit
#' @return a model that can be passed to `gurobi::gurobi` that contains the QP
#'   problem defined by MI-SVM in Andrews et al. (2003)
#'
#' @author Sean Kent
#' @noRd
misvm_qpheuristic_model <- function(y, bags, x, c, weights = NULL) {
  # assumes that the data is already re-ordered to save time

  # Build constraint matrix
  # order of variables is [w, b, xi]
  n_w <- ncol(x)
  n_b <- 1
  n_xi <- length(unique(bags))

  w_constraint <- y*x
  b_constraint <- y*1
  xi_col <- function(b, n_xi) {
    # puts a 1 in the `b`th entry of a n_xi-length 0 vector
    vec <- rep(0, n_xi)
    vec[b] <- 1
    return(vec)
  }
  xi_constraint <- t(sapply(bags, xi_col, n_xi = n_xi))
  constraint <- as.matrix(cbind(w_constraint, b_constraint, xi_constraint))

  pos_bags <- unique(bags[y == 1])
  if (is.null(weights)) {
    c_vec <- rep(c, n_xi)
  } else {
    c_vec <- numeric(n_xi)
    c_vec[which(unique(bags) %in% pos_bags)] <- weights[["1"]] * c
    c_vec[which(unique(bags) %ni% pos_bags)] <- weights[["-1"]] * c
  }

  model <- list()

  # Constraints
  model[["modelsense"]] <- "min"
  model[["obj"]] <- c(rep(0, n_w + n_b), c_vec) # linear portion of objective
  model[["Q"]] <- diag(c(rep(1, n_w), rep(0, n_b + n_xi))) # quadratic portion of objective
  # Objective
  model[["varnames"]] <- c(paste0("w", 1:n_w), "b", paste0("xi", 1:n_xi))
  model[["A"]] <- constraint
  model[["sense"]] <- rep(">=", nrow(x))
  model[["rhs"]] <- rep(1, nrow(x))
  model[["lb"]] <- c(rep(-Inf, n_w + n_b), rep(0, n_xi))

  return(model)
}

#' INTERNAL Create optimization model for MI-SVM problem
#'
#' Internal function to build an optimization model (that can be passed to
#' `gurobi::gurobi`) based on the MI-SVM quadratic problem for the
#' representative instances in the dual
#'
#' @inheritParams misvm_mip_fit
#' @param kernel kernel matrix.  This represents `X %*% t(X)` in the linear kernel,
#'   and has a more complicated form for other kernels.
#' @return a model that can be passed to `gurobi::gurobi` that contains the dual
#'   QP problem defined by MI-SVM in Andrews et al. (2003)
#'
#' @author Sean Kent
#' @noRd
misvm_dualqpheuristic_model <- function(y, bags, kernel, c, weights = NULL) {
  # assumes that the data is already re-ordered to save time

  n_a <- length(y)
  n_bags <- length(unique(bags))

  e_vec <- function(b, len) {
    # puts a 1 in the `b`th entry of a `len`-length 0 vector
    vec <- rep(0, len)
    vec[b] <- 1
    return(vec)
  }
  bag_sum_constr <- sapply(bags, e_vec, len = n_bags)

  constraint <- as.matrix(rbind(bag_sum_constr, -bag_sum_constr, t(y), -t(y)))

  pos_bags <- unique(bags[y == 1])
  if (is.null(weights)) {
    c_vec <- rep(c, n_bags)
  } else {
    c_vec <- numeric(n_bags)
    c_vec[which(unique(bags) %in% pos_bags)] <- weights[["1"]] * c
    c_vec[which(unique(bags) %ni% pos_bags)] <- weights[["-1"]] * c
  }

  model <- list()

  # Objective
  model[["modelsense"]] <- "max"
  model[["obj"]] <- rep(1, n_a)
  model[["Q"]] <- - 1/2 * y %*% t(y) * kernel
  # Constraints
  model[["varnames"]] <- c(paste0("a", 1:n_a))
  model[["A"]] <- constraint
  model[["sense"]] <- rep(">=", nrow(constraint))
  model[["rhs"]] <- c(rep(0, n_bags), -c_vec, 0, 0)
  model[["lb"]] <- c(rep(0, n_a))

  return(model)
}
