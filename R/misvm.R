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
#'   It is a variation on the traditional SVM framework that carefully treats
#'   data from the multiple instance learning paradigm, where instances are
#'   grouped into bags, and a label is only available for each bag. Several
#'   choices of fitting algorithm are available, including a version of the
#'   heuristic algorithm proposed by Andrews et al. (2003) and a novel algorithm
#'   that explicitly solves the mixed-integer programming (MIP) problem using
#'   the `gurobi` optimization back-end.
#'
#' @param x a data.frame, matrix, or similar object of covariates, where each
#'   row represents an instance.
#' @param y a numeric, character, or factor vector of bag labels for each
#'   instance.  Must satisfy `length(y) == nrow(x)`. Suggest that one of the
#'   levels is 1, '1', of TRUE, which becomes the positive class in MI-SVM;
#'   otherwise, a positive class is chosen and a message will be supplied.
#' @param bags a vector specifying which instance belongs to each bag.  Can be
#'   a string, numeric, of factor.
#' @param formula a formula with specification `mi(y, bags) ~ x` which uses the
#'   `mi` function to create the bag-instance structure. This argument is an
#'   alternative to the `x, y, bags` arguments, but requires the `data` argument.
#'   See examples.
#' @param data a data.frame or similar from which formula elements will be
#'   extracted.  Used only when the first argument is a formula object.
#' @param cost The cost parameter in SVM. If `method` = 'heuristic', this will
#'   be fed to `e1071::svm`, otherwise it is similarly in internal functions.
#' @param method MI-SVM algorithm to use in fitting; default is 'heuristic',
#'   which employs an algorithm similar to Andrews et al. (2003). When `method`
#'   = 'mip', the novel MIP method will be used.  See details.
#' @param weights named vector, or TRUE, to control the weight of the cost
#'   parameter for each possible y value.  Weights multiply against the cost
#'   vector. If TRUE, weights are calculated based on inverse counts of
#'   instances with given label, where we only count one positive instance per
#'   bag. Otherwise, names must match the levels of `y`.
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   - `kernel` argument used when `method` = 'heuristic'.  The kernel function
#'   to be used for `e1071::svm`. Currently, only 'radial' is supported.
#'   - `sigma` argument needed for radial basis kernel.
#'   - `max_step` argument used when `method` = 'heuristic'. Maximum steps of
#'   iteration for the heuristic algorithm.
#'   - `type` argument used when `method` = 'heuristic'. The `type` argument is
#'   passed to `e1071::svm`.
#'   - `scale` argument used for all methods. Logical; whether to rescale
#'   the input before fitting
#'   - `verbose` argument used when `method` = 'mip'. Whether to message output
#'   to the console.
#'   - `time_limit` argument used when `method` = 'mip'. FALSE, or a time limit
#'   (in seconds) passed to `gurobi` parameters.  If FALSE< no time limit is
#'   given.
#'   - `start` argument used when `method` = 'mip'.  If TRUE, the mip program
#'   will be warm_started with the solution from `method` = 'qp-heuristic' to
#'   improve speed.
#' @param .fns (argument for `misvm.MilData()` method) list of functions to
#'   summarize instances over.
#' @param cor (argument for `misvm.MilData()` method) logical, whether to
#'   include correlations between all features in the summarization.
#'
#' @return an object of class 'misvm'.  The object contains the following
#'   components:
#'   - `model`: a model that will depend on the method used to fit.  It holds
#'   the main model components used for prediction.  If the model is fit with
#'   method = 'heuristic', this object is of class 'svm' from the package
#'   `e1071`.
#'   - `representative_inst`: Instances from positive bags that are selected to
#'   be most representative of the positive instance.
#'   - `features`: the features used for prediction.  These are needed for
#'   prediction.
#'   - `call_type`: the call type, which specifies whether `misvm()` was called
#'   via the formula or data.frame method.
#'   - `levels`: levels of `y` that are recorded for future prediction.
#'
#' @examples
#' set.seed(8)
#' mil_data <- GenerateMilData(
#'   positive_dist = 'mvt',
#'   negative_dist = 'mvnormal',
#'   remainder_dist = 'mvnormal',
#'   nbag = 20,
#'   nsample = 20,
#'   positive_degree = 3,
#'   positive_prob = 0.15,
#'   positive_mean = rep(0, 5)
#' )
#' df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
#'
#' # Heuristic method
#' mdl1 <- misvm(x = df[, 4:123], y = df$bag_label,
#'               bags = df$bag_name, method = "heuristic")
#' mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)
#'
#' if (require(gurobi)) {
#'   # solve using the MIP method
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
misvm <- function(x, y, bags, ...) {
  UseMethod("misvm")
}

#' @describeIn misvm Method for passing formula
#' @export
misvm.formula <- function(formula, data, cost = 1, method = c("heuristic", "mip", "qp-heuristic"), weights = TRUE,
                          control = list(kernel = "linear",
                                         sigma = 1,
                                         nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random'),
                                         max_step = 500,
                                         type = "C-classification",
                                         scale = TRUE,
                                         verbose = FALSE,
                                         time_limit = 60,
                                         start = FALSE)) {
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

  res <- misvm.default(x, y, bags, cost = cost, method = method, weights = weights, control = control)

  res$call_type <- "misvm.formula"
  res$formula <- formula
  res$bag_name <- bag_name
  return(res)
}

#' @describeIn misvm Method for data.frame-like objects
#' @export
misvm.default <- function(x, y, bags, cost = 1, method = c("heuristic", "mip", "qp-heuristic"), weights = TRUE,
                          control = list(kernel = "linear",
                                         sigma = 1,
                                         nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random'),
                                         max_step = 500,
                                         type = "C-classification",
                                         scale = TRUE,
                                         verbose = FALSE,
                                         time_limit = 60,
                                         start = FALSE)) {

  method <- match.arg(method)
  if ("kernel" %ni% names(control)) control$kernel <- "linear"
  if ("sigma" %ni% names(control)) control$sigma <- 1
  if ("nystrom_args" %ni% names(control)) control$nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random')
  if ("max_step" %ni% names(control)) control$max_step <- 500
  if ("type" %ni% names(control)) control$type <- "C-classification"
  if ("scale" %ni% names(control)) control$scale <- TRUE
  if ("verbose" %ni% names(control)) control$verbose <- FALSE
  if ("time_limit" %ni% names(control)) control$time_limit <- 60
  if ("start" %ni% names(control)) control$start <- FALSE

  # store the levels of y and convert to 0,1 numeric format.
  y_info <- convert_y(y)
  y <- y_info$y
  lev <- y_info$lev

  # store colnames of x
  col_x <- colnames(x)

  ## weights
  if (is.numeric(weights)) {
    stopifnot(names(weights) == lev | names(weights) == rev(lev))
    weights <- weights[lev]
    names(weights) <- c("-1", "1")
  } else if (weights) {
    bag_labels <- sapply(split(y, factor(bags)), unique)
    weights <- c("-1" = sum(bag_labels == 1) / sum(y == 0), "1" = 1)
  } else {
    weights <- NULL
  }

  ## Nystrom approximation to x for mip and qp-heuristic methods
  # NOTE: this isn't strictly necessary for qp-heuristic, but it's the easiest way to implement
  if (method %in% c("mip", "qp-heuristic") & control$kernel == "radial") {
    control$nystrom_args$df <- x
    control$nystrom_args$kernel <- "rbf"
    control$nystrom_args$sigma <- control$sigma
    kfm_fit <- do.call(kfm_nystrom, args = control$nystrom_args)

    x <- predict_kfm_nystrom(kfm_fit, x)
  }

  if (method == "heuristic") {
    data <- cbind(bag_label = y,
                  bag_name = bags,
                  instance_name = as.character(1:length(y)),
                  x)
    y = 2*y - 1 # convert {0,1} to {-1, 1}
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
    y = 2*y - 1 # convert {0,1} to {-1, 1}
    res <- misvm_mip_fit(y, bags, x,
                         c = cost,
                         rescale = control$scale,
                         weights = weights,
                         verbose = control$verbose,
                         time_limit = control$time_limit,
                         start = control$start)
  } else if (method == "qp-heuristic") {
    y = 2*y - 1 # convert {0,1} to {-1, 1}
    res <- misvm_qpheuristic_fit(y, bags, x,
                                 c = cost,
                                 rescale = control$scale,
                                 weights = weights,
                                 verbose = control$verbose,
                                 time_limit = control$time_limit,
                                 max_step = control$max_step)
  } else {
    stop("misvm requires method = 'heuristic', 'mip', or 'qp-heuristic'.")
  }

  res$features <- col_x
  res$call_type <- "misvm.default"
  res$bag_name <- NULL
  res$levels <- lev
  if (method %in% c("mip", "qp-heuristic") & control$kernel == "radial") {
    res$kfm_fit <- kfm_fit
  }
  new_misvm(res, method = method)
  # return(res)
}

#' @describeIn misvm Method for 'MilData' objects. Summarize samples to the
#'   instance level based on specified functions, then perform misvm on instance
#'   level data.
#' @export
misvm.MilData <- function(data, .fns = list(mean = mean, sd = sd), cor = FALSE, ...)
{
  form <- mi(bag_label, bag_name) ~ . - instance_name
  instance_data <- summarize_samples(data, .fns, cor)
  res <- misvm(form, data = instance_data, ...)

  res$call_type <- "misvm.MilData"
  res$instance_name <- "instance_name"
  res$summary_fns <- .fns
  res$summary_cor <- cor
  res$features <- setdiff(colnames(instance_data), c("bag_label", "bag_name", "instance_name"))
  return(res)
}



#' Predict method for 'misvm' object
#' @param object an object of class misvm
#' @param new_data matrix to predict from.  Needs to have the same number of
#'   columns as the X that trained the misvm object
#' @param type if 'class', return predicted values with threshold of 0 as
#'   -1 or +1.  If 'raw', return the raw predicted scores.
#' @param layer if 'bag', return predictions at the bag level.  If 'instance',
#'   return predictions at the instance level.
#' @param new_bags character or character vector.  Can specify a singular
#'   character that provides the column name for the bag names in `new_data`,
#'   default = "bag_name".  Can also specify a vector of length `nrow(new_data)`
#'   that has bag name for each instance.  When `object` was fitted with
#'   `misvm.formula`, this parameter is not necessary as the bag name can be
#'   pulled directly from new_data, if available.
#'
#' @return tibble with `nrow(new_data)` rows.  If type = 'class', the tibble
#'   will have a column '.pred_class'.  If type = 'raw', the tibble will have
#'   a column '.pred'.
#'
#' @examples
#' mil_data <- GenerateMilData(
#'   positive_dist = 'mvt',
#'   negative_dist = 'mvnormal',
#'   remainder_dist = 'mvnormal',
#'   nbag = 20,
#'   nsample = 20,
#'   positive_degree = 3,
#'   positive_prob = 0.15,
#'   positive_mean = rep(0, 5)
#' )
#' df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
#' mdl1 <- misvm.default(df1, cost = 1, kernel = "radial", method = "mip")
#'
#' predict(mdl1, new_data = df1, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df1 %>%
#'   bind_cols(predict(mdl2, df1, type = "class")) %>%
#'   bind_cols(predict(mdl2, df1, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#' @export
#' @author Sean Kent
predict.misvm <- function(object, new_data,
                          type = c("class", "raw"), layer = c("bag", "instance"),
                          new_bags = "bag_name")
{
  type <- match.arg(type)
  layer <- match.arg(layer)
  method <- attr(object, "method")

  if (object$call_type == "misvm.MilData") {
    mil_cols <- c("bag_label", "bag_name", "instance_name")
    mil_info <- new_data[, mil_cols]
    new_data <- summarize_samples(new_data,
                                  group_cols = mil_cols,
                                  .fns = object$summary_fns,
                                  cor = object$summary_cor)
  }

  if (object$call_type == "misvm.formula") {
    new_x <- x_from_mi_formula(object$formula, new_data)
  } else {
    new_x <- new_data[, object$features, drop = FALSE]
  }
  if ("kfm_fit" %in% names(object)) {
    new_x <- predict_kfm_nystrom(object$kfm_fit, new_x)
  }

  if (method == "heuristic") {
    pos <- predict(object = object$model, newdata = new_x, decision.values = TRUE)
    scores <- attr(pos, "decision.values")
    pos <- as.numeric(as.character(pos))

  } else if (method == "mip" | method == "qp-heuristic") {
    scores <- as.matrix(new_x) %*% object$model$w + object$model$b
    pos <- 2*(scores > 0) - 1

  } else {
    stop("predict.misvm requires method = 'heuristic' or 'mip'.")
  }

  if (layer == "bag") {
    if (object$call_type == "misvm.formula" & new_bags[1] == "bag_name" & length(new_bags) == 1) {
      new_bags <- object$bag_name
    }
    if (length(new_bags) == 1 & new_bags[1] %in% colnames(new_data)) {
      bags <- new_data[[new_bags]]
    } else {
      bags <- new_bags
    }
    scores <- classify_bags(scores, bags, condense = FALSE)
    pos <- classify_bags(pos, bags, condense = FALSE)
  }
  pos <- factor(pos, levels = c(-1, 1), labels = object$levels)

  res <- switch(type,
                "raw" = tibble::tibble(.pred = as.numeric(scores)),
                "class" = tibble::tibble(.pred_class = pos))

  if (object$call_type == "misvm.MilData") {
    # bring back the predictions from instance level to the sample level
    ind <- match(mil_info$instance_name, new_data$instance_name)
    res <- res[ind, ]
  }
  # TODO: consider returning the AUC here as an attribute.  Can only do if we have the true bag labels
  # attr(res, "AUC") <- calculated_auc
  attr(res, "layer") <- layer
  res
}


# Specific implementation methods below ----------------------------------------


#' INTERNAL Fit MI-SVM model based on full MIP problem
#'
#' Function to train an MI-SVM classifier based on the full
#' specification of the Mixed Integer Programming (MIP) problem.  The optimization
#' problem is solved using the `gurobi` R package through the Gurobi backend.
#'
#' @param y a nx1 numeric vector of bag labels with length equal to the number
#'   of instances (not the number of bags). Must have -1 for negative bags and
#'   +1 for positive bags
#' @param bags a nx1 vector specifying which instance belongs to each bag.  Can be
#'   a string, numeric, of factor
#' @param X an nxp data.frame of covariates.  Can also supply a matrix.
#' @param c scalar indicating the penalty term on the sum of Xi in the
#'   objective function
#' @param rescale logical; whether to rescale the input before fitting
#' @param weights named vector to control the weight of the cost parameter
#'   for each possible y value.  Weights multiply against the cost vector.
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
#'   - `model`: a list with components:
#'     - `w`: weights to apply to the predictors to make the classifier
#'     - `b`: intercept term used to make the classifier
#'     - `xi`: slack variables returned from the optimization
#'     - `z`: integer variables returned from the optimization that determine selected instances
#'     `status`: solution status from `gurobi::gurobi`
#'     `itercount`: itercount from `gurobi::gurobi`
#'     `baritercount`: baritercount from `gurobi::gurobi`
#'     `objval`: value of the objective at the solution
#'     `c`: value of the cost parameter used in solving the optimization
#'   - `representative_inst`: NULL, TODO: mesh with other misvm method
#'   - `traindata`: NULL, TODO: mesh with other misvm method
#'   - `useful_inst_idx`: NULL, TODO: mesh with other misvm method
#'
#' @author Sean Kent
#' @keywords internal
misvm_mip_fit <- function(y, bags, X, c, rescale = TRUE, weights = NULL,
                          verbose = FALSE, time_limit = FALSE, start = FALSE) {
  # TODO: maybe change function call to y, X, bags?
  if (rescale) X <- scale(X)
  bags <- as.numeric(factor(bags, levels = unique(bags)))

  warm_start <- NULL
  if (start) {
    qp_fit <- misvm_qpheuristic_fit(y, bags, X, c, rescale, weights, verbose = FALSE, time_limit)
    warm_start <- list(
      opt = c(qp_fit$model$w, qp_fit$model$b, rep(NA, length(qp_fit$model$xi))),
      selected = qp_fit$representative_inst
    )
  }

  model <- misvm_mip_model(y, bags, X, c, weights, warm_start)
  params <- list()
  params$OutputFlag = 1*verbose
  params$IntFeasTol = 1e-5
  if (time_limit) params$TimeLimit = time_limit
  gurobi_result <- gurobi::gurobi(model, params = params)

  w <- gurobi_result$x[grepl("w", model$varnames)]
  b_ <- gurobi_result$x[grepl("b", model$varnames)]
  if (rescale) {
    b_ <- b_ - sum(attr(X, "scaled:center") * w / attr(X, "scaled:scale"))
    w <- w / attr(X, "scaled:scale")
  }

  res <- list(
    model = list(
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
    representative_inst = NULL, # TODO: fill in these parameters or remove
    traindata = NULL,
    useful_inst_idx = NULL
  )
  names(res$model$w) <- colnames(X)

  return(new_misvm(res, method = "mip"))
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
#' @return a model that can be passed to `gurobi::gurobi` that contains the MIQP
#'   problem defined by MI-SVM in Andrews et al. (2003)
#'
#' @author Sean Kent
#' @keywords internal
misvm_mip_model <- function(y, bags, X, c, weights = NULL, warm_start = NULL) {
  L <- 1e2 * sum(abs(X) / nrow(X))
  # TODO: check that y has only -1 and 1
  r <- .reorder(y, bags, X)
  y <- r$y
  bags <- r$b
  X <- r$X

  ## Build constraint matrix
  # order of variables is [w, b, xi, z]
  n_w <- ncol(X)
  n_b <- 1
  n_xi <- length(unique(bags))
  n_z <- sum(y == 1)

  # constraint1 is related to the data
  w_constraint <- y*X
  b_constraint <- y*1
  xi_col <- function(b, n_xi) {
    # puts a 1 in the `b`th entry of a n_xi-length 0 vector
    vec <- rep(0, n_xi)
    vec[b] <- 1
    return(vec)
  }
  xi_constraint <- t(sapply(bags, xi_col, n_xi = n_xi))
  z_constraint <- rbind(matrix(0, nrow = sum(y == -1), ncol = n_z),
                        L*diag(nrow = n_z, ncol = n_z))

  constraint1 <- as.matrix(cbind(w_constraint, b_constraint, xi_constraint, z_constraint))
  rhs1 <- rep(1, nrow(X))

  # constraint2 is related to how many z can be non-zero
  pos_bags <- unique(bags[y == 1])
  pos_bag_counts <- as.data.frame(table(bags[y==1]))$Freq
  z_constraint <- matrix(NA , nrow = length(pos_bags), ncol = n_z)
  for (bag in pos_bags) {
    row <- unlist(mapply(rep, x = 1*(bag == pos_bags), times = pos_bag_counts))
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
  ## Objective
  model$modelsense <- "min"
  model$obj <- c(rep(0, n_w + n_b), c_vec, rep(0, n_z)) # linear portion of objective
  model$Q <- diag(c(rep(1, n_w), rep(0, n_b + n_xi + n_z))) # quadratic portion of objective
  ## Constraints
  model$varnames <- c(paste0("w",1:n_w), "b", paste0("xi",1:n_xi), paste0("z",1:n_z))
  model$A <- rbind(constraint1, constraint2)
  model$sense <- c(rep(">=", length(rhs1)), rep("<=", length(rhs2))) # rep("<=", length(model$rhs))
  model$rhs <- c(rhs1, rhs2)
  model$vtype <- c(rep("C", n_w + n_b + n_xi), rep("B", n_z))
  model$lb <- c(rep(-Inf, n_w + n_b), rep(0, n_xi + n_z))
  ## Advanced
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
#' MilData1 <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 50,
#'                             nsample = 20,
#'                             positive_degree = 3,
#'                             positive_prob = 0.15,
#'                             positive_mean = rep(0, 5))
#' df1 <- build_instance_feature(MilData1, seq(0.05, 0.95, length.out = 10))
#' mdl <- MI_SVM(data = df1, cost = 1, kernel = 'radial')
#' @importFrom e1071 svm
#' @author Yifei Liu, Sean Kent
#' @keywords internal
misvm_heuristic_fit <- function(y, bags, X, c, rescale = TRUE, weights = NULL,
                                kernel = "radial", sigma = 1, max_step = 500, type = "C-classification",
                                scale = TRUE) {
  r <- .reorder(y, bags, X)
  y <- r$y
  bags <- r$b
  X <- r$X
  # if (rescale) X <- scale(X)

  # compute initial selection variables for the positively labeled bags as mean within each bag
  pos_bags <- unique(bags[y==1])
  if (ncol(X) == 1) {
    X_selected <- sapply(pos_bags, function(bag) { mean(X[bags == bag, ]) })
    X_selected <- as.matrix(X_selected)
  } else {
    X_selected <- t(sapply(pos_bags,
                           function(bag) { apply(X[bags == bag, , drop = FALSE], 2, mean) }))
  }

  past_selection <- matrix(NA, length(pos_bags), max_step+1)
  past_selection[, 1] <- rep(0, length(pos_bags))
  selection_changed <- TRUE
  n_selections = 0

  while (selection_changed & n_selections < max_step) {
    y_model <- c(rep(1, nrow(X_selected)), y[y == -1])
    b_model <- c(pos_bags, bags[y == -1])
    X_model <- rbind(X_selected,
                     X[y == -1, , drop = FALSE])

    # y_model <- c(y[y == -1], rep(1, nrow(X_selected)))
    # b_model <- c(bags[y == -1], pos_bags)
    # X_model <- rbind(X[y == -1, , drop = FALSE],
    #                  X_selected)

    model <- e1071::svm(x = X_model,
                        y = y_model,
                        class.weights = weights,
                        cost = c,
                        kernel = kernel,
                        gamma = sigma,
                        scale = rescale,
                        type = type)

    pred_all_inst <- predict(object = model, newdata = X, decision.values = TRUE)
    pred_all_score <- attr(pred_all_inst, "decision.values")

    # update selections and check for repeats
    selected <- sapply(pos_bags, function(bag) { which(pred_all_score == max(pred_all_score[bags == bag]))[1] })
    n_selections = n_selections + 1
    # selection_changed <- !identical(X_selected, X[selected, , drop = FALSE])
    for (i in 1:n_selections) {
      # cat("Selected:", selected)
      selection_changed <- !all(selected == past_selection[, i])
      if (!selection_changed) {
        break
      }
      # cat("n_selections", n_selections, "Selection changed", selection_changed, "selection", selected, "\n")
    }
    if (selection_changed) {
      X_selected <- X[selected, , drop = FALSE]
      past_selection[, (n_selections+1)] <- selected
    }
  }
  if (n_selections == max_step) {
    message = paste0("Number of iterations of heuristic algorithm reached threshold of ", max_step, ". Stopping with current selection.")
    warning(message)
    cat(message, "Value of c is ", c, "\n")
  }

  # vector representing selected positive instances
  selected_vec <- rep(0, length(y))
  selected_vec[selected] <- 1
  selected_vec <- selected_vec[order(r$order)] # un-order the vector

  return(list(
    model = model,
    total_step = n_selections,
    representative_inst = selected_vec
  ))
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
#'   - `model`: a list with components:
#'     - `w`: weights to apply to the predictors to make the classifier
#'     - `b`: intercept term used to make the classifier
#'     - `xi`: slack variables returned from the optimization
#'     - `status`: solution status from `gurobi::gurobi`
#'     - `itercount`: itercount from `gurobi::gurobi`
#'     - `baritercount`: baritercount from `gurobi::gurobi`
#'     - `objval`: value of the objective at the solution
#'     - `c`: value of the cost parameter used in solving the optimization
#'     - `n_selections`: the number of selections used in fitting
#'   - `representative_inst`: NULL, TODO: mesh with other misvm method
#'   - `traindata`: NULL, TODO: mesh with other misvm method
#'   - `useful_inst_idx`: NULL, TODO: mesh with other misvm method
#'
#' @author Sean Kent
#' @keywords internal
misvm_qpheuristic_fit <- function(y, bags, X, c, rescale = TRUE, weights = NULL,
                                  verbose = FALSE, time_limit = FALSE, max_step = 500) {
  r <- .reorder(y, bags, X)
  y <- r$y
  bags <- r$b
  X <- r$X
  if (rescale) X <- scale(X)

  # compute initial selection variables for the positively labeled bags as mean within each bag
  pos_bags <- unique(bags[y==1])
  if (ncol(X) == 1) {
    X_selected <- sapply(pos_bags, function(bag) { mean(X[bags == bag, ]) })
    X_selected <- as.matrix(X_selected)
  } else {
    X_selected <- t(sapply(pos_bags,
                           function(bag) { apply(X[bags == bag, , drop = FALSE], 2, mean) }))
  }

  params <- list()
  params$OutputFlag = 1*verbose
  params$IntFeasTol = 1e-5
  if (time_limit) params$TimeLimit = time_limit

  selection_changed <- TRUE
  itercount = 0
  baritercount = 0
  n_selections = 0

  while (selection_changed & n_selections < max_step) {
    y_model <- c(y[y == -1], rep(1, nrow(X_selected)))
    b_model <- c(bags[y == -1], pos_bags)
    X_model <- rbind(X[y == -1, , drop = FALSE],
                     X_selected)
    model <- misvm_qpheuristic_model(y_model, b_model, X_model, c, weights)
    gurobi_result <- gurobi::gurobi(model, params = params)

    w <- gurobi_result$x[grepl("w", model$varnames)]
    b_ <- gurobi_result$x[grepl("b", model$varnames)]
    f <- as.matrix(X) %*% w + b_
    itercount <- itercount + gurobi_result$itercount
    baritercount <- baritercount + gurobi_result$baritercount

    selected <- sapply(pos_bags, function(bag) { which(f == max(f[bags == bag]))[1] })
    selection_changed <- !identical(X_selected, X[selected, , drop = FALSE])
    if (selection_changed) {
      X_selected <- X[selected, , drop = FALSE]
      n_selections = n_selections + 1
    }
  }
  if (n_selections == max_step) {
    message = paste0("Number of iterations of heuristic algorithm reached threshold of ", max_step, ". Stopping with current selection.")
    warning(message)
    cat(message, "Value of c is ", c, "\n")
  }

  if (rescale) {
    b_ <- b_ - sum(attr(X, "scaled:center") * w / attr(X, "scaled:scale"))
    w <- w / attr(X, "scaled:scale")
  }
  # vector representing selected positive instances
  selected_vec <- rep(0, length(y))
  selected_vec[selected] <- 1
  selected_vec <- selected_vec[order(r$order)] # un-order the vector

  res <- list(
    model = list(
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
    representative_inst = selected_vec,
    traindata = NULL,
    useful_inst_idx = NULL
  )
  names(res$model$w) <- colnames(X)

  return(new_misvm(res, method = "qp-heuristic"))
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
#' @keywords internal
misvm_qpheuristic_model <- function(y, bags, X, c, weights = NULL) {
  # assumes that the data is already re-ordered to save time

  ## Build constraint matrix
  # order of variables is [w, b, xi]
  n_w <- ncol(X)
  n_b <- 1
  n_xi <- length(unique(bags))

  w_constraint <- y*X
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

  ## Objective
  model$modelsense <- "min"
  model$obj <- c(rep(0, n_w + n_b), c_vec) # linear portion of objective
  model$Q <- diag(c(rep(1, n_w), rep(0, n_b + n_xi))) # quadratic portion of objective
  ## Constraints
  model$varnames <- c(paste0("w",1:n_w), "b", paste0("xi",1:n_xi))
  model$A <- constraint
  model$sense <- rep(">=", nrow(X))
  model$rhs <- rep(1, nrow(X))
  model$lb <- c(rep(-Inf, n_w + n_b), rep(0, n_xi))

  return(model)
}
