new_omisvm <- function(x = list(), method = c("qp-heuristic")) {
  stopifnot(is.list(x))
  method <- match.arg(method)
  structure(
    x,
    class = "omisvm",
    method = method
  )
}

validate_omisvm <- function(x) {
  message("No validations currently in place for object of class 'omisvm'.")
  x
}

#' Fit MI-SVM-OR model to ordinal outcome data
#'
#' This function fits a modification of MI-SVM to ordinal outcome data based on
#' the research method proposed by Kent and Yu.
#'
#' Currently, the only method available is a heuristic algorithm in linear SVM
#' space. Additional methods should be available shortly.
#'
#' @inheritParams misvm
#' @param x A data.frame, matrix, or similar object of covariates, where each
#'   row represents an instance. If a `mi_df` object is passed, `y, bags` are
#'   automatically extracted, and all other columns will be used as predictors.
#' @param h A scalar that controls the trade-off between maximizing the margin
#'   and minimizing distance between hyperplanes.
#' @param s An integer for how many replication points to add to the dataset. If
#'   `k` represents the number of labels in y, must have `1 <= s <= k-1`. The
#'   default, `Inf`, uses the maximum number of replication points, `k-1`.
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `omisvm.`  The object contains at least the
#'   following components:
#'   * `*_fit`: A fit object depending on the `method` parameter.  If `method =
#'   'qp-heuristic'` this will be `gurobi_fit` from a model optimization.
#'   * `call_type`: A character indicating which method `omisvm()` was called
#'   with.
#'   * `features`: The names of features used in training.
#'   * `levels`: The levels of `y` that are recorded for future prediction.
#'   * `cost`: The cost parameter from function inputs.
#'   * `weights`: The calculated weights on the `cost` parameter.
#'   * `repr_inst`: The instances from positive bags that are selected to be
#'   most representative of the positive instances.
#'   * `n_step`: If `method == 'qp-heuristic'`, the total steps used in the
#'   heuristic algorithm.
#'   * `x_scale`: If `scale = TRUE`, the scaling parameters for new predictions.
#'
#' @seealso [predict.omisvm()] for prediction on new data.
#'
#' @examples
#' if (require(gurobi)) {
#'   data("ordmvnorm")
#'   x <- ordmvnorm[, 3:7]
#'   y <- ordmvnorm$bag_label
#'   bags <- ordmvnorm$bag_name
#'
#'   mdl1 <- omisvm(x, y, bags, weights = NULL)
#'   predict(mdl1, x, new_bags = bags)
#' }
#'
#' @author Sean Kent
#' @name omisvm
NULL

#' @export
omisvm <- function(x, ...) {
  UseMethod("omisvm")
}

#' @describeIn omisvm Method for data.frame-like objects
#' @export
omisvm.default <- function(
    x,
    y,
    bags,
    cost = 1,
    h = 1,
    s = Inf,
    method = c("qp-heuristic"),
    weights = TRUE,
    control = list(kernel = "linear",
                   sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                   max_step = 500,
                   type = "C-classification",
                   scale = TRUE,
                   verbose = FALSE,
                   time_limit = 60),
    ...) {

  method <- match.arg(method, c("qp-heuristic"))

  defaults <- list(
    kernel = "linear",
    sigma = if (is.vector(x)) 1 else 1 / ncol(x),
    max_step = 500,
    type = "C-classification",
    scale = TRUE,
    verbose = FALSE,
    time_limit = 60
  )
  control <- .set_default(control, defaults)
  control <- .set_scale(control)

  # store the levels of y and convert to numeric format.
  y_info <- .convert_y_ordinal(y)
  y <- y_info$y
  lev <- y_info$lev
  k <- length(lev)

  # remove NaN columns and columns with no variance, store col names, scale
  x_info <- .convert_x(x, control$scale)
  x <- x_info$x
  col_x <- x_info$col_x
  x_scale <- x_info$x_scale

  s <- .warn_omisvm_s(s, k, method, control$kernel)

  if (method == "qp-heuristic" && control$kernel == "linear") {
    weights <- .warn_no_weights(weights, "omisvm")
    res <- omisvm_qpheuristic_fit(y, bags, x,
                                  c = cost,
                                  h = h,
                                  rescale = control$scale,
                                  weights = weights,
                                  verbose = control$verbose,
                                  time_limit = control$time_limit,
                                  max_step = control$max_step)
  } else if (method == "qp-heuristic") {
    # must reorder before fitting to ensure data order doesn't affect results
    r <- .reorder(y, bags, x)
    y <- r$y
    bags <- r$b
    x <- r$X

    # Perform data replication on `y`, `bags`, `kernel`, `x`
    ind <- .include_datarep(y, s, k)
    y <- .y_datarep(y, k)[ind]
    bags <- .bags_datarep(bags, k)[ind, , drop = FALSE]
    kernel <- .convert_kernel(x, control$kernel, sigma = control$sigma)
    kernel <- .kernel_datarep(kernel, k, h)[ind, ind, drop = FALSE]
    x <- .x_datarep(x, k, h)[ind, , drop = FALSE]

    weights <- .set_weights(weights, list(y = y > 0, lev = NULL), bags$.repl)

    # Run misvm on replicated data, passing in kernel for faster computation
    res <- misvm_dualqpheuristic_fit(y, bags$.repl, x,
                                     c = cost,
                                     rescale = FALSE,
                                     weights = weights,
                                     kernel = kernel,
                                     sigma = control$sigma,
                                     verbose = control$verbose,
                                     time_limit = control$time_limit,
                                     max_step = control$max_step)
    res$gurobi_fit$kernel <- control$kernel
  }

  out <- res[1]
  out$call_type <- "misvm.default"
  out$x <- res$x
  out$features <- col_x
  out$levels <- lev
  out$cost <- cost
  out$h <- h
  out$s <- s
  out$weights <- weights
  out$kernel <- control$kernel
  out$kernel_param <- switch(
    out$kernel,
    "radial" = list("sigma" = control$sigma),
    "linear" = NULL,
  )
  out$repr_inst <- res$repr_inst
  out$n_step <- res$n_step
  out$x_scale <- x_scale
  new_omisvm(out, method = method)
}

#' @describeIn omisvm Method for passing formula
#' @export
omisvm.formula <- function(formula, data, ...) {
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

  res <- omisvm.default(x, y, bags, ...)

  res$call_type <- "omisvm.formula"
  res$formula <- formula
  res$bag_name <- bag_name
  return(res)
}

#' @describeIn omisvm Method for `mi_df` objects, automatically handling bag
#'   names, labels, and all covariates.
#' @export
omisvm.mi_df <- function(x, ...) {
  x <- as.data.frame(validate_mi_df(x))
  y <- x$bag_label
  bags <- x$bag_name
  x$bag_label <- x$bag_name <- NULL

  res <- omisvm.default(x, y, bags, ...)
  res$call_type <- "omisvm.mi_df"
  res$bag_name <- "bag_name"
  return(res)
}

#' Predict method for `omisvm` object
#'
#' @details
#' When the object was fitted using the `formula` method, then the parameters
#' `new_bags` and `new_instances` are not necessary, as long as the names match
#' the original function call.
#'
#' @param object An object of class `omisvm`
#' @inheritParams predict.misvm
#'
#' @return A tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column `.pred_class`.  If `type = 'raw'`, the tibble will have
#'   a column `.pred`.
#'
#' @seealso [omisvm()] for fitting the `omisvm` object.
#'
#' @examples
#' if (require(gurobi)) {
#'   data("ordmvnorm")
#'   x <- ordmvnorm[, 3:7]
#'   y <- ordmvnorm$bag_label
#'   bags <- ordmvnorm$bag_name
#'
#'   mdl1 <- omisvm(x, y, bags, weights = NULL)
#'
#'   # summarize predictions at the bag layer
#'   library(dplyr)
#'   df1 <- bind_cols(y = y, bags = bags, as.data.frame(x))
#'   df1 %>%
#'     bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
#'     bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
#'     distinct(y, bags, .pred_class, .pred)
#' }
#'
#' @export
#' @author Sean Kent
predict.omisvm <- function(object,
                           new_data,
                           type = c("class", "raw"),
                           layer = c("bag", "instance"),
                           new_bags = "bag_name",
                           ...) {
  type <- match.arg(type, c("class", "raw"))
  layer <- match.arg(layer, c("bag", "instance"))
  method <- attr(object, "method")
  if (!is.null(new_data)) new_data <- as.data.frame(new_data)

  k <- length(object$lev)
  h <- object$h
  new_x <- .get_new_x(object, new_data)

  if (method == "qp-heuristic" && object$gurobi_fit$kernel == "linear") {
    scores <- as.matrix(new_x) %*% object$gurobi_fit$w
    scores_matrix <- outer(as.vector(scores), object$gurobi_fit$b, `+`)

  } else if (method == "qp-heuristic") {
    new_x_dr <- .x_datarep(new_x, k, h)
    kernel_dr <- .compute_kernel_datarep(
      as.matrix(new_x_dr),
      object$gurobi_fit$xmatrix,
      k = k,
      h = h,
      type = object$gurobi_fit$kernel,
      sigma = object$gurobi_fit$sigma
    )

    scores_dr <- kernel_dr %*% object$gurobi_fit$ay + object$gurobi_fit$b
    scores_matrix <- matrix(scores_dr, nrow = nrow(new_x), ncol = k-1)
    scores <- scores_matrix[, 1, drop = TRUE]
  }
  class_ <- rowSums(scores_matrix > 0) + 1


  if (layer == "bag") {
    bags <- .get_bags(object, new_data, new_bags)
    scores <- classify_bags(scores, bags, condense = FALSE)
    class_ <- classify_bags(class_, bags, condense = FALSE)
  }
  class_ <- factor(class_, levels = seq_along(object$levels), labels = object$levels)

  res <- .pred_output(type, scores, class_)
  attr(res, "layer") <- layer
  res
}

#' @export
print.omisvm <- function(x, digits = getOption("digits"), ...) {
  method <- attr(x, "method")
  kernel_param <- .get_kernel_param_str(x, digits)
  weights <- .get_weights_str(x)

  cat("An misvm object called with", x$call_type, "\n")
  cat("", "\n")
  cat("Parameters:", "\n")
  cat("  method:", method, "\n")
  cat("  kernel:", x$kernel, kernel_param, "\n")
  cat("  cost:", x$cost, "\n")
  cat("  h:", x$h, "\n")
  cat("  s:", x$s, "\n")
  cat("  scale:", !is.null(x$x_scale), "\n")
  cat("  weights:", weights, "\n")
  cat("", "\n")
  cat("Model info:", "\n")
  cat("  Levels of `y`:")
  utils::str(x$levels, width = getOption("width")-14)
  cat("  Features:")
  utils::str(x$features, width = getOption("width")-14)
  cat("  Number of iterations:", x$gurobi_fit$n_selections, "\n")
  cat("\n")
}

# Specific implementation methods below ----------------------------------------

omisvm_qpheuristic_model <- function(y, bags, x, x_s, c, h, weights = NULL) {

  k <- max(y) # assumes that y contains values from 1, ... K
  n_bags <- length(unique(bags))
  yb <- classify_bags(y, bags)

  # create versions of x, y, bags corresponding to the number of constraints
  x_ <- y_ <- bags_ <- q_ <- list()
  for (q in 1:(k-1)) {
    ind1 <- y <= q  # similar constraint to y_I = -1, need all i in I
    ind2 <- yb > q  # similar constraint to y_I = +1, need only s(I)

    x_[[q]] <- rbind(x[ind1, , drop = FALSE], x_s[ind2, , drop = FALSE])
    y_[[q]] <- c(y[ind1], yb[ind2])
    bags_[[q]] <- c(bags[ind1], unique(bags)[ind2])
    q_[[q]] <- rep(q, sum(ind1) + sum(ind2))
  }
  y_ <- unlist(y_)
  bags_ <- unlist(bags_)
  q_ <- unlist(q_)

  # Build constraint matrix
  # order of variables is [w, b, xi]
  n_w <- ncol(x)
  n_b <- k-1
  n_xi <- (k-1) * n_bags
  sgn <- 2 * (y_ > q_) - 1
  # w constraints (n_col(X) columns)
  w_constraint <- sgn * do.call(rbind, x_)
  # b constraints (K-1 columns)
  b_blocks <- lapply(1:(k-1), function(q) sgn[q_ == q])
  b_constraint <- Matrix::bdiag(b_blocks)
  # xi_constraints ((K-1) * n_bags columns)
  xi_col <- function(b, n_xi) {
    # puts a 1 in the `b`th entry of a n_xi-length 0 vector
    vec <- rep(0, n_xi)
    vec[b] <- 1
    return(vec)
  }
  xi_blocks <- lapply(1:(k-1), function(q) {
    t(sapply(bags_[q_ == q], xi_col, n_xi = n_bags))
  })
  xi_constraint <- Matrix::bdiag(xi_blocks)

  constraint <- cbind(as.matrix(w_constraint), b_constraint, xi_constraint)

  # TODO: allow for weights on different classes
  if (is.null(weights)) {
    c_vec <- rep(c, n_xi)
  }

  # Build quadratic objective
  # ||w||^2
  w_q_mat <- diag(rep(1, n_w))
  # sum_{q=2}^{K-1} (b_q - b1)^2
  b_q_mat <- matrix(0, n_b, n_b)
  diag(b_q_mat) <- 1
  b_q_mat[1, ] <- -1
  b_q_mat[, 1] <- -1
  b_q_mat[1, 1] <- k-2
  # no terms
  xi_q_mat <- diag(rep(0, n_xi))

  q_mat <- Matrix::bdiag(1/2 * w_q_mat, 1 / (2*h^2) * b_q_mat, xi_q_mat)

  model <- list()
  # Objective
  model[["modelsense"]] <- "min"
  model[["obj"]] <- c(rep(0, n_w + n_b), c_vec) # linear portion of objective
  model[["Q"]] <- q_mat # quadratic portion of objective
  # Constraints
  model[["varnames"]] <- c(paste0("w", 1:n_w), paste0("b", 1:n_b), paste0("xi", 1:n_xi))
  model[["A"]] <- constraint
  model[["sense"]] <- rep(">=", nrow(constraint))
  model[["rhs"]] <- rep(1, nrow(constraint))
  model[["lb"]] <- c(rep(-Inf, n_w + n_b), rep(0, n_xi))

  return(model)
}

omisvm_qpheuristic_fit <- function(y,
                                   bags,
                                   x,
                                   c,
                                   h,
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

  # Compute initial selections for all bags as mean within that bag
  # TODO: evaluate whether this is a smart choice in the ordinal procedure
  if (ncol(x) == 1) {
    x_selected <- sapply(unique(bags), function(bag) {
      mean(x[bags == bag, ])
    })
    x_selected <- as.matrix(x_selected)
  } else {
    x_selected <- t(sapply(unique(bags),
                           function(bag) {
                             apply(x[bags == bag, , drop = FALSE], 2, mean)
                           }))
  }

  params <- .gurobi_params(verbose, time_limit)
  params[["PSDTol"]] <- NULL

  selection_changed <- TRUE
  itercount <- 0
  baritercount <- 0
  n_selections <- 0

  while (selection_changed && n_selections < max_step) {
    model <- omisvm_qpheuristic_model(y, bags, x, x_selected, c, h, weights)
    gurobi_result <- gurobi::gurobi(model, params = params)

    w <- gurobi_result$x[grepl("w", model$varnames)]
    b_ <- gurobi_result$x[grepl("b", model$varnames)]
    f <- as.matrix(x) %*% w
    itercount <- itercount + gurobi_result$itercount
    baritercount <- baritercount + gurobi_result$baritercount

    selected <- sapply(unique(bags), function(bag) {
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

  if (rescale) { # TODO: edit this to work
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
      kernel = "linear",
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
  if (!rescale) res$x_scale <- NULL
  return(res)
}


