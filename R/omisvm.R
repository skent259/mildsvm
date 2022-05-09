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
#' the reserach method proposed by Kent and Yu.
#'
#' Currently, the only method available is a heuristic algorithm in linear SVM
#' space. Additional methods should be available shortly.
#'
#' @inheritParams misvm
#' @param x A data.frame, matrix, or similar object of covariates, where each
#'   row represents a sample.
#' @param h A scalar that controls the trade-off between maximizing the margin
#'   and minimizing distance between hyperplanes.
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `omisvm.`  The object contains at least the
#'   following components:
#'   * `*_fit`: A fit object depending on the `method` parameter.  If `method =
#'   'qp-heuristic'` this will be `gurobi_fit` from a model optimization.
#'   * `call_type`: A character indicating which method `omisvm()` was
#'   called with.
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
#' set.seed(8)
#' # make some data
#' n <- 500
#' y <- sample(1:5, size = n, prob = (1 / 1:5)^2, replace = TRUE)
#' bags <- rep(1:(n/5), each = 5)
#' X <- matrix(NA, nrow = length(y), ncol = 5)
#' for (y_ in unique(y)) {
#'   to_fill <- which(y_ == y)
#'   X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(2*y_, -1*y_, 1*y_, 0, 0))
#' }
#' colnames(X) <- paste0("V", 1:ncol(X))
#' y <- classify_bags(y, bags, condense = FALSE)
#'
#' mdl1 <- omisvm(X, y, bags, weights = NULL)
#' predict(mdl1, X, new_bags = bags)
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
omisvm.default <- function(x, y, bags,
                           cost = 1,
                           h = 1,
                           method = c("qp-heuristic"),
                           weights = TRUE,
                           control = list(kernel = "linear",
                                          sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                                          max_step = 500,
                                          type = "C-classification",
                                          scale = TRUE,
                                          verbose = FALSE,
                                          time_limit = 60
                           ),
                           ...)
{

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
  # x_info <- .convert_x(x, FALSE)
  x <- x_info$x
  col_x <- x_info$col_x
  x_scale <- x_info$x_scale

  weights <- .warn_no_weights(weights, "omisvm")

  if (method == "qp-heuristic") {

    # Perform data replication on `y`, `bags`, `kernel`, `x`
    y <- .y_datarep(y, k)
    bags <- .bags_datarep(bags, k)
    kernel <- .convert_kernel(x, control$kernel, sigma = control$sigma)
    kernel <- .kernel_datarep(kernel, k, h)
    x <- .x_datarep(x, k, h)

    # browser()
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
    # res <- omisvm_qpheuristic_fit(y, bags, x,
    #                               c = cost,
    #                               h = h,
    #                               rescale = control$scale,
    #                               weights = weights,
    #                               verbose = control$verbose,
    #                               time_limit = control$time_limit,
    #                               max_step = control$max_step)
  }

  # browser()
  out <- res[1]
  out$call_type <- "misvm.default"
  out$x <- res$x
  out$features <- col_x
  out$levels <- lev
  out$cost <- cost
  out$h <- h
  out$weights <- weights
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
#' set.seed(8)
#' # make some data
#' n <- 50
#' y <- sample(1:5, size = n, prob = (1 / 1:5)^2, replace = TRUE)
#' bags <- rep(1:(n/5), each = 5)
#' X <- matrix(NA, nrow = length(y), ncol = 5)
#' for (y_ in unique(y)) {
#'   to_fill <- which(y_ == y)
#'   X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(2*y_, -1*y_, 1*y_, 0, 0))
#' }
#' colnames(X) <- paste0("V", 1:ncol(X))
#' y <- classify_bags(y, bags, condense = FALSE)
#'
#' mdl1 <- omisvm(X, y, bags, weights = NULL)
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df1 <- bind_cols(y = y, bags = bags, as.data.frame(X))
#' df1 %>%
#'   bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
#'   bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
#'   distinct(y, bags, .pred_class, .pred)
#'
#' @export
#' @author Sean Kent
predict.omisvm <- function(object,
                           new_data,
                           type = c("class", "raw"),
                           layer = c("bag", "instance"),
                           new_bags = "bag_name",
                           ...)
{
  type <- match.arg(type, c("class", "raw"))
  layer <- match.arg(layer, c("bag", "instance"))
  method <- attr(object, "method")

  k <- length(object$lev)
  h <- object$h


  new_x <- .get_new_x(object, new_data)
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
  class_ <- rowSums(scores_matrix > 0) + 1

  if (layer == "bag") {
    bags <- .get_bags(object, new_data, new_bags)
    scores <- classify_bags(scores, bags, condense = FALSE)
    class_ <- classify_bags(class_, bags, condense = FALSE)
  }
  class_ <- factor(class_, levels = seq_along(object$levels), labels = object$levels)

  res <- switch(
    type,
    "raw" = tibble::tibble(.pred = as.numeric(scores)),
    "class" = tibble::tibble(.pred_class = class_)
  )

  attr(res, "layer") <- layer
  res
}


# Specific implementation methods below ----------------------------------------

omisvm_qpheuristic_model <- function(y, bags, X, Xs, c, h, weights = NULL) {

  K <- max(y) # assumes that y contains values from 1, ... K
  n_bags <- length(unique(bags))
  yb <- classify_bags(y, bags)

  # create versions of x, y, bags corresponding to the number of constraints
  x_ <- y_ <- bags_ <- q_ <- list()
  for (q in 1:(K-1)) {
    ind1 <- y <= q  # similar constraint to y_I = -1, need all i in I
    ind2 <- yb > q  # similar constraint to y_I = +1, need only s(I)

    x_[[q]] <- rbind(X[ind1, , drop = FALSE], Xs[ind2, , drop = FALSE])
    y_[[q]] <- c(y[ind1], yb[ind2])
    bags_[[q]] <- c(bags[ind1], unique(bags)[ind2])
    q_[[q]] <- rep(q, sum(ind1) + sum(ind2))
  }
  y_ <- unlist(y_)
  bags_ <- unlist(bags_)
  q_ <- unlist(q_)

  # Build constraint matrix
  # order of variables is [w, b, xi]
  n_w <- ncol(X)
  n_b <- K-1
  n_xi <- (K-1) * n_bags
  sgn <- 2*(y_ > q_) - 1
  # w constraints (n_col(X) columns)
  w_constraint <- sgn * do.call(rbind, x_)
  # b constraints (K-1 columns)
  b_blocks <- lapply(1:(K-1), function(q) sgn[q_ == q])
  b_constraint <- Matrix::bdiag(b_blocks)
  # xi_constraints ((K-1) * n_bags columns)
  xi_col <- function(b, n_xi) {
    # puts a 1 in the `b`th entry of a n_xi-length 0 vector
    vec <- rep(0, n_xi)
    vec[b] <- 1
    return(vec)
  }
  xi_blocks <- lapply(1:(K-1), function(q) {
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
  w_Q <- diag(rep(1, n_w))
  # sum_{q=2}^{K-1} (b_q - b1)^2
  b_Q <- matrix(0, n_b, n_b)
  diag(b_Q) <- 1
  b_Q[1, ] <- -1
  b_Q[, 1] <- -1
  b_Q[1, 1] <- K-2
  # no terms
  xi_Q <- diag(rep(0, n_xi))

  Q <- Matrix::bdiag(1/2 * w_Q, 1 / (2*h^2) * b_Q, xi_Q)

  model <- list()
  # Objective
  model$modelsense <- "min"
  model$obj <- c(rep(0, n_w + n_b), c_vec) # linear portion of objective
  model$Q <- Q # quadratic portion of objective
  # Constraints
  model$varnames <- c(paste0("w",1:n_w), paste0("b",1:n_b), paste0("xi",1:n_xi))
  model$A <- constraint
  model$sense <- rep(">=", nrow(constraint))
  model$rhs <- rep(1, nrow(constraint))
  model$lb <- c(rep(-Inf, n_w + n_b), rep(0, n_xi))

  return(model)
}

omisvm_qpheuristic_fit <- function(y, bags, X, c, h, rescale = TRUE, weights = NULL,
                                   verbose = FALSE, time_limit = FALSE, max_step = 500) {
  r <- .reorder(y, bags, X)
  y <- r$y
  bags <- r$b
  X <- r$X
  if (rescale) X <- scale(X)

  # Compute initial selections for all bags as mean within that bag
  # TODO: evaluate whether this is a smart choice in the ordinal procedure
  if (ncol(X) == 1) {
    X_selected <- sapply(unique(bags), function(bag) { mean(X[bags == bag, ]) })
    X_selected <- as.matrix(X_selected)
  } else {
    X_selected <- t(sapply(unique(bags),
                           function(bag) { apply(X[bags == bag, , drop = FALSE], 2, mean) }))
  }

  # # Alternatively, can just pick a random instance
  # selected <- sapply(unique(bags), function(bag) { sample(which(bags == bag), 1) })
  # X_selected <- X[selected, , drop = FALSE]

  params <- list()
  params$OutputFlag = 1*verbose
  params$IntFeasTol = 1e-5
  if (time_limit) params$TimeLimit = time_limit

  selection_changed <- TRUE
  itercount = 0
  baritercount = 0
  n_selections = 0

  while (selection_changed & n_selections < max_step) {
    model <- omisvm_qpheuristic_model(y, bags, X, X_selected, c, h, weights)
    gurobi_result <- gurobi::gurobi(model, params = params)

    w <- gurobi_result$x[grepl("w", model$varnames)]
    b_ <- gurobi_result$x[grepl("b", model$varnames)]
    f <- as.matrix(X) %*% w # + b_
    itercount <- itercount + gurobi_result$itercount
    baritercount <- baritercount + gurobi_result$baritercount

    selected <- sapply(unique(bags), function(bag) {
      which(f == max(f[bags == bag]))[1]
    })
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

  if (rescale) { # TODO: edit this to work
    b_ <- b_ - sum(attr(X, "scaled:center") * w / attr(X, "scaled:scale"))
    w <- w / attr(X, "scaled:scale")
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
      "center" = attr(X, "scaled:center"),
      "scale" = attr(X, "scaled:scale")
    )
  )
  names(res$gurobi_fit$w) <- colnames(X)
  if (!rescale) res$x_scale <- NULL
  return(res)
}


