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
                                          # nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random'),
                                          max_step = 500,
                                          type = "C-classification",
                                          scale = TRUE,
                                          verbose = FALSE,
                                          time_limit = 60
                                          # start = FALSE
                           ),
                           ...)
{

  method <- match.arg(method)
  if ("kernel" %ni% names(control)) control$kernel <- "linear"
  if ("sigma" %ni% names(control)) control$sigma <- if (is.vector(x)) 1 else 1 / ncol(x)
  # if ("nystrom_args" %ni% names(control)) control$nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random')
  if ("max_step" %ni% names(control)) control$max_step <- 500
  if ("type" %ni% names(control)) control$type <- "C-classification"
  if ("scale" %ni% names(control)) control$scale <- TRUE
  if ("verbose" %ni% names(control)) control$verbose <- FALSE
  if ("time_limit" %ni% names(control)) control$time_limit <- 60
  # if ("start" %ni% names(control)) control$start <- FALSE
  #
  # if (control$start && control$kernel != "linear") {
  #   control$start <- FALSE
  #   rlang::inform(c(
  #     "Warm start is not available when `kernel` is not equal to 'linear'.",
  #     i = "Setting `start` = FALSE. "
  #   ))
  # }
  # store the levels of y and convert to 0,1 numeric format.
  y_info <- .convert_y_ordinal(y)
  y <- y_info$y
  lev <- y_info$lev

  # store colnames of x
  col_x <- colnames(x)

  # weights
  if (!is.null(weights)) {
    weights <- NULL
    warning("Weights are not currently implemented for `omisvm()`.")
  }
  # if (is.numeric(weights)) {
  #   stopifnot(names(weights) == lev | names(weights) == rev(lev))
  #   weights <- weights[lev]
  #   names(weights) <- c("-1", "1")
  # } else if (weights) {
  #   bag_labels <- sapply(split(y, factor(bags)), unique)
  #   weights <- c("-1" = sum(bag_labels == 1) / sum(y == 0), "1" = 1)
  # } else {
  #   weights <- NULL
  # }

  # Nystrom approximation to x for mip and qp-heuristic methods
  # NOTE: this isn't strictly necessary for qp-heuristic, but it's the easiest way to implement
  # if (method %in% c("mip") & control$kernel == "radial") {
  #   control$nystrom_args$df <- as.matrix(x)
  #   control$nystrom_args$kernel <- control$kernel
  #   control$nystrom_args$sigma <- control$sigma
  #   kfm_fit <- do.call(kfm_nystrom, args = control$nystrom_args)
  #
  #   x <- build_fm(kfm_fit, x)
  # }

  # if (method == "heuristic") {
  #   data <- cbind(bag_label = y,
  #                 bag_name = bags,
  #                 instance_name = as.character(1:length(y)),
  #                 x)
  #   y = 2*y - 1 # convert {0,1} to {-1, 1}
  #   res <- misvm_heuristic_fit(y, bags, x,
  #                              c = cost,
  #                              rescale = control$scale,
  #                              weights = weights,
  #                              kernel = control$kernel,
  #                              sigma = control$sigma,
  #                              max_step = control$max_step,
  #                              type = control$type,
  #                              scale = control$scale)
  # } else if (method == "mip") {
  #   y = 2*y - 1 # convert {0,1} to {-1, 1}
  #   res <- misvm_mip_fit(y, bags, x,
  #                        c = cost,
  #                        rescale = control$scale,
  #                        weights = weights,
  #                        verbose = control$verbose,
  #                        time_limit = control$time_limit,
  #                        start = control$start)
  # } else
  #
  if (method == "qp-heuristic") {
    # y = 2*y - 1 # convert {0,1} to {-1, 1}
    res <- omisvm_qpheuristic_fit(y, bags, x,
                                  c = cost,
                                  h = h,
                                  rescale = control$scale,
                                  weights = weights,
                                  verbose = control$verbose,
                                  time_limit = control$time_limit,
                                  max_step = control$max_step)
  } else {
    stop("omisvm requires method = 'qp-heuristic'.")
  }

  out <- res[1]
  # if (method %in% c("mip") & control$kernel == "radial") {
  #   out$kfm_fit <- kfm_fit
  # }
  out$call_type <- "misvm.default"
  out$x <- res$x
  out$features <- col_x
  out$levels <- lev
  out$cost <- cost
  out$h <- h
  out$weights <- weights
  out$repr_inst <- res$repr_inst
  out$n_step <- res$n_step
  out$x_scale <- res$x_scale
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

  # if (object$call_type == "misvm.mild_df") {
  #   mil_cols <- c("bag_label", "bag_name", "instance_name")
  #   mil_info <- new_data[, mil_cols, drop = FALSE]
  #   new_data <- summarize_samples(new_data,
  #                                 group_cols = mil_cols,
  #                                 .fns = object$summary_fns,
  #                                 cor = object$summary_cor)
  # }

  if (object$call_type == "omisvm.formula") {
    new_x <- x_from_mi_formula(object$formula, new_data)
  } else {
    new_x <- new_data[, object$features, drop = FALSE]
  }
  # if ("kfm_fit" %in% names(object)) {
  #   new_x <- build_fm(object$kfm_fit, as.matrix(new_x))
  # }
  # if (method == "qp-heuristic" & "x_scale" %in% names(object)) {
  #   new_x <- as.data.frame(scale(new_x, center = object$x_scale$center, scale = object$x_scale$scale))
  # }

  # kernel
  # if (method == "qp-heuristic") {
  #   kernel <- compute_kernel(as.matrix(new_x),
  #                            object$gurobi_fit$xmatrix,
  #                            type = object$gurobi_fit$kernel,
  #                            sigma = object$gurobi_fit$sigma)
  # }

  # if (method == "heuristic") {
  #   pos <- predict(object = object$svm_fit, newdata = new_x, decision.values = TRUE)
  #   scores <- attr(pos, "decision.values")
  #   pos <- as.numeric(as.character(pos))
  #
  # } else if (method == "mip") {
  #   scores <- as.matrix(new_x) %*% object$gurobi_fit$w + object$gurobi_fit$b
  #   pos <- 2*(scores > 0) - 1
  #
  # } else if (method == "qp-heuristic") {
  #   scores <- kernel %*% object$gurobi_fit$ay + object$gurobi_fit$b
  #   pos <- 2*(scores > 0) - 1
  if (method == "qp-heuristic") {
    scores <- as.matrix(new_x) %*% object$gurobi_fit$w
    scores_matrix <- outer(as.vector(scores), object$gurobi_fit$b, `+`)
    class_ <- rowSums(scores_matrix > 0) + 1
  } else {
    stop("predict.misvm requires method = 'qp-heuristic'.")
  }

  if (layer == "bag") {
    if (object$call_type == "omisvm.formula" & new_bags[1] == "bag_name" & length(new_bags) == 1) {
      new_bags <- object$bag_name
    }
    if (length(new_bags) == 1 & new_bags[1] %in% colnames(new_data)) {
      bags <- new_data[[new_bags]]
    } else {
      bags <- new_bags
    }
    scores <- classify_bags(scores, bags, condense = FALSE)
    # scores_matrix <- apply(scores_matrix, 2, classify_bags, bags = bags, condense = FALSE)
    # colnames(scores_matrix) <- paste0(".pred_", )
    class_ <- classify_bags(class_, bags, condense = FALSE)
  }
  class_ <- factor(class_, levels = 1:length(object$levels), labels = object$levels)

  res <- switch(type,
                "raw" = tibble::tibble(.pred = as.numeric(scores)),
                "class" = tibble::tibble(.pred_class = class_))

  # if (object$call_type == "misvm.mild_df") {
  #   # bring back the predictions from instance level to the sample level
  #   ind <- match(mil_info$instance_name, new_data$instance_name)
  #   res <- res[ind, , drop = FALSE]
  # }
  # TODO: consider returning the AUC here as an attribute.  Can only do if we have the true bag labels
  # attr(res, "AUC") <- calculated_auc
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


