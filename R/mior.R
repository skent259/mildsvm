new_mior <- function(x = list()) {
  stopifnot(is.list(x))
  structure(
    x,
    class = "mior"
  )
}

validate_mior <- function(x) {
  message("No validations currently in place for object of class 'mior'.")
  x
}

#' Fit MIOR model to the data
#'
#' This function fits the MIOR model, proposed by Xiao Y, Liu B, and Hao Z
#' (2018) in "Multiple-instance Ordinal Regression".  MIOR is a modified SVM
#' framework with parallel, ordered hyperplanes where the error terms are based
#' only on the instance closest to a midpoint between hyperplanes.
#'
#' Predictions (see [predict.mior()]) are determined by considering the smallest
#' distance from each point to the midpoint hyperplanes across all instances in
#' the bag.  The prediction corresponds to the hyperplane having such a minimal
#' distance.
#'
#' It appears as though an error in Equation (12) persists to the dual form in
#' (21). A corrected version of this dual formulation can be used with
#' `control$option = 'corrected'`, or the formulation as writted can be used
#' with `control$option = 'xiao'`.
#'
#'
#' @inheritParams misvm
#' @param cost_eta The additional cost parameter in MIOR which controls how far
#'   away the first and last separating hyperplanes are relative to other costs.
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   * `kernel` either a character the describes the kernel ('linear' or
#'   'radial') or a kernel matrix at the instance level.
#'   * `sigma` argument needed for radial basis kernel.
#'   * `max_step` argument used when `method = 'heuristic'`. Maximum steps of
#'   iteration for the heuristic algorithm.
#'   * `scale` argument used for all methods. A logical for whether to rescale
#'   the input before fitting.
#'   * `verbose` argument used when `method = 'mip'`. Whether to message output
#'   to the console.
#'   * `time_limit` argument used when `method = 'mip'`. `FALSE`, or a time
#'   limit (in seconds) passed to `gurobi()` parameters.  If `FALSE`, no time
#'   limit is given.
#'   * `option` argument the controls the constraint calculation.  See details.
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `mior`  The object contains at least the following
#'   components:
#'   * `gurobi_fit`: A fit from model optimization that includes relevant
#'   components.
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
#' @seealso [predict.misvm()] for prediction on new data.
#'
#' @examples
#' set.seed(8)
#' # make some data
#' n <- 15
#' X <- rbind(
#'   mvtnorm::rmvnorm(n/3, mean = c(4, -2, 0)),
#'   mvtnorm::rmvnorm(n/3, mean = c(0, 0, 0)),
#'   mvtnorm::rmvnorm(n/3, mean = c(-2, 1, 0))
#' )
#' score <- X %*% c(2, -1, 0)
#' y <- as.numeric(cut(score, c(-Inf, quantile(score, probs = 1:2 / 3), Inf)))
#' bags <- 1:length(y)
#'
#' # add in points outside boundaries
#' X <- rbind(
#'   X,
#'   mvtnorm::rmvnorm(n, mean = c(6, -3, 0)),
#'   mvtnorm::rmvnorm(n, mean = c(-6, 3, 0))
#' )
#' y <- c(y, rep(-1, 2*n))
#' bags <- rep(bags, 3)
#' repr <- c(rep(1, n), rep(0, 2*n))
#'
#' y_bag <- classify_bags(y, bags, condense = FALSE)
#'
#' mdl1 <- mior(X, y_bag, bags)
#' predict(mdl1, X, new_bags = bags)
#'
#' @author Sean Kent
#' @name mior
NULL

#' @export
mior <- function(x, ...) {
  UseMethod("mior")
}

#' @describeIn mior Method for data.frame-like objects
#' @export
mior.default <- function(x, y, bags,
                         cost = 1,
                         cost_eta = 1,
                         method = "qp-heuristic",
                         weights = NULL,
                         control = list(kernel = "linear",
                                        sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                                        max_step = 500,
                                        scale = TRUE,
                                        verbose = FALSE,
                                        time_limit = 60,
                                        option = c("corrected", "xiao")
                         ),
                         ...)
{
  method <- match.arg(method, c("qp-heuristic"))

  defaults <- list(
    kernel = "linear",
    sigma = if (is.vector(x)) 1 else 1 / ncol(x),
    max_step = 500,
    scale = TRUE,
    verbose = FALSE,
    time_limit = 60,
    option = "corrected"
  )
  control <- .set_default(control, defaults)
  control$option <- match.arg(control$option, c("corrected", "xiao"))

  # store the levels of y and convert to 0,1 numeric format.
  y_info <- .convert_y_ordinal(y)
  y <- y_info$y
  lev <- y_info$lev

  # store colnames of x
  x <- as.data.frame(x)
  col_x <- colnames(x)

  # weights
  if (!is.null(weights)) {
    weights <- NULL
    warning("Weights are not currently implemented for `mior()`.")
  }

  if (method == "qp-heuristic") {
    res <- mior_dual_fit(y, bags, x,
                         c0 = cost_eta,
                         c1 = cost,
                         rescale = control$scale,
                         weights = weights,
                         kernel = control$kernel,
                         sigma = control$sigma,
                         verbose = control$verbose,
                         time_limit = control$time_limit,
                         max_step = control$max_step,
                         option = control$option)
  } else {
    stop("`mior()` requires method = 'qp-heuristic'.")
  }

  out <- res[1]
  out$call_type <- "mior.default"
  out$x <- res$x
  out$features <- col_x
  out$levels <- lev
  out$cost <- cost
  out$cost_eta <- cost_eta
  out$weights <- weights
  out$repr_inst <- res$repr_inst
  out$n_step <- res$n_step
  out$x_scale <- res$x_scale
  new_mior(out)
}

#' @describeIn mior Method for passing formula
#' @export
mior.formula <- function(formula, data, ...) {
  mi_names <- as.character(stats::terms(formula, data = data)[[2]])
  bag_name <- mi_names[[3]]

  x <- x_from_mi_formula(formula, data)
  response <- stats::get_all_vars(formula, data = data)
  y <- response[, 1]
  bags <- response[, 2]

  res <- mior.default(x, y, bags, ...)

  res$call_type <- "mior.formula"
  res$formula <- formula
  res$bag_name <- bag_name
  return(res)
}


#' Predict method for `mior` object
#'
#' @details
#' When the object was fitted using the `formula` method, then the parameters
#' `new_bags` and `new_instances` are not necessary, as long as the names match
#' the original function call.
#'
#' @param object An object of class `mior`
#' @inheritParams predict.misvm
#'
#' @return A tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column `.pred_class`.  If `type = 'raw'`, the tibble will have
#'   a column `.pred`.
#'
#' @seealso [mior()] for fitting the `mior` object.
#'
#' @examples
#' set.seed(8)
#' # make some data
#' n <- 15
#' X <- rbind(
#'   mvtnorm::rmvnorm(n/3, mean = c(4, -2, 0)),
#'   mvtnorm::rmvnorm(n/3, mean = c(0, 0, 0)),
#'   mvtnorm::rmvnorm(n/3, mean = c(-2, 1, 0))
#' )
#' score <- X %*% c(2, -1, 0)
#' y <- as.numeric(cut(score, c(-Inf, quantile(score, probs = 1:2 / 3), Inf)))
#' bags <- 1:length(y)
#'
#' # add in points outside boundaries
#' X <- rbind(
#'   X,
#'   mvtnorm::rmvnorm(n, mean = c(6, -3, 0)),
#'   mvtnorm::rmvnorm(n, mean = c(-6, 3, 0))
#' )
#' y <- c(y, rep(-1, 2*n))
#' bags <- rep(bags, 3)
#' repr <- c(rep(1, n), rep(0, 2*n))
#'
#' y_bag <- classify_bags(y, bags, condense = FALSE)
#'
#' mdl1 <- mior(X, y_bag, bags)
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df1 <- bind_cols(y = y_bag, bags = bags, as.data.frame(X))
#' df1 %>%
#'   bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
#'   bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
#'   distinct(y, bags, .pred_class, .pred)
#'
#' @export
#' @author Sean Kent
predict.mior <- function(object,
                         new_data,
                         type = c("class", "raw"),
                         layer = c("bag", "instance"),
                         new_bags = "bag_name",
                         ...)
{
  type <- match.arg(type, c("class", "raw"))
  layer <- match.arg(layer, c("bag", "instance"))

  if (is.matrix(new_data)) {
    new_data <- as.data.frame(new_data)
  }
  if (object$call_type == "mior.formula") {
    new_x <- x_from_mi_formula(object$formula, new_data)
  } else {
    new_x <- new_data[, object$features, drop = FALSE]
  }

  scores <- as.matrix(new_x) %*% object$gurobi_fit$w
  b_ <- object$gurobi_fit$b
  ind <- 2:length(b_)
  midpoints <- (b_[ind-1] + b_[ind]) / 2

  dist_from_mp <- abs(outer(as.vector(scores), midpoints, `-`))

  if (layer == "bag") {
    if (object$call_type == "mior.formula" & new_bags[1] == "bag_name" & length(new_bags) == 1) {
      new_bags <- object$bag_name
    }
    if (length(new_bags) == 1 & new_bags[1] %in% colnames(new_data)) {
      bags <- new_data[[new_bags]]
    } else {
      bags <- new_bags
    }

    class_ <- rep(NA, length(bags))
    for (b in unique(bags)) {
      ind <- which(bags == b)
      by_row <- 2
      instance_min <- apply(dist_from_mp[ind, , drop = FALSE], by_row, min)
      class_[ind] <- which.min(instance_min)

      by_col <- 1
      mp_min <- apply(dist_from_mp[ind, , drop = FALSE], by_col, min)
      repr_inst <- ind[which.min(mp_min)]
      scores[ind] <- scores[repr_inst]
    }

  } else {
    by_col <- 1
    class_ <- apply(dist_from_mp, by_col, which.min)
  }

  class_ <- factor(class_, levels = seq_along(object$levels), labels = object$levels)

  res <- switch(type,
                "raw" = tibble::tibble(.pred = as.numeric(scores)),
                "class" = tibble::tibble(.pred_class = class_))

  # TODO: consider returning the AUC here as an attribute.  Can only do if we have the true bag labels
  # attr(res, "AUC") <- calculated_auc
  attr(res, "layer") <- layer
  attr(res, "midpoints") <- midpoints
  return(res)
}

# Specific implementation methods below ----------------------------------------

mior_dual_fit <- function(y, bags, x, c0, c1, rescale = TRUE, weights = NULL,
                          kernel = "linear", sigma = NULL,
                          verbose = FALSE, time_limit = FALSE, max_step = 500,
                          option = "xiao") {

  r <- .reorder(y, bags, x)
  y <- r$y
  bags <- r$b
  x <- r$X
  if (rescale) x <- scale(x)

  # kernel
  # if (!is.matrix(kernel)) {
  #   K <- compute_kernel(X, type = kernel, sigma = sigma)
  # } else {
  #   K <- kernel
  # }

  # pre-specified parameters
  # x, y, bags,
  # max_step <- 500
  # c0 <- 1
  # c1 <- 1
  threshold <- 0.1
  # verbose <- FALSE

  # gurobi parameters
  params <- list()
  params$OutputFlag = 1*(verbose == 2)
  params$IntFeasTol = min(1e-5, 1e-5*c0, 1e-5*c1)
  params$IntFeasTol = max(params$IntFeasTol, 1e-9) # 1e-9 is smallest gurobi will accept
  params$BarQCPConvTol = 1e-9
  params$PSDTol = 1e-4
  if (time_limit) params$TimeLimit = time_limit

  # initialize parameters
  K <- max(y)
  t <- 0
  delta_j <- 1e-3
  j <- numeric(max_step+2)
  j_ <- function(t) {
    j[t+2] # let j_ start at index -1, so j[1] = j_(-1), j[2] = j_(0), etc.
  }
  j[1] <- 1e-3 # j(-1)

  w_t <- stats::rnorm(ncol(x)) # check to see if there is a suggested initialization
  b_t <- sort(stats::rnorm(K+1))

  while (abs(delta_j / j_(t-1)) > threshold && t < max_step) {
    if (min(abs(j[t+1] - j[-(t+1)])) / j[t+1] < 1e-5) {
      # print(min(abs(j[t+1] - j)))
      rlang::warn(c(
        "Optimization appears to be repeating solutions.",
        i = "Stopping with best solution."
      ))
      break
    }

    # compute theta and lambda
    scores <- as.matrix(x) %*% w_t - (b_t[y] + b_t[y+1]) / 2
    scores <- as.numeric(scores)
    # g <- abs(scores)
    # h <- -classify_bags(-abs(scores), bags, condense = FALSE)
    theta <- compute_theta(g = abs(scores), bags)
    lambda <- sign(scores)
    delta <- theta*lambda

    model <- mior_dual_model(x, y, bags, delta, c0, c1, option)
    gurobi_result <- gurobi::gurobi(model, params = params)

    ind <- delta != 0
    a <- gurobi_result$x[grepl("a", model$varnames)]
    # update w, b, j
    t <- t + 1
    w_t <- - colSums(a * delta[ind] * x[ind, , drop = FALSE])
    b_t <- compute_b(gurobi_result, model, delta, y, bags, c0, c1, option, t)
    j[t+1] <- gurobi_result$objval # or, sum(a) + t(gurobi_result$x) %*% model$Q %*% gurobi_result$x
    delta_j <- j_(t-2) - j_(t-1)

    if (verbose) {
      cat(t, ": ", sep = "")
      cat("J: ", j[t+1], "; ", sep = "")
      cat("b: ", paste0(round(b_t, 2), collapse = " "), "\n", sep = "")
    }
  }

  if (t == max_step) {
    rlang::warn(c(
      paste0("The number of iterations of heuristic algorithm reached the threshold of ", max_step, "."),
      i = "Stopping with current selection."
    ))
  }
  # TODO: figure out why this doesn't converge.  Might be an error in my
  # implementation... But it seems to find something that's fairly reasonable.

  if (rescale) { # TODO: edit this to work
    # NOTE: this differs from other rescaling because we use wx-b instead of wx+b
    b_t <- b_t + sum(attr(x, "scaled:center") * w_t / attr(x, "scaled:scale"))
    w_t <- w_t / attr(x, "scaled:scale")
  }

  res <- list(
    gurobi_fit = list(
      w = w_t,
      b = b_t,
      xmatrix = x[ind, , drop = FALSE],
      a = a,
      # ay = a * y[ind],
      # kernel = kernel,
      # sigma = sigma,
      # xi = gurobi_result$x[grepl("xi", model$varnames)],
      status = gurobi_result$status,
      itercount = gurobi_result$itercount,
      baritercount = gurobi_result$baritercount,
      objval = gurobi_result$objval,
      c0 = c0,
      c1 = c1,
      n_selections = t
    ),
    n_step = t,
    repr_inst = theta,
    x = NULL
  )
  if (rescale) {
    res$x_scale <- list(
      "center" = attr(x, "scaled:center"),
      "scale" = attr(x, "scaled:scale")
    )
  }
  # names(res$model$w) <- colnames(X)
  return(res)
}

mior_dual_model <- function(x, y, bags, delta, c0, c1, option = "xiao") {

  n_a <- length(unique(bags))
  n_mu <- K <- max(y)
  n_rho <- 1

  y_bag <- classify_bags(y, bags) # TODO: make sure that the bag label is passed here still

  # Build constraint matrix
  .e_vec <- function(b, len) {
    # puts a 1 in the `b`th entry of a `len`-length 0 vector
    vec <- rep(0, len)
    vec[b] <- 1
    return(vec)
  }

  alpha_constr <- matrix(0, nrow = n_mu+1, n_a)
  if (option == "xiao") {
    sum_delta_plus <- sapply(unique(bags), function(bag) sum(1 + delta[bags == bag]))
    sum_delta_minus <- sapply(unique(bags), function(bag) sum(1 - delta[bags == bag]))
  } else if (option == "corrected") {
    sum_delta_plus <- sapply(unique(bags), function(bag) 1 + sum(delta[bags == bag]))
    sum_delta_minus <- sapply(unique(bags), function(bag) 1 - sum(delta[bags == bag]))
  }

  for (i in 1:n_a) {
    p <- y_bag[i] + 1 # +1 for zero-indexing
    alpha_constr[p, i] <- alpha_constr[p, i] - sum_delta_plus[i] / 2
    alpha_constr[p-1, i] <- alpha_constr[p-1, i] + sum_delta_minus[i] / 2
  }
  mu_constr <- sapply(1:(K), .e_vec, len = n_mu+1) - sapply(2:(K+1), .e_vec, len = n_mu+1)
  rho_constr <- .e_vec(K+1, n_mu+1) - .e_vec(1, n_mu+1)

  constraints <- cbind(alpha_constr, mu_constr, rho_constr)

  # Quadratic objective matrix
  ind <- delta != 0
  kernel <- x[ind, , drop = FALSE] %*% t(x[ind, , drop = FALSE])
  alpha_Q <- - 0.5 * (delta[ind] %*% t(delta[ind])) * kernel
  mu_rho_Q <- matrix(0, n_mu + n_rho, n_mu + n_rho)
  Q <- Matrix::bdiag(alpha_Q, mu_rho_Q)
  # worry about bags that are unordered, maybe take care of this at the beginning of the function

  model <- list()
  # Objective
  model$modelsense <- "max"
  model$obj <- c(rep(1, n_a), rep(0, n_mu + n_rho))
  model$Q <- Q # TODO: replace this with kernel at some point
  # Constraints
  model$varnames <- c(paste0("a", 1:n_a), paste0("mu", 1:n_mu), "rho")
  model$A <- constraints
  model$sense <- rep("=", nrow(constraints))
  model$rhs <- rep(0, nrow(constraints))
  model$lb <- rep(0, n_a + n_mu + n_rho)
  model$ub <- c(rep(c1, n_a), rep(Inf, n_mu), c0)

  return(model)
}

# helper functions
compute_theta <- function(g, bags) {
  # theta is 1 only for the instance with minimal g in the bag
  theta <- rep(0, length(g))
  for (bag in unique(bags)) {
    ind <- bag == bags
    argmin <- which.min(g[ind])
    theta[ind][argmin] <- 1
  }
  return(theta)
}

compute_b <- function(gurobi_result, model, delta, y, bags, c0, c1, option = "xiao", t) {
  # names(gurobi_result$x) <- model$varnames
  a <- gurobi_result$x[grepl("a", model$varnames)]
  mu <- gurobi_result$x[grepl("mu", model$varnames)]
  rho <- gurobi_result$x[grepl("rho", model$varnames)]

  n_b <- length(mu) + 1
  eps <- 1e-5 * c1
  ind <- which(a > 0 + eps & a < c1 - eps)
  if (length(ind) == 0) {
    rlang::warn(c(
      paste0("[Step ", t, "] The optimization didn't return any support vectors."),
      i = "Resetting the values of `b` randomly. "
    ))
    return(sort(stats::rnorm(n_b)))
  }

  support_bags <- unique(bags)[ind]
  y_support <- classify_bags(y, bags)[ind]

  Q_tilde <- -2 * model$Q[seq_along(a), seq_along(a), drop = FALSE] # recovers delta * kernel
  if (option == "xiao") {
    b_q <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag] + 1))
    b_q1 <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag] - 1))
  } else if (option == "corrected") {
    b_q <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag]) + 1)
    b_q1 <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag]) - 1)
  }
  # linear model using complementary slackness constraints: resp ~ pred_matrix
  resp <- as.numeric(- a %*% Q_tilde[, ind, drop = FALSE] ) + 1
  # resp <- as.numeric(- a %*% Q_tilde ) + 1
  resp <- c(resp, rep(0, length(mu) + 1))
  pred_matrix <- matrix(0, nrow = length(ind) + length(mu) + 1, ncol = n_b)
  # information from alpha
  for (i in seq_along(ind)) {
    pred_matrix[i, y_support[i]+1] <- b_q[i]
    pred_matrix[i, y_support[i]] <- b_q1[i]
  }

  # information from mu; b_q = b_{q-1} if \mu_q > 0
  for (q in seq_along(mu)) {
    if (mu[q] > 0 + eps) {
      rlang::inform(c(
        paste0("[Step ", t, "] The optimization solution suggests that two intercepts are equal: b[", q-1, "] == b[", q, "].")
      ))
      start <- length(ind)
      pred_matrix[start + q, q] <- 1
      pred_matrix[start + q, q+1] <- -1
    }
  }
  # information from rho, gamma, eta; b_0 = b_K if \rho < C_0
  if (rho < c0 - eps) {
    rlang::inform(c(
      paste0("[Step ", t, "] The optimization solution suggests that endpoints are equal: b[0] == b[K].")
    ))
    start <- length(ind) + length(mu)
    pred_matrix[start + 1, 1] <- -1
    pred_matrix[start + 1, n_b] <- 1
  }
  b <- stats::coef(stats::lm(resp ~ 0 + pred_matrix))
  if (any(is.na(b))) {
    # If values are NA, that means the particular column isn't needed for
    # prediction, i.e. that the dropped coefficient is 0.
    rlang::warn(
      paste0("[Step ", t, "] There were NA values in `b`.  Replacing with 0.")
    )
    b[which(is.na(b))] <- 0
  }
  return(b)
}
