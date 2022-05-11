new_svor_exc <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("svor_exc"))
}

validate_svor_exc <- function(x) {
  message("No validations currently in place for object of class 'svor_exc'.")
  x
}

#' Fit SVOR-EXC model to ordinal outcome data
#'
#' This function fits the Support Vector Ordinal Regression with Explicit
#' Constraints based on the research of Chu and Keerthi (2007).
#'
#' @inheritParams omisvm
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted.
#' @param cost The cost parameter in SVM.
#' @param method The algorithm to use in fitting (default  `'smo'`).  When
#'   `method = 'smo'`, the modified SMO algorithm from Chu and Keerthi (2007) is
#'   used.
#' @param weights `NULL`, since weights are not implemented for this function.
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
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `svor_exc`  The object contains at least the
#'   following components:
#'   * `smo_fit`: A fit object from running the modified ordinal smo algorithm.
#'   * `call_type`: A character indicating which method `svor_exc()` was called
#'   with.
#'   * `features`: The names of features used in training.
#'   * `levels`: The levels of `y` that are recorded for future prediction.
#'   * `cost`: The cost parameter from function inputs.
#'   * `n_step`: The total steps used in the heuristic algorithm.
#'   * `x_scale`: If `scale = TRUE`, the scaling parameters for new predictions.
#'
#' @seealso [predict.svor_exc()] for prediction on new data.
#'
#' @examples
#' data("ordmvnorm")
#' x <- ordmvnorm[, 4:8]
#' y <- ordmvnorm$inst_label
#'
#' mdl1 <- svor_exc(x, y)
#' predict(mdl1, x)
#'
#' @author Sean Kent
#' @name svor_exc
NULL

#' @export
svor_exc <- function(x, ...) {
  UseMethod("svor_exc")
}

#' @describeIn svor_exc Method for data.frame-like objects
#' @export
svor_exc.default <- function(x, y,
                             cost = 1,
                             method = c("smo"),
                             weights = NULL,
                             control = list(kernel = "linear",
                                            sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                                            max_step = 500,
                                            scale = TRUE,
                                            verbose = FALSE
                             ),
                             ...)
{
  method <- match.arg(method, c("smo"))

  defaults <- list(
    kernel = "linear",
    sigma = if (is.vector(x)) 1 else 1 / ncol(x),
    max_step = 500,
    scale = TRUE,
    verbose = FALSE
  )
  control <- .set_default(control, defaults)

  # store the levels of y and convert to 0,1 numeric format.
  y_info <- .convert_y_ordinal(y)
  y <- y_info$y
  lev <- y_info$lev

  # store colnames of x
  col_x <- colnames(x)

  weights <- .warn_no_weights(weights, "svor_exc")

  if (method == "smo") {
    res <- svor_exc_fit(y, x,
                        c = cost,
                        rescale = control$scale,
                        weights = weights,
                        kernel = control$kernel,
                        sigma = control$sigma,
                        verbose = control$verbose,
                        max_step = control$max_step)
  } else {
    stop("`svor_exc()` requires method = 'smo'.")
  }

  out <- res[1]
  out$call_type <- "svor_exc.default"
  out$x <- res$x
  out$features <- col_x
  out$levels <- lev
  out$cost <- cost
  out$weights <- weights
  out$n_step <- res$n_step
  out$x_scale <- res$x_scale
  return(new_svor_exc(out))
}

#' @describeIn svor_exc Method for passing formula
#' @export
svor_exc.formula <- function(formula, data, ...) {
  # NOTE: other 'professional' functions use a different type of call that I
  #   couldn't get to work. See https://github.com/therneau/survival/blob/master/R/survfit.R
  #   or https://github.com/cran/e1071/blob/master/R/svm.R
  #   right now we're using something that should work for most generic formulas

  x <- x_from_formula(formula, data)
  response <- stats::get_all_vars(formula, data = data)
  y <- response[, 1]

  res <- svor_exc.default(x, y, ...)
  res$call_type <- "svor_exc.formula"
  res$formula <- formula
  return(res)
}

#' Predict method for `svor_exc` object
#'
#' @details
#' When the object was fitted using the `formula` method, then the parameter
#' `new_bags` is not necessary, as long as the names match
#' the original function call.
#'
#' @param object An object of class `svor_exc`.
#' @inheritParams predict.misvm
#'
#' @return A tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column `.pred_class`.  If `type = 'raw'`, the tibble will have
#'   a column `.pred`.
#'
#' @seealso [svor_exc()] for fitting the `svor_exc` object.
#'
#' @examples
#' data("ordmvnorm")
#' y <- ordmvnorm$inst_label
#' x <- ordmvnorm[, 4:8]
#'
#' mdl1 <- svor_exc(x, y)
#' predict(mdl1, x)
#' predict(mdl1, x, type = "raw")
#'
#' @export
#' @author Sean Kent
predict.svor_exc <- function(object,
                             new_data,
                             type = c("class", "raw"),
                             layer = c("instance", "bag"),
                             new_bags = "bag_name",
                             ...)
{
  type <- match.arg(type, c("class", "raw"))
  layer <- match.arg(layer, c("instance", "bag"))

  # new_x
  if (object$call_type == "svor_exc.formula") {
    new_x <- x_from_formula(object$formula, new_data)
    new_x <- new_x[, object$features, drop = FALSE]
  } else {
    new_x <- new_data[, object$features, drop = FALSE]
  }

  if ("x_scale" %in% names(object)) {
    new_x <- as.data.frame(scale(new_x, center = object$x_scale$center, scale = object$x_scale$scale))
  }

  # kernel
  kernel <- compute_kernel(as.matrix(new_x),
                           object$x,
                           type = object$smo_fit$kernel,
                           sigma = object$smo_fit$sigma)


  scores <- .calculate_f(object$smo_fit$alpha, kernel)
  scores_matrix <- outer(as.vector(scores), object$smo_fit$b, `-`)
  class_ <- rowSums(scores_matrix > 0) + 1

  if (layer == "bag") {
    if (length(new_bags) == 1 && new_bags %in% colnames(new_data)) {
      bags <- new_data[[new_bags]]
    } else if (length(new_bags) == nrow(new_data)) {
      bags <- new_bags
    } else {
      rlang::abort(c(
        "The `new_bags` must have length 1 or length `nrow(new_data)`.",
        x = paste0("`length(new_bags)` = ", length(new_bags), ".")
      ))
    }
    scores <- classify_bags(scores, bags, condense = FALSE)
    class_ <- classify_bags(class_, bags, condense = FALSE)
  }
  class_ <- factor(class_, levels = seq_along(object$levels), labels = object$levels)

  res <- .pred_output(type, scores, class_)
  # TODO: remember, for adding an SI-svor_exc option, I can add a bags option to
  # the predict function, but also need perhaps an updated fit.
  return(res)
}


# Specific implementation methods below ----------------------------------------

#' INTERNAL fit function for SVOR-EXC
#' @author Sean Kent
#' @noRd
svor_exc_fit <- function(y, X, c, rescale = TRUE, weights = NULL,
                         kernel = "linear", sigma = NULL,
                         verbose = FALSE, max_step = 500) {
  r <- .reorder(y, y, X)
  y <- r$y
  X <- r$X
  if (rescale) X <- scale(X)
  unorder <- match(seq_along(r$order), r$order)

  K <- .convert_kernel(X, kernel, sigma = sigma)

  smo_fit <- smo(y, K, c, max_step)

  # unorder the alpha values
  smo_fit$alpha$reg <- unname(smo_fit$alpha$reg[unorder])
  smo_fit$alpha$star <- unname(smo_fit$alpha$star[unorder])

  # add components to `smo_fit`
  smo_fit$kernel <- kernel
  smo_fit$sigma <- kernel
  smo_fit$c <- c

  res <- list(
    smo_fit = smo_fit,
    n_step = smo_fit$i,
    x = X[unorder, ],
    x_scale = list(
      "center" = attr(X, "scaled:center"),
      "scale" = attr(X, "scaled:scale")
    )
  )
  if (!rescale) res$x_scale <- NULL
  return(res)
}

#' INTERNAL smo algorithm for ordinal regression
#' @author Sean Kent
#' @noRd
smo <- function(y, K, c, max_step) {
  tau <- 1e-3

  opt <- .find_initial_point(y, c)
  f <- .calculate_f(opt$alpha, K)
  b_info <- .calculate_b(opt$alpha, opt$mu, f, c, y)

  i <- 0
  while(max(b_info$B_low - b_info$B_up) > tau & i < max_step) {

    J <- which.max(b_info$B_low - b_info$B_up) # active threshold

    opt <- .update_alpha_mu(b_info, opt$alpha, opt$mu, J, f, c, y, K)
    f <- .calculate_f(opt$alpha, K)
    b_info <- .calculate_b(opt$alpha, opt$mu, f, c, y)

    i <- i + 1
  }

  if (i == max_step) {
    rlang::inform(paste("The SMO algorithm reached the maximum of", max_step, "steps."))
  }

  opt$b <- 0.5 * (b_info$B_low + b_info$B_up)
  out <- c(
    opt,
    i = i,
    max_step = max_step,
    b_info
  )
  return(out)
}

.find_initial_point <- function(y, c) {
  j_min <- 1
  j_max <- max(y) - 1

  alpha <- list(
    "reg" = rep(0, length(y)),
    "star" = rep(0, length(y))
  )
  names(alpha$reg) <- seq_along(alpha$reg)
  names(alpha$star) <- -1*(seq_along(alpha$star))

  mu <- rep(1, j_max)

  return(list(
    alpha = alpha,
    mu = mu
  ))
}

.calculate_f <- function(alpha, K) {
  as.numeric(K %*% (alpha$star - alpha$reg))
}

.calculate_b <- function(alpha, mu, f, c, y) {
  j_max <- max(y) - 1 # r-1

  I <- lapply(1:j_max, function(j) {
    a_reg <- alpha[["reg"]][which(y == j)]
    a_st <- alpha[["star"]][which(y == j+1)]

    list(
      "0a" = which(a_reg > 0 & a_reg < c),
      "0b" = which(a_st > 0 & a_st < c),
      "1" = which(a_st == 0),
      "2" = which(a_reg == 0),
      "3" = which(a_reg == c),
      "4" = which(a_st == c)
    )
  })

  b_up <- sapply(1:j_max, function(j) {
    ind1 <- which(y == j)
    ind2 <- which(y == j+1)

    set1 <- c(I[[j]][["0a"]], I[[j]][["3"]])
    set2 <- c(I[[j]][["0b"]], I[[j]][["1"]])

    f_up <- c(f[ind1][set1] + 1,
              f[ind2][set2] - 1)
    names(f_up) <- c(names(set1), names(set2))

    if (length(f_up) == 0) {
      # degenerate case when I_up = NULL, no upper constraint
      # This is generally a really bad thing. It implies the value of b is
      # completely to the right of the data for j and j+1.
      return(Inf)
    } else {
      return(.min(f_up))
    }
  })

  b_low <- sapply(1:j_max, function(j) {
    ind1 <- which(y == j)
    ind2 <- which(y == j+1)

    set1 <- c(I[[j]][["0a"]], I[[j]][["2"]])
    set2 <- c(I[[j]][["0b"]], I[[j]][["4"]])

    f_low <- c(f[ind1][set1] + 1,
               f[ind2][set2] - 1)
    names(f_low) <- c(names(set1), names(set2))

    if (length(f_low) == 0) {
      # degenerate case when I_low = NULL, no lower constraint
      # This is generally a really bad thing. It implies the value of b is
      # completely to the left of the data for j and j+1.
      return(-Inf)
    } else {
      return(.max(f_low))
    }
  })

  B_tilde_low <- sapply(1:j_max, function(j) .max(b_low[1:j]))
  B_tilde_up <- sapply(1:j_max, function(j) .min(b_up[j:j_max]))

  B_low <- sapply(1:j_max, function(j) {
    # when j == j_max, mu[j+1] == 0
    if (mu[j+1] > 0 & j != j_max) {
      B_tilde_low[j+1]
    } else {
      B_tilde_low[j]
    }
  })
  B_up <- sapply(1:j_max, function(j) {
    # when j == 1, mu[0] == 0
    if (mu[j] > 0 & j != 1) {
      B_tilde_up[j-1]
    } else {
      B_tilde_up[j]
    }
  })
  B_low <= B_up
  return(list(
    I = I,
    b_low = b_low,
    b_up = b_up,
    B_low = B_low,
    B_up = B_up
  ))
}

.update_alpha_mu <- function(self, alpha, mu, J, f, c, y, K) {

  o <- names(which(self$B_low[J] == self$b_low)[1])
  u <- names(which(self$B_up[J] == self$b_up)[1])
  o_ <- abs(as.numeric(o)) # for indexing f, y, X, or K
  u_ <- abs(as.numeric(u))

  j_o <- ifelse(as.numeric(o) > 0, y[o_], y[o_] - 1) # alpha$star ones used j+1 in `.compute_b()`
  j_u <- ifelse(as.numeric(u) > 0, y[u_], y[u_] - 1)

  if (j_o == j_u) {
    mu_a <- NULL
  } else {
    mu_a <- (min(j_o, j_u) + 1):(max(j_o, j_u))
  }

  s_o <- ifelse(o %in% names(self$I[[j_o]][["0a"]]) |
                  o %in% names(self$I[[j_o]][["2"]]),
                -1, 1)
  s_u <- ifelse(u %in% names(self$I[[j_u]][["0a"]]) |
                  u %in% names(self$I[[j_u]][["3"]]),
                -1, 1)

  denom <- K[o_, o_] + K[u_, u_] - 2 * K[o_, u_]
  delta_mu <- -f[o_] + f[u_] + s_o - s_u
  if (denom != 0) {
    delta_mu <- delta_mu / denom
  }

  d_mu <- ifelse(j_o <= j_u, 1, -1)

  # Update alpha, mu, keeping in mind the constraints
  alpha_o <- ifelse(as.numeric(o) > 0, alpha$reg[o_], alpha$star[o_])
  alpha_u <- ifelse(as.numeric(u) > 0, alpha$reg[u_], alpha$star[u_])

  lower1 <- ifelse(s_o == 1, -alpha_o, alpha_o - c)
  lower2 <- ifelse(s_u == 1, alpha_u - c, -alpha_u)
  upper1 <- ifelse(s_o == 1, c - alpha_o, alpha_o)
  upper2 <- ifelse(s_u == 1, alpha_u, c - alpha_u)
  if (is.null(mu_a)) {
    lower3 <- upper3 <- NULL
  } else if (d_mu == 1) {
    lower3 <- rep(-Inf, length(mu_a))
    upper3 <- mu[mu_a]
  } else {
    lower3 <- -mu[mu_a]
    upper3 <- rep(Inf, length(mu_a))
  }

  # update the parameters
  if (as.numeric(o) > 0) {
    alpha$reg[o_] <- alpha$reg[o_] + s_o * min(max(delta_mu, lower1), upper1)
  } else {
    alpha$star[o_] <- alpha$star[o_] + s_o * min(max(delta_mu, lower1), upper1)
  }

  if (as.numeric(u) > 0) {
    alpha$reg[u_] <- alpha$reg[u_] - s_u * min(max(delta_mu, lower2), upper2)
  } else {
    alpha$star[u_] <- alpha$star[u_] - s_u * min(max(delta_mu, lower2), upper2)
  }

  mu[mu_a] <- mu[mu_a] - d_mu * pmin(pmax(delta_mu, lower3), upper3)

  return(list(
    alpha = alpha,
    mu = mu
  ))
}

