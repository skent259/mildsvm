new_kfm_exact <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "kfm_exact")
}

validate_kfm_exact <- function(x) {
  message("No validations currently in place for object of class 'kfm_exact'.")
  x
}

#' Create an exact kernel feature map
#'
#' For some kernels, it is possible to create the exact features from given
#' data. This function stores the information needed to build those exact
#' features.
#'
#' Currently, the following kernels are supported:
#' * `'polynomial'`, with `degree` = d and `const` = c
#'
#' @inheritParams kfm_nystrom
#' @param degree A numeric value (default 2) that provides the degree for
#'   `kernel` = 'polynomial'
#' @param const A numeric value (default 1) for the constant term when `kernel
#'   = 'polynomial'`.
#'
#' @return An object of class `kfm_exact` with the following components,
#'   returned from the inputs:
#'   * `kernel`
#'   * `degree`
#'   * `const`
#'
#' @examples
#' df <- data.frame(
#'   X1 = c(2,   3,   4,   5,   6, 7, 8),
#'   X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
#'   X3 = rnorm(7)
#' )
#'
#' fit <- kfm_exact(kernel = "polynomial", degree = 2, const = 1)
#' fm <- build_fm(fit, df)
#'
#' @family kernel feature map functions
#' @export
#' @author Sean Kent
kfm_exact <- function(kernel = "polynomial", degree = 2, const = 1) {
  if (kernel != "polynomial") {
    rlang::abort("`kernel` must equal 'polynomial'")
  }
  if (degree != 2) {
    rlang::abort(c(
      paste0("Using `degree` = ", degree, "is not currently supported."),
      i = "Choose `degree` = 2."
    ))
  }

  return(new_kfm_exact(list(
    kernel = kernel,
    degree = degree,
    const = const
  )))
}

#' @export
print.kfm_exact <- function(x, digits = getOption("digits"), ...) {

  cat("An exact kernel feature map object", "\n")
  cat("", "\n")
  cat("Parameters:", "\n")
  cat("  kernel:", x$kernel, "\n")
  cat("  degree:", x$degree, "\n")
  cat("  const:", x$const, "\n")
  cat("\n")
}

#' @describeIn build_fm Method for `kfm_exact` class.
#' @export
build_fm.kfm_exact <- function(kfm_fit, new_data, ...) {
  if (inherits(new_data, "mild_df")) {
    info <- new_data[, c("bag_label", "bag_name", "instance_name"), drop = FALSE]
    new_data$bag_label <- new_data$bag_name <- new_data$instance_name <- NULL
  } else {
    info <- NULL
  }

  if (kfm_fit$kernel == "polynomial") {
    degree <- kfm_fit$degree
    const <- kfm_fit$const

    if (degree == 2) {
      x <- as.matrix(new_data)
      d <- ncol(x)

      x_squared <- x^2
      colnames(x_squared) <- paste0(colnames(x), "_sq")

      x_crossterms <- matrix(NA, nrow(x), choose(d, 2))
      names_crossterrms <- rep(NA, choose(d, 2))
      col <- 1
      for (k in 2:d) {
        for (l in 1:(k-1)) {
          x_crossterms[, col] <- sqrt(2) * x[, k] * x[, l]
          names_crossterrms[col] <- paste0(colnames(x)[l], ".", colnames(x)[k])
          col <- col + 1
        }
      }
      colnames(x_crossterms) <- names_crossterrms

      x_linear <- sqrt(2*const) * x

      fm <- cbind(x_squared, x_crossterms, x_linear)
      # TODO: consider passing the constant term in here, especially if not going to rescale after
    }
  }

  if (!is.null(info)) {
    fm <- cbind(info, fm)
  }
  return(fm)
}
