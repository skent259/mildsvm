#' Predict feature map using exact features
#'
#' TODO: add description
#'
#' @param object a object from a call to `fit_kernel_feature_map()` with
#'   `method` = 'exact'.
#' @param newx An object containing features for prediction. Usually a data.frame
#'   or matrix.
#'
#' @return a matrix of features that use the exact kernel feature map for each
#'   observation in `newx`.
#'
#' @examples
#' df <- data.frame(
#'   X1 = c(2,   3,   4,   5,   6, 7, 8),
#'   X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
#'   X3 = rnorm(7)
#' )
#'
#' fit <- fit_kernel_feature_map(df, method = "exact", kernel = "polynomial", degree = 2, const = 1)
#' fm <- predict_exact(fit, df)
#'
#' @export
#' @author Sean Kent
predict_exact <- function(object, newx) {
  UseMethod("predict_exact", newx)
}

#' @describeIn predict_exact For use on objects of class `data.frame` or `matrix`.
#' @export
predict_exact.default <- function(object, newx) {
  if(object$kernel == "polynomial") {
    stopifnot("degree" %in% names(object$kernel_params))
    stopifnot("const" %in% names(object$kernel_params))
    degree <- object$kernel_params$degree
    const <- object$kernel_params$const

    if (degree == 2) {
      X <- as.matrix(newx)
      d <- ncol(X)

      x_squared <- X^2
      colnames(x_squared) <- paste0(colnames(X), "_sq")

      x_crossterms <- matrix(NA, nrow(X), choose(d,2))
      names_crossterrms <- rep(NA, choose(d,2))
      col <- 1
      for (k in 2:d) {
        for (l in 1:(k-1)) {
          x_crossterms[, col] <- sqrt(2) * X[,k] * X[,l]
          names_crossterrms[col] <- paste0(colnames(X)[l], ".", colnames(X)[k])
          col <- col + 1
        }
      }
      colnames(x_crossterms) <- names_crossterrms

      x_linear <- sqrt(2*const) * X

      res <- cbind(x_squared, x_crossterms, x_linear)
      # TODO: consider passing the constant term in here, especially if not going to rescale after
    } else {
      stop(paste0("degree = ", degree, " not currently supported. Use degree = 2."))
    }
  } else {
    stop(paste0("kernel = ", object$kernel, " not currently supported. Use kernel = 'polynomial'."))
  }

  return(res)
}

#' @describeIn predict_exact Ignore the information columns with 'bag_label',
#'   'bag_name', and 'instance_name' when calculating kernel approximation.
#'   These columns are re-appended upon prediction.
#' @export
predict_exact.MilData <- function(object, newx) {
  info <- subset(newx, select = c(bag_label, bag_name, instance_name))
  newx <- subset(newx, select = -c(bag_label, bag_name, instance_name))
  fm <- predict_exact.default(object, newx)
  cbind(info, fm)
}
