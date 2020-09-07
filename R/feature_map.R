
#' This flattens the MilData type of data to regular multiple instance data where each instance is a vector
#'
#' This flatten the MilData type of data to regular multiple instance data where
#' each instance is a vector by extracting the features that would be present
#' in a polynomial kernel with given degree (default = 2).  Each instance_name
#' has features calculated in the input space based on the underlying kernel
#' (x'y + c)^d where c = constant and d = degree are input parameters.
#' TODO: update this to reflect more general methods
#'
#' @param data A MilData object.  Ignored if `method` = 'exact' and `output` =
#'   "fit".
#' @param method
#' @param kernel
#' @param ...
#'
#' @return A data.frame that is ready to be used in `MI_SVM()` function.
#' @return a function
#'
#' @details
#' TODO: update this
#'  degree degree of the polynomial kernel, default = 2
#'  constant term added to inner product before applying the degree
#'
#' @examples
#' MilData1 <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 50,
#'                             nsample = 20,
#'                             positive_degree = 3,
#'                             positive_prob = 0.15,
#'                             positive_mean = rep(0, 5))
#' df1 <- build_kernel_features(MilData1, degree = 2)
#' @export
#' @author Sean Kent
fit_kernel_feature_map <- function(data, method = c("nystrom", "exact"),
  kernel = c("rbf", "polynomial"), output = c("fit", "feature_map", "mean_map"), ...) {

  method = method[1]
  kernel = kernel[1]
  output = output[1]
  args <- list(...)
  stopifnot("MilData" %in% class(data))

  if (method == "nystrom") {
    if (kernel == "rbf") {
      if(is.null(args$m)) {
        args$m <- nrow(data)
        message(paste0("Using parameter m = ", args$m, "."))
      }
      if(is.null(args$r)) {
        args$r <- args$m
        message(paste0("Using parameter r = ", args$r, "."))
      }
      if(is.null(args$sigma)) stop("sigma must be specificed in ... for method 'nystrom' and kernel 'rbf'.")
      fit <- kfm_nystrom(data, m = args$m, r = args$r, kernel = "rbf", sigma = args$sigma)
    }
  } else if (method == "exact") {
    if (kernel == "polynomial") {
      if(is.null(args$degree)) {
        args$degree <- 2
        message("Using parameter degree = 2.")
      }
      if(is.null(args$const)) {
        args$const <- 1
        message("Using parameter const = 1.")
      }
      fit <- list(method = method,
                  kernel = kernel,
                  kernel_params = list(degree = args$degree,
                                       const = args$const))
    }
  }

  if (output == "fit") {
    return(fit)
  } else if (output == "feature_map") {
    # TODO: remember, this is returning the mean feature map
    fm <- build_kernel_feature_map(fit, data)
    return(fm)
  } else if (output == "mean_map") {
    fm <- build_kernel_mean_map(fit, data)
    return(fm)
  }

}

#' Build a kernel feature map from fitted object
#'
#' TODO: make documentation
#' @export
#' @author Sean Kent
build_kernel_feature_map <- function(fit, data) {
  method = fit$method
  kernel = fit$kernel

  if (method == "nystrom") {
    if (kernel == "rbf") {
      fm <- predict_kfm_nystrom(fit, data)
    }
  } else if (method == "exact") {
    if (kernel == "polynomial") {
      fm <- predict_exact(fit, data)
    }
  }

  return(fm)
}

#' Build a kernel mean map from fitted object
#'
#' TODO: documentation
#' @export
#' @author Sean Kent
build_kernel_mean_map <- function(fit, data) {
  fm <- build_kernel_feature_map(fit, data)

  info <- subset(fm, select = c(bag_label, bag_name, instance_name))
  fm <- subset(fm, select = -c(bag_label, bag_name, instance_name))

  split_fm <- split(fm, factor(info$instance_name))
  mean_fm <- lapply(split_fm,
                    FUN = function(x) apply(x, MARGIN = 2, mean))
  mean_fm <- as.data.frame(do.call(rbind, mean_fm))
  mean_fm$instance_name <- rownames(mean_fm)

  res <- merge(unique(info),
               mean_fm,
               sort = FALSE)
  res[, c(2, 3, 1, 4:ncol(res))] # re-order first 3 columns
}

#' Fit a Nystrom method kernel approximation
#'
#' TODO: add description
#'
#' @param df An object containing features for training.  Usually a data.frame
#'   or matrix.
#' @param m the number of examples from \code{df} to sample in fitting.
#' @param r the rank of matrix approximation to use. Must be less than or equal
#'   to \code{m}, the default.
#' @param kernel a character determining the kernel to use.  Currently, only
#'   'rbf' is implemented.
#' @param ... additional parameters needed for the kernels.  See details.
#'
#' @return an S3 object with the following components
#'   * `df_sub` the sub-sampled version of `df`
#'   * `dv` pre-multiplication matrix which contains information on the
#'   eigenvalues and eigenvectors of `df_sub`
#'   * `method` 'nystrom'
#'   * `kernel` the input parameter `kernel`
#'   * `kernel_params` parameters passed to `...`
#'
#' @details For the `...` argument, the additional parameters depend on which
#'   kernel is used:
#'   * For `kernel` = 'rbf', specify `sigma` to define kernel bandwidth.
#'
#' @examples
#' df <- data.frame(
#'   X1 = c(2,   3,   4,   5,   6, 7, 8),
#'   X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
#'   X3 = rnorm(7)
#' )
#'
#' fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
#' fm <- predict_kfm_nystrom(fit, df)
#'
#' @export
#' @author Sean Kent
kfm_nystrom <- function(df, m, r, kernel, ...) {
  UseMethod("kfm_nystrom", df)
}

#' @describeIn kfm_nystrom For use on objects of class `data.frame` or `matrix`.
#' @export
kfm_nystrom.default <- function(df, m = nrow(df), r = m, kernel = "rbf", ...) {
  # TODO: check all columns are numeric
  `%ni%` <- Negate(`%in%`)
  kernel_params <- list(...)
  # kernel_params <- list()

  df <- as.matrix(df)
  random_rows <- sample(1:nrow(df), m)
  tmp <- df[random_rows, ]

  if (kernel == "rbf") {
    if("sigma" %ni% names(kernel_params)) {
      message("sigma not specified in ... for kernel 'rbf'.  Defaulting to sigma = 0.05.")
      kernel_params$sigma <- 0.05
    }
    k_hat <- rbf_kernel_matrix(kernel_params$sigma, tmp, tmp)
  } else {
    stop("kernel must be 'rbf'.")
  }

  e <- eigen(k_hat)
  D <- diag(1 / sqrt(e$values[1:r]))
  V <- t(e$vectors[, 1:r])

  return(list(df_sub = tmp,
              dv = D %*% V,
              method = "nystrom",
              kernel = kernel,
              kernel_params = kernel_params))

}

#' @describeIn kfm_nystrom Ignore the information columns with 'bag_label',
#'   'bag_name', and 'instance_name' when calculating kernel approximation.
#'   These columns are re-appended upon prediction.
#' @export
kfm_nystrom.MilData <- function(df, m = nrow(df), r = m, kernel = "rbf", ...) {
  df <- subset(df, select = -c(bag_label, bag_name, instance_name))
  kfm_nystrom.default(df, m, r, kernel, ...)
}


#' Predict feature map based on Nystrom approximation
#'
#' TODO: add description
#'
#' @param object a object from a call to `kfm_nystrom()`
#' @param newx An object containing features for prediction. Usually a data.frame
#'   or matrix. Must contain the same columns as `df` from the fitting.
#'
#' @return a matrix of features that approximate the fitted kernel for each
#'   observation in `newx`.
#'
#' @examples
#' df <- data.frame(
#'   X1 = c(2,   3,   4,   5,   6, 7, 8),
#'   X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
#'   X3 = rnorm(7)
#' )
#'
#' fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
#' fm <- predict_kfm_nystrom(fit, df)
#'
#' @export
#' @author Sean Kent
predict_kfm_nystrom <- function(object, newx) {
  UseMethod("predict_kfm_nystrom", newx)
}

#' @describeIn kfm_nystrom For use on objects of class `data.frame` or `matrix`.
#' @export
predict_kfm_nystrom.default <- function(object, newx) {
  stopifnot("newx must have the same columns as object$df_sub." = colnames(newx) == colnames(object$df_sub))
  newx <- as.matrix(newx)
  if (object$kernel == "rbf") {
    stopifnot("sigma" %in% names(object$kernel_params))
    sigma <- object$kernel_params$sigma
    k <- rbf_kernel_matrix(sigma, newx, object$df_sub)
  } else {
    stop("kernel must be 'rbf'.")
  }

  return(k %*% t(object$dv))
}

#' @describeIn kfm_nystrom Ignore the information columns with 'bag_label',
#'   'bag_name', and 'instance_name' when calculating kernel approximation.
#'   These columns are re-appended upon prediction.
#' @export
predict_kfm_nystrom.MilData <- function(object, newx) {
  info <- subset(newx, select = c(bag_label, bag_name, instance_name))
  newx <- subset(newx, select = -c(bag_label, bag_name, instance_name))
  fm <- predict_kfm_nystrom.default(object, newx)
  cbind(info, fm)
}

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

