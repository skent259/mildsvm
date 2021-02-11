
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
#' @param sampling determines how to sample instances.  Default it 'random'.
#'   For kfm_nystrom.MilData, can specify `sampling` = 'stratified' to ensure
#'   that samples are chosen evenly from bags and instances.  `sampling` can
#'   also be a numeric vector of length `m` of pre-determined samples.
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
kfm_nystrom.default <- function(df, m = nrow(df), r = m, kernel = "rbf", sampling = 'random', ...) {
  # TODO: check all columns are numeric
  `%ni%` <- Negate(`%in%`)
  kernel_params <- list(...)

  df <- as.matrix(df)
  if (is.numeric(sampling)) {
    if (length(sampling) != m)  {
      warning("Length of input 'sampling' is not equal to 'm', reverting to sampling = 'random'.")
      sampling <- sample(1:nrow(df), m)
    }
  } else if (sampling == 'random') {
    sampling <- sample(1:nrow(df), m)
  } else {
    stop("parameter 'sampling' must be a numeric vector or the character 'random'. ")
  }
  df_sub <- df[sampling, ]

  if (kernel == "rbf") {
    if("sigma" %ni% names(kernel_params)) {
      message("sigma not specified in ... for kernel 'rbf'.  Defaulting to sigma = 0.05.")
      kernel_params$sigma <- 0.05
    }
    k_hat <- rbf_kernel_matrix(kernel_params$sigma, df_sub, df_sub)
  } else {
    stop("kernel must be 'rbf'.")
  }

  e <- eigen(k_hat)
  # sometimes when data is duplicated we get 0 eigenvalues and NA columns in the output
  n_rep <- sum(e$values[1:r] < 1e-10)
  if (n_rep > 0) {
    r <- r - n_rep
    warning(paste0("Data chosen in subsample appears to be duplicated, reducing number of features to ", r))
  }
  D <- diag(1 / sqrt(e$values[1:r]))
  V <- t(e$vectors[, 1:r])

  return(list(df_sub = df_sub,
              dv = D %*% V,
              method = "nystrom",
              kernel = kernel,
              kernel_params = kernel_params))

}

#' @describeIn kfm_nystrom Ignore the information columns with 'bag_label',
#'   'bag_name', and 'instance_name' when calculating kernel approximation.
#'   These columns are re-appended upon prediction.
#' @export
kfm_nystrom.MilData <- function(df, m = nrow(df), r = m, kernel = "rbf", sampling = "random", ...) {
  if (sampling[1] == 'stratified') {
    sampling <- bag_instance_sampling(df, m)
  }
  df <- subset(df, select = -c(bag_label, bag_name, instance_name))
  kfm_nystrom.default(df, m, r, kernel, sampling, ...)
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

#' Sample MilData object by bags and instances
#'
#' From a 'MilData' object, return a sample that evenly pulls from the unique
#' bags and unique instances from each bag as much as possible.  This is a form
#' of stratified sampling to avoid randomly sampling many rows from a few bags.
#'
#' @param data a 'MilData' object containing the data
#' @param size a non-negative integer giving the number of rows to choose from
#'   `data`.
#' @return a numeric vector of length `size` indicating which rows were sampled.
#'
#' @examples
#' mil_data <- mildsvm::GenerateMilData(positive_dist = "mvnormal",
#'                                      negative_dist = "mvnormal",
#'                                      remainder_dist = "mvnormal",
#'                                      nbag = 2,
#'                                      ninst = 2,
#'                                      nsample = 2)
#'
#' rows <- bag_instance_sampling(mil_data, 6)
#' table(mil_data$bag_name[rows])
#' table(mil_data$instance_name[rows])
#'
#' rows <- bag_instance_sampling(mil_data, 4)
#' table(mil_data$bag_name[rows])
#' table(mil_data$instance_name[rows])
#'
#' @export
#' @author Sean Kent
bag_instance_sampling <- function(data, size) {
  stopifnot(inherits(data, "MilData"))
  resample <- function(x, ...) x[sample.int(length(x), ...)] # safer version of sample


  bags <- unique(data$bag_name)
  sampled_bags <- resample(c(rep(bags, size %/% length(bags)),
                           sample(bags, size %% length(bags))))
  sampled_instances <- character(size)
  sampled_rows <- numeric(size)

  for (bag in unique(sampled_bags)) {
    ind <- which(bag == sampled_bags)
    k <- length(ind)
    instances <- unique(data$instance_name[which(data$bag_name == bag)])
    sampled_instances[ind] <- resample(c(rep(instances, k %/% length(instances)),
                                       sample(instances, k %% length(instances))))

    for (instance in instances) {
      ind2 <- which(instance == sampled_instances)
      l <- length(ind2)
      rows <- which(data$instance_name == instance)
      sampled_rows[ind2] <- resample(c(rep(rows, l %/% length(rows)),
                                     sample(rows, l %% length(rows))))
    }
  }
  return(sampled_rows)
}

##' This flatten the MilData type of data to regular multiple instance data where each instance is a vector
##'
##' This flatten the MilData type of data to regular multiple instance data where each instance is a vector by extracting distribution sample quantiles, mean and sd.
##' @param data A MilData object.
##' @param qtls Quantiles to be extracted from each instance empirical distribution.
##' @param mean Whether or not to extract mean.
##' @param sd Whether or not to extract median.
##' @return A data.frame that is ready to be used in `MI_SVM()` function.
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 50,
##'                             nsample = 20,
##'                             positive_degree = 3,
##'                             positive_prob = 0.15,
##'                             positive_mean = rep(0, 5))
##' df1 <- build_instance_feature(MilData1, seq(0.05, 0.95, length.out = 10))
##' @importFrom stats quantile
##' @export
##' @author Yifei Liu
build_instance_feature <- function(data, qtls = seq(0.05, 0.95, length.out = 10),
                                   mean = TRUE, sd = TRUE) {
  ## Let's assume here that `data` is a MilData object which looks
  ## something like bag_label | bag_name | instance_name | feature_1 |
  ## ...  bag_label should be one of 0 and 1, where 0 is negative bags
  ## and 1 is positive bags we need to prepare the data into the
  ## following format so that the MI_SVM function can be called.
  ## bag_label | bag_name | instance_name | feature_1 | ...

  ## first we need to convert the distributional features into quantiles
  ## find the quantiles for each instance

  instance_name <- unique(data$instance_name)
  df <- NULL
  for (i in 1:length(instance_name)) {
    data_i <- data[data$instance_name == instance_name[i], -(1:3)]
    qtls_i <- as.vector(apply(data_i, 2, function(x) stats::quantile(x,
                                                                     probs = qtls)))
    if (mean) {
      mean_i <- colMeans(data_i)
      qtls_i <- c(qtls_i, mean_i)
    }

    if (sd) {
      sd_i <- apply(data_i, 2, function(x) sd(x))
      qtls_i <- c(qtls_i, sd_i)
    }
    df <- rbind(df, qtls_i)
  }
  col_name <- as.vector(sapply(colnames(data[, -(1:3)]), function(x) paste0(x,
                                                                            "_", qtls)))
  if (mean) {
    col_name <- c(col_name, paste0(colnames(data[, -(1:3)]), "_mean"))
  }
  if (sd) {
    col_name <- c(col_name, paste0(colnames(data[, -(1:3)]), "_sd"))
  }

  colnames(df) <- col_name
  df <- cbind(unique(data[, 1:3]), as.data.frame(df))
  return(df)
}
