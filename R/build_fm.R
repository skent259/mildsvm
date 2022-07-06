#' Build a feature map on new data
#'
#' Feature maps provide a set of covariates in a transformed space.  The
#' `build_fm()` function creates these covariates based on an object that
#' specifies the feature map and a provided dataset.
#'
#' @param kfm_fit An object from a function in the `kfm_*` family, such as
#'   [kfm_nystrom()].
#' @param new_data The data to generate features from.
#' @param ... Additional arguments for methods.
#'
#' @return A matrix of covariates in the feature space, with the same number of
#'   rows as `new_data`.  If `new_data` is a `mild_df` object, `build_fm()`
#'   will also return the columns containing 'bag_label', 'bag_name',
#'   'instance_name'.
#'
#' @seealso
#' * [kfm_nystrom()] fit a Nystrom kernel feature map approximation.
#' * [kfm_exact()] create an exact kernel feature map.
#'
#' @examples
#' df <- data.frame(
#'   X1 = c(2,   3,   4,   5,   6, 7, 8),
#'   X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
#'   X3 = rnorm(7)
#' )
#'
#' fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.05)
#' fm <- build_fm(fit, df)
#'
#' fit <- kfm_exact(kernel = "polynomial", degree = 2, const = 1)
#' fm <- build_fm(fit, df)
#'
#' @author Sean Kent
#' @export
build_fm <- function(kfm_fit, new_data, ...) {
  UseMethod("build_fm")
}

#' Flatten `mild_df` data to the instance level
#'
#' Flatten `mild_df` type of data to regular multiple instance data where
#' each instance is a vector by extracting distribution sample quantiles, mean
#' and sd.
#'
#' @param data A `mild_df` object.
#' @param qtls Quantiles to be extracted from each instance empirical
#'   distribution.
#' @param mean A logical for whether or not to extract mean.
#' @param sd A logical for whether or not to extract standard deviation.
#'
#' @return A summarized data.frame at the instance level.
#'
#' @seealso [summarize_samples()] for a more general way to make a similar data
#'   frame.
#'
#' @examples
#' mild_df1 <- generate_mild_df(positive_degree = 3, nbag = 3)
#' df1 <- build_instance_feature(mild_df1, seq(0.05, 0.95, length.out = 10))
#'
#' @export
#' @author Yifei Liu
build_instance_feature <- function(data,
                                   qtls = seq(0.05, 0.95, length.out = 10),
                                   mean = TRUE,
                                   sd = TRUE) {

  data <- as.data.frame(validate_mild_df(data))
  instance_name <- unique(data$instance_name)
  df <- NULL
  for (i in seq_along(instance_name)) {
    data_i <- data[data$instance_name == instance_name[i], - (1:3), drop = FALSE]
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
  col_name <- as.vector(sapply(colnames(data[, - (1:3), drop = FALSE]), function(x) paste0(x,
                                                                            "_", qtls)))
  if (mean) {
    col_name <- c(col_name, paste0(colnames(data[, - (1:3), drop = FALSE]), "_mean"))
  }
  if (sd) {
    col_name <- c(col_name, paste0(colnames(data[, - (1:3), drop = FALSE]), "_sd"))
  }

  colnames(df) <- col_name
  df <- cbind(unique(data[, 1:3, drop = FALSE]), as.data.frame(df))
  return(df)
}
