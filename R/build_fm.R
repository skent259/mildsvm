#' Build a feature map on new data
#'
#' Feature maps provide a set of covariates in a transformed space.  The
#' `build_fm()` function creates these covariates based on an object that
#' specifies the feature map and a provided dataset.
#'
#' @param kfm_fit object from a function in the `kfm_*` family, such as
#'   `kfm_nystrom()`.
#' @param new_data data to generate features from.
#'
#' @return a matrix of covariates in the feature space, with the same number of
#'   rows as `new_data`.  If `new_data` is an 'MilData' object, `build_fm()`
#'   will also return the columns containing 'bag_label', 'bag_name',
#'   'instance_name'.
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

#' This flatten the MilData type of data to regular multiple instance data where each instance is a vector
#'
#' This flatten the MilData type of data to regular multiple instance data where each instance is a vector by extracting distribution sample quantiles, mean and sd.
#' @param data A MilData object.
#' @param qtls Quantiles to be extracted from each instance empirical distribution.
#' @param mean Whether or not to extract mean.
#' @param sd Whether or not to extract median.
#' @return A data.frame that is ready to be used in `MI_SVM()` function.
#' @examples
#' MilData1 <- generate_mild_df(positive_dist = 'mvt',
#'                              negative_dist = 'mvnormal',
#'                              remainder_dist = 'mvnormal',
#'                              nbag = 50,
#'                              nsample = 20,
#'                              positive_degree = 3,
#'                              positive_prob = 0.15,
#'                              positive_mean = rep(0, 5))
#' df1 <- build_instance_feature(MilData1, seq(0.05, 0.95, length.out = 10))
#' @importFrom stats quantile
#' @export
#' @author Yifei Liu
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
