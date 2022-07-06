#' Generate mild_df using multivariate t and normal distributions.
#'
#' This function samples multiple instance distributional data (a `mild_df`
#' object) where each row corresponds to a sample from a given instance
#' distribution.  Instance distributions can be multivariate t and normal, with
#' mean and variance parameters that can be fixed or sampled based on prior
#' parameters.  These instances are grouped into bags and the bag labels
#' follow the standard MI assumption.
#'
#' The first consideration to use this function is to determine the number of
#' bags, instances per bag, and samples per instance using the `nbag`, `ninst`,
#' and `nsample` arguments. Next, one must consider the number of covariates
#' `ncov`, and how those covariates will differ between instances with positive
#' and negative labels.  Some covariates can be common between the positive and
#' negative instances, which we call the remainder distribution.  Use `nimp_pos`
#' and `nimp_neg` to specify the index of the important (non-remainder)
#' covariates in the distributions with positive and negative instance labels.
#'
#' The structure of how many instances/bags are positive and negative is
#' determined by `positive_prob` or the joint specification of
#' `positive_bag_prob` and `n_noise_inst`. In the first case, instances labels
#' have independent Bernoulli draws based on `positive_prob` and bag labels are
#' determined by the standard MI assumption (i.e. positive if any instance in
#' the bag is positive).  In the second case, bag labels are drawn independently
#' as Bernoilli with `positive_bag_prob` chance of success.  Each positive bag
#' will be given `n_noise_inst` values with instance label of 0, and the
#' remaining with instance label of 1.
#'
#' The remaining arguments are used to determine the distributions used for the
#' positive, negative, and remaining features.  Each argument will be a vector
#' of list of length 3 corresponding to these 3 different groups.  To create
#' different distributions, the strategy is to first draw the mean parameter
#' from Normal(`mean`, `sd_of_mean` * I) and the covariance parameter from
#' Wishart(`df_wishart_cov`, `cov`), with expectation equal to `cov`.  Then we
#' can sample i.i.d. draws from the specified distribution (either multivariate
#' normal or student's t). To ensure that each instance distribution has the
#' same mean, set `sd_of_mean` to 0. To ensure that each instance distribution
#' has the same covariance, set `sample_cov = FALSE`.
#'
#' The final data.frame will have `nsample` * `nbag` * `ninst` rows and `ncov +
#' 3` columns including the bag_label, bag_name, instance_name, and `ncov`
#' sampled covariates.
#'
#' @param nbag The number of bags (default 50).
#' @param ninst The number of instances for each bag (default 4).
#' @param nsample The number of samples for each instance (default 50).
#' @param ncov The number of total covariates (default 10).
#' @param nimp_pos An index of important covariates for positve instances
#'   (default `1:ncov`).
#' @param nimp_neg An index of important covariates for negative instances
#'   (default `1:ncov`).
#'   (default `1:ncov`).
#' @param positive_prob A numeric value between 0 and 1 indicating the
#'   probability of an instance being positive (default 0.2).
#' @param dist A vector (length 3) of distributions for the positive, negative, and
#'   remaining instances, respectively.  Distributions can be one of
#'   `'mvnormal'` for multivariate normal or `'mvt'` for multivariate
#'   student's t.
#' @param mean A list (length 3) of mean vectors for the positive, negative, and
#'   remaining distributions.  `mean[[1]]` should match `nimp_pos` in length;
#'   `mean[[2]]` should match `nimp_neg` in length.
#' @param sd_of_mean A vector (length 3) of standard deviations in sampling the
#'   mean for positive, negative, and remaining distributions, where the prior
#'   is given by `mean`.  Use `sd_of_mean = c(0, 0, 0)` to keep the mean
#'   consistent across all instances.
#' @param cov A list (length 3) of covariance matrices for the positive,
#'   negative, and remaining distributions.  `cov[[3]]` should be an integer
#'   since the dimension of remaining features can vary depending on if the
#'   important distribution is positive or negative.
#' @param sample_cov A logical value for whether to sample the covariance for
#'   each distribution.  If `FALSE` (the default), each covariance is fixed at
#'   `cov`. If `TRUE`, the prior is given by `cov` and sampled from a Wishart
#'   distribution with `df_wishart_cov` degrees of freedom to have an
#'   expectation of `cov`.
#' @param df_wishart_cov A vector (length 3) of degrees-of-freedom to use in the
#'   Wishart covariance matrix sampling.
#' @param degree A vector (length 3) of degrees-of-freedom used when any of
#'   `dist` is `'mvt'`.  This parameter is ignored when `dist[i] == 'mvnormal'`,
#'   in which case `NA` can be specified.
#' @param positive_bag_prob A numeric value between 0 and 1 indicating the
#'   probability of a bag being positive. Must be specified jointly with
#'   `n_noise_inst`, in which case `positive_prob` is ignored.  If `NULL` (the
#'   default), instance labels are sampled first according to `positive_prob`.
#' @param n_noise_inst An integer indicating the number of negative instances in
#'   a positive bag. Must be specified jointly with `positive_bag_prob`.
#'   `n_noise_inst` should be less than `ninst`.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A `mild_df` object.
#'
#' @examples
#' set.seed(8)
#' mild_data <- generate_mild_df(nbag = 7, ninst = 3, nsample = 20,
#'                               ncov = 2,
#'                               nimp_pos = 1,
#'                               dist = rep("mvnormal", 3),
#'                               mean = list(
#'                                 rep(5, 1),
#'                                 rep(15, 2),
#'                                 0
#'                               ))
#'
#' library(dplyr)
#' distinct(mild_data, bag_label, bag_name, instance_name)
#' split(mild_data[, 4:5], mild_data$instance_name) %>%
#'   sapply(colMeans) %>%
#'   round(2) %>%
#'   t()
#' @export
#' @author Yifei Liu, Sean Kent
generate_mild_df <- function(
    nbag = 50,
    ninst = 4,
    nsample = 50,
    ncov = 10,
    nimp_pos = 1:ncov,
    nimp_neg = 1:ncov,
    positive_prob = 0.2,
    dist = c("mvt", "mvnormal", "mvnormal"),
    mean = list(rep(0, length(nimp_pos)), rep(0, length(nimp_neg)), 0),
    sd_of_mean = c(0.5, 0.5, 0.5),
    cov = list(diag(1, nrow = length(nimp_pos)), diag(1, nrow = length(nimp_neg)), 1),
    sample_cov = FALSE,
    df_wishart_cov = c(length(nimp_pos), length(nimp_neg), ncov - length(nimp_pos)),
    degree = c(3, NA, NA),
    positive_bag_prob = NULL,
    n_noise_inst = NULL,
    ...) {
  pos <- 1
  neg <- 2
  rem <- 3

  .check_args(dist, degree, nimp_pos, nimp_neg, ncov)
  dist[pos] <- match.arg(dist[pos], c("mvnormal", "mvt"))
  dist[neg] <- match.arg(dist[neg], c("mvnormal", "mvt"))
  dist[rem] <- match.arg(dist[rem], c("mvnormal", "mvt"))

  # Create bag, instance, label structure
  bag_name <- paste0("bag", 1:nbag)
  bag_name <- rep(bag_name, each = ninst)
  inst_name <- paste0(bag_name, paste0("inst", 1:ninst))

  if (!is.null(positive_bag_prob)) {
    if (is.null(n_noise_inst)) {
      stop("Must specify `n_noise_inst` when `positive_bag_prob` is specified.")
    } else if (n_noise_inst >= ninst) {
      stop("Must have `n_noise_inst` < `ninst`.")
    }
    bag_label <- stats::rbinom(nbag, 1, positive_bag_prob)
    inst_label <- lapply(1:nbag, function(i) {
      if (bag_label[i] == 1) {
        rep(c(0, 1), c(n_noise_inst, ninst-n_noise_inst))
      } else {
        rep(0, ninst)
      }
    })
    inst_label <- unlist(inst_label)
  } else {
    inst_label <- stats::rbinom(nbag * ninst, 1, positive_prob)
    bag_label <- classify_bags(inst_label, bag_name, condense = FALSE)
  }

  # Sample features for each instance
  .generate_instance_samples <- function(j) {
    x_ij <- matrix(NA, nsample, ncov)

    if (inst_label[j] == 1) {
      imp <- pos
      nimp <- nimp_pos
    } else if (inst_label[j] == 0) {
      imp <- neg
      nimp <- nimp_neg
    }

    dist_mean <- sapply(mean[[imp]], stats::rnorm, n = 1, sd = sd_of_mean[[imp]])
    if (sample_cov) {
      dist_cov <- stats::rWishart(1, df_wishart_cov[[imp]], cov[[imp]])
      dist_cov <- dist_cov[, , 1] / df_wishart_cov[[imp]]
    } else {
      dist_cov <- cov[[imp]]
    }

    imp_features <- switch(
      dist[imp],
      mvt = mvtnorm::rmvt(n = nsample,
                          sigma = dist_cov / (degree[imp] / (degree[imp] - 2)),
                          df = degree[imp],
                          delta = dist_mean,
                          type = "shifted"),
      mvnormal = mvtnorm::rmvnorm(n = nsample,
                                  mean = dist_mean,
                                  sigma = dist_cov)
    )

    p_rem <- ncov - length(nimp)
    if (p_rem > 0) {

      dist_mean <- stats::rnorm(p_rem, mean[[rem]], sd_of_mean[[rem]])
      if (sample_cov) {
        dist_cov <- stats::rWishart(1, df_wishart_cov[[rem]], diag(cov[[rem]], p_rem))
        dist_cov <- dist_cov[, , 1] / df_wishart_cov[[rem]]
      } else {
        dist_cov <- diag(cov[[rem]], p_rem)
      }

      rem_features <- switch(
        dist[rem],
        mvt = mvtnorm::rmvt(n = nsample,
                            sigma = dist_cov / (degree[rem] / (degree[rem] - 2)),
                            df = degree[rem],
                            delta = dist_mean,
                            type = "shifted"),
        mvnormal = mvtnorm::rmvnorm(n = nsample,
                                    mean = dist_mean,
                                    sigma = dist_cov)
      )
    } else {
      rem_features <- numeric(nsample)
    }

    x_ij[, nimp] <- imp_features
    x_ij[, -nimp] <- rem_features
    colnames(x_ij) <- paste0("X", 1:ncov)
    return(as.data.frame(x_ij))
  }

  x <- lapply(1:(nbag*ninst), .generate_instance_samples)
  x <- do.call(rbind, x)

  # Combine for output
  bag_label <- rep(bag_label, each = nsample)
  bag_name <- rep(bag_name, each = nsample)
  inst_name <- rep(inst_name, each = nsample)
  inst_label <- rep(inst_label, each = nsample)

  bag_label <- as.numeric(bag_label)

  out <- tibble::tibble(
    bag_label,
    bag_name,
    instance_name = inst_name,
    x,
    instance_label = inst_label
  )

  return(as_mild_df(out))
}

#' Check arguments for `generate_mild_df()`
#' @inheritParams generate_mild_df
#' @param pos Index of positive distribution (default = 1)
#' @param neg Index of positive distribution (default = 2)
#' @param rem Index of positive distribution (default = 3)
#' @noRd
.check_args <- function(dist, degree, nimp_pos, nimp_neg, ncov, pos = 1, neg = 2, rem = 3) {
  for (k in c(pos, neg, rem)) {
    if (dist[k] == "mvt" && is.na(degree[k])) {
      msg <- paste0("Must specify `degree[", k, "]` when `dist[", k, "] == 'mvt'`.")
      stop(msg, call. = FALSE)
    }
  }
  if (length(nimp_pos) > ncov) {
    stop("The number of important variables `length(nimp_pos)` can't exceed `ncov`.", call. = FALSE)
  }
  if (length(nimp_neg) > ncov) {
    stop("The number of important variables `length(nimp_neg)` can't exceed `ncov`.", call. = FALSE)
  }
}
