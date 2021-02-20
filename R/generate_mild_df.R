#' Function to generate mild_df using multivariate t and normal distributions.
#'
#' This function generates multiple instance data (a `mild_df` object) where
#' each row corresponds to a sample from a given instance distribution.
#' Instances are grouped into bags and the bag labels follow the standard MI
#' assumption.
#'
#' To use the function, one needs to provide the total number of covariates
#' `ncov`, to generate, and the index of the important covariates for positive
#' and negative instances, `nimp_pos` and `nimp_neg`, respectively. The
#' distribution of these important covariates will be a multivariate t or
#' multivariate normal distribution, specified by `positive_dist`,
#' `negative_dist`. The `remainder_dist` specifies the distribution of the
#' unimportant covariates where their distributions do not differ in positive or
#' negative instances. `nsample` i.i.d draws will be taken from the respective
#' distributions for each instance. The `ninst` parameter specifies the number
#' of instances in each bag and `nbag` gives how many bags in total will be
#' generated. Hence in total the number of observations is `nsample` * `nbag` *
#' `ninst`.
#'
#' The default parameters for the remainder distribution is zero mean and
#' identity covariance matrix.
#'
#' @param positive_dist The distribution to be generated for important
#'   covariates of positive instances (default 'mvt').
#' @param negative_dist The distribution to be generated for important
#'   covariates of negative instances (default 'mvnormal').
#' @param remainder_dist The distribution to be generated for unimportant
#'   covariates (default 'mvnormal').
#' @param ncov The number of total covariates (default 10).
#' @param nimp_pos An index of important covariates for positve covariates
#'   (default `1:5`).
#' @param nimp_neg An index of important covariates for negative covariates
#'   (default `1:5`).
#' @param nsample The number of observations for each instance (default 50).
#' @param ninst The number of instances for each bag (default 4).
#' @param nbag The number of bags (default 50).
#' @param positive_mean The mean vector of important covariates for positive
#'   instances. This should be of same length as `nimp_pos`
#' @param positive_cov The covariance matrix of important covariates for
#'   positive instances.
#' @param negative_mean The mean vector of important covariates for negative
#'   instances, should be of same length as `nimp_neg`
#' @param negative_cov The covariance matrix of important covariates for
#'   negative instances.
#' @param positive_prob A numberic number between 0 and 1 indicating the
#'   probability of an instance being positive.
#' @param ... Other covariates when using t distribution or using a different
#'   bag labeling scheme, such as:
#'   * `positive_degree` The distribution degree, if `positive_dist = 'mvt'`.
#'   * `negative_degree` The distribution degree, if `negative_dist = 'mvt'`.
#'   * `remainder_degree` The distribution degree, if `remainder_dist = 'mvt'`.
#'   * `positive_bag_prob` The Bernoulli success probability of a bag being a
#'   positive bag. Specify this argument jointly with `n_noise_inst` to generate
#'   the label of each bag first.
#'   * `n_noise_inst` The number of negative instances in a positive bag.  This
#'   should be strictly less that `ninst`.
#'
#' @return A mild_df object.
#'
#' @examples
#' mild_df1 <- generate_mild_df(positive_dist = 'mvt',
#'                              negative_dist = 'mvnormal',
#'                              remainder_dist = 'mvnormal',
#'                              positive_degree = 3)
#' @export
#' @author Yifei Liu
generate_mild_df <- function(positive_dist = c("mvt", "mvnormal"),
                             negative_dist = c("mvnormal", "mvt"),
                             remainder_dist = c("mvnormal", "mvt"),
                             ncov = 10,
                             nimp_pos = 1:5,
                             nimp_neg = 1:5,
                             nsample = 50,
                             ninst = 4,
                             nbag = 50,
                             positive_mean = rep(0, length(nimp_pos)),
                             positive_cov = diag(1, nrow = length(nimp_pos)),
                             negative_mean = rep(0, length(nimp_neg)),
                             negative_cov = diag(1, nrow = length(nimp_neg)),
                             positive_prob = 0.2, ...)
{
    # remainder follows a distr with mean 0 and scale matrix identity. (Hence
    # different cov matrix for t and normal) the positive_cov and negative_cov's
    # are exactly the cov for the positive or negative distributions, whether or
    # not they are t or normal.  should pass 'positive_degree',
    # 'negative_degree' or 'remainder_degree' in '...' if any of these
    # distributions is specified as 'mvt' sanity check

    positive_dist <- match.arg(positive_dist)
    negative_dist <- match.arg(negative_dist)
    remainder_dist <- match.arg(remainder_dist)
    dots = list(...)

    data <- NULL
    for (i in 1:nbag) {
        data_i <- NULL
        if (!is.null(dots$positive_bag_prob)) {
            bag_label <- stats::rbinom(1, 1, dots$positive_bag_prob)
            if (is.null(dots$n_noise_inst)) {
                stop("Needs to supply 'n_noise_inst' in '...' when using 'positive_bag_prob' as an argument!")
            } else if (dots$n_noise_inst >= ninst) {
                stop(" 'args$n_noise_inst' should be at least 1 less than 'ninst'!")
            }
            if (bag_label == 1) {
                ins_labels <- c(rep(0, dots$n_noise_inst), rep(1, ninst - dots$n_noise_inst))  ## positive label
            } else ins_labels <- rep(0, ninst)
        } else {
            ins_labels <- NULL
            bag_label <- NULL
        }

        for (j in 1:ninst) {
            if (is.null(ins_labels)) {
                instance_label <- stats::rbinom(1, 1, positive_prob)
            } else {
                instance_label <- ins_labels[j]
            }

            if (instance_label == 1) {
                positive_features <- switch(
                    positive_dist,
                    mvt = if (is.null(dots$positive_degree)) {
                        stop("Needs to supply 'positive_degree' in '...' when using mvt for positive distribution!")
                    } else {
                        mvtnorm::rmvt(n = nsample,
                                      sigma = positive_cov/(dots$positive_degree/(dots$positive_degree - 2)),
                                      df = dots$positive_degree,
                                      delta = positive_mean,
                                      type = "shifted")
                    },
                    mvnormal = mvtnorm::rmvnorm(n = nsample,
                                                mean = positive_mean,
                                                sigma = positive_cov)
                )
                remainder_p <- ncov - length(nimp_pos)
                data_ij <- matrix(NA, nsample, ncov)
                data_ij[, nimp_pos] <- positive_features
                if (remainder_p > 0) {
                    remainder_features <- switch(
                        remainder_dist,
                        mvt = if (is.null(dots$remainder_degree)) {
                            stop("Needs to supply 'remainder_degree' in '...' when using mvt for remainder distribution!")
                        } else {
                            mvtnorm::rmvt(n = nsample,
                                          sigma = diag(1, remainder_p)/(dots$remainder_degree/(dots$remainder_degree - 2)),
                                          df = dots$remainder_degree,
                                          delta = rep(0, remainder_p),
                                          type = "shifted")
                        },
                        mvnormal = mvtnorm::rmvnorm(n = nsample,
                                                    mean = rep(0, remainder_p), sigma = diag(1, remainder_p))
                    )
                    data_ij[, -nimp_pos] <- remainder_features
                } else if (remainder_p < 0) {
                    stop("The number of important variables exceeds the number of total variables!")
                }

            } else {
                negative_features <- switch(
                    negative_dist,
                    mvt = if (is.null(dots$negative_degree)) {
                        stop("Needs to supply 'negative_degree' in '...' when using mvt for negative distribution!")
                    } else {
                        mvtnorm::rmvt(n = nsample,
                                      sigma = negative_cov/(dots$negative_degree/(dots$negative_degree - 2)),
                                      df = dots$negative_degree,
                                      delta = negative_mean,
                                      type = "shifted")
                    },
                    mvnormal = mvtnorm::rmvnorm(n = nsample,
                                                mean = negative_mean,
                                                sigma = negative_cov)
                )
                remainder_p <- ncov - length(nimp_neg)

                data_ij <- matrix(NA, nsample, ncov)
                data_ij[, nimp_neg] <- negative_features
                if (remainder_p > 0) {
                    remainder_features <- switch(
                        remainder_dist,
                        mvt = if (is.null(dots$remainder_degree)) {
                            stop("Needs to supply 'remainder_degree' in '...' when using mvt for remainder distribution!")
                        } else {
                            mvtnorm::rmvt(n = nsample,
                                          sigma = diag(1, remainder_p)/(dots$remainder_degree/(dots$remainder_degree - 2)),
                                          df = dots$remainder_degree,
                                          delta = rep(0, remainder_p),
                                          type = "shifted")
                        },
                        mvnormal = mvtnorm::rmvnorm(n = nsample,
                                                    mean = rep(0, remainder_p),
                                                    sigma = diag(1, remainder_p))
                    )
                    data_ij[, -nimp_neg] <- remainder_features
                } else if (remainder_p < 0) {
                    stop("The number of important variables exceeds the number of total variables!")
                }
            }

            data_ij <- cbind(rep(paste0("bag", i, "inst", j), nsample),
                             as.data.frame(data_ij), rep(instance_label, nsample))
            data_i <- rbind(data_i, data_ij)
        }
        bag_name <- rep(paste0("bag", i), nrow(data_i))
        if (is.null(bag_label)) {
            bag_label <- rep(any(data_i[, ncov + 2] == 1), nrow(data_i))
        }
        data <- rbind(data, cbind(bag_label, bag_name, data_i))
    }
    colnames(data) = c("bag_label", "bag_name", "instance_name",
                       paste0("X", 1:ncov), "instance_label")

    data$bag_label <- as.numeric(data$bag_label)
    data$bag_name <- as.character(data$bag_name)
    data$instance_name <- as.character(data$instance_name)

    return(as_mild_df(data))
}
