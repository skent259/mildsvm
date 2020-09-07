if (getRversion() >= "2.15.1") utils::globalVariables(c("bag_name", "bag_label",
                                                        "instance_label", "instance_name", "label", "bag_score_pred", "instance_label_pred",
                                                        "instance_score_pred"), add = FALSE)

##' Constructor function for MilData object
##'
##' Constructor function for MilData object which gives a more convenient way of handling multiple instance data. If x contains a column for 'instance_label', then this function will add `instance_label` in the object attributes but will remove it from the data.frame.
##' @param x A data.frame which contains 'bag_label', 'bag_name', 'instance_name' as the first three columns and the rest as features.
##' @return A MilData object (S3) which is built on data.frame
##' @examples
##' new_MilData(x = data.frame('bag_label' = c(1, 1),
##'                            'bag_name' = rep('bag_1', 2),
##'                            'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2'),
##'                            'X1' = c(-0.4, 0.5))
##'                            )
##' new_MilData(x = data.frame('bag_label' = c(1, 1),
##'                            'bag_name' = rep('bag_1', 2),
##'                            'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2'),
##'                            'X1' = c(-0.4, 0.5),
##'                            'instance_label' = c(0, 1))
##'                            )
##' @export
##'
##' @author Yifei Liu
new_MilData <- function(x = data.frame()) {
    stopifnot(is.data.frame(x))

    if (is.null(x$instance_label)) {
        x_instance_label <- NULL
    } else {
        x_instance_label = x$instance_label
        x$instance_label = NULL
    }
    structure(x, class = c("MilData", "data.frame"), instance_label = x_instance_label)
}

##' Validation function for MilData object
##'
##' x should have the first three column names representing 'bag_label', 'bag_name' and 'instance_name'. The rest should all be features. Therefore, the bag_label of the same bag_name should be the same.
##'
##' @param x A data.frame. x should have the first three column names representing 'bag_label', 'bag_name' and 'instance_name'. The rest should all be features. Therefore, the bag_label of the same bag_name should be the same.
##' @return A MilData object if it checks out, or an error message
##' @examples
##' x = data.frame('bag_label' = c(1, 1, 0),
##'               'bag_name' = c(rep('bag_1', 2), 'bag_2'),
##'               'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
##'               'X1' = c(-0.4, 0.5, 2),
##'               'instance_label' = c(0, 1, 0))
##' validate_MilData(x)
##' @import dplyr
##' @export
##' @author Yifei Liu
validate_MilData <- function(x) {
    value = x

    colnames <- colnames(value)
    if (is.null(colnames)) {
        stop("Colnames should be given to x")
        call. = FALSE
    }
    if (any(colnames[1:3] != c("bag_label", "bag_name", "instance_name"))) {
        stop("The first three column names should be 'bag_label', 'bag_name', 'instance_name'")
        call. = FALSE
    }

    ## bag_label should be of numeric 0, 1 or T, F
    if (!all(as.numeric(unique(value$bag_label)) %in% c(0, 1))) {
        stop("Bag_label should be one of 0 (negative) or 1 (positive)")
        call. = FALSE
    }

    ## verify that the same bag names correspond to the same bag label.
    consist <- unclass(value %>% dplyr::group_by(bag_name) %>% dplyr::summarise(consist = length(unique(bag_label)) ==
                                                                                    1))$consist
    if (any(consist != TRUE)) {
        stop(paste("There is inconsistency bag labeling at bag ", unique(value$bag_name)[which(consist !=
                                                                                                   TRUE)]))
        call. = FALSE
    }

    ## verify that the bag_label is determined by whether there is at
    ## least a positive label in instances of a bag.

    x_instance_label <- attr(x, "instance_label")
    if (!is.null(x_instance_label)) {
        value$instance_label = x_instance_label
        consist_inst <- unclass(value %>% dplyr::group_by(bag_name) %>%
                                    dplyr::summarise(consist_inst = any(as.logical(instance_label)) ==
                                                         unique(bag_label)))$consist_inst
        if (any(consist_inst != TRUE)) {
            stop(paste("There is inconsistency in instance-bag labelling in bag ",
                       unique(value$bag_name)[which(consist_inst != TRUE)]))
            call. = FALSE
        }
    }
    x
}

##' Helper function to generate MilData object
##'
##' Helper function to generate MilData object. Changes the first three columns
##'  to the required column names ('bag_label', 'bag_name', 'instance_name').
##'  If the original `bag_label` is a factor, will use the first level as
##'  negative label and code it as numeric '0' and use the second level as
##'  positive label and code it as numeric '1'. Levels >= 3 not supported and
##'  will raise an error.
##'
##' The column name for instance label should always be `instance_label` otherwise will be taken as features of x.
##' @param x A data.frame that to be used to create MilData object.
##' @return A MilData object.
##' @examples
##' x = data.frame('bag_LABEL' = factor(c(1, 1, 0)),
##'               'bag_name' = c(rep('bag_1', 2), 'bag_2'),
##'               'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
##'               'X1' = c(-0.4, 0.5, 2),
##'               'instance_label' = c(0, 1, 0))
##' MilData(x)
##' @export
##' @author Yifei Liu
MilData <- function(x = data.frame()) {
    if (any(colnames(x)[1:3] != c("bag_label", "bag_name", "instance_name"))) {
        colnames(x)[1:3] <- c("bag_label", "bag_name", "instance_name")
        message("Changed the first three columns to 'bag_label', 'bag_name', 'instance_name'")
    }
    if (is.factor(x$bag_label)) {
        levels = levels(x$bag_label)
        if (length(levels) == 1)
            stop("There is only one level for the bag label!")
        x$bag_label = unclass(x$bag_label) - 1
        message(paste("Using the first level of bag label, ", levels[1],
                      ", as negative label and the second level, ", levels[2],
                      ", as positive label"))
    }
    validate_MilData(new_MilData(x))
}

##' Function to generate MilData using multivariate t and normal distributions.
##'
##' This function generates multiple instance data (a MilData object) where each instance corresponds to `nsample` i.i.d. observations. To use the function, one needs to provide the total number of covariates `ncov`, to generate, and the index of the important covariates for positive and negative instances, `nimp_pos` and `nimp_neg`, respectively. The distribution of these important covariates will be a multivariate t or multivariate normal distribution, specified by `positive_dist`, `negative_dist`. The `remainder_dist` specifies the distribution of the unimportant covariates where their distributions do not differ in positive or negative instances. The `ninst` covariates specifies the number of instances in each bag and `nbag` gives how many bags in total will be generated. Hence in total the number of observations is `nsample` * `nbag` * `ninst`.
##'
##' The default parameters for the remainder distribution is zero mean and identity covariance matrix.
##'
##' @param positive_dist The distribution to be generated for important covariates of positive instances, one of 'mvt' and 'mvnormal'
##' @param negative_dist The distribution to be generated for important covariates of negative instances, one of 'mvt' and 'mvnormal'
##' @param remainder_dist The distribution to be generated for unimportant covariates, one of 'mvt' and 'mvnormal'.
##' @param ncov Number of total covariates.
##' @param nimp_pos Index of important covariates for positve covariates.
##' @param nimp_neg Index of important covariates for negative covariates.
##' @param nsample Number of observations for each instance.
##' @param ninst Number of instances for each bag.
##' @param nbag Number of bags.
##' @param positive_mean The mean vector of important covariates for positive instances, should be of same length as `nimp_pos`
##' @param positive_cov The covariance matrix of important covariates for positive instances.
##' @param negative_mean The mean vector of important covariates for negative instances, should be of same length as `nimp_neg`
##' @param negative_cov The covariance matrix of important covariates for negative instances.
##' @param positive_prob A numberic number between 0 and 1 indicating the probability of an instance being positive.
##' @param ... Other covariates when using t distribution or using a different bag labeling scheme. If using t distribution, should pass 'positive_degree', 'negative_degree' or 'remainder_degree' in '...' if any of these distributions is specified as 'mvt'. If want to generate the label of each bag first, one needs to specify 'positive_bag_prob' and 'n_noise_inst' in '...'. 'positive_bag_prob' is the Bernoulli success probability of a bag being a positive bag, and 'n_noise_inst' is the number of negative instances in a positive bag, should be strictly less than 'ninst'.
##' @return A MilData object.
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             positive_degree = 3)
##' @export
##' @import mvtnorm dplyr
##' @importFrom stats rbinom
##' @author Yifei Liu
GenerateMilData <- function(positive_dist = c("mvt", "mvnormal"),
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
                            positive_prob = 0.2, ...) {
    ## remainder follows a distr with mean 0 and scale matrix identity.
    ## (Hence different cov matrix for t and normal) the positive_cov and
    ## negative_cov's are exactly the cov for the positive or negative
    ## distributions, whether or not they are t or normal.  should pass
    ## 'positive_degree', 'negative_degree' or 'remainder_degree' in '...'
    ## if any of these distributions is specified as 'mvt' sanity check

    args = list(...)

    data <- NULL
    for (i in 1:nbag) {
        data_i <- NULL
        if (!is.null(args$positive_bag_prob)) {
            bag_label <- stats::rbinom(1, 1, args$positive_bag_prob)
            if (is.null(args$n_noise_inst)) {
                stop("Needs to supply 'n_noise_inst' in '...' when using 'positive_bag_prob' as an argument!")
            } else if (args$n_noise_inst >= ninst) {
                stop(" 'args$n_noise_inst' should be at least 1 less than 'ninst'!")
            }
            if (bag_label == 1) {
                ins_labels <- c(rep(0, args$n_noise_inst), rep(1, ninst - args$n_noise_inst))  ## positive label
            } else ins_labels <- rep(0, ninst)
        } else {
            ins_labels <- NULL
            bag_label <- NULL
        }

        for (j in 1:ninst) {
            if (is.null(ins_labels))
                instance_label <- stats::rbinom(1, 1, positive_prob) else instance_label <- ins_labels[j]

                if (instance_label == 1) {
                    positive_features <-
                        switch(positive_dist,
                               mvt = if (is.null(args$positive_degree)) {
                                   stop("Needs to supply 'positive_degree' in '...' when using mvt for positive distribution!")
                               } else {
                                   mvtnorm::rmvt(n = nsample,
                                                 sigma = positive_cov/(args$positive_degree/(args$positive_degree - 2)),
                                                 df = args$positive_degree,
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
                        remainder_features <-
                            switch(remainder_dist,
                                   mvt = if (is.null(args$remainder_degree)) {
                                       stop("Needs to supply 'remainder_degree' in '...' when using mvt for remainder distribution!")
                                   } else {
                                       mvtnorm::rmvt(n = nsample,
                                                     sigma = diag(1, remainder_p)/(args$remainder_degree/(args$remainder_degree - 2)),
                                                     df = args$remainder_degree,
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
                    negative_features <-
                        switch(negative_dist,
                               mvt = if (is.null(args$negative_degree)) {
                                   stop("Needs to supply 'negative_degree' in '...' when using mvt for negative distribution!")
                               } else {
                                   mvtnorm::rmvt(n = nsample,
                                                 sigma = negative_cov/(args$negative_degree/(args$negative_degree - 2)),
                                                 df = args$negative_degree,
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
                        remainder_features <-
                            switch(remainder_dist,
                                   mvt = if (is.null(args$remainder_degree)) {
                                       stop("Needs to supply 'remainder_degree' in '...' when using mvt for remainder distribution!")
                                   } else {
                                       mvtnorm::rmvt(n = nsample,
                                                     sigma = diag(1, remainder_p)/(args$remainder_degree/(args$remainder_degree - 2)),
                                                     df = args$remainder_degree,
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
        if (is.null(bag_label))
            bag_label <- rep(any(data_i[, ncov + 2] == 1), nrow(data_i))
        data <- rbind(data, cbind(bag_label, bag_name, data_i))
    }
    colnames(data) = c("bag_label", "bag_name", "instance_name", paste0("X",
                                                                        1:ncov), "instance_label")
    data <- data %>% dplyr::mutate(bag_label = as.numeric(bag_label),
                                   bag_name = as.character(bag_name), instance_name = as.character(instance_name))
    data <- MilData(data)
    return(data)
}
