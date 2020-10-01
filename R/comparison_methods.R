
## define a new class 'smmBag'
new_smmBag <- function(x = list()) {
    stopifnot(is.list(x))
    structure(x, class = c("smmBag"))
}

validate_smmBag <- function(x) {
    if (!is.list(x)) {
        stop("x should be a list")
        call. = FALSE
    }
    if (is.null(x$object)) {
        stop("x should have the 'object' element")
        call. = FALSE
    } else if (!"smm" %in% class(x$object)) {
        stop(" x$object should be of class 'smm'")
        call. = FALSE
    }
    if (is.null(x$traindata)) {
        stop(" x should have the 'traindata' element")
        call. = FALSE
    }
    x
}

##' Function to perform Support Measure Machines for MilData objects.
##'
##' This function solves the mil with distributional data
##' classification problem using the naive SMM method by regarding bag
##' label as instance label.
##' @param data A MilData object.
##' @param kernel_mild Same as `SMM()`
##' @param cost Same as `SMM()`
##' @param class.weights Same as `SMM()`
##' @param sigma Same as `SMM()`
##' @param ... If kernel_mild is a matrix, need to pass instance level label `y`.
##' @return A list
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 50,
##'                             nsample = 20,
##'                             positive_degree = 3,
##'                             positive_prob = 0.15,
##'                             positive_mean = rep(0, 5))
##' foo_smm <- smm_bag(data = MilData1)
##' @export
##' @author Yifei Liu
smm_bag <- function(data, kernel_mild = "rbf", cost = 1, class.weights,
    sigma = 0.05, ...) {
    ## Let's assume here that the data structure looks something like
    ## bag_label | bag_name | instance_name | feature_1 | ...  bag_label
    ## should be one of 0 and 1, where 0 is negative bags and 1 is
    ## positive bags we need to prepare the data into the following format
    ## so that the smm() function can be called.  instance_label |
    ## instance_name | feature_1 | ...

    original_data <- data
    colnames(data)[1] <- "instance_label"
    data$bag_name <- NULL
    if (missing(class.weights)) {
        bag_info <- unique(data[, 1:2])
        n_positive <- sum(bag_info$bag_label)
        n_bag <- length(bag_info$bag_label)
        class.weights <- c(1, n_positive/(n_bag - n_positive))
    }
    if (is.matrix(kernel_mild)) {
        if (is.null(list(...)$y)) {
            # stop(" 'y' should be supplied if 'kernel_mild' is a matrix")
            inst_info <- unique(data[, c("instance_label", "instance_name")])
            y <- inst_info$instance_label
        } else {
            y <- list(...)$y
        }
        res <- SMM(df = data, kernel_mild = kernel_mild, cost = cost,
            class.weights = class.weights, sigma = sigma, y = y)
    } else {
        res <- SMM(df = data, kernel_mild = kernel_mild, cost = cost,
            class.weights = class.weights, sigma = sigma, y = NULL)
    }

    return(validate_smmBag(new_smmBag(list(object = res, traindata = original_data))))
}



##' Predict method for smmBag objects
##'
##' Predict method for smmBag objects
##' @param object An smmBag object
##' @param ... newdata, GramMatrix for faster calculation.
##' @return An list
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 20,
##'                             nsample = 20,
##'                             positive_degree = 3,
##'                             positive_prob = 0.15,
##'                             positive_mean = rep(0, 5))
##' foo_smm <- smm_bag(data = MilData1)
##' foo_predict <- predict(foo_smm, newdata = MilData1)
##' @export
##' @author Yifei Liu
predict.smmBag <- function(object, ...) {
    ## newdata should have | bag_name | instance_name | feature 1 | ...,
    ## with an optional bag_label as the first column.

    args <- list(...)
    newdata = args$newdata
    if (is.null(newdata)) {
        newdata <- object$traindata
        if (!is.null(newdata$bag_label))
            true_bag_info <- unique(newdata[, c("bag_label", "bag_name")]) else true_bag_info <- NULL

    } else {
        if (!is.null(args$true_bag_info))
            true_bag_info <- args$true_bag_info else if (!is.null(newdata$bag_label))
            true_bag_info <- unique(newdata[, c("bag_label", "bag_name")]) else true_bag_info <- NULL
    }
    newdata$bag_label <- NULL  ## newdata now only has bag_name, instance_name

    if (is.null(args$GramMatrix)) {
        instance_score_pred <- predict(object = object$object, newdata = base::subset(newdata,
            select = -c(bag_name)), traindata = base::subset(object$object$traindata,
            select = -c(instance_label)))
    } else {
        instance_score_pred <- predict(object = object$object, kernel_mild = args$GramMatrix)
    }

    results <- cbind(unique(newdata[, c("bag_name", "instance_name")]),
        instance_score_pred, (instance_score_pred > 0))
    colnames(results)[3:4] <- c("instance_score_pred", "instance_label_pred")
    results_bag <- results %>% dplyr::group_by(bag_name) %>% dplyr::summarise(bag_score_pred = max(instance_score_pred),
        bag_label_pred = bag_score_pred > 0)
    if (!is.null(true_bag_info)) {
        results_bag <- dplyr::inner_join(results_bag, true_bag_info,
            by = "bag_name")
        ROC <- pROC::roc(response = results_bag$bag_label, predictor = results_bag$bag_score_pred)
        AUC <- pROC::auc(ROC)
        return(list(instance_level_prediction = results, bag_level_prediction = results_bag,
            AUC = AUC, ROC = ROC))

    } else {
        return(list(instance_level_prediction = results, bag_level_prediction = results_bag))

    }
}

##' Cross validation for smm_bag method.
##'
##' Cross validation for smm_bag method.
##' @param data A MilData object
##' @param n_fold The total number of folds
##' @param fold_id An optional integer vector representing the fold each bag belongs to
##' @param cost_seq A sequence of costs.
##' @param kernel_mild Currently can only support 'rbf'
##' @param class.weights The class weights, if missing, default to 1:positive/negative bag ratio
##' @param sigma The parameter for rbf kernel.
##' @return A list of 4 elements.
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 50,
##'                             nsample = 20,
##'                             positive_degree = 3,
##'                             positive_prob = 0.15,
##'                             positive_mean = rep(0, 5))
##' ## use about 5 minutes
##' ## foo_cv_smm <- cv_smm_bag(data = MilData1, n_fold = 2, cost_seq = 2^(-5:4))
##' @export
##' @author Yifei Liu
cv_smm_bag <- function(data, n_fold, fold_id, cost_seq, kernel_mild = "rbf",
    class.weights, sigma = 0.05) {

    bag_info <- unique(data[, c("bag_label", "bag_name")])
    if (missing(fold_id)) {
        if (missing(n_fold))
            n_fold = 5

        positive_bag_idx <- which(bag_info$bag_label == 1)
        negative_bag_idx <- which(bag_info$bag_label == 0)
        positive_fold_id <- base::sample((1:length(positive_bag_idx))%%n_fold +
            1)
        negative_fold_id <- base::sample((1:length(negative_bag_idx))%%n_fold +
            1)

        bag_id <- numeric(nrow(bag_info))
        bag_id[positive_bag_idx] <- positive_fold_id
        bag_id[negative_bag_idx] <- negative_fold_id

        temp_data <- data.frame(bag_name = unique(data$bag_name), bag_id = bag_id,
            stringsAsFactors = FALSE) %>% right_join(data %>% select(bag_name),
            by = "bag_name")
        fold_id <- temp_data$bag_id
    } else {
        n_fold <- max(fold_id)
        if (!is.null(setdiff(fold_id, 1:n_fold)))
            stop("The argument fold_id has some 'holes'!")
    }

    AUCs <- numeric(length(cost_seq))
    for (C in 1:length(cost_seq)) {
        temp_auc <- 0
        for (i in 1:n_fold) {
            data_train <- data[fold_id != i, ]
            data_valid <- data[fold_id == i, ]
            mdl <- smm_bag(data = data_train, cost = cost_seq[C], kernel_mild = kernel_mild,
                class.weights = class.weights, sigma = sigma)
            predictions_i <- predict(object = mdl, newdata = data_valid)
            temp_auc <- temp_auc + predictions_i$AUC
        }
        AUCs[C] <- temp_auc/n_fold
    }

    bestC <- cost_seq[which.max(AUCs)]
    BestMdl <- smm_bag(data = data, cost = bestC, kernel_mild = kernel_mild,
        class.weights = class.weights, sigma = sigma)
    return(list(BestMdl = BestMdl, BestC = bestC, AUCs = AUCs, cost_seq = cost_seq))
}


##' Use MI_SVM on flattened dataset
##'
##' This function just combines build_instance_feature() and MI_SVM()
##' @param data A MilData object
##' @param kernel kernel to be used by MI_SVM
##' @param cost Cost to be used by MI_SVM
##' @param class.weights class weights to be used by MI_SVM
##' @param sigma sigma to be used by MI_SVM
##' @param type type to be used by MI_SVM
##' @param qtls quantiles to be used by build_instance_feature
##' @param max.step maximum steps to be used by MI_SVM
##' @return The result returned by MI_SVM
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 50,
##'                             nsample = 20,
##'                             positive_degree = 3,
##'                             positive_prob = 0.15,
##'                             positive_mean = rep(0, 5))
##' foo <- mil_with_feature(data = MilData1)
##' @export
##' @author Yifei Liu
mil_with_feature <- function(data, kernel = "radial", cost = 1, class.weights = NULL,
    sigma = 0.05, type = "C-classification", qtls = seq(0.05, 0.95, length.out = 10),
    max.step = 500) {
    df <- build_instance_feature(data, qtls)
    df <- na.omit(df)
    mdl <- MI_SVM(data = df, cost = cost, kernel = kernel, max.step = max.step,
        type = type)
    return(mdl)
}

