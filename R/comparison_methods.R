new_MI_SVM <- function(x = list()) {
    stopifnot(is.list(x))

    structure(x, class = c("MI_SVM", "list"))
}

validate_MI_SVM <- function(x) {
    if (!is.list(x)) {
        stop("x should be a list!")
        call. = FALSE
    }
    if (is.null(x$svm_mdl) | is.null(x$total_step) | is.null(x$representative_inst)) {
        stop("x should contain 'svm_mdl', 'total_step', and 'representative_inst'")
        call. = FALSE
    }
    x
}

##' MI-SVM algorithm implementation in R
##'
##' This function implements the MI-SVM algorithm proposed by Andrews et al (2003)
##' @param data A data.frame whose first three columns are `bag_label`, `bag_name` and `instance_name`.
##' @param cost The cost parameter to be fed to e1071::svm
##' @param kernel The kernel function to be used for e1071::svm.
##' @param max.step Maximum steps of iteration for the iterative SVM methods.
##' @param type type that to be used for e1071::svm.
##' @return An object of class 'MI_SVM'
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
##' mdl <- MI_SVM(data = df1, cost = 1, kernel = 'radial')
##' @importFrom e1071 svm
##' @export
##' @author Yifei Liu
MI_SVM <- function(data, cost, kernel = "radial", max.step = 500, type = "C-classification") {

    ## divide the bags to positive bags and negative bags The format of
    ## data is bag_label | bag_name | instance_name

    bag_name <- data$bag_name
    bag_label <- data$bag_label
    if (length(unique(bag_label)) == 1)
        stop("Only one class label, cannot perform classification!")
    positive_bag_name <- unique(bag_name[bag_label == 1])
    negative_bag_name <- unique(bag_name[bag_label == 0])
    unique_bag_name <- unique(bag_name)
    n_bag <- length(unique_bag_name)

    ## initialize the feature
    sample_instance <- NULL  ## this records the instances selected by finding the largest score positive instance of a bag and all negative instances.
    sample_label <- NULL

    for (i in 1:n_bag) {
        data_i <- data[data$bag_name == unique_bag_name[i], ]  ## find data from i-th bag
        n_inst <- nrow(data_i)
        bag_i_label <- data_i$bag_label[1]
        if (bag_i_label == 0) {
            ## this indicates a negative bag
            sample_instance <- rbind(sample_instance, data_i[, -(1:3)])
            sample_label <- c(sample_label, rep(0, n_inst))

        } else if (bag_i_label == 1) {
            sample_instance <- rbind(sample_instance, colMeans(data_i[,
                -(1:3)]))
            sample_label <- c(sample_label, 1)
        }
    }
    sample_label <- factor(sample_label, levels = c(0, 1), labels = c("0",
        "1"))

    n_negative_inst <- length(sample_label) - length(positive_bag_name)
    weights <- c(1, length(positive_bag_name)/n_negative_inst)
    names(weights) <- c("1", "0")
    ## iterate between updating the model and selecting the most positive
    ## bag from an instance.

    selection <- rep(0, length(positive_bag_name))
    past_selection <- matrix(NA, length(positive_bag_name), max.step)
    past_selection[, 1] <- selection
    step <- 1
    while (step < max.step) {

        svm_model <- e1071::svm(x = sample_instance, y = sample_label,
            class.weights = weights, cost = cost, kernel = kernel, type = type)
        pred_all_inst <- predict(object = svm_model, newdata = data[,
            -(1:3)], decision.values = TRUE)
        pred_all_score <- attr(pred_all_inst, "decision.values")
        ## update sample
        idx <- 1
        pos_idx <- 1
        sample_instance <- NULL
        sample_label <- NULL

        for (i in 1:n_bag) {
            data_i <- data[data$bag_name == unique_bag_name[i], ]
            n_inst <- nrow(data_i)
            bag_label_i <- data_i$bag_label[1]
            if (bag_label_i == 0) {
                sample_instance <- rbind(sample_instance, data_i[, -(1:3)])
                sample_label <- c(sample_label, rep(0, n_inst))
            } else if (bag_label_i == 1) {
                id_max <- which.max(pred_all_score[idx:(idx + n_inst -
                  1)])
                sample_instance <- rbind(sample_instance, data_i[id_max,
                  -(1:3)])
                sample_label <- c(sample_label, 1)
                selection[pos_idx] <- id_max
                pos_idx <- pos_idx + 1
            } else stop(paste("The sample_label for the ", i, "th bag doesn't belong to either 1 or 0!"))
            idx <- idx + n_inst

        }

        difference = sum(past_selection[, step] != selection)
        if (difference == 0)
            break

        repeat_selection <- 0
        for (i in 1:step) {
            if (all(selection == past_selection[, i])) {
                repeat_selection <- 1
                break
            }
        }

        if (repeat_selection == 1)
            break

        step <- step + 1
        past_selection[, step] <- selection
    }
    return(validate_MI_SVM(new_MI_SVM(list(svm_mdl = svm_model, total_step = step,
        representative_inst = cbind(positive_bag_name, selection)))))
}

##' Prediction function for MI_SVM object
##'
##' Predictionn function for MI_SVM object.
##' @param object The return from MI_SVM()
##' @param ... Should include `newdata` to represent the newdata to be predicting on, and optionally `true_bag_info` data.frame that contains 'bag_label' and 'bag_name' for AUC and ROC calculation.
##' @return A list which contains a bag level prediction `bag_level_prediction` and an instance level prediction `instance_level_prediction`. If `true_bag_label` is fed to arguments, will also return `ROC` and `AUC` as list elements.
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
##' mdl <- MI_SVM(data = df1, cost = 1, kernel = 'radial')
##' predictions1_MI <- predict(mdl, newdata = df1, true_bag_info = unique(df1[, 1:2]))
##' @importFrom pROC roc auc
##' @importFrom dplyr summarise group_by
##' @export
##' @author Yifei Liu
predict.MI_SVM <- function(object, ...) {
    arguments <- list(...)
    newdata <- arguments$newdata
    if (!is.null(newdata$bag_label) && !is.null(newdata$bag_name)) {
        true_bag_info <- unique(newdata[, c("bag_label", "bag_name")])
    } else if (!is.null(arguments$true_bag_info)) {
        true_bag_info <- arguments$true_bag_info
    } else {
        true_bag_info <- NULL
    }
    instance_label_pred <- predict(object = object$svm_mdl, newdata = newdata[, -(1:3)], decision.values = TRUE)
    instance_score_pred <- attr(instance_label_pred, "decision.values")
    data_instance <- cbind(newdata[, 1:2], instance_label_pred, instance_score_pred)
    colnames(data_instance)[4] <- "instance_score_pred"
    data_group <- dplyr::group_by(.data = data_instance, bag_name)
    data_bag <- dplyr::summarise(data_group, bag_score_pred = max(instance_score_pred),
        bag_label_pred = factor(bag_score_pred > 0, levels = c(TRUE,
            FALSE), labels = c("1", "0")))
    if (!is.null(true_bag_info)) {
        data_bag <- inner_join(data_bag, true_bag_info, by = "bag_name")
        ROC <- pROC::roc(response = data_bag$bag_label, predictor = data_bag$bag_score_pred)
        AUC <- pROC::auc(ROC)
        return(list(instance_level_prediction = data_instance, bag_level_prediction = data_bag,
            ROC = ROC, AUC = AUC))
    } else return(list(instance_level_prediction = data_instance, bag_level_prediction = data_bag))

}

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
##' lable as instance label.
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
        if (is.null(list(...)$y))
            stop(" 'y' should be supplied if 'kernel_mild' is a matrix")
        res <- SMM(df = NULL, kernel_mild = kernel_mild, cost = cost,
            class.weights = class.weights, sigma = sigma, y = list(...)$y)
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


##' This flattens the MilData type of data to regular multiple instance data where each instance is a vector
##'
##' This flatten the MilData type of data to regular multiple instance data where
##' each instance is a vector by extracting the features that would be present
##' in a polynomial kernel with given degree (default = 2).  Each instance_name
##' has features calculated in the input space based on the underlying kernel
##' (x'y + c)^d where c = constant and d = degree are input parameters.
##' @param data A MilData object.
##' @param degree degree of the polynomial kernel, default = 2
##' @param constant term added to inner product before applying the degree
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
##' df1 <- build_poly_instance_feature(MilData1, degree = 2)
##' @export
##' @author Sean Kent
build_poly_instance_feature <- function(data, degree = 2, constant = 1) {
    ## Let's assume here that `data` is a MilData object which looks
    ## something like bag_label | bag_name | instance_name | feature_1 |
    ## ...  bag_label should be one of 0 and 1, where 0 is negative bags
    ## and 1 is positive bags we need to prepare the data into the
    ## following format so that the MI_SVM function can be called.
    ## bag_label | bag_name | instance_name | feature_1 | ...

    stopifnot("build_poly_instance_feature only implemented for degree = 2" = degree == 2)

    phi <- function(X, constant = 1) {
        X <- as.matrix(X)
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

        x_linear <- sqrt(2*constant) * X

        cbind(x_squared, x_crossterms, x_linear)
    }

    phi_bar <- function(X, constant = 1) {
        apply(phi(X, constant), MARGIN = 2, mean)
    }

    original_features <- subset(data, select = -c(bag_label, bag_name, instance_name))

    new_features <- lapply(split(original_features, factor(data$instance_name)),
                           FUN = phi_bar)
    new_features <- as.data.frame(do.call(rbind, new_features))
    new_features$instance_name <- rownames(new_features)

    out_df <- merge(unique(subset(data, select = c(bag_label, bag_name, instance_name))),
                    new_features,
                    sort = FALSE)
    return(out_df)
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
    mdl <- MI_SVM(data = df, cost = cost, kernel = kernel, max.step = max.step,
        type = type)
    return(mdl)
}

##' Cross-validation function for MI_SVM
##'
##' Cross-validation function for MI_SVM.
##' @param data Same as in MI_SVM()
##' @param n_fold Default to be 5
##' @param fold_id Can be skipped if n_fold is set.
##' @param cost_seq The cost sequence.
##' @param kernel The kernel to be used in MI_SVM()
##' @param max.step Maximum steps to be used in MI_SVM()
##' @param type type to be used in MI_SVM()
##' @return A list that contains best model, optimal cost value, the AUCs, and the cost sequence.
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
##' foo_cv <- cv_MI_SVM(data = df1, n_fold = 3, cost_seq = 2^(-2:2))
##' @export
##' @author Yifei Liu
cv_MI_SVM <- function(data, n_fold, fold_id, cost_seq, kernel = "radial",
    max.step = 500, type = "C-classification") {
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
            mdl <- MI_SVM(data = data_train, cost = cost_seq[C], kernel = kernel,
                max.step = max.step, type = type)
            predictions_i <- predict(object = mdl, newdata = data_valid,
                true_bag_info = unique(data_valid[, 1:2]))
            temp_auc <- temp_auc + predictions_i$AUC
        }
        AUCs[C] <- temp_auc/n_fold
    }

    bestC <- cost_seq[which.max(AUCs)]
    BestMdl <- MI_SVM(data = data, cost = bestC, kernel = kernel, max.step = max.step,
        type = type)
    return(list(BestMdl = BestMdl, BestC = bestC, AUCs = AUCs, cost_seq = cost_seq))
}

