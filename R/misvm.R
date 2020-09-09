new_misvm <- function(x = list(), method = "mip") {
  stopifnot(is.list(x))
  method <- match.arg(method, c("mip", "heuristic"))
  structure(
    x,
    class = "misvm",
    method = method
  )
}

validate_misvm <- function(x) {
  message("No validations currently in place for object of class 'misvm'.")
  x
}

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

#' Fit MI-SVM model based on full MIP problem
#'
#' Function to run the Gurobi model to train MI-SVM problem based on the full
#' specification of the Mixed Integer Programming (MIP) problem.
#'
#' @param y a nx1 numeric vector of bag labels with length equal to the number
#'   of instances (not the number of bags). Must have -1 for negative bags and
#'   +1 for positive bags
#' @param bags a nx1 vector specifying which instance belongs to each bag.  Can be
#'   a string, numeric, of factor
#' @param X a nxp data.frame of covariates.  Can also supply a matrix.
#' @param c scalar indicating the penalty term on the sum of Xi in the
#'   objective function
#' @param rescale logical; whether to rescale the input before fitting
#' @param weights named vector to control the weight of the cost parameter
#'   for each possible y value.  Weights multiply against the cost vector.
#' @param verbose whether to message output to the console; default is FALSE.
#' @param time_limit FALSE, or a time limit (in seconds) passed to gurobi
#'   parameters. If FALSE, no time limit is given.
#'
#' @return `misvm_mip_gurobi()` returns an object of class `"misvm"`.
#'   An object of class "misvm" is a list containing at least the following
#'   components:
#'   - `w` a named weight vector matching the number of columns of X
#'   TODO: fill out the rest of this...
misvm_mip_gurobi <- function(y, bags, X, c, rescale = TRUE, weights = NULL,
                             verbose = FALSE, time_limit = FALSE) {
  # TODO: maybe change function call to y, X, bags?
  if (rescale) X <- scale(X)
  bags <- as.numeric(factor(bags, levels = unique(bags)))

  model <- misvm_mip_model_gurobi(y, bags, X, c, weights)
  params <- list()
  params$OutputFlag = 1*verbose
  params$IntFeasTol = 1e-5
  if (time_limit) params$TimeLimit = time_limit
  gurobi_result <- gurobi::gurobi(model, params = params)

  w <- gurobi_result$x[grepl("w", model$varnames)]
  b_ <- gurobi_result$x[grepl("b", model$varnames)]
  if (rescale) {
    b_ <- b_ - sum(attr(X, "scaled:center") * w / attr(X, "scaled:scale"))
    w <- w / attr(X, "scaled:scale")
  }

  res <- list(
    model = list(
      w = w,
      b = b_,
      xi = gurobi_result$x[grepl("xi", model$varnames)],
      z = gurobi_result$x[grepl("z", model$varnames)],
      status = gurobi_result$status,
      itercount = gurobi_result$itercount,
      baritercount = gurobi_result$baritercount,
      objval = gurobi_result$objval,
      c = c
    ),
    representative_inst = NULL, # TODO: fill in these parameters or remove
    traindata = NULL,
    useful_inst_idx = NULL
  )
  names(res$model$w) <- colnames(X)

  return(new_misvm(res, method = "mip"))
  # return(res)
}

#' Create optimization model for MI-SVM problem in Gurobi
#'
#' Internal function to build a Gurobi optimization model based on the MI-SVM
#' problem.
#' @inheritParams misvm_mip_gurobi
#' @return a model that can be passed to `gurobi::gurobi` that contains the MIQP
#'   problem defined by MI-SVM in Andrews et al. (2003)
#'
#' @export
#' @author Sean Kent
misvm_mip_model_gurobi <- function(y, bags, X, c, weights = NULL) {
  L <- 1e0 * sum(abs(X))
  # TODO: check that y has only -1 and 1
  r <- reorder(y, bags, X)
  y <- r$y
  bags <- r$b
  X <- r$X


  ## Build constraint matrix
  # order of variables is [w, b, xi, z]
  n_w <- ncol(X)
  n_b <- 1
  n_xi <- length(unique(bags))
  n_z <- sum(y == 1)

  # constraint1 is related to the data
  w_constraint <- y*X
  b_constraint <- y*1
  xi_col <- function(b, n_xi) {
    # puts a 1 in the `b`th entry of a n_xi-length 0 vector
    vec <- rep(0, n_xi)
    vec[b] <- 1
    return(vec)
  }
  xi_constraint <- t(sapply(bags, xi_col, n_xi = n_xi))
  z_constraint <- rbind(matrix(0, nrow = sum(y == -1), ncol = n_z),
                        L*diag(nrow = n_z, ncol = n_z))

  constraint1 <- as.matrix(cbind(w_constraint, b_constraint, xi_constraint, z_constraint))
  rhs1 <- rep(1, nrow(X))

  # constraint2 is related to how many z can be non-zero
  pos_bags <- unique(bags[y == 1])
  pos_bag_counts <- as.data.frame(table(bags[y==1]))$Freq
  z_constraint <- matrix(NA , nrow = length(pos_bags), ncol = n_z)
  for (bag in pos_bags) {
    row <- unlist(mapply(rep, x = 1*(bag == pos_bags), times = pos_bag_counts))
    z_constraint[bag == pos_bags, ] <- row
  }
  wbxi_constraint <- matrix(0, nrow = length(pos_bags), ncol = n_w + n_b + n_xi)
  constraint2 <- cbind(wbxi_constraint, z_constraint)
  colnames(constraint2) <- NULL
  rhs2 <- pos_bag_counts - 1

  if (is.null(weights)) {
    c_vec <- rep(c, n_xi)
  } else {
    c_vec <- numeric(n_xi)
    c_vec[which(unique(bags) %in% pos_bags)] <- weights[["1"]] * c
    c_vec[which(unique(bags) %ni% pos_bags)] <- weights[["0"]] * c
  }

  model <- list()
  ## Objective
  model$modelsense <- "min"
  model$obj <- c(rep(0, n_w + n_b), rep(c, n_xi), rep(0, n_z)) # linear portion of objective
  model$Q <- diag(c(rep(1, n_w), rep(0, n_b + n_xi + n_z))) # quadratic portion of objective
  ## Constraints
  model$varnames <- c(paste0("w",1:n_w), "b", paste0("xi",1:n_xi), paste0("z",1:n_z))
  model$A <- rbind(constraint1, constraint2)
  model$sense <- c(rep(">=", length(rhs1)), rep("<=", length(rhs2))) # rep("<=", length(model$rhs))
  model$rhs <- c(rhs1, rhs2)
  model$vtype <- c(rep("C", n_w + n_b + n_xi), rep("B", n_z))
  model$lb <- c(rep(-Inf, n_w + n_b), rep(0, n_xi + n_z))
  return(model)
}

#' Fit a Cross-Validated MI-SVM model based on full MIP problem
#'
#' Function to run the gurobi model to train MI-SVM problem.  The hyperparameter
#'  C is chosen through cross-validation.  Cross-validation is done over the
#'  bags and evaluated based on bag AUC.
#'
#' @inheritParams misvm_mip_gurobi
#' @param fold_id a vector indicating which instances belong in each fold for
#'   cross validation.
#' @param cost_seq vector of values of c to perform cross vailidation over.  C is
#'   the penalty term on the sum of $\xi$ in the objective function
#' @return `cv_misvm_mip_gurobi()` returns an object of class `"misvm"`.
#' An object of class "misvm" is a list containing at least the following components:
#' `w` a named weight vector matching the number of columns of X
cv_misvm_mip_gurobi <- function(y, bags, X, fold_id, cost_seq = 2^(-5:15),
                                rescale = TRUE, verbose = FALSE, time_limit = FALSE) {

  n_fold <- max(fold_id)
  bag_labels <- sapply(split(y, factor(bags)), unique)
  weights <- c("0" = sum(bag_labels == 1) / sum(bag_labels == -1), "1" = 1)

  AUCs <- numeric(length(cost_seq))
  for (C in 1:length(cost_seq)) {
    auc_sum <- 0
    for (fold in 1:n_fold) {
      ind <- fold_id != fold

      model_i_fold <- misvm_mip_gurobi(y[ind], bags[ind], X[ind, ], cost_seq[C],
                                       rescale = rescale, verbose = verbose, time_limit = time_limit)
      pred_scores <- predict(model_i_fold, newX = X[!ind, ], type = "score")

      auc_fold <- pROC::auc(pROC::roc(response = classify_bags(y[!ind], bags[!ind]),
                                      predictor = classify_bags(pred_scores, bags[!ind])))
      auc_sum <- auc_sum + auc_fold

    }
    AUCs[C] <- auc_sum/n_fold
  }

  bestC <- cost_seq[which.max(AUCs)]
  misvm_fit <- misvm_mip_gurobi(y, bags, X, bestC, rescale = rescale,
                                verbose = verbose, time_limit = time_limit)

  # TODO: fix this output
  res <- new_misvm(list(model = misvm_fit$model,
                        total_step = NULL,
                        representative_inst = misvm_fit$representative_inst,
                        traindata = misvm_fit$traindata,
                        useful_inst_idx = misvm_fit$useful_inst_idx))
  return(list(BestMdl = res, BestC = bestC, AUCs = AUCs, cost_seq = cost_seq))
}

#' Predict method for 'misvm' object
#' @param object an object of class misvm
#' @param newX matrix to predict from.  Needs to have the same number of
#'   columns as the X that trained the misvm object
#' @param type if 'prediction', return predicted values with threshold of 0 as
#'   -1 or +1.  If 'score', return the raw predicted scores.
#'
#' @return vector of length `nrow(newX)`.  If 'prediction', return predicted
#'   values with threshold of 0 as -1 or +1.  If 'scores', return the raw
#'   predicted scores.
#'
#' @export
#' @author Sean Kent
predict.misvm <- function(object, newX, type = c("prediction", "score")) {
  type <- type[1]
  if (attr(object, "method") == "mip") {
    scores <- as.matrix(newX) %*% object$model$w + object$model$b
    if (type == "score") {
      return(scores)
    } else if (type == "prediction") {
      pos <- scores > 0
      return(2*as.vector(pos) - 1)
    }
  }
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


