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
#' @param c scalar indicating the penalty term on the sum of $\xi$ in the
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
