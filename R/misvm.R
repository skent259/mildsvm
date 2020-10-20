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

#' Fit MI-SVM model to the data
#'
#' This function fits the MI-SVM model, first proposed by Andrews et al. (2003).
#'   It is a variation on the traditional SVM framework that carefully treats
#'   data from the multiple instance learning paradigm, where instances are
#'   grouped into bags, and a label is only available for each bag. Several
#'   choices of fitting algorithm are available, including a version of the
#'   heuristic algorithm proposed by Andrews et al. (2003) and a novel algorithm
#'   that explicitly solves the mixed-integer programming (MIP) problem using
#'   the `gurobi` optimization back-end.
#'
#' @param x a data.frame, matrix, or similar object of covariates, where each
#'   row represents an instance.
#' @param y a numeric, character, or factor vector of bag labels for each
#'   instance.  Must satisfy `length(y) == nrow(x)`. Suggest that one of the
#'   levels is 1, '1', of TRUE, which becomes the positive class in MI-SVM;
#'   otherwise, a positive class is chosen and a message will be supplied.
#' @param bags a vector specifying which instance belongs to each bag.  Can be
#'   a string, numeric, of factor.
#' @param formula a formula with specification `mi(y, bags) ~ x` which uses the
#'   `mi` function to create the bag-instance structure. This argument is an
#'   alternative to the `x, y, bags` arguments, but requires the `data` argument.
#'   See examples.
#' @param data a data.frame or similar from which formula elements will be
#'   extracted.  Used only when the first argument is a formula object.
#' @param cost The cost parameter in SVM. If `method` = 'heuristic', this will
#'   be fed to `e1071::svm`, otherwise it is similarly in internal functions.
#' @param method MI-SVM algorithm to use in fitting; default is 'heuristic',
#'   which employs an algorithm similar to Andrews et al. (2003). When `method`
#'   = 'mip', the novel MIP method will be used.  See details.
#' @param weights named vector, or TRUE, to control the weight of the cost parameter
#'   for each possible y value.  Weights multiply against the cost vector. If
#'   TRUE, weights are calculated based on inverse counts of bags with given label.
#'   Otherwise, names must match the levels of `y`.
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   - `kernel` argument used when `method` = 'heuristic'.  The kernel function
#'   to be used for `e1071::svm`.
#'   - `max_step` argument used when `method` = 'heuristic'. Maximum steps of
#'   iteration for the heuristic algorithm.
#'   - `type` argument used when `method` = 'heuristic'. The `type` argument is
#'   passed to `e1071::svm`.
#'   - `scale` argument used for all methods. Logical; whether to rescale
#'   the input before fitting
#'   - `verbose` argument used when `method` = 'mip'. Whether to message output
#'   to the console.
#'   - `time_limit` argument used when `method` = 'mip'. FALSE, or a time limit
#'   (in seconds) passed to `gurobi` parameters.  If FALSE< no time limit is
#'   given.
#'
#' @examples
#' set.seed(8)
#' mil_data <- GenerateMilData(
#'   positive_dist = 'mvt',
#'   negative_dist = 'mvnormal',
#'   remainder_dist = 'mvnormal',
#'   nbag = 20,
#'   nsample = 20,
#'   positive_degree = 3,
#'   positive_prob = 0.15,
#'   positive_mean = rep(0, 5)
#' )
#' df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
#'
#' # Heuristic method
#' mdl1 <- misvm(x = df[, 4:123], y = df$bag_label,
#'               bags = df$bag_name, method = "heuristic")
#' mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)
#'
#' if (require(gurobi)) {
#'   # solve using the MIP method
#'   mdl3 <- misvm(x = df[, 4:123], y = df$bag_label,
#'                 bags = df$bag_name, method = "mip")
#' }
#'
#' predict(mdl1, new_data = df, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df %>%
#'   bind_cols(predict(mdl2, df1, type = "class")) %>%
#'   bind_cols(predict(mdl2, df1, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#'
#' @author Sean Kent, Yifei Liu
#' @name misvm
NULL

#' @export
misvm <- function(x, y, bags, ...) {
  UseMethod("misvm")
}

#' @describeIn misvm Method for passing formula
#' @export
misvm.formula <- function(formula, data, cost = 1, method = c("heuristic", "mip"), weights = TRUE,
                          control = list(kernel = "radial",
                                         max_step = 500,
                                         type = "C-classification",
                                         scale = TRUE,
                                         verbose = FALSE,
                                         time_limit = 60)) {
  # NOTE: other 'professional' functions use a different type of call that I
  #   couldn't get to work. See https://github.com/therneau/survival/blob/master/R/survfit.R
  #   or https://github.com/cran/e1071/blob/master/R/svm.R
  #   right now we're using something that should work for most generic formulas

  mi_names <- as.character(terms(formula, data = data)[[2]])
  bag_label <- mi_names[[2]]
  bag_name <- mi_names[[3]]

  predictors <- setdiff(colnames(data), c(bag_label, bag_name))

  x <- model.matrix(formula[-2], data = data[, predictors])
  if (attr(terms(formula, data = data), "intercept") == 1) x <- x[, -1, drop = FALSE]
  x <- as.data.frame(x)

  response <- get_all_vars(formula, data = data)
  y <- response[, 1]
  bags <- response[, 2]

  res <- misvm.default(x, y, bags, cost = cost, method = method, weights = weights, control = control)

  res$call_type <- "misvm.formula"
  res$formula <- formula
  res$bag_name <- bag_name
  return(res)
}

#' @describeIn misvm Method for data.frame-like objects
#' @export
misvm.default <- function(x, y, bags, cost = 1, method = c("heuristic", "mip"), weights = TRUE,
                          control = list(kernel = "radial",
                                         max_step = 500,
                                         type = "C-classification",
                                         scale = TRUE,
                                         verbose = FALSE,
                                         time_limit = 60)) {

  method <- match.arg(method)
  if ("kernel" %ni% names(control)) control$kernel <- "radial"
  if ("max_step" %ni% names(control)) control$max_step <- 500
  if ("type" %ni% names(control)) control$type <- "C-classification"
  if ("scale" %ni% names(control)) control$scale <- TRUE
  if ("verbose" %ni% names(control)) control$verbose <- FALSE
  if ("time_limit" %ni% names(control)) control$time_limit <- 60

  # store the levels of y and convert to 0,1 numeric format.
  y <- factor(y)
  lev <- levels(y)
  if (length(lev) == 1) {
    stop(paste0("Response y has only one level, ", lev, ", cannot perform misvm fitting."))
  } else if (length(lev) > 2) {
    stop(paste0("Response y has more than two levels, ", lev, ", cannot perform misvm fitting."))
  }
  if (lev[1] == 1 | lev[1] == "1" | lev[1] == TRUE) {
    lev <- rev(lev)
    y <- factor(y, levels = lev) # make sure 1 is second level
  } else if (lev[2] != 1 & lev[2] != "1" & lev[2] != TRUE) {
    message(paste0("Setting level ", lev[2], " to be the positive class for misvm fitting."))
  } # else lev[2] is like 1, keep it that way.
  y <- as.numeric(y) - 1

  ## weights
  if (is.numeric(weights)) {
    stopifnot(names(weights) == lev | names(weights) == rev(lev))
    weights <- weights[lev]
    names(weights) <- c("0", "1")
  } else if (weights) {
    bag_labels <- sapply(split(y, factor(bags)), unique)
    weights <- c("0" = sum(bag_labels == 1) / sum(bag_labels == 0), "1" = 1)
  } else {
    weights <- NULL
  }

  if (method == "heuristic") {
    data <- cbind(bag_label = y,
                  bag_name = bags,
                  instance_name = as.character(1:length(y)),
                  x)
    res <- misvm_heuristic_fit(data,
                  cost = cost,
                  weights = weights,
                  kernel = control$kernel,
                  max_step = control$max_step,
                  type = control$type,
                  scale = control$scale)
  } else if (method == "mip") {
    y = 2*y - 1 # convert {0,1} to {-1, 1}
    res <- misvm_mip_fit(y, bags, x,
                     c = cost,
                     rescale = control$scale,
                     weights = weights,
                     verbose = control$verbose,
                     time_limit = control$time_limit)
  } else {
    stop("misvm requires method = 'heuristic' or 'mip'.")
  }

  res$features <- colnames(x)
  res$call_type <- "misvm.default"
  res$bag_name <- NULL
  res$levels <- lev
  new_misvm(res, method = method)
  # return(res)
}

#' Predict method for 'misvm' object
#' @param object an object of class misvm
#' @param new_data matrix to predict from.  Needs to have the same number of
#'   columns as the X that trained the misvm object
#' @param type if 'class', return predicted values with threshold of 0 as
#'   -1 or +1.  If 'raw', return the raw predicted scores.
#' @param layer if 'bag', return predictions at the bag level.  If 'instance',
#'   return predictions at the instance level.
#' @param new_bags character or character vector.  Can specify a singular
#'   character that provides the column name for the bag names in `new_data`,
#'   default = "bag_name".  Can also specify a vector of length `nrow(new_data)`
#'   that has bag name for each instance.  When `object` was fitted with
#'   `misvm.formula`, this parameter is not necessary as the bag name can be
#'   pulled directly from new_data, if available.
#'
#' @return tibble with `nrow(new_data)` rows.  If type = 'class', the tibble
#'   will have a column '.pred_class'.  If type = 'raw', the tibble will have
#'   a column '.pred'.
#'
#' @examples
#' mil_data <- GenerateMilData(
#'   positive_dist = 'mvt',
#'   negative_dist = 'mvnormal',
#'   remainder_dist = 'mvnormal',
#'   nbag = 20,
#'   nsample = 20,
#'   positive_degree = 3,
#'   positive_prob = 0.15,
#'   positive_mean = rep(0, 5)
#' )
#' df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
#' mdl1 <- misvm.default(df1, cost = 1, kernel = "radial", method = "mip")
#'
#' predict(mdl1, new_data = df1, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df1 %>%
#'   bind_cols(predict(mdl2, df1, type = "class")) %>%
#'   bind_cols(predict(mdl2, df1, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#' @export
#' @author Sean Kent
predict.misvm <- function(object, new_data,
                          type = c("class", "raw"), layer = c("bag", "instance"),
                          new_bags = "bag_name") {
  type <- match.arg(type)
  layer <- match.arg(layer)
  method <- attr(object, "method")

  if (object$call_type == "misvm.formula") {
    new_x <- model.matrix(object$formula[-2], data = new_data)
    if (attr(terms(object$formula), "intercept") == 1) new_x <- new_x[, -1, drop = FALSE]
    new_x <- as.data.frame(new_x)
  } else {
    new_x <- new_data[, object$features, drop = FALSE]
  }



  if (method == "heuristic") {
    pos <- predict(object = object$svm_mdl, newdata = new_x, decision.values = TRUE)
    scores <- attr(pos, "decision.values")
    pos <- as.numeric(as.character(pos))

  } else if (method == "mip") {
    scores <- as.matrix(new_x) %*% object$model$w + object$model$b
    pos <- 1*(scores > 0)

  } else {
    stop("predict.misvm requires method = 'heuristic' or 'mip'.")
  }

  if (layer == "bag") {
    if (object$call_type == "misvm.formula" & new_bags[1] == "bag_name" & length(new_bags) == 1) {
      new_bags <- object$bag_name
    }
    if (length(new_bags) == 1 & new_bags[1] %in% colnames(new_data)) {
      bags <- new_data[[new_bags]]
    } else {
      bags <- new_bags
    }
    scores <- classify_bags(scores, bags, condense = FALSE)
    pos <- classify_bags(pos, bags, condense = FALSE)
  }
  pos <- factor(pos, levels = c(0, 1), labels = object$levels) # TODO: adjust this to accept any labels, passed from model object

  res <- switch(type,
                "raw" = tibble::tibble(.pred = as.numeric(scores)),
                "class" = tibble::tibble(.pred_class = pos))

  # TODO: consider returning the AUC here as an attribute.  Can only do if we have the true bag labels
  # attr(res, "AUC") <- calculated_auc
  attr(res, "layer") <- layer
  res
}


# Specific implementation methods below ----------------------------------------


#' INTERNAL Fit MI-SVM model based on full MIP problem
#'
#' Function to train an MI-SVM classifier based on the full
#' specification of the Mixed Integer Programming (MIP) problem.  The optimization
#' problem is solved using the `gurobi` R package through the Gurobi backend.
#'
#' @param y a nx1 numeric vector of bag labels with length equal to the number
#'   of instances (not the number of bags). Must have -1 for negative bags and
#'   +1 for positive bags
#' @param bags a nx1 vector specifying which instance belongs to each bag.  Can be
#'   a string, numeric, of factor
#' @param X an nxp data.frame of covariates.  Can also supply a matrix.
#' @param c scalar indicating the penalty term on the sum of Xi in the
#'   objective function
#' @param rescale logical; whether to rescale the input before fitting
#' @param weights named vector to control the weight of the cost parameter
#'   for each possible y value.  Weights multiply against the cost vector.
#' @param verbose whether to message output to the console; default is FALSE.
#' @param time_limit FALSE, or a time limit (in seconds) passed to gurobi
#'   parameters. If FALSE, no time limit is given.
#'
#' @return `misvm_mip_fit()` returns an object of class `"misvm"`.
#'   An object of class "misvm" is a list containing at least the following
#'   components:
#'   - `model`: a list with components:
#'     - `w`: weights to apply to the predictors to make the classifier
#'     - `b`: intercept term used to make the classifier
#'     - `xi`: slack variables returned from the optimization
#'     - `z`: integer variables returned from the optimization that determine selected instances
#'     `status`: solution status from `gurobi::gurobi`
#'     `itercount`: itercount from `gurobi::gurobi`
#'     `baritercount`: baritercount from `gurobi::gurobi`
#'     `objval`: value of the objective at the solution
#'     `c`: value of the cost parameter used in solving the optimization
#'   - `representative_inst`: NULL, TODO: mesh with other misvm method
#'   - `traindata`: NULL, TODO: mesh with other misvm method
#'   - `useful_inst_idx`: NULL, TODO: mesh with other misvm method
misvm_mip_fit <- function(y, bags, X, c, rescale = TRUE, weights = NULL,
                             verbose = FALSE, time_limit = FALSE) {
  # TODO: maybe change function call to y, X, bags?
  if (rescale) X <- scale(X)
  bags <- as.numeric(factor(bags, levels = unique(bags)))

  model <- misvm_mip_model(y, bags, X, c, weights)
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
}

#' INTERNAL Create optimization model for MI-SVM problem
#'
#' Internal function to build an optimization model (that can be passed to
#' `gurobi::gurobi`) based on the MI-SVM problem.
#'
#' @inheritParams misvm_mip_fit
#' @return a model that can be passed to `gurobi::gurobi` that contains the MIQP
#'   problem defined by MI-SVM in Andrews et al. (2003)
#'
#' @author Sean Kent
misvm_mip_model <- function(y, bags, X, c, weights = NULL) {
  L <- 1e0 * sum(abs(X))
  # TODO: check that y has only -1 and 1
  r <- .reorder(y, bags, X)
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
#' @inheritParams misvm_mip_fit
#' @param fold_id a vector indicating which instances belong in each fold for
#'   cross validation.
#' @param cost_seq vector of values of c to perform cross vailidation over.  C is
#'   the penalty term on the sum of Xi in the objective function
#' @return `cv_misvm_mip()` returns an object of class `"misvm"`.
#' An object of class "misvm" is a list containing at least the following components:
#' `w` a named weight vector matching the number of columns of X
cv_misvm_mip <- function(y, bags, X, fold_id, cost_seq = 2^(-5:15),
                                rescale = TRUE, verbose = FALSE, time_limit = FALSE) {

  n_fold <- max(fold_id)
  bag_labels <- sapply(split(y, factor(bags)), unique)
  weights <- c("0" = sum(bag_labels == 1) / sum(bag_labels == -1), "1" = 1)

  AUCs <- numeric(length(cost_seq))
  for (C in 1:length(cost_seq)) {
    auc_sum <- 0
    for (fold in 1:n_fold) {
      ind <- fold_id != fold

      model_i_fold <- misvm_mip_fit(y[ind], bags[ind], X[ind, , drop = FALSE], cost_seq[C],
                                       rescale = rescale, verbose = verbose, time_limit = time_limit)
      pred_scores <- predict(model_i_fold, newX = X[!ind, , drop = FALSE], type = "score")

      auc_fold <- pROC::auc(pROC::roc(response = classify_bags(y[!ind], bags[!ind]),
                                      predictor = classify_bags(pred_scores, bags[!ind])))
      auc_sum <- auc_sum + auc_fold
    }
    AUCs[C] <- auc_sum/n_fold
  }

  bestC <- cost_seq[which.max(AUCs)]
  misvm_fit <- misvm_mip_fit(y, bags, X, bestC, rescale = rescale,
                                verbose = verbose, time_limit = time_limit)

  # TODO: fix this output
  res <- new_misvm(list(model = misvm_fit$model,
                        total_step = NULL,
                        representative_inst = misvm_fit$representative_inst,
                        traindata = misvm_fit$traindata,
                        useful_inst_idx = misvm_fit$useful_inst_idx))
  return(list(BestMdl = res, BestC = bestC, AUCs = AUCs, cost_seq = cost_seq))
}



#' INTERNAL MI-SVM algorithm implementation in R
#'
#' This function implements the MI-SVM algorithm proposed by Andrews et al (2003)
#' @param data A data.frame whose first three columns are `bag_label`, `bag_name` and `instance_name`.
#' @param cost The cost parameter to be fed to `e1071::svm`.
#' @param kernel The kernel function to be used for `e1071::svm`.
#' @param max_step Maximum steps of iteration for the iterative SVM methods.
#' @param type type that to be used for `e1071::svm`.
#' @return An object of class 'MI_SVM'
#' @examples
#' MilData1 <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 50,
#'                             nsample = 20,
#'                             positive_degree = 3,
#'                             positive_prob = 0.15,
#'                             positive_mean = rep(0, 5))
#' df1 <- build_instance_feature(MilData1, seq(0.05, 0.95, length.out = 10))
#' mdl <- MI_SVM(data = df1, cost = 1, kernel = 'radial')
#' @importFrom e1071 svm
#' @author Yifei Liu
misvm_heuristic_fit <- function(data, cost, weights, kernel = "radial",
                                max_step = 500, type = "C-classification",
                                scale = TRUE) {

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
    data_i <- data[data$bag_name == unique_bag_name[i], , drop = FALSE]  ## find data from i-th bag
    n_inst <- nrow(data_i)
    bag_i_label <- data_i$bag_label[1]
    if (bag_i_label == 0) {
      ## this indicates a negative bag
      sample_instance <- rbind(sample_instance, data_i[, -(1:3), drop = FALSE])
      sample_label <- c(sample_label, rep(0, n_inst))

    } else if (bag_i_label == 1) {
      sample_instance <- rbind(sample_instance, colMeans(data_i[, -(1:3), drop = FALSE]))
      sample_label <- c(sample_label, 1)
    }
  }
  sample_label <- factor(sample_label, levels = c(0, 1), labels = c("0", "1"))

  n_negative_inst <- length(sample_label) - length(positive_bag_name)
  # weights <- c(1, length(positive_bag_name)/n_negative_inst)
  # names(weights) <- c("1", "0")
  ## iterate between updating the model and selecting the most positive
  ## bag from an instance.

  selection <- rep(0, length(positive_bag_name))
  past_selection <- matrix(NA, length(positive_bag_name), max_step)
  past_selection[, 1] <- selection
  step <- 1
  while (step < max_step) {

    svm_model <- e1071::svm(x = sample_instance, y = sample_label,
                            class.weights = weights, cost = cost,
                            kernel = kernel, scale = scale,
                            type = type)
    pred_all_inst <- predict(object = svm_model, newdata = data[, -(1:3), drop = FALSE], decision.values = TRUE)
    pred_all_score <- attr(pred_all_inst, "decision.values")
    ## update sample
    idx <- 1
    pos_idx <- 1
    sample_instance <- NULL
    sample_label <- NULL

    for (i in 1:n_bag) {
      data_i <- data[data$bag_name == unique_bag_name[i], , drop = FALSE]
      n_inst <- nrow(data_i)
      bag_label_i <- data_i$bag_label[1]
      if (bag_label_i == 0) {
        sample_instance <- rbind(sample_instance, data_i[, -(1:3), drop = FALSE])
        sample_label <- c(sample_label, rep(0, n_inst))
      } else if (bag_label_i == 1) {
        id_max <- which.max(pred_all_score[idx:(idx + n_inst - 1)])
        sample_instance <- rbind(sample_instance, data_i[id_max, -(1:3), drop = FALSE])
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
  return(list(svm_mdl = svm_model, total_step = step,
              representative_inst = cbind(positive_bag_name, selection)))
}

#' Cross-validation function for MI_SVM
#'
#' Cross-validation function for MI_SVM.
#'
#' @inheritParams misvm_heuristic_fit
#' @inheritParams cv_mildsvm
#' @return A list that contains best model, optimal cost value, the AUCs, and the cost sequence.
#' @examples
#' MilData1 <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 50,
#'                             nsample = 20,
#'                             positive_degree = 3,
#'                             positive_prob = 0.15,
#'                             positive_mean = rep(0, 5))
#' df1 <- build_instance_feature(MilData1, seq(0.05, 0.95, length.out = 10))
#' foo_cv <- cv_MI_SVM(data = df1, n_fold = 3, cost_seq = 2^(-2:2))
#' @export
#' @author Yifei Liu
cv_MI_SVM <- function(data, n_fold, fold_id, cost_seq, kernel = "radial",
                      max_step = 500, type = "C-classification") {
  bag_info <- unique(data[, c("bag_label", "bag_name"), drop = FALSE])

  fold_info <- select_cv_folds(data, n_fold, fold_id)
  fold_id <- fold_info$fold_id

  AUCs <- numeric(length(cost_seq))
  for (C in 1:length(cost_seq)) {
    temp_auc <- 0
    for (i in 1:n_fold) {
      data_train <- data[fold_id != i, , drop = FALSE]
      data_valid <- data[fold_id == i, , drop = FALSE]
      mdl <- misvm_heuristic_fit(data = data_train, cost = cost_seq[C], kernel = kernel,
                    max_step = max_step, type = type)
      predictions_i <- predict(object = mdl, newdata = data_valid,
                               true_bag_info = unique(data_valid[, 1:2, drop = FALSE]))
      temp_auc <- temp_auc + predictions_i$AUC
    }
    AUCs[C] <- temp_auc/n_fold
  }

  bestC <- cost_seq[which.max(AUCs)]
  BestMdl <- misvm_heuristic_fit(data = data, cost = bestC, kernel = kernel, max_step = max_step,
                    type = type)
  return(list(BestMdl = BestMdl, BestC = bestC, AUCs = AUCs, cost_seq = cost_seq))
}


# Deprecated methods below, need to delete eventually --------------------------

# new_MI_SVM <- function(x = list()) {
#   stopifnot(is.list(x))
#
#   structure(x, class = c("MI_SVM", "list"))
# }
#
# validate_MI_SVM <- function(x) {
#   if (!is.list(x)) {
#     stop("x should be a list!")
#     call. = FALSE
#   }
#   if (is.null(x$svm_mdl) | is.null(x$total_step) | is.null(x$representative_inst)) {
#     stop("x should contain 'svm_mdl', 'total_step', and 'representative_inst'")
#     call. = FALSE
#   }
#   x
# }


#' Prediction function for MI_SVM object
#'
#' Predictionn function for MI_SVM object.
#' @param object The return from MI_SVM()
#' @param ... Should include `newdata` to represent the newdata to be predicting on, and optionally `true_bag_info` data.frame that contains 'bag_label' and 'bag_name' for AUC and ROC calculation.
#' @return A list which contains a bag level prediction `bag_level_prediction` and an instance level prediction `instance_level_prediction`. If `true_bag_label` is fed to arguments, will also return `ROC` and `AUC` as list elements.
#' @examples
#' MilData1 <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 50,
#'                             nsample = 20,
#'                             positive_degree = 3,
#'                             positive_prob = 0.15,
#'                             positive_mean = rep(0, 5))
#' df1 <- build_instance_feature(MilData1, seq(0.05, 0.95, length.out = 10))
#' mdl <- MI_SVM(data = df1, cost = 1, kernel = 'radial')
#' predictions1_MI <- predict(mdl, newdata = df1, true_bag_info = unique(df1[, 1:2]))
#' @importFrom pROC roc auc
#' @importFrom dplyr summarise group_by
#' @author Yifei Liu
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


