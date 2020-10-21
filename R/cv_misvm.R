new_cv_misvm <- function(x = list(), method = c("mip", "heuristic")) {
  stopifnot(is.list(x))
  method <- match.arg(method)
  structure(
    x,
    class = "cv_misvm",
    method = method
  )
}

validate_cv_misvm <- function(x) {
  message("No validations currently in place for object of class 'cv_misvm'.")
  x
}

#' Fit MI-SVM model to the data using cross-validation
#'
#' Cross-validation wrapper on the `misvm()` function to fit the MI-SVM model
#'   over a variety of specified cost parameters.  The optimal cost parameter
#'   is chosen by the best AUC of the cross-fit models.  See `?misvm` for
#'   more details on the fitting function.
#'
#' @inheritParams misvm
#' @param cost_seq Sequence of `cost` argument in `misvm()`, default is 2^(-2:2).
#' @param n_fold The number of folds. If this is specified, `fold_id` need not
#'   be specified.  Default is 5
#' @param fold_id Ids for the specific the fold for each instance. Care must be
#'   taken to ensure that ids respect the bag structure to avoid information
#'   leakage.  If `n_fold` is specified, `fold_id` will be computed
#'   automatically.
#'
#' @return an object of class 'cv_misvm'.  The object contains the following
#'   components:
#'   - `model`: a model of class 'misvm' trained on the full data with the
#'   cross-validated choice of cost parameter.   See `?misvm` for more details.
#'   - `cost_seq`: the input sequence of cost arguments
#'   - `cost_aucs`: estimated AUC for the models trained for each `cost_seq`
#'   parameter.  These are the average of the fold models for that cost,
#'   excluding any folds that don't have both levels of `y` in the validation
#'   set.
#'   - `best_cost`: The optimal choice of cost parameter, chosen as that which
#'   has the maximum AUC.  If there are ties, this will pick the smallest cost
#'   with maximum AUC.
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
#' cost_seq <- 2^seq(-5, 7, length.out = 5)
#'
#' # Heuristic method
# mdl1 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
#                  bags = df$bag_name, cost_seq = cost_seq,
#                  n_fold = 3, method = "heuristic")
#' mdl2 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df,
#'                  cost_seq = cost_seq, n_fold = 3)
#'
#' if (require(gurobi)) {
#'   # solve using the MIP method
#'   mdl3 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
#'                    bags = df$bag_name, cost_seq = cost_seq,
#'                    n_fold = 3, method = "mip")
#' }
#'
#' predict(mdl1, new_data = df, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df %>%
#'   bind_cols(predict(mdl2, df, type = "class")) %>%
#'   bind_cols(predict(mdl2, df, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#' @author Sean Kent, Yifei Liu
#' @name cv_misvm
NULL

#' @export
cv_misvm <- function(x, y, bags, ...) {
  UseMethod("cv_misvm")
}

#' @describeIn cv_misvm Method for passing formula
#' @export
cv_misvm.formula <- function(formula, data, cost_seq, n_fold, fold_id,
                             method = c("heuristic", "mip"), weights = TRUE,
                             control = list(kernel = "radial",
                                            max_step = 500,
                                            type = "C-classification",
                                            scale = TRUE,
                                            verbose = FALSE,
                                            time_limit = 60)) {

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

  res <- cv_misvm.default(x, y, bags, cost_seq = cost_seq, n_fold = n_fold,
                          fold_id = fold_id, method = method, weights = weights,
                          control = control)

  # TODO: may need to adjust these outputs and put them in the model
  res$model$call_type <- "misvm.formula"
  res$model$formula <- formula
  res$model$bag_name <- bag_name
  res$call_type <- "cv_misvm.formula"
  return(res)

}

#' @describeIn cv_misvm Method for data.frame-like objects
#' @export
cv_misvm.default <- function(x, y, bags, cost_seq, n_fold, fold_id,
                          method = c("heuristic", "mip"), weights = TRUE,
                          control = list(kernel = "radial",
                                         max_step = 500,
                                         type = "C-classification",
                                         scale = TRUE,
                                         verbose = FALSE,
                                         time_limit = 60)) {

  method = match.arg(method)
  if (!missing(n_fold) & !missing(fold_id)) {
    message("Both n_fold and fold_id are supplied, ignoring n_fold in favor of fold_id")
  }
  stopifnot(is.numeric(cost_seq))

  # store the levels of y and convert to 0,1 numeric format.
  y_info <- convert_y(y)
  y <- y_info$y
  lev <- y_info$lev

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

  fold_info <- select_cv_folds2(y, bags, n_fold, fold_id)
  fold_id <- fold_info$fold_id
  n_fold <- fold_info$n_fold

  aucs <- matrix(NA, nrow = length(cost_seq), ncol = n_fold)
  for (i in 1:length(cost_seq)) {
    # auc_sum <- 0
    for (fold in 1:n_fold) {
      ind <- fold_id != fold

      model_i_fold <- misvm(x[ind, , drop = FALSE], y[ind], bags[ind],
                            cost = cost_seq[i], method = method,
                            weights = weights, control = control)
      pred_i_fold <- predict(model_i_fold,
                             new_data = x[!ind, , drop = FALSE],
                             new_bags = bags[!ind],
                             type = "raw")

      if (length(unique(y[!ind])) != 2) {
        aucs[i, fold] <- NA
      } else {
        suppressMessages({
          aucs[i, fold] <- pROC::auc(response = classify_bags(y[!ind], bags[!ind]),
                                     predictor = classify_bags(pred_i_fold$.pred, bags[!ind]))
        })
      }
    }
  }
  cost_aucs <- rowMeans(aucs, na.rm = TRUE)
  best_cost <- cost_seq[which.max(cost_aucs)]

  misvm_fit <- misvm(x, y, bags,
                     cost = best_cost, method = method,
                     weights = weights, control = control)

  res <- list(model = misvm_fit,
              cost_seq = cost_seq,
              cost_aucs = cost_aucs,
              best_cost = best_cost)

  res$model$levels <- lev
  new_cv_misvm(res, method = method)
}


#' Predict method for 'cv_misvm' object
#'
#' @param object an object of class 'cv_misvm'
#' @inheritParams predict.misvm
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
#' mdl1 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
#'                  bags = df$bag_name, cost_seq = cost_seq,
#'                  n_fold = 3, method = "heuristic")
#'
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
predict.cv_misvm <- function(object, new_data,
                          type = c("class", "raw"), layer = c("bag", "instance"),
                          new_bags = "bag_name") {
  type <- match.arg(type)
  layer <- match.arg(layer)

  predict.misvm(object$model, new_data = new_data, type = type, layer = layer,
                new_bags = new_bags)
}

