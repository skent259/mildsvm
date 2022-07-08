new_misvm_orova <- function(x = list(), method = c("qp-heuristic")) {
  stopifnot(is.list(x))
  method <- match.arg(method, c("heuristic", "mip", "qp-heuristic"))
  structure(
    x,
    class = "misvm_orova",
    method = method
  )
}

validate_misvm_orova <- function(x) {
  message("No validations currently in place for object of class 'misvm_orova'.")
  x
}

#' Fit MI-SVM model to ordinal outcome data using One-vs-All
#'
#' This function uses the one-vs-all multiclass classification strategy to fit a
#' series of MI-SVM models for predictions on ordinal outcome data.  For an
#' ordinal outcome with K levels, we fit K MI-SVM models to predict an
#' individual level vs not.
#'
#' @inheritParams omisvm
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted
#' @param ... Arguments passed to or from other methods.
#'
#' @return An object of class `misvm_orova`  The object contains at least the
#'   following components:
#'   * `fits`: a list of `misvm` objects with length equal to the number of
#'   classes in `y`. See [misvm()] for details on the `misvm` object.
#'   * `call_type`: A character indicating which method `misvm_orova()` was
#'   called with.
#'   * `features`: The names of features used in training.
#'   * `levels`: The levels of `y` that are recorded for future prediction.
#'
#' @references Andrews, S., Tsochantaridis, I., & Hofmann, T. (2002). Support
#'   vector machines for multiple-instance learning. *Advances in neural
#'   information processing systems*, *15*.
#'
#' @seealso [predict.misvm_orova()] for prediction on new data.
#'
#' @examples
#' data("ordmvnorm")
#' x <- ordmvnorm[, 3:7]
#' y <- ordmvnorm$bag_label
#' bags <- ordmvnorm$bag_name
#'
#' mdl1 <- misvm_orova(x, y, bags)
#' predict(mdl1, x, new_bags = bags)
#'
#' @author Sean Kent
#' @name misvm_orova
NULL

#' @export
misvm_orova <- function(x, ...) {
  UseMethod("misvm_orova")
}

#' @describeIn misvm_orova Method for data.frame-like objects
#' @export
misvm_orova.default <- function(
    x,
    y,
    bags,
    cost = 1,
    method = c("heuristic", "mip", "qp-heuristic"),
    weights = TRUE,
    control = list(kernel = "linear",
                   sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                   nystrom_args = list(m = nrow(x),
                                       r = nrow(x),
                                       sampling = "random"),
                   max_step = 500,
                   type = "C-classification",
                   scale = TRUE,
                   verbose = FALSE,
                   time_limit = 60,
                   start = FALSE),
    ...) {
  # store the levels of y and convert to numeric format.
  y_info <- .convert_y_ordinal(y)
  y <- y_info$y
  lev <- y_info$lev

  labels <- unique(y)
  fits <- list()
  for (k in sort(labels)) {
    fits[[as.character(k)]] <- misvm(x, 1 * (y==k), bags,
                                     method = method,
                                     weights = weights,
                                     control = control,
                                     ...)
  }

  out <- list()
  out$fits <- fits
  out$call_type <- "misvm_orova.default"
  out$levels <- lev
  out$features <- fits[[1]]$features
  out$kernel <- fits[[1]]$kernel
  out$kernel_param <- fits[[1]]$kernel_param
  return(new_misvm_orova(out, method = method))
}

#' @describeIn misvm_orova Method for passing formula
#' @export
misvm_orova.formula <- function(formula, data, ...) {
  # NOTE: other 'professional' functions use a different type of call that I
  #   couldn't get to work. See https://github.com/therneau/survival/blob/master/R/survfit.R
  #   or https://github.com/cran/e1071/blob/master/R/svm.R
  #   right now we're using something that should work for most generic formulas

  mi_names <- as.character(stats::terms(formula, data = data)[[2]])
  bag_name <- mi_names[[3]]

  x <- x_from_mi_formula(formula, data)
  response <- stats::get_all_vars(formula, data = data)
  y <- response[, 1]
  bags <- response[, 2]

  res <- misvm_orova.default(x, y, bags, ...)

  for (i in seq_along(res$fits)) {
    res$fits[[i]]$call_type <- "misvm.formula"
    res$fits[[i]]$formula <- formula
    res$fits[[i]]$bag_name <- bag_name
  }

  res$call_type <- "misvm_orova.formula"
  res$formula <- formula
  res$bag_name <- bag_name
  return(res)
}

#' @describeIn misvm_orova Method for `mi_df` objects, automatically handling bag
#'   names, labels, and all covariates.
#' @export
misvm_orova.mi_df <- function(x, ...) {
  x <- as.data.frame(validate_mi_df(x))
  y <- x$bag_label
  bags <- x$bag_name
  x$bag_label <- x$bag_name <- NULL

  res <- misvm_orova.default(x, y, bags, ...)
  for (i in seq_along(res$fits)) {
    res$fits[[i]]$call_type <- "misvm.mi_df"
    res$fits[[i]]$bag_name <- "bag_name"
  }
  res$call_type <- "misvm_orova.mi_df"
  res$bag_name <- "bag_name"
  return(res)
}

#' Predict method for `misvm_orova` object
#'
#' Predict method for `misvm_orova` object.  Predictions use the K fitted MI-SVM
#' models.  For class predictions, we return the class whose MI-SVM model has
#' the highest raw predicted score.  For raw predictions, a full matrix of
#' predictions is returned, with one column for each model.
#'
#' @details When the object was fitted using the `formula` method, then the
#'   parameters `new_bags` and `new_instances` are not necessary, as long as the
#'   names match the original function call.
#'
#' @param object An object of class `misvm_orova`
#' @param type If `'class'`, return predicted values based on the highest output
#'   of an individual model.  If `'raw'`, return the raw predicted scores for
#'   each model.
#' @inheritParams predict.misvm
#'
#' @return A tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column `.pred_class`.  If `type = 'raw'`, the tibble will have
#'   K columns `.pred_{class_name}` corresponding to the raw predictions of the
#'   K models.
#'
#' @seealso [misvm_orova()] for fitting the `misvm_orova` object.
#'
#' @examples
#' data("ordmvnorm")
#' x <- ordmvnorm[, 3:7]
#' y <- ordmvnorm$bag_label
#' bags <- ordmvnorm$bag_name
#'
#' mdl1 <- misvm_orova(x, y, bags)
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' df1 <- bind_cols(y = y, bags = bags, as.data.frame(x))
#' df1 %>%
#'   bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
#'   bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
#'   select(-starts_with("V")) %>%
#'   distinct()
#'
#' @export
#' @author Sean Kent
predict.misvm_orova <- function(object,
                                new_data,
                                type = c("class", "raw"),
                                layer = c("bag", "instance"),
                                new_bags = "bag_name",
                                ...) {
  type <- match.arg(type, c("class", "raw"))
  layer <- match.arg(layer, c("bag", "instance"))
  method <- attr(object, "method")
  levels <- object$levels

  preds <- lapply(object$fits, predict,
                  new_data = new_data, type = "raw", layer = layer,
                  new_bags = new_bags, ...)

  scores_df <- do.call(cbind, preds)
  colnames(scores_df) <- paste0(".pred_", levels)

  class_ <- apply(scores_df, 1, which.max)
  class_ <- factor(class_, levels = seq_along(levels), labels = levels)

  res <- switch(type,
                "raw" = tibble::as_tibble(scores_df),
                "class" = tibble::tibble(.pred_class = class_))
  attr(res, "layer") <- layer
  return(res)
}

#' @export
print.misvm_orova <- function(x, digits = getOption("digits"), ...) {
  method <- attr(x, "method")
  kernel_param <- .get_kernel_param_str(x, digits)
  weights <- !is.null(x$fits[[1]]$weights)
  n_steps <- sapply(x$fits, function(.x) .x$n_step)
  mipgaps <- sapply(x$fits, function(.x) .x$gurobi_fit$mipgap)

  cat("An misvm_orova object called with", x$call_type, "\n")
  cat("", "\n")
  cat("Parameters:", "\n")
  cat("  method:", method, "\n")
  cat("  kernel:", x$kernel, kernel_param, "\n")
  cat("  cost:", x$fits[[1]]$cost, "\n")
  cat("  scale:", !is.null(x$fits[[1]]$x_scale), "\n")
  cat("  weights:", weights, "\n")
  cat("", "\n")
  cat("Model info:", "\n")
  cat("  Number of models:", length(x$fits), "\n")
  cat("  Levels of `y`:")
  utils::str(x$levels, width = getOption("width")-14)
  cat("  Features:")
  utils::str(x$features, width = getOption("width")-14)
  if (method == "heuristic" || method == "qp-heuristic") {
    cat("  Number of iterations:", n_steps, "\n")
  }
  if (method == "mip") {
    cat("  Gap to optimality:", mipgaps, "\n")
  }
  cat("\n")
}
