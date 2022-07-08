new_smm <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("smm"))
}

validate_smm <- function(x) {
  message("No validations currently in place for object of class 'smm'.")
  x
}

#' Fit SMM model to the data
#'
#' Function to carry out support measure machines algorithm which is appropriate
#' for multiple instance learning. The algorithm calculates the kernel matrix of
#' different empirical measures using kernel mean embedding. The data set should
#' be passed in with rows corresponding to samples from a set of instances.  SMM
#' will compute a kernel on the instances and pass that to `kernlab::ksvm()` to
#' train the appropriate SVM model.
#'
#' @inheritParams mismm
#' @param x A data.frame, matrix, or similar object of covariates, where each
#'   row represents a sample. If a `mild_df` object is passed, `y, instances`
#'   are automatically extracted, `bags` is ignored, and all other columns will
#'   be used as predictors.
#' @param formula A formula with specification `y ~ x`. This argument is an
#'   alternative to the `x`, `y` arguments, but requires the `data` and
#'   `instances` argument. See examples.
#' @param cost The cost parameter in SVM, fed to the `C` argument in
#'   `kernlab::ksvm()`.
#' @param control A list of additional parameters passed to the method that
#'   control computation with the following components:
#'   * `kernel` either a character the describes the kernel ('linear' or
#'   'radial') or a kernel matrix at the instance level.
#'   * `sigma` argument needed for radial basis kernel.
#'   * `scale` argument used for all methods. A logical for whether to rescale
#'   the input before fitting.
#'
#' @return An object of class `smm`  The object contains at least the
#'   following components:
#'   * `ksvm_fit`: A fit of class `ksvm` from the kernlab package.
#'   * `call_type`: A character indicating which method `smm()` was called with.
#'   * `x`: The training data needed for computing the kernel matrix in
#'   prediction.
#'   * `features`: The names of features used in training.
#'   * `levels`: The levels of `y` that are recorded for future prediction.
#'   * `cost`: The cost parameter from function inputs.
#'   * `sigma`: The radial basis function kernel parameter.
#'   * `weights`: The calculated weights on the `cost` parameter, if applicable.
#'   * `x_scale`: If `scale = TRUE`, the scaling parameters for new predictions.
#'
#' @references Muandet, K., Fukumizu, K., Dinuzzo, F., & Schölkopf, B. (2012).
#'   Learning from distributions via support measure machines. *Advances in
#'   neural information processing systems*, *25*.
#'
#' @seealso [predict.smm()] for prediction on new data.
#
#' @examples
#' set.seed(8)
#' n_instances <- 10
#' n_samples <- 20
#' y <- rep(c(1, -1), each = n_samples * n_instances / 2)
#' instances <- as.character(rep(1:n_instances, each = n_samples))
#' x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
#'                 x2 = rnorm(length(y), mean = 2*(y==1)),
#'                 x3 = rnorm(length(y), mean = 3*(y==1)))
#'
#' df <- data.frame(instance_name = instances, y = y, x)
#'
#' mdl <- smm(x, y, instances)
#' mdl2 <- smm(y ~ ., data = df)
#'
#' # instance level predictions
#' suppressWarnings(library(dplyr))
#' df %>%
#'   dplyr::bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
#'   dplyr::bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
#'   dplyr::distinct(instance_name, y, .pred, .pred_class)
#'
#' @author Sean Kent, Yifei Liu
#' @name smm
NULL

#' @export
smm <- function(x, ...) {
  UseMethod("smm")
}

#' @describeIn smm Method for data.frame-like objects
#' @export
smm.default <- function(
    x,
    y,
    instances,
    cost = 1,
    weights = TRUE,
    control = list(kernel = "radial",
                   sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                   scale = TRUE),
    ...) {
  defaults <- list(
    kernel = "radial",
    sigma = if (is.vector(x)) 1 else 1 / ncol(x)
  )
  control <- .set_default(control, defaults)
  control$kernel <- .set_kernel(control$kernel,
                                size = length(unique(instances)),
                                size_str = "unique instances")
  control <- .set_scale(control)
  kernel_arg_passed <- .set_kernel_arg_passed(control)

  # remove NaN columns and columns with no variance, store col names, scale
  x_info <- .convert_x(x, control$scale)
  x <- x_info$x
  col_x <- x_info$col_x
  x_scale <- x_info$x_scale

  x <- data.frame(instance_name = instances, x)

  # store the levels of y and convert to -1,1 factor format at instance level.
  y_info <- convert_y(y, to = "-1,1")
  y <- factor(classify_bags(y_info$y, instances))
  lev <- y_info$lev

  weights <- .set_weights(weights, list(y = 1 * (y == 1), lev = lev))

  # kernel
  if (all(control$kernel == "radial")) {
    control$kernel <- kme(x, sigma = control$sigma)
  }

  fit <- kernlab::ksvm(x = control$kernel,
                       y = y,
                       kernel = "matrix",
                       C = cost,
                       class.weights = weights)

  res <- list(
    ksvm_fit = fit,
    call_type = "smm.default",
    x = x,
    features = col_x,
    levels = lev,
    cost = cost
  )
  res$sigma <- control$sigma
  res$weights <- weights
  res$kernel <- kernel_arg_passed
  res$kernel_param <- switch(
    res$kernel,
    "radial" = list("sigma" = control$sigma),
    "linear" = NULL,
    "user supplied matrix" = NULL
  )
  res$x_scale <- x_scale
  return(new_smm(res))
}

#' @describeIn smm Method for passing formula
#' @export
smm.formula <- function(formula, data, instances = "instance_name", ...) {
  # instance information
  if (length(instances) == 1 && is.character(instances)) {
    instance_name <- instances
    instances <- data[[instance_name]]
  } else {
    instance_name <- NULL
  }

  x <- x_from_formula(formula, data, skip = instance_name)
  response <- stats::get_all_vars(formula, data = data)
  y <- response[, 1]

  res <- smm.default(x, y, instances, ...)
  res$call_type <- "smm.formula"
  res$formula <- formula
  res$instance_name <- instance_name
  return(res)
}

#' @describeIn smm Method for `mild_df` objects. Use the `bag_label` as `y` at
#'   the instance level, then perform `smm()` ignoring the MIL structure.
#' @export
smm.mild_df <- function(x, ...) {
  y <- x$bag_label
  instances <- x$instance_name
  x$bag_label <- x$bag_name <- x$instance_name <- NULL

  res <- smm.default(x, y, instances, ...)
  res$call_type <- "smm.mild_df"
  res$bag_name <- "bag_name"
  res$instance_name <- "instance_name"
  return(res)
}


#' Predict method for `smm` object
#'
#' @details
#' When the object was fitted using the `formula` method, then the parameters
#' `new_bags` and `new_instances` are not necessary, as long as the names match
#' the original function call.
#'
#' @inheritParams predict.mismm
#' @param object an object of class `smm`
#' @param layer If `'instance'`, return predictions at the instance level.
#'   Option `'bag'` returns predictions at the bag level, but only if the model
#'   was fit with `smm.mild_df()`,
#' @param new_bags A character or character vector.  Only relevant when fit with
#'   `smm.mild_df()`, which contains bag level information.  Can specify a
#'   singular character that provides the column name for the bag names in
#'   `new_data`, default = "bag_name".  Can also specify a vector of length
#'   `nrow(new_data)` that has bag name for each instance.
#'
#' @return tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column named `.pred_class`.  If `type = 'raw'`, the tibble will
#'   have a column name `.pred`.
#'
#' @seealso [smm()] for fitting the `smm` object.
#'
#' @examples
#' set.seed(8)
#' n_instances <- 10
#' n_samples <- 20
#' y <- rep(c(1, -1), each = n_samples * n_instances / 2)
#' instances <- as.character(rep(1:n_instances, each = n_samples))
#' x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
#'                 x2 = rnorm(length(y), mean = 2*(y==1)),
#'                 x3 = rnorm(length(y), mean = 3*(y==1)))
#'
#' mdl <- smm(x, y, instances, control = list(sigma = 1/3))
#'
#' # instance level predictions (training data)
#' suppressWarnings(library(dplyr))
#' data.frame(instance_name = instances, y = y, x) %>%
#'   bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
#'   bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
#'   distinct(instance_name, y, .pred, .pred_class)
#'
#' # test data
#' new_inst <- rep(c("11", "12"), each = 30)
#' new_y <- rep(c(1, -1), each = 30)
#' new_x <- data.frame(x1 = rnorm(length(new_inst), mean = 1*(new_inst=="11")),
#'                     x2 = rnorm(length(new_inst), mean = 2*(new_inst=="11")),
#'                     x3 = rnorm(length(new_inst), mean = 3*(new_inst=="11")))
#'
#' # instance level predictions (test data)
#' data.frame(instance_name = new_inst, y = new_y, new_x) %>%
#'   bind_cols(predict(mdl, type = "raw", new_data = new_x, new_instances = new_inst)) %>%
#'   bind_cols(predict(mdl, type = "class", new_data = new_x, new_instances = new_inst)) %>%
#'   distinct(instance_name, y, .pred, .pred_class)
#'
#' @export
#' @importFrom stats predict
#' @author Sean Kent
predict.smm <- function(object,
                        new_data,
                        type = c("class", "raw"),
                        layer = "instance",
                        new_instances = "instance_name",
                        new_bags = "bag_name",
                        kernel = NULL,
                        ...) {
  type <- match.arg(type)
  layer <- match.arg(layer, c("instance", "bag"))
  if (!is.null(new_data)) {
    new_data <- as.data.frame(new_data)
  }
  .warn_kernel_scaling(kernel, object$x_scale)

  instances <- .get_instances(object, new_data, new_instances)
  new_x <- .get_new_x(object, new_data, kernel = kernel)
  kernel_m <- .calculate_pred_kernel_smm(object, kernel, instances, new_x)

  raw <- kernlab::predict(object$ksvm_fit, kernel_m, type = "decision")
  raw <- as.numeric(raw)
  names(raw) <- unique(instances)
  raw <- raw[instances]

  if (layer == "bag") {
    bags <- .get_bags(object, new_data, new_bags)
    raw <- classify_bags(raw, bags, condense = FALSE)
  }

  pos <- .to_plus_minus(raw)
  pos <- factor(pos, levels = c(-1, 1), labels = object$levels)

  res <- .pred_output(type, raw, pos)
  attr(res, "layer") <- layer
  return(res)
}

#' @export
print.smm <- function(x, digits = getOption("digits"), ...) {
  method <- attr(x, "method")
  kernel_param <- .get_kernel_param_str(x, digits)
  weights <- .get_weights_str(x)

  cat("A smm object called with", x$call_type, "\n")
  cat("", "\n")
  cat("Parameters:", "\n")
  cat("  kernel: kme w/", x$kernel, kernel_param, "\n")
  cat("  cost:", x$cost, "\n")
  cat("  scale:", !is.null(x$x_scale), "\n")
  cat("  weights:", weights, "\n")
  cat("", "\n")
  cat("Model info:", "\n")
  cat("  Features:")
  utils::str(x$features, width = getOption("width")-14)
  cat("\n")
}
