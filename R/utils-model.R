#' Convert to -1, +1
#'
#' @param x A numeric vector. This can be input from {0, 1} or raw scores from
#'   the real line.
#' @param thresh A numeric threshold (default 0) that `x` must exceed to be
#'   considered positive.
#' @noRd
.to_plus_minus <- function(x, thresh = 0) {
  2 * (x > 0) - 1
}

#' Set default values in a list, when not present
#' @param x A list to modify.
#' @param args A list or vector of named arguments for defaults
#' @noRd
.set_default <- function(x, args) {
  for (i in seq_along(args)) {
    nm <- names(args[i])
    if (nm %ni% names(x)) {
      x[nm] <- args[i]
    }
  }
  return(x)
}

#' Reset 'scale' value in control list
#' @inheritParams .set_default
#' @noRd
.set_scale <- function(x) {
  if ("scale" %ni% names(x) && is.matrix(x$kernel)) {
    x$scale <- FALSE
    rlang::inform(c(
      "Scaling is not available when `kernel` is of class matrix.",
      i = "Setting `scale` = FALSE."
    ))
  } else if ("scale" %ni% names(x)) {
    x$scale <- TRUE
  }
  return(x)
}

#' Reset 'start' value in control list
#' @inheritParams .set_default
#' @noRd
.set_start <- function(x) {
  if (x$start && x$kernel != "linear") {
    x$start <- FALSE
    rlang::inform(c(
      "Warm start is not available when `kernel` is not equal to 'linear'.",
      i = "Setting `start` = FALSE."
    ))
  }
  return(x)
}

#' Set 'kernel' value from control list
#'
#' Set the kernel based on a passed parameter (usually to `control$kernel`). The
#' logic handles cases where a character is passed or a matrix is passed:
#' - `kernel` is a matrix: checks the matrix dimensions are correct. If they are
#'   not, it warns and sets `kernel = 'radial'`
#' - `kernel` is not a matrix: sets `kernel = 'radial'` since the linear kernel
#'   is not currently supported
#'
#' @param kernel A matrix or character to be checked and potentially modified.
#' @param size An integer for the expected row and column size of `kernel`
#' @param size_str A character for the size warning that represents what the
#'   size is
#' @inheritParams mismm
#' @param fun A string for the function used
#' @noRd
.set_kernel <- function(kernel, size, size_str, method = "default", fun = "smm") {
  if (all(kernel != "radial") && !is.matrix(kernel)) {
    kernel <- "radial"
    rlang::warn(c(
      "Argument `control$kernel` must either be 'radial' or a square matrix.",
      i = "Setting `control$kernel` = 'radial'."
    ))
  } else if (fun == "mismm" && method == "mip" && is.matrix(kernel)) {
    kernel <- "radial"
    rlang::warn(c(
      "Must not pass `control$kernel` as matrix when using `mismm()` with `method == 'mip'`.",
      i = "Setting `control$kernel` = 'radial'."
    ))
  } else if (is.matrix(kernel)) {
    if (all(dim(kernel) != c(size, size))) {
      kernel <- "radial"
      rlang::warn(c(
        paste0("Any matrix passed to `control$kernel` must be of size (n, n) where n is the number of ", size_str, "."),
        x = paste0("Matrix was of size (", nrow(kernel), ", ", ncol(kernel) , ")."),
        x = paste0("There are ", size, "unique instances."),
        i = "Setting `control$kernel` = 'radial'."
      ))
    }
  }
  return(kernel)
}

#' Store the levels of y and convert to 0,1 numeric format.
#' @inheritParams .reorder
#' @param to An option to convert to 0,1 numeric or -1,1 numeric format
#' @noRd
convert_y <- function(y, to = "0,1") {
  to <- match.arg(to, c("0,1", "-1,1"))
  y <- factor(y)
  lev <- levels(y)
  if (length(lev) == 1) {
    stop(paste0("Response y has only one level, ", lev, ", cannot perform misvm fitting."))
  } else if (length(lev) > 2) {
    stop(paste0("Response y has more than two levels, ", lev, ", cannot perform misvm fitting."))
  }
  if (lev[1] == 1 || lev[1] == "1" || lev[1] == TRUE) {
    lev <- rev(lev)
    y <- factor(y, levels = lev) # make sure 1 is second level
  } else if (lev[2] != 1 && lev[2] != "1" && lev[2] != TRUE) {
    message(paste0("Setting level ", lev[2], " to be the positive class for misvm fitting."))
  } # else lev[2] is like 1, keep it that way.
  y <- as.numeric(y) - 1
  if (to == "-1,1") {
    y <- 2*y - 1
  }
  list(y = y, lev = lev)
}

#' Store the levels of y and convert to integer format.
#' @inheritParams .reorder
#' @noRd
.convert_y_ordinal <- function(y) {
  y <- ordered(y)
  lev <- levels(y)
  if (length(lev) == 1) {
    stop(paste0("Response y has only one level, ", lev, ", cannot perform misvm fitting."))
  } else if (length(lev) == 2) {
    warning(paste0("Only 2 levels detected.  Consider using a non-ordinal method."))
  }
  if (lev[1] != 1 && lev[1] != "1") {
    message(paste0("Setting level ", lev[1], " to be the lowest ordinal level"))
  }
  y <- as.numeric(y)
  list(y = y, lev = lev)
}

#' Check columns of x for NaN and no variance
#'
#' This function will return x after checking for columns with NaN values and
#' columns with no variance. Both types will provide warnings about which columns are
#' being removed.
#' @inheritParams misvm
#' @noRd
.check_x_columns <- function(x) {
  # store colnames of x
  x <- as.data.frame(x)
  col_x <- colnames(x)

  # remove NaN columns and columns with no variance
  nan_columns <- vapply(x, function(.x) any(is.nan(.x)), FUN.VALUE = logical(1))
  if (any(nan_columns)) {
    rlang::warn(c(
      "Cannot use columns with NaN values.",
      x = paste("Removing columns", paste0(names(which(nan_columns)), collapse = ", "))
    ))
  }
  ident_columns <- vapply(x, function(.x) all(.x == .x[1]), FUN.VALUE = logical(1))
  if (any(ident_columns, na.rm = TRUE)) {
    rlang::warn(c(
      "Cannot use columns that have the same value for all rows.",
      x = paste("Removing columns", paste0(names(which(ident_columns)), collapse = ", "))
    ))
  }
  col_x <- setdiff(col_x, names(which(nan_columns)))
  col_x <- setdiff(col_x, names(which(ident_columns)))
  x <- x[, col_x, drop = FALSE]
  x <- as.matrix(x)
  return(x)
}

#' Process x
#'
#' Check columns of x for NaN and no variance, then optionally scale
#' @inheritParams misvm
#' @param scale A logical for whether to rescale the input before fitting
#'   (default `FALSE`).
#' @noRd
.convert_x <- function(x, scale = FALSE) {
  x <- .check_x_columns(x)

  if (scale) {
    x <- scale(x)
    x_scale <- list(
      "center" = attr(x, "scaled:center"),
      "scale" = attr(x, "scaled:scale")
    )
  } else {
    x_scale <- NULL
  }

  list(x = x, col_x = colnames(x), x_scale = x_scale)
}

#' Convert kernel argument to matrix
#'
#' @param x A data.frame of covariates.
#' @param k The kernel argument. Either a character that describes the kernel
#'   (`'linear'` or `'radial'`) or a kernel matrix at the instance level.
#' @param ... Additional arguments passed to `compute_kernel()`, such as
#'   `sigma`.
#' @noRd
.convert_kernel <- function(x, kernel, ...) {
  if (!is.matrix(kernel)) {
    compute_kernel(x, type = kernel, ...)
  } else {
    kernel
  }
}

#' Calculate weights
#'
#' These weights control the importance of the cost parameter in SVM on errors
#' within different `y` values by multiplying against it.  Calculation depends
#' on what is passed to `w`:
#' - `TRUE`: weights are calculated based on inverse counts of instances within
#'   a given label, where we only count one positive instance per bag.  This
#'   reflects an inverse weighting on the counts of `y` used in training.
#' - A named vector: the weights of the vector are used, provided they match the
#'   levels of `y`. If names don't match, an error is thrown.
#' - `FALSE` or `NULL`: weights not used, returned as `NULL`.
#'
#' @param w A named vector, or `TRUE`, to control the weight of the cost
#'   parameter for each possible y value.  Weights multiply against the cost
#'   vector. If `TRUE`, weights are calculated based on inverse counts of
#'   instances with given label, where we only count one positive instance per
#'   bag. Otherwise, names must match the levels of `y`.
#' @param y Output from `.convert_y()`
#' @param pos_group A vector indicating groups for counting positive
#'   contributions (usually bags)
#' @param neg_group A vector indicating groups for counting negative
#'   contributions (usually instances)
#' @inheritParams misvm
#' @noRd
.set_weights <- function(w, y, pos_group = NULL, neg_group = NULL) {
  lev <- y$lev
  y <- y$y
  pos_group <- pos_group %||% seq_along(y)
  neg_group <- neg_group %||% seq_along(y)

  if (is.numeric(w)) {
    stopifnot(names(w) == lev | names(w) == rev(lev))
    w <- w[lev]
    names(w) <- c("-1", "1")
  } else if (isTRUE(w)) {
    pos_y <- sapply(split(y, factor(pos_group)), unique)
    neg_y <- sapply(split(y, factor(neg_group)), unique)
    w <- c("-1" = sum(pos_y == 1) / sum(neg_y == 0), "1" = 1)
  } else {
    w <- NULL
  }
  return(w)
}

#' Warn about no weights
#' @inheritParams .set_weights
#' @param fun The function name to use in the warning message
#' @noRd
.warn_no_weights <- function(w, fun = "omisvm") {
  fun <- match.arg(fun, c("mior", "omisvm", "svor_exc"))

  if (!is.null(w)) {
    w <- NULL
    if (fun == "omisvm") {
      msg <- paste0("Weights are not currently implemented for `",
                    fun, "()` when `kernel == 'linear'`.")
      rlang::warn(msg)
    } else {
      msg <- paste0("Weights are not currently implemented for `", fun, "()`.")
      rlang::warn(msg)
    }
  }
  return(w)
}

#' Warn about argument `s` in `omisvm()`
#' @inheritParams omisvm
#' @param k An integer for the number of levels in the outcome
#' @param kernel Taken from `control$kernel` in `omisvm()`
#' @noRd
.warn_omisvm_s <- function(s, k, method, kernel) {
  if (method == "qp-heuristic" && kernel == "linear" && s != Inf) {
    rlang::warn(
      "The argument `s` is not currently used for `kernel == 'linear'`."
    )
  }

  if (s == Inf) {
    s <- k-1 # all points replicated
  } else if (s < 1) {
    s <- 1
    rlang::warn(c(
      "The value of `s` must not be smaller than 1.",
      i = "Setting `s <- 1` for a minimal number of replicated points."
    ))
  } else if (s > k-1) {
    s <- k-1
    rlang::warn(c(
      "The value of `s` must not be larger than the number of levels in `y` minus 1.",
      i = "Setting `s <- k-1` for a maximal number of replicated points"
    ))
  }
  return(s)
}

#' Warn about scaling with matrix kernels
#' @inheritParams predict.smm
#' @param scale A logical for whether scaling was done in model fitting
#' @noRd
.warn_kernel_scaling <- function(kernel, scale) {
  if (is.matrix(kernel) && !is.null(scale)) {
    rlang::inform(
      "Since model was fit using scaling, make sure that kernel matrix was similarly scaled."
    )
  }
}

#' Calculate x-matrix from a standard formula
#' @inheritParams smm
#' @param skip a vector of variable names to skip, or `NULL` to keep all
#'   (default `NULL`).
#' @noRd
x_from_formula <- function(formula, data, skip = NULL) {
  data <- as.data.frame(data)
  response <- as.character(formula[[2]])
  skip <- c(skip, response)
  predictors <- setdiff(colnames(data), skip)

  x <- stats::model.matrix(formula[-2], data = data[, predictors, drop = FALSE])
  if (attr(stats::terms(formula, data = data), "intercept") == 1) x <- x[, -1, drop = FALSE]
  x <- as.data.frame(x)
}

#' Calculate x-matrix from a formula with `mi()` in it
#' @inheritParams misvm
#' @noRd
x_from_mi_formula <- function(formula, data) {
  data <- as.data.frame(data)
  mi_names <- as.character(stats::terms(formula, data = data)[[2]])
  bag_label <- mi_names[[2]]
  bag_name <- mi_names[[3]]
  predictors <- setdiff(colnames(data), c(bag_label, bag_name))

  x <- stats::model.matrix(formula[-2], data = data[, predictors, drop = FALSE])
  if (attr(stats::terms(formula, data = data), "intercept") == 1) x <- x[, -1, drop = FALSE]
  x <- as.data.frame(x)
}

#' Calculate x-matrix from a formula with `mild()` in it
#' @inheritParams mismm
#' @noRd
x_from_mild_formula <- function(formula, data) {
  data <- as.data.frame(data)
  mild_names <- as.character(stats::terms(formula, data = data)[[2]])
  bag_label <- mild_names[[2]]
  bag_name <- mild_names[[3]]
  instance_name <- mild_names[[4]]
  predictors <- setdiff(colnames(data), c(bag_label, bag_name, instance_name))

  x <- stats::model.matrix(formula[-2], data = data[, predictors, drop = FALSE])
  if (attr(stats::terms(formula, data = data), "intercept") == 1) x <- x[, -1, drop = FALSE]
  x <- as.data.frame(x)
}

#' Get bags for prediction function
#'
#' Used in `misvm()`, `omisvm()`
#' @inheritParams predict.misvm
#' @noRd
.get_bags <- function(object, new_data, new_bags) {
  if (grepl("formula", object$call_type) && new_bags[1] == "bag_name" && length(new_bags) == 1) {
    new_bags <- object$bag_name
  }
  if (length(new_bags) == 1 && new_bags[1] %in% colnames(new_data)) {
    bags <- new_data[[new_bags]]
  } else {
    bags <- new_bags
  }
}

#' Get instances for prediction function
#'
#' Used in `predict.smm()`
#' @inheritParams predict.smm
#' @noRd
.get_instances <- function(object, new_data, new_instances) {
  if (grepl("formula", object$call_type) && new_instances[1] == "instance_name" && length(new_instances) == 1) {
    new_instances <- object$instance_name
  }
  if (length(new_instances) == 1 && new_instances[1] %in% colnames(new_data)) {
    instances <- new_data[[new_instances]]
  } else {
    instances <- new_instances
  }
}

#' Get new_x for prediction function
#'
#' Used in `misvm()`, `omisvm()`
#' @inheritParams predict.misvm
#' @noRd
.get_new_x <- function(object, new_data, kernel = NULL, instances = NULL) {
  method <- attr(object, "method")
  call_type <- object$call_type

  if (grepl("mismm.formula", call_type)) {
    new_x <- x_from_mild_formula(object$formula, new_data)
  } else if (grepl("smm.formula", call_type)) {
    new_x <- x_from_formula(object$formula, new_data, skip = object$instance_name)
  } else if (grepl("formula", call_type)) {
    new_x <- x_from_mi_formula(object$formula, new_data)
  } else {
    new_x <- new_data
  }
  new_x <- new_x[, object$features, drop = FALSE]

  if ("kfm_fit" %in% names(object)) {
    new_x <- build_fm(object$kfm_fit, as.matrix(new_x))
    if (grepl("mismm", object$call_type)) {
      new_x <- average_over_instances(new_x, instances)
    }
  }
  scale_eligible <- method == "qp-heuristic" || grepl("smm", object$call_type)
  if (scale_eligible && "x_scale" %in% names(object) && is.null(kernel)) {
    new_x <- as.data.frame(scale(new_x, object$x_scale$center, object$x_scale$scale))
  }
  new_x
}

#' Calculate a kernel for prediction in `smm()`
#'
#' When `kernel` is already a matrix, pull the support vector columns.
#' Otherwise, calculate the kernel from the new data and the training data at
#' the support vector instances based on the kernel mean embedding `kme()`
#' function
#' @inheritParams predict.smm
#' @param instances A vector of instances, as from `.get_instances()`
#' @param new_x A data frame of new predictors
#' @noRd
.calculate_pred_kernel_smm <- function(object, kernel, instances, new_x) {
  train_df <- object$x
  sv_ind <- kernlab::SVindex(object$ksvm_fit)
  if (is.matrix(kernel)) {
    kernel_m <- kernel[, sv_ind, drop = FALSE] # future note, I don't think this actually filters anything out...
  } else {
    train_inst <- train_df$instance_name
    sv_inst_names <- unique(train_inst)[sv_ind]
    sv_inst <- which(train_inst %in% sv_inst_names)
    new_df <- data.frame(instance_name = instances, new_x)

    kernel_m <- kme(new_df,
                    train_df[sv_inst, , drop = FALSE],
                    sigma = object$sigma)
  }
  return(kernlab::as.kernelMatrix(kernel_m))
}

#' Calculate a kernel for prediction in `mismm()`
#'
#' @inheritParams .calculate_pred_kernel_smm
#' @noRd
.calculate_pred_kernel_mismm <- function(object, kernel, instances, new_x) {

  train_inst <- object$gurobi_fit$instances
  sv_inst_names <- unique(train_inst)[object$repr_inst == 1]
  sv_inst <- which(train_inst %in% sv_inst_names)

  if (is.null(kernel)) {
    train_df <- data.frame(instance_name = train_inst[sv_inst], object$x[sv_inst, , drop = FALSE])
    new_df <- data.frame(instance_name = instances, new_x)
    kernel_m <- kme(new_df,
                    train_df,
                    sigma = object$gurobi_fit$sigma)
  } else {
    kernel_m <- kernel[, object$repr_inst == 1]
  }
  colnames(kernel_m) <- unique(train_inst[sv_inst])
  return(kernel_m)
}

#' Return prediction output
#' @inheritParams predict.misvm
#' @param scores A vector of raw output scores
#' @param class_ A vector of class labels
#' @noRd
.pred_output <- function(type, scores, class_) {
  switch(
    type,
    "raw" = tibble::tibble(.pred = as.numeric(scores)),
    "class" = tibble::tibble(.pred_class = class_)
  )
}

#' Default gurobi parameters
#' @inheritParams misvm_mip_fit
#' @noRd
.gurobi_params <- function(verbose, time_limit) {
  params <- list()
  params[["OutputFlag"]] <- 1*verbose
  params[["IntFeasTol"]] <- 1e-5
  params[["PSDTol"]] <- 1e-4
  if (time_limit) {
    params[["TimeLimit"]] <- time_limit
  }

  params
}

#' Set the kernel arg passed
#' @param control The control in a modeling function
#' @noRd
.set_kernel_arg_passed <- function(control) {
  if (is.matrix(control$kernel)) {
    "user supplied matrix"
  } else {
    control$kernel
  }
}

#' Set the sampling arg passed
#' @param x The sampling arg
#' @noRd
.set_sampling_arg_passed <- function(x) {
  if (is.character(x) && length(x) == 1) {
    x
  } else {
    "user supplied sampling"
  }
}

#' Get string for `kernel_param` print
#' @param x A model object
#' @noRd
.get_kernel_param_str <- function(x, digits = getOption("digits")) {
  kernel_param <- switch(
    x$kernel,
    "linear" = "",
    "radial" = paste0(" (sigma = ",
                      format(x$kernel_param$sigma, digits = digits),
                      ")"),
    "user supplied matrix" = ""
  )
}

#' Get string for `weights` print
#' @inheritParams .get_kernel_param_str
#' @noRd
.get_weights_str <- function(x) {
  if (!is.null(x$weights)) {
    weights <- paste0(
      "(",  "'", x$levels[1], "' = ", x$weights[1],
      ", ", "'", x$levels[2], "' = ", x$weights[2], ")"
    )
  } else {
    weights <- "FALSE"
  }
  return(weights)
}

#' Initialize Instance Selection
#'
#' Use bag_label and instance_name information to initialize the selected
#' instances. When bag label is 0, select all instances.  When bag label is
#' 1, select the first instance in each bag.
#' @param data A mild_df object, potentially generated by mild_df().
#' @return A list of 3:
#'   * `useful_inst_names` includes the names of instances that were selected.
#'   * `useful_inst_idx` includes the index of instances that were selected, at
#'   the instance level
#'   * `yy` includes the bag labels, at the instance level
#' @noRd
initialize_instance_selection <- function(data) {
  s_bag <- split(data, factor(data$bag_name, levels = unique(data$bag_name)))
  unique_bag_label <- function(x) unique(x$bag_label)
  unique_instance_name <- function(x) unique(x$instance_name)
  select_useful_inst <- function(label, name) {
    if (label == 1) name[1] else name
  }

  labels <- lapply(s_bag, FUN = unique_bag_label)
  instance_names <- lapply(s_bag, FUN = unique_instance_name)
  useful_inst_names <- unlist(mapply(FUN = select_useful_inst,
                                     label = labels,
                                     name = instance_names),
                              use.names = FALSE)
  useful_inst_idx <- which(unique(data$instance_name) %in% useful_inst_names)

  s_inst <- split(data, factor(data$instance_name, levels = unique(data$instance_name)))
  yy <-  unlist(lapply(s_inst, FUN = unique_bag_label),
                use.names = FALSE)
  yy <- factor(yy, levels = c(-1, 1), labels = c("-1", "1"))  ## change yy to a factor.

  return(list(useful_inst_names = useful_inst_names,
              useful_inst_idx = useful_inst_idx,
              yy = yy))
}

#' Select Cross Validation folds from mild_df
#'
#' Uses the bag information from a `data` object to generate folds for use in
#' cross validation.
#'
#' @inheritParams initialize_instance_selection
#' @inheritParams cv_misvm
#'
#' @return A list with the following components:
#'   - `n_fold` the number of folds for cross validation.
#'   - `fold_id` if fold_id is missing in input, returns the folds calculated by
#'   splitting the positive and negative bags separately.
#'
#' @noRd
select_cv_folds <- function(data, n_fold, fold_id) {
  # TODO: I think I can make this a lot cleaner with the following idea from my other code
  # L1 bags_for_train <- which(foldid != fold)
  # L2 ind <- bags %in% unique(bags)[bags_for_train]
  # # The last line goes from the bag index to the instance index, avoiding right joins

  bag_info <- unique(data[, c("bag_label", "bag_name"), drop = FALSE])
  if (missing(fold_id)) {
    if (missing(n_fold))
      n_fold <- 5

    positive_bag_idx <- which(bag_info$bag_label == 1)
    negative_bag_idx <- which(bag_info$bag_label == 0)
    positive_fold_id <- .resample(seq_along(positive_bag_idx) %% n_fold + 1)
    negative_fold_id <- .resample(seq_along(negative_bag_idx) %% n_fold + 1)

    bag_id <- numeric(nrow(bag_info))
    bag_id[positive_bag_idx] <- positive_fold_id
    bag_id[negative_bag_idx] <- negative_fold_id

    temp_data <- data.frame(bag_name = unique(data$bag_name),
                            bag_id = bag_id,
                            stringsAsFactors = FALSE) %>%
      dplyr::right_join(unique(data %>% dplyr::select(.data$bag_name, .data$instance_name)),
                        by = "bag_name")
    fold_id <- temp_data$bag_id  ## now fold_id is of length(unique(data$instance_name))
  } else {
    n_fold <- max(fold_id)
    if (!is.null(setdiff(fold_id, 1:n_fold)))
      stop("The argument fold_id has some 'holes'!")
  }
  return(list(n_fold = n_fold, fold_id = fold_id))
}

#' Function that will eventually supercede select_cv_folds because it supports
#' the same variables as `misvm()`, `mismm()` and `smm()`.
#' @inheritParams classify_bags
#' @inheritParams select_cv_folds
#' @noRd
select_cv_folds2 <- function(y, bags, n_fold, fold_id) {

  info <- data.frame(y = y, bags = bags)
  info_bag_layer <- unique(info)

  if (missing(fold_id)) {
    if (missing(n_fold))
      n_fold <- 5

    positive_bag_idx <- which(info_bag_layer$y == 1)
    negative_bag_idx <- which(info_bag_layer$y == 0)
    positive_fold_id <- .resample(seq_along(positive_bag_idx) %% n_fold + 1)
    negative_fold_id <- .resample(seq_along(negative_bag_idx) %% n_fold + 1)

    bag_id <- numeric(nrow(info_bag_layer))
    bag_id[positive_bag_idx] <- positive_fold_id
    bag_id[negative_bag_idx] <- negative_fold_id
    info_bag_layer$bag_id <- bag_id

    tmp <- info %>%
      dplyr::left_join(info_bag_layer, by = c("bags", "y"))

    fold_id <- tmp$bag_id
  } else {
    n_fold <- max(fold_id)
    if (!is.null(setdiff(fold_id, 1:n_fold)))
      stop("The argument fold_id has some 'holes'!")
  }
  return(list(n_fold = n_fold, fold_id = fold_id))
}
