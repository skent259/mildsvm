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
  if ("scale" %ni% names(x) && inherits(x$kernel, "matrix")) {
    # if kernel matrix is passed in, then really no re-scaling was done.
    x$scale <- FALSE
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
      i = "Setting `start` = FALSE. "
    ))
  }
  return(x)
}

#' Store the levels of y and convert to 0,1 numeric format.
#' @inheritParams .reorder
#' @noRd
convert_y <- function(y) {
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
  if (lev[1] != 1 & lev[1] != "1") {
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
#' @inheritParams misvm
#' @noRd
.set_weights <- function(w, y, bags) {
  lev <- y$lev
  y <- y$y

  if (is.numeric(w)) {
    stopifnot(names(w) == lev | names(w) == rev(lev))
    w <- w[lev]
    names(w) <- c("-1", "1")
  } else if (w) {
    bag_labels <- sapply(split(y, factor(bags)), unique)
    w <- c("-1" = sum(bag_labels == 1) / sum(y == 0), "1" = 1)
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
    msg <- paste0("Weights are not currently implemented for `", fun, "()`.")
    rlang::warn(msg)
  }
  return(w)
}

#' Calculate x-matrix from a standard formula
#' @inheritParams smm
#' @param skip a vector of variable names to skip, or `NULL` to keep all
#'   (default `NULL`).
#' @noRd
x_from_formula <- function(formula, data, skip = NULL) {
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
  if (grepl("formula", object$call_type) & new_bags[1] == "bag_name" & length(new_bags) == 1) {
    new_bags <- object$bag_name
  }
  if (length(new_bags) == 1 & new_bags[1] %in% colnames(new_data)) {
    bags <- new_data[[new_bags]]
  } else {
    bags <- new_bags
  }
}

#' Get new_x for prediction function
#'
#' Used in `misvm()`, `omisvm()`
#' @inheritParams predict.misvm
#' @noRd
.get_new_x <- function(object, new_data) {
  method <- attr(object, "method")

  if (grepl("formula", object$call_type)) {
    new_x <- x_from_mi_formula(object$formula, new_data)
    new_x <- new_x[, object$features, drop = FALSE]
  } else {
    new_x <- new_data[, object$features, drop = FALSE]
  }
  if ("kfm_fit" %in% names(object)) {
    new_x <- build_fm(object$kfm_fit, as.matrix(new_x))
  }
  if (method == "qp-heuristic" & "x_scale" %in% names(object)) {
    new_x <- as.data.frame(scale(new_x, center = object$x_scale$center, scale = object$x_scale$scale))
  }
  new_x
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
  unique_bag_label <- function(x) { unique(x$bag_label) }
  unique_instance_name <- function(x) { unique(x$instance_name) }
  select_useful_inst <- function(label, name) { if (label == 1) {name[1]} else {name}}

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
  # bags_for_train <- which(foldid != fold)
  # ind <- bags %in% unique(bags)[bags_for_train]
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
