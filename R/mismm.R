new_mismm <- function(x = list(), method = c("heuristic", "mip", "qp-heuristic")) {
    stopifnot(is.list(x))
    method <- match.arg(method)
    structure(
        x,
        class = "mismm",
        method = method
    )
}

validate_mismm <- function(x) {
    message("No validations currently in place for object of class 'mismm'.")
    x
}

#' Fit MILD-SVM model to the data
#'
#' This function fits the MILD-SVM model, which takes a multiple-instance
#' learning with distributions (MILD) data set and fits a modified SVM to it.
#' The MILD-SVM methodology is based on research in progress.
#'
#' Several choices of fitting algorithm are available, including a version of
#' the heuristic algorithm proposed by Andrews et al. (2003) and a novel
#' algorithm that explicitly solves the mixed-integer programming (MIP) problem
#' using the gurobi package optimization back-end.
#'
#' @inheritParams misvm
#' @param instances A vector specifying which samples belong to each instance.
#'   Can be a string, numeric, of factor.
#' @param method The algorithm to use in fitting (default `'heuristic'`).  When
#'   `method = 'heuristic'`, the algorithm iterates between selecting positive
#'   witnesses and solving an underlying [smm()] problem.  When `method =
#'   'mip'`, the novel MIP method will be used.  When `method = 'qp-heuristic'`,
#'   the heuristic algorithm is computed using a slightly modified dual SMM.
#'   See details
#' @param formula A formula with specification `mild(y, bags, instances) ~ x`
#'   which uses the `mild` function to create the bag-instance structure. This
#'   argument is an alternative to the `x, y, bags, instances ` arguments, but
#'   requires the `data` argument. See examples.
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   * `kernel` either a character the describes the kernel ('linear' or
#'   'radial') or a kernel matrix at the instance level.
#'   * `sigma` argument needed for radial basis kernel.
#'   * `nystrom_args` a list of parameters to pass to [kfm_nystrom()]. This is
#'   used when `method = 'mip'` and `kernel = 'radial'` to generate a Nystrom
#'   approximation of the kernel features.
#'   * `max_step` argument used when `method = 'heuristic'`. Maximum steps of
#'   iteration for the heuristic algorithm.
#'   * `scale` argument used for all methods. A logical for whether to rescale
#'   the input before fitting.
#'   * `verbose` argument used when `method = 'mip'`. Whether to message output
#'   to the console.
#'   * `time_limit` argument used when `method = 'mip'`. `FALSE`, or a time
#'   limit (in seconds) passed to `gurobi()` parameters.  If `FALSE`, no time
#'   limit is given.
#'   * `start` argument used when `method = 'mip'`.  If `TRUE`, the mip program
#'   will be warm_started with the solution from `method = 'qp-heuristic'` to
#'   potentially improve speed.
#'
#' @return An object of class `mismm`  The object contains at least the
#'   following components:
#'   * `*_fit`: A fit object depending on the `method` parameter.  If `method =
#'   'heuristic'`, this will be a `ksvm` fit from the kernlab package.  If
#'   `method = 'mip'` this will be `gurobi_fit` from a model optimization.
#'   * `call_type`: A character indicating which method `misvm()` was called
#'   with.
#'   * `x`: The training data needed for computing the kernel matrix in
#'   prediction.
#'   * `features`: The names of features used in training.
#'   * `levels`: The levels of `y` that are recorded for future prediction.
#'   * `cost`: The cost parameter from function inputs.
#'   * `weights`: The calculated weights on the `cost` parameter.
#'   * `sigma`: The radial basis function kernel parameter.
#'   * `repr_inst`: The instances from positive bags that are selected to be
#'   most representative of the positive instances.
#'   * `n_step`: If `method %in% c('heuristic', 'qp-heuristic')`, the total
#'   steps used in the heuristic algorithm.
#'   * `useful_inst_idx`: The instances that were selected to represent the bags
#'    in the heuristic fitting.
#'   * `inst_order`: A character vector that is used to modify the ordering of
#'   input data.
#'   * `x_scale`: If `scale = TRUE`, the scaling parameters for new predictions.
#'
#' @seealso [predict.mismm()] for prediction on new data.
#'
#' @examples
#' set.seed(8)
#' mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
#'                              sd_of_mean = rep(0.1, 3))
#'
#' # Heuristic method
#' mdl1 <- mismm(mil_data)
#' mdl2 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data)
#'
#' # MIP method
#' if (require(gurobi)) {
#'   mdl3 <- mismm(mil_data, method = "mip", control = list(nystrom_args = list(m = 10, r = 10)))
#'   predict(mdl3, mil_data)
#' }
#'
#' predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")
#'
#' # summarize predictions at the bag layer
#' library(dplyr)
#' mil_data %>%
#'   bind_cols(predict(mdl2, mil_data, type = "class")) %>%
#'   bind_cols(predict(mdl2, mil_data, type = "raw")) %>%
#'   distinct(bag_name, bag_label, .pred_class, .pred)
#'
#'
#' @author Sean Kent, Yifei Liu
#' @name mismm
NULL

#' @export
mismm <- function(x, ...) {
    UseMethod("mismm")
}

#' @describeIn mismm Method for data.frame-like objects
#' @export
mismm.default <- function(x, y, bags, instances, cost = 1,
                            method = c("heuristic", "mip", "qp-heuristic"),
                            weights = TRUE,
                            control = list(kernel = "radial",
                                           sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                                           nystrom_args = list(m = nrow(x), r = nrow(x), sampling = "random"),
                                           max_step = 500,
                                           scale = TRUE,
                                           verbose = FALSE,
                                           time_limit = 60,
                                           start = FALSE),
                            ...)
{
    method <- match.arg(method, c("heuristic", "mip", "qp-heuristic"))

    defaults <- list(
      kernel = "radial",
      sigma = if (is.vector(x)) 1 else 1 / ncol(x),
      nystrom_args = list(m = nrow(x), r = nrow(x), sampling = "random"),
      max_step = 500,
      verbose = FALSE,
      time_limit = 60,
      start = FALSE
    )
    control <- .set_default(control, defaults)
    if ("scale" %ni% names(control) && inherits(control$kernel, "matrix")) {
        # if kernel matrix is passed in, then really no re-scaling was done.
        control$scale <- FALSE
    } else if ("scale" %ni% names(control)) {
        control$scale <- TRUE
    }

    # store the levels of y and convert to 0,1 numeric format.
    y_info <- convert_y(y)
    y <- y_info$y
    lev <- y_info$lev

    # store column names of x
    col_x <- colnames(x)
    x <- as.data.frame(x)

    # weights
    if (is.numeric(weights)) {
        stopifnot(names(weights) == lev | names(weights) == rev(lev))
        weights <- weights[lev]
        names(weights) <- c("-1", "1")
    } else if (weights) {
        bag_labels <- sapply(split(y, factor(bags)), unique)
        inst_labels <- sapply(split(y, factor(instances)), unique)
        weights <- c("-1" = sum(bag_labels == 1) / sum(inst_labels == 0), "1" = 1)
    } else {
        weights <- NULL
    }

    # kernel
    is_matrix_kernel <- is.matrix(control$kernel)
    n_instances <- length(unique(instances))
    if (all(control$kernel != "radial") && !is_matrix_kernel) {
        warning("control$kernel must either be 'radial' or a square matrix.  Defaulting to 'radial'.")
        control$kernel <- "radial"
    } else if (method == "mip" && is_matrix_kernel) {
        warning("Cannot pass matrix to control$kernel when method = 'mip'. Defaulting to 'radial'.")
        control$kernel <- "radial"
    } else if (is_matrix_kernel) {
        if (all(dim(control$kernel) != c(n_instances, n_instances))) {
            warning("Matrix passed to control$kernel is not of correct size. Defaulting to 'radial'.")
            control$kernel <- "radial"
        }
    }
    if (method == "qp-heuristic") {
        if (all(control$kernel == "radial")) {
            control$kernel <- kme(df = data.frame(instance_name = instances, x), sigma = control$sigma)
        }
        stopifnot(is.matrix(control$kernel))
    }

    if (method == "mip" && control$kernel == "radial") {
        # Nystrom approximation to x for mip and qp-heuristic methods
        control$nystrom_args$df <- x
        control$nystrom_args$kernel <- control$kernel
        control$nystrom_args$sigma <- control$sigma
        kfm_fit <- do.call(kfm_nystrom.default, args = control$nystrom_args)

        x <- build_fm(kfm_fit, x)
        x <- average_over_instances(x, instances)
    }

    if (method == "heuristic") {

        if (control$scale) {
            x <- scale(x)
            center <- attr(x, "scaled:center")
            scale <- attr(x, "scaled:scale")
            x <- as.data.frame(x)
        }
        y = 2*y - 1 # convert {0,1} to {-1, 1}

        r <- .reorder(y, bags, x, instances)

        data <- mild_df(bag_label = y[r$order],
                        bag_name = bags[r$order],
                        instance_name = instances[r$order],
                        x[r$order, , drop = FALSE])
        data <- dplyr::arrange(data, .data$bag_label, .data$bag_name, .data$instance_name)

        inst_order <- match(unique(instances[r$order]), unique(instances))
        if (is.matrix(control$kernel)) {
            control$kernel <- control$kernel[inst_order, inst_order, drop = FALSE]
        }

        res <- mil_distribution(data,
                                cost = cost,
                                weights = weights,
                                kernel = control$kernel,
                                max.step = control$max_step,
                                sigma = control$sigma)

    } else if (method == "mip") {

        y <- classify_bags(y, instances)
        bags <- classify_bags(bags, instances)

        stopifnot(length(y) == nrow(x))
        stopifnot(length(bags) == nrow(x))

        y <- 2*y - 1 # convert {0, 1} to {-1, 1}
        res <- misvm_mip_fit(y, bags, x,
                             c = cost,
                             rescale = control$scale,
                             weights = weights,
                             verbose = control$verbose,
                             time_limit = control$time_limit,
                             start = control$start)

    } else if (method == "qp-heuristic") {
        y <- classify_bags(y, instances)
        bags <- classify_bags(bags, instances)
        ind <- sapply(unique(instances), function(i) min(which(instances == i)))

        y = 2*y - 1 # convert {0,1} to {-1, 1}
        res <- misvm_dualqpheuristic_fit(y, bags, x[ind, , drop = FALSE],
                                         c = cost,
                                         rescale = control$scale,
                                         weights = weights,
                                         kernel = control$kernel,
                                         sigma = control$sigma,
                                         verbose = control$verbose,
                                         time_limit = control$time_limit,
                                         max_step = control$max_step)
        res$x <- x # need the full x matrix for future kernel computations
        res$gurobi_fit$instances <- instances
        res$gurobi_fit$xmatrix <- NULL # remove for space
        res$gurobi_fit$kernel <- NULL # remove for space
    } else {
        stop("misvm requires method = 'heuristic', 'mip', or 'qp-heuristic'.")
    }

    out <- res[1]
    if (method %in% c("mip") && all(control$kernel == "radial")) {
        out$kfm_fit <- kfm_fit
    }
    out$call_type <- "mismm.default"
    out$x <- res$x
    out$features <- col_x
    out$levels <- lev
    out$cost <- cost
    out$sigma <- control$sigma
    out$weights <- weights
    out$repr_inst <- res$repr_inst
    out$n_step <- res$n_step
    out$useful_inst_idx <- res$useful_inst_idx
    if (method == "heuristic") {
        out$inst_order <- inst_order
    }
    if (control$scale & method == "heuristic") {
        out$x_scale <- list("center" = center, "scale" = scale)
    } else {
        out$x_scale <- res$x_scale
    }
    new_mismm(out, method = method)
}

#' @describeIn mismm Method for passing formula
#' @export
mismm.formula <- function(formula, data, ...) {
    # NOTE: other 'professional' functions use a different type of call that I
    #   couldn't get to work. See https://github.com/therneau/survival/blob/master/R/survfit.R
    #   or https://github.com/cran/e1071/blob/master/R/svm.R
    #   right now we're using something that should work for most generic formulas

    mi_names <- as.character(stats::terms(formula, data = data)[[2]])
    bag_name <- mi_names[[3]]
    instance_name <- mi_names[[4]]

    x <- x_from_mild_formula(formula, data)
    response <- stats::get_all_vars(formula, data = data)
    y <- response[, 1]
    bags <- response[, 2]
    instances <- response[, 3]

    res <- mismm.default(x, y, bags, instances, ...)

    res$call_type <- "mismm.formula"
    res$formula <- formula
    res$bag_name <- bag_name
    res$instance_name <- instance_name
    return(res)
}


#' @describeIn mismm Method for `mild_df` objects
#' @export
mismm.mild_df <- function(data, ...) {

    # x <- as.data.frame(subset(data, select = -c(bag_label, bag_name, instance_name)))
    x <- data
    x$bag_label <- x$bag_name <- x$instance_name <- NULL
    y <- data$bag_label
    bags <- data$bag_name
    instances <- data$instance_name

    res <- mismm.default(x, y, bags, instances, ...)
    res$call_type <- "mismm.mild_df"
    # res$formula <- formula
    res$bag_name <- "bag_name"
    res$instance_name <- "instance_name"
    return(res)
}

#' Predict method for `mismm` object
#'
#' @details
#' When the object was fitted using the `formula` method, then the parameters
#' `new_bags` and `new_instances` are not necessary, as long as the names match
#' the original function call.
#'
#' @inheritParams predict.misvm
#' @param object An object of class `mismm`.
#' @param new_instances A character or character vector.  Can specify a singular
#'   character that provides the column name for the instance names in
#'   `new_data` (default `'instance_name'`).  Can also specify a vector of length
#'   `nrow(new_data)` that has instance name for each row.
#' @param kernel An optional pre-computed kernel matrix at the instance level or
#'   `NULL` (default `NULL`). The rows should correspond to instances in the new
#'   data to predict, and columns should correspond to instances in the original
#'   training data, such as a call to [kme()].
#'
#' @return A tibble with `nrow(new_data)` rows.  If `type = 'class'`, the tibble
#'   will have a column `.pred_class`.  If `type = 'raw'`, the tibble will have
#'   a column `.pred`.
#'
#' @seealso [mismm()] for fitting the `mismm` object.
#'
#' @examples
#' mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
#'                              sd_of_mean = rep(0.1, 3))
#'
#' mdl1 <- mismm(mil_data, control = list(sigma = 1/5))
#'
#' # bag level predictions
#' library(dplyr)
#' mil_data %>%
#'     bind_cols(predict(mdl1, mil_data, type = "class")) %>%
#'     bind_cols(predict(mdl1, mil_data, type = "raw")) %>%
#'     distinct(bag_name, bag_label, .pred_class, .pred)
#'
#' # instance level prediction
#' mil_data %>%
#'     bind_cols(predict(mdl1, mil_data, type = "class", layer = "instance")) %>%
#'     bind_cols(predict(mdl1, mil_data, type = "raw", layer = "instance")) %>%
#'     distinct(bag_name, instance_name, bag_label, .pred_class, .pred)
#'
#' @export
#' @author Sean Kent
predict.mismm <- function(object, new_data,
                            type = c("class", "raw"),
                            layer = c("bag", "instance"),
                            new_bags = "bag_name",
                            new_instances = "instance_name",
                            kernel = NULL,
                            ...)
{
    type <- match.arg(type)
    layer <- match.arg(layer)
    method <- attr(object, "method")

    if (method == "heuristic") {
        # pass on to the predict.smm method
        object$call_type <- gsub("mismm", "smm", object$call_type)
        if (!is.null(kernel)) {
            kernel <- kernel[, object$inst_order, drop = FALSE]
            kernel <- kernel[, object$useful_inst_idx, drop = FALSE]
        }
        return(predict.smm(object, new_data, type, layer, new_instances, new_bags, kernel, ...))
    }

    # Find the instance information
    if (object$call_type == "mismm.formula" & new_instances[1] == "instance_name" & length(new_instances) == 1) {
        new_instances <- object$instance_name
    }
    if (length(new_instances) == 1 & new_instances[1] %in% colnames(new_data)) {
        instances <- new_data[[new_instances]]
    } else {
        instances <- new_instances
    }

    if (object$call_type == "mismm.formula") {
        new_x <- x_from_mild_formula(object$formula, new_data)
    } else {
        new_x <- new_data[, object$features, drop = FALSE]
    }
    if ("kfm_fit" %in% names(object)) {
        new_x <- build_fm(object$kfm_fit, as.matrix(new_x))
        new_x <- average_over_instances(new_x, instances)
    }
    if (method == "qp-heuristic") {
        old_instances <- object$gurobi_fit$instances
        used_instance_names <- unique(old_instances)[object$repr_inst == 1]
        ind <- which(old_instances %in% used_instance_names)

        if (is.null(kernel)) {
            traindata <- data.frame(instance_name = old_instances[ind], object$x[ind, , drop = FALSE])
            kernel <- kme(data.frame(instance_name = instances, new_x),
                          traindata,
                          sigma = object$gurobi_fit$sigma)
        } else {
            kernel <- kernel[, object$repr_inst == 1]
        }
        colnames(kernel) <- unique(old_instances[ind])
    }

    if (method == "mip") {
        # these scores are at the instance level
        scores <- as.matrix(new_x) %*% object$gurobi_fit$w + object$gurobi_fit$b
        # map scores back to the sample level to match nrow(new_data)
        scores <- sapply(instances, function(i) scores[which(rownames(scores) == i)])
    } else if (method == "qp-heuristic") {
        ay_order <- names(object$gurobi_fit$ay)
        scores <- kernel[, ay_order] %*% object$gurobi_fit$ay + object$gurobi_fit$b
        scores <- as.numeric(scores)
        names(scores) <- unique(instances)
        scores <- scores[instances]
    } else {
        stop("predict.mismm requires method = 'heuristic', 'mip', 'qp-heuristic'.")
    }
    pos <- 2*(scores > 0) - 1

    if (layer == "bag") {
        if (object$call_type == "mismm.formula" & new_bags[1] == "bag_name" & length(new_bags) == 1) {
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
    pos <- factor(pos, levels = c(-1, 1), labels = object$levels)

    res <- switch(
        type,
        "raw" = tibble::tibble(.pred = as.numeric(scores)),
        "class" = tibble::tibble(.pred_class = pos)
    )

    # TODO: consider returning the AUC here as an attribute.  Can only do if we have the true bag labels
    # attr(res, "AUC") <- calculated_auc
    attr(res, "layer") <- layer
    return(res)
}

# Specific implementation methods below ----------------------------------------

#' Function to perform the SMM iteration using full Gram matrix.
#'
#' Internal function to perform SMM iteration using full Gram matrix.
#' @param kernel_full The full Gram matrix, should be of length n_inst * n_inst.
#' @param data_info the instance level data information which is a data.frame
#'   with 3 columns, 'bag_label', 'bag_name' and 'instance_name'
#' @param max.step maximum iteration steps
#' @param cost the cost used in SMM
#' @param weights Weights of each class
#' @param sigma the rbf kernel parameter
#' @param yy the response at instance level.
#' @param useful_inst_idx a vector specifying which indices are of use.
#' @return A list with several entries.
#'
#' @author Yifei Liu
#' @noRd
kernel_mil <- function(kernel_full, data_info, max.step, cost, weights,
    sigma, yy, useful_inst_idx) {

    # data_info is at instance_level
    bag_name <- data_info$bag_name
    bag_label <- data_info$bag_label
    positive_bag_name <- unique(bag_name[bag_label == 1])
    unique_bag_name <- unique(bag_name)
    n_bag <- length(unique_bag_name)  # total number of bags

    len_y <- length(yy)

    selection <- rep(0, length(positive_bag_name))  # this records which instance is selected in which bag
    past_selection <- matrix(NA, length(positive_bag_name), max.step)  # this is the history of past selection.
    past_selection[, 1] <- selection

    yy_inst <- yy[useful_inst_idx]
    step <- 1
    while (step < max.step) {
        smm_model <- smm(x = seq_along(yy_inst),
                         y = yy_inst,
                         instances = seq_along(yy_inst),
                         cost = cost,
                         weights = weights,
                         control = list(kernel = kernel_full[useful_inst_idx, useful_inst_idx, drop = FALSE],
                                        sigma = sigma,
                                        scale = FALSE))

        pred_all_score <- predict(smm_model,
                                  type = "raw",
                                  new_data = NULL,
                                  new_instances = seq_len(nrow(kernel_full)),
                                  kernel = kernel_full[, useful_inst_idx, drop = FALSE])
        pred_all_score <- pred_all_score$.pred

        # update sample
        last_inst_idx <- 0
        pos_idx <- 1
        useful_inst_idx <- NULL  # the same as the previous useful_inst_idx

        for (i in 1:n_bag) {
            data_i <- data_info[bag_name == unique_bag_name[i], , drop = FALSE]
            n_inst_i <- nrow(data_i)  # total number of instances

            if (data_i$bag_label[1] == -1) {
                # negative bag
                useful_inst_idx <- c(useful_inst_idx, (last_inst_idx +
                  1):(last_inst_idx + n_inst_i))
            } else {
                id_max <- which.max(pred_all_score[(last_inst_idx + 1):(last_inst_idx +
                  n_inst_i)])
                selection[pos_idx] <- id_max
                useful_inst_idx <- c(useful_inst_idx, last_inst_idx +
                  id_max)
                pos_idx <- pos_idx + 1
            }
            last_inst_idx <- last_inst_idx + n_inst_i
        }

        # if the selection is not changed, break.
        difference <- sum(past_selection[, step] != selection)
        repeat_selection <- 0
        if (difference == 0)
            break

        # if the current selection is the same as a previous one, break.
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

    smm_model$useful_inst_idx <- useful_inst_idx
    smm_model$n_step <- step
    smm_model$selection <- selection
    smm_model$difference <- difference
    smm_model$repeat_selection <- repeat_selection
    smm_model$n_bag <- n_bag
    return(smm_model)
}

#' Function to implement the iterative multiple instance learning with
#' distributional data algorithm.
#'
#' Workhorse for the package. This implements the algorithm that iteratively
#' find the representative positive instances and use smm to get the model.
#' @param data A mild_df object, potentially generated by mild_df().
#' @param cost The cost for smm.
#' @param max.step The total number of iterations.
#' @param sigma The parameter for the rbf kernel.
#' @param kernel either 'radial', or the Gram matrix at instance level for fast
#'   computation
#' @return A mild object which contains the results.
#' @examples
#' mild_df1 <- generate_mild_df(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 10,
#'                             positive_degree = 3
#'                            )
#' foo <- mil_distribution(data = mild_df1, cost = 1) # uses about 10 seconds.
#' @author Yifei Liu
#' @noRd
mil_distribution <- function(data, cost, weights, max.step = 500, sigma = 0.05, kernel = "radial") {
    # data should be of a mild_df object.  bag_label should be one of '0'
    # and '1', where '0' is negative bags and '1' is positive bags

    # divide the bags to positive bags and negative bags

    bag_name <- data$bag_name
    bag_label <- data$bag_label
    instance_name <- unique(data$instance_name)
    if (length(unique(bag_label)) == 1)
        stop("Only one class label, cannot perform classification!")

    positive_bag_name <- unique(bag_name[bag_label == 1])
    negative_bag_name <- unique(bag_name[bag_label == 0])
    unique_bag_name <- unique(bag_name)
    n_bag <- length(unique_bag_name)  # total number of bags

    if (all(kernel == "radial")) {
        kernel <- kme(df = data, sigma = sigma)
    }
    stopifnot(inherits(kernel, "matrix"))

    # initialize the feature
    instance_selection <- initialize_instance_selection(data)
    useful_inst_idx = instance_selection[["useful_inst_idx"]]
    yy = instance_selection[["yy"]]

    num_neg_inst <- length(useful_inst_idx) - length(positive_bag_name)  # calculate the number of negative instances.

    data_info <- unique(data[, c("bag_label", "bag_name", "instance_name"), drop = FALSE])
    res <- kernel_mil(kernel, data_info, max.step, cost, weights,
        sigma, yy, useful_inst_idx)

    sample_df <- data[data$instance_name %in% instance_name[res$useful_inst_idx],
        -c(1, 2), drop = FALSE]

    res$x <- sample_df
    res$repr_inst <- cbind(positive_bag_name, res$selection)
    return(res)
}
