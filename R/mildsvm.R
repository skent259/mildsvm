new_mildsvm <- function(x = list(), method = c("heuristic", "mip")) {
    stopifnot(is.list(x))
    method <- match.arg(method)
    structure(
        x,
        class = "mildsvm",
        method = method
    )
}

validate_mildsvm <- function(x) {
    message("No validations currently in place for object of class 'mildsvm'.")
    x
}

#' Fit MILD-SVM model to the data
#'
#' This function fits the MILD-SVM model, which takes a multiple-instance
#' learning with distributions (MILD) dataset and fits a modified SVM to it.  A
#' core feature of MILD data is that we have multiple levels: bags which contain
#' instances, instances which we think of as distributions with samples, and
#' finally samples that comprise the rows of the data. The MILD-SVM methodology
#' is based on research in progress. Several choices of fitting algorithm are
#' available, including a version of the heuristic algorithm proposed by Andrews
#' et al. (2003) and a novel algorithm that explicitly solves the mixed-integer
#' programming (MIP) problem using the `gurobi` optimization back-end.
#'
#' @param x a data.frame, matrix, or similar object of covariates, where each
#'   row represents a sample.
#' @param y a numeric, character, or factor vector of bag labels for each
#'   instance.  Must satisfy `length(y) == nrow(x)`. Suggest that one of the
#'   levels is 1, '1', or TRUE, which becomes the positive class;
#'   otherwise, a positive class is chosen and a message will be supplied.
#' @param bags a vector specifying which instance belongs to each bag.  Can be a
#'   string, numeric, of factor.
#' @param instances a vector specifying which samples belong to each instance.
#'   Can be a string, numeric, of factor.
#' @param formula a formula with specification `mild(y, bags, instances) ~ x`
#'   which uses the `mild` function to create the bag-instance structure. This
#'   argument is an alternative to the `x, y, bags, instances ` arguments, but
#'   requires the `data` argument. See examples.
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted.  Otherwise, a 'MilData' object from
#'   which `x, y, bags, instances` are automatically extracted. If a 'MilData'
#'   object is used, all columns will be used as predictors.
#' @param cost The cost parameter in SVM. If `method` = 'heuristic', this will
#'   be fed to `kernlab::ksvm`, otherwise it is similarly in internal functions.
#' @param method MILD-SVM algorithm to use in fitting; default is 'heuristic',
#'   which employs an algorithm similar to Andrews et al. (2003). When `method`
#'   = 'mip', the novel MIP method will be used.  See details.
#' @param weights named vector, or TRUE, to control the weight of the cost
#'   parameter for each possible y value.  Weights multiply against the cost
#'   vector. If TRUE, weights are calculated based on inverse counts of
#'   instances with given label, where we only count one positive instance per
#'   bag. Otherwise, names must match the levels of `y`.
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   - `kernel` either a character the describes the kernel ('linear' or
#'   'radial') or a kernel matrix at the instance level.
#'   - `sigma` argument needed for radial basis kernel.
#'   - `nystrom_args` a list of parameters to pass to `kfm_nystrom` function,
#'   used when `method` = 'mip' and `kernel` = 'radial' to generate a nystrom
#'   approximation of the kernel features.
#'   - `max_step` argument used when `method` = 'heuristic'. Maximum steps of
#'   iteration for the heuristic algorithm.
#'   - `scale` argument used for all methods. Logical; whether to rescale the
#'   input before fitting.
#'   - `verbose` argument used when `method` = 'mip'. Whether to message output
#'   to the console.
#'   - `time_limit` argument used when `method` = 'mip'. FALSE, or a time limit
#'   (in seconds) passed to `gurobi` parameters.  If FALSE, no time limit is
#'   given.
#'   - `start` argument used when `method` = 'mip'.  If TRUE, the mip program
#'   will be warm_started with the solution from `method` = 'qp-heuristic' to
#'   potentially improve speed.
#'
#' @return an object of class 'mildsvm'.  The object contains the following
#'   components, if applicable:
#'   - `model`: a model that will depend on the method used to fit.
#'   It holds the main model components used for prediction.  If the model is
#'   fit with method = 'heuristic', this object is of class 'smm'.
#'   - `total_step`: the number of steps used in the heuristic algorithm, if
#'   applicable.
#'   - `representative_inst`: instances from positive bags that
#'   are selected to be most representative of the positive instance.
#'   - `traindata`: training data from the underlying fitting.  This data will
#'   get used when computing the kernel matrix for prediction.
#'   - `useful_inst_idx`: The instances that were selected to represent the bags
#'    in the heuristic fitting.
#'   - `features`: the features used for prediction.
#'   - `call_type`: the call type, which specifies whether `mildsvm()`
#'   was called via the formula, data.frame, of MilData method.
#'   - `levels`: levels of `y` that are recorded for future prediction.
#'   - `bag_name`: the name of the column used for bags, if the formula or
#'   MilData method is applied.
#'   - `instance_name`: the name of the column used for instances, if the
#'   formula or MilData method is applied.
#'   - `kfm_fit`: the fit from building nystrom features, if method = 'mip' and
#'   kernel = 'radial'.  This is used for prediction.
#'   - `center`: values used to center x, if `scale` = TRUE.
#'   - `scale`: values used to scale x, if `scale` = TRUE.
#'
#' @examples
#' set.seed(8)
#' mil_data <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 15,
#'                             positive_degree = 3,
#'                             nsample = 20
#' )
#' # Heuristic method
#' mdl1 <- mildsvm(mil_data)
#' mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3., data = MilData)
#'
#' if (require(gurobi)) {
#'   foo <- mildsvm(mil_data, method = "mip", control = list(nystrom_args = list(m = 10, r = 10)))
#'   predict(foo, mil_data)
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
#' @name mildsvm
NULL

#' @export
mildsvm <- function(x, y, bags, instances, ...) {
    UseMethod("mildsvm")
}

#' @describeIn mildsvm Method for passing formula
#' @export
mildsvm.formula <- function(formula, data, cost = 1,
                            method = c("heuristic", "mip"),
                            weights = TRUE,
                            control = list(kernel = "radial",
                                           sigma = 1,
                                           nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random'),
                                           max_step = 500,
                                           scale = TRUE,
                                           verbose = FALSE,
                                           time_limit = 60,
                                           start = FALSE)) {
    # NOTE: other 'professional' functions use a different type of call that I
    #   couldn't get to work. See https://github.com/therneau/survival/blob/master/R/survfit.R
    #   or https://github.com/cran/e1071/blob/master/R/svm.R
    #   right now we're using something that should work for most generic formulas
    method <- match.arg(method)

    mi_names <- as.character(stats::terms(formula, data = data)[[2]])
    bag_name <- mi_names[[3]]
    instance_name <- mi_names[[4]]

    x <- x_from_mild_formula(formula, data)
    response <- stats::get_all_vars(formula, data = data)
    y <- response[, 1]
    bags <- response[, 2]
    instances <- response[, 3]

    res <- mildsvm.default(x, y, bags, instances, cost = cost, method = method, weights = weights, control = control)

    res$call_type <- "mildsvm.formula"
    res$formula <- formula
    res$bag_name <- bag_name
    res$instance_name <- instance_name
    return(res)
}

#' @describeIn mildsvm Method for data.frame-like objects
#' @export
mildsvm.default <- function(x, y, bags, instances, cost = 1,
                            method = c("heuristic", "mip"),
                            weights = TRUE,
                            control = list(kernel = "radial",
                                           sigma = 1,
                                           nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random'),
                                           max_step = 500,
                                           scale = TRUE,
                                           verbose = FALSE,
                                           time_limit = 60,
                                           start = FALSE)) {

    method <- match.arg(method)
    if ("kernel" %ni% names(control)) control$kernel <- "radial"
    if ("sigma" %ni% names(control)) control$sigma <- 1
    if ("nystrom_args" %ni% names(control)) control$nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random')
    if ("max_step" %ni% names(control)) control$max_step <- 500
    if ("scale" %ni% names(control) && inherits(control$kernel, "matrix")) {
        # if kernel matrix is passed in, then really no re-scaling was done.
        control$scale <- FALSE
    } else if ("scale" %ni% names(control)) {
        control$scale <- TRUE
    }
    if ("verbose" %ni% names(control)) control$verbose <- FALSE
    if ("time_limit" %ni% names(control)) control$time_limit <- 60
    if ("start" %ni% names(control)) control$start <- FALSE

    # store the levels of y and convert to 0,1 numeric format.
    y_info <- convert_y(y)
    y <- y_info$y
    lev <- y_info$lev

    # store column names of x
    col_x <- colnames(x)
    x <- as.data.frame(x)

    ## weights
    if (is.numeric(weights)) {
        stopifnot(names(weights) == lev | names(weights) == rev(lev))
        weights <- weights[lev]
        names(weights) <- c("-1", "1")
    } else if (weights) {
        bag_labels <- sapply(split(y, factor(bags)), unique)
        weights <- c("-1" = sum(bag_labels == 1) / sum(y == 0), "1" = 1)
    } else {
        weights <- NULL
    }

    ## kernel
    is_matrix_kernel <- inherits(control$kernel, "matrix")
    n_instances <- length(unique(instances))
    if (control$kernel != "radial" && !is_matrix_kernel) {
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

    if (method == "mip" && control$kernel == "radial") {
        ## Nystrom approximation to x for mip and qp-heuristic methods
        control$nystrom_args$df <- x
        control$nystrom_args$kernel <- "rbf"
        control$nystrom_args$sigma <- control$sigma
        kfm_fit <- do.call(kfm_nystrom.default, args = control$nystrom_args)

        x <- predict_kfm_nystrom.default(kfm_fit, x)
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

        data <- MilData(cbind(bag_label = y[r$order],
                              bag_name = bags[r$order],
                              instance_name = instances[r$order],
                              x[r$order, , drop = FALSE]))
        data <- dplyr::arrange(data, bag_label, bag_name, instance_name)

        inst_order <- match(unique(instances[r$order]), unique(instances))
        if (is.matrix(control$kernel)) {
            control$kernel <- control$kernel[inst_order, inst_order]
        }
        
        res <- mil_distribution(data,
                                cost = cost,
                                weights = weights,
                                kernel = control$kernel,
                                max.step = control$max_step,
                                sigma = control$sigma)
        res$model$features <- col_x
        res$model$traindata <- res$traindata
        res$model$inst_order <- inst_order

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

    } else {
        stop("mildsvm requires method = 'heuristic', or 'mip'.")
    }

    res$features <- col_x
    res$call_type <- "mildsvm.default"
    res$bag_name <- NULL
    res$instance_name <- NULL
    res$levels <- lev
    if (method %in% c("mip") && control$kernel == "radial") {
        res$kfm_fit <- kfm_fit
    }
    if (control$scale & method == "heuristic") {
        res$center <- center
        res$scale <- scale
    }
    new_mildsvm(res, method = method)
}

#' @describeIn mildsvm Method for MilData objects
#' @export
mildsvm.MilData <- function(data, cost = 1,
                            method = c("heuristic", "mip"),
                            weights = TRUE,
                            control = list(kernel = "radial",
                                           sigma = 1,
                                           nystrom_args = list(m = nrow(x), r = nrow(x), sampling = 'random'),
                                           max_step = 500,
                                           scale = TRUE,
                                           verbose = FALSE,
                                           time_limit = 60,
                                           start = FALSE))
{
    method <- match.arg(method)

    x <- as.data.frame(subset(data, select = -c(bag_label, bag_name, instance_name)))
    y <- data$bag_label
    bags <- data$bag_name
    instances <- data$instance_name

    res <- mildsvm.default(x, y, bags, instances, cost, method, weights, control)
    res$call_type <- "mildsvm.MilData"
    # res$formula <- formula
    res$bag_name <- "bag_name"
    res$instance_name <- "instance_name"
    return(res)
}

#' Predict method for 'mildsvm' object
#' @param object an object of class mildsvm
#' @param new_data matrix to predict from.  Needs to have the same number of
#'   columns as the X that trained the mildsvm object
#' @inheritParams predict.misvm
#' @param new_bags character or character vector.  Can specify a singular
#'   character that provides the column name for the bag names in `new_data`,
#'   default = "bag_name".  Can also specify a vector of length `nrow(new_data)`
#'   that has bag name for each instance.  When `object` was fitted with
#'   `mildsvm.formula`, this parameter is not necessary as the bag name can be
#'   pulled directly from new_data, if available.
#' @param new_instances character or character vector.  Can specify a singular
#'   character that provides the column name for the instance names in
#'   `new_data`, default = "instance_name".  Can also specify a vector of length
#'   `nrow(new_data)` that has instance name for each row.  When `object` was
#'   fitted with `mildsvm.formula`, this parameter is not necessary as the bag
#'   name can be pulled directly from new_data, if available.
#' @param kernel optional pre-computed kernel matrix at the instance level,
#'   default = NULL. This can be specified to speed up computations.  The rows
#'   should correspond to instances in the new data to predict, and columns
#'   should correspond to instances in the original training data.
#'
#' @return tibble with `nrow(new_data)` rows.  If type = 'class', the tibble
#'   will have a column '.pred_class'.  If type = 'raw', the tibble will have a
#'   column '.pred'.
#'
#' @examples
#' mil_data <- GenerateMilData(
#'     positive_dist = 'mvt',
#'     negative_dist = 'mvnormal',
#'     remainder_dist = 'mvnormal',
#'     nbag = 20,
#'     ncov = 5,
#'     nsample = 50,
#'     positive_degree = 3,
#'     positive_mean = rep(5, 5)
#' )
#'
#' mdl1 <- mildsvm(mil_data, control = list(sigma = 0.05))
#'
#' # bag level predictions
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
predict.mildsvm <- function(object, new_data,
                          type = c("class", "raw"), layer = c("bag", "instance"),
                          new_bags = "bag_name", new_instances = "instance_name",
                          kernel = NULL)
{
    type <- match.arg(type)
    layer <- match.arg(layer)
    method <- attr(object, "method")

    # Find the instance information
    if (object$call_type == "misvm.formula" & new_instances[1] == "instance_name" & length(new_instances) == 1) {
        new_instances <- object$instance_name
    }
    if (length(new_instances) == 1 & new_instances[1] %in% colnames(new_data)) {
        instances <- new_data[[new_instances]]
    } else {
        instances <- new_instances
    }

    if (object$call_type == "misvm.formula") {
        new_x <- x_from_mild_formula(object$formula, new_data)
    } else {
        new_x <- new_data[, object$features, drop = FALSE]
    }
    if ("kfm_fit" %in% names(object)) {
        new_x <- predict_kfm_nystrom.default(object$kfm_fit, new_x)
        new_x <- average_over_instances(new_x, instances)
    }
    if (method == "heuristic" & "center" %in% names(object)) {
        new_x <- as.data.frame(scale(new_x, center = object$center, scale = object$scale))
    }

    if (method == "heuristic") {
        new_x <- as.data.frame(new_x) # in case someone passes MilData to this...
        new_x$instance_name <- instances
        # these scores are at the instance level
        if (!is.null(kernel)) {
            kernel <- kernel[, object$model$inst_order]
            # TODO: would be good to check that matrix is of the right size here
            scores <- predict(object$model, new_data = new_x,
                              type = "raw",
                              kernel = kernel[, object$useful_inst_idx])
            scores <- scores$.pred
        } else {
            scores <- predict(object$model, new_data = new_x, type = "raw")
            scores <- scores$.pred
        }

    } else if (method == "mip") {
        # these scores are at the instance level
        scores <- as.matrix(new_x) %*% object$model$w + object$model$b
        # map scores back to the sample level to match nrow(new_data)
        scores <- sapply(instances, function(i) scores[which(rownames(scores) == i)])
    } else {
        stop("predict.mildsvm requires method = 'heuristic' or 'mip'.")
    }
    pos <- 2*(scores > 0) - 1

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
    pos <- factor(pos, levels = c(-1, 1), labels = object$levels)

    res <- switch(
        type,
        "raw" = tibble::tibble(.pred = as.numeric(scores)),
        "class" = tibble::tibble(.pred_class = pos)
    )

    # TODO: consider returning the AUC here as an attribute.  Can only do if we have the true bag labels
    # attr(res, "AUC") <- calculated_auc
    attr(res, "layer") <- layer
    res
}

# Specific implementation methods below ----------------------------------------

##' Function to perform the SMM iteration using full Gram matrix.
##'
##' Internal function to perform SMM iteration using full Gram matrix.
##' @param kernel_full The full Gram matrix, should be of length n_inst * n_inst.
##' @param data_info the instance level data information which is a data.frame with 3 columns, 'bag_label', 'bag_name' and 'instance_name'
##' @param max.step maximum iteration steps
##' @param cost the cost used in SMM
##' @param weights Weights of each class
##' @param sigma the rbf kernel parameter
##' @param yy the response at instance level.
##' @param useful_inst_idx a vector specifying which indices are of use.
##' @return A list with several entries.
##'
##' @author Yifei Liu
##' @keywords internal
kernel_mil <- function(kernel_full, data_info, max.step, cost, weights,
    sigma, yy, useful_inst_idx) {

    ## data_info is at instance_level
    bag_name <- data_info$bag_name
    bag_label <- data_info$bag_label
    positive_bag_name <- unique(bag_name[bag_label == 1])
    unique_bag_name <- unique(bag_name)
    n_bag <- length(unique_bag_name)  ## total number of bags

    len_y <- length(yy)

    selection <- rep(0, length(positive_bag_name))  ## this records which instance is selected in which bag
    past_selection <- matrix(NA, length(positive_bag_name), max.step)  ## this is the history of past selection.
    past_selection[, 1] <- selection

    yy_inst <- yy[useful_inst_idx]
    step <- 1
    while (step < max.step) {
        svm_model <- smm(x = 1:length(yy_inst),
                         y = yy_inst,
                         instances = 1:length(yy_inst),
                         cost = cost,
                         weights = weights,
                         control = list(kernel = kernel_full[useful_inst_idx, useful_inst_idx],
                                        sigma = sigma,
                                        scale = FALSE))

        pred_all_score <- predict(svm_model,
                                  type = "raw",
                                  new_data = NULL,
                                  new_instances = 1:nrow(kernel_full),
                                  kernel = kernel_full[, useful_inst_idx])
        pred_all_score <- pred_all_score$.pred

        ## update sample
        last_inst_idx <- 0
        pos_idx <- 1
        useful_inst_idx <- NULL  ## the same as the previous useful_inst_idx

        for (i in 1:n_bag) {
            data_i <- data_info[bag_name == unique_bag_name[i], ]
            n_inst_i <- nrow(data_i)  ## total number of instances

            if (data_i$bag_label[1] == -1) {
                ## negative bag
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

        ## if the selection is not changed, break.
        difference = sum(past_selection[, step] != selection)
        repeat_selection <- 0
        if (difference == 0)
            break

        ## if the current selection is the same as a previous one, break.
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

    list(
        svm_model = svm_model,
        useful_inst_idx = useful_inst_idx,
        step = step,
        selection = selection,
        difference = difference,
        repeat_selection = repeat_selection,
        n_bag = n_bag
    )
}

#' Function to implement the iterative multiple instance learning with
#' distributional data algorithm.
#'
#' Workhorse for the package. This implements the algorithm that iteratively
#' find the representative positive instances and use smm to get the model.
#' @param data A MilData object, potentially generated by MilData().
#' @param cost The cost for smm.
#' @param max.step The total number of iterations.
#' @param sigma The parameter for the rbf kernel.
#' @param kernel either 'radial', or the Gram matrix at instance level for fast
#'   computation
#' @return A mild object which contains the results.
#' @examples
#' MilData1 <- GenerateMilData(positive_dist = 'mvt',
#'                             negative_dist = 'mvnormal',
#'                             remainder_dist = 'mvnormal',
#'                             nbag = 10,
#'                             positive_degree = 3
#'                            )
#' foo <- mil_distribution(data = MilData1, cost = 1) ## uses about 10 seconds.
#' @export
#' @author Yifei Liu
#' @keywords internal
mil_distribution <- function(data, cost, weights, max.step = 500, sigma = 0.05, kernel = "radial") {
    ## data should be of a MilData object.  bag_label should be one of '0'
    ## and '1', where '0' is negative bags and '1' is positive bags

    ## divide the bags to positive bags and negative bags

    bag_name <- data$bag_name
    bag_label <- data$bag_label
    instance_name <- unique(data$instance_name)
    if (length(unique(bag_label)) == 1)
        stop("Only one class label, cannot perform classification!")

    positive_bag_name <- unique(bag_name[bag_label == 1])
    negative_bag_name <- unique(bag_name[bag_label == 0])
    unique_bag_name <- unique(bag_name)
    n_bag <- length(unique_bag_name)  ## total number of bags

    ## Calculate the full kernel matrix, kernel_full is a
    ## length(instance_name) by length(instance_name) matrix.
    # if (is.null(list(...)$kernel_full)) {
    #     kernel_full <- kme(df = data, sigma = sigma)
    # } else {
    #     kernel_full <- list(...)$kernel_full
    # }
    if (all(kernel == "radial")) {
        kernel <- kme(df = data, sigma = sigma)
    }
    stopifnot(inherits(kernel, "matrix"))

    ## initialize the feature
    instance_selection <- initialize_instance_selection(data)
    useful_inst_idx = instance_selection[["useful_inst_idx"]]
    yy = instance_selection[["yy"]]

    num_neg_inst <- length(useful_inst_idx) - length(positive_bag_name)  ## calculate the number of negative instances.
    # weights <- c(length(positive_bag_name)/num_neg_inst, 1)  ## this is less affected by the total number of bags.
    # names(weights) <- c("0", "1")

    # ## iterate between updating the model and selecting the most positive
    # ## bag from an instance.
    # selection <- rep(0, length(positive_bag_name))  ## this records which instance is selected in which bag
    # past_selection <- matrix(NA, length(positive_bag_name), max.step)  ## this is the history of past selection.
    # past_selection[, 1] <- selection
    # ## step <- 1

    data_info <- unique(data[, c("bag_label", "bag_name", "instance_name")])
    temp_res <- kernel_mil(kernel, data_info, max.step, cost, weights,
        sigma, yy, useful_inst_idx)

    sample_df <- data[data$instance_name %in% instance_name[temp_res$useful_inst_idx],
        -c(1, 2)]

    res <- list(
        model = temp_res$svm_model,
        total_step = temp_res$step,
        representative_inst = cbind(positive_bag_name, temp_res$selection),
        traindata = sample_df,
        useful_inst_idx = temp_res$useful_inst_idx
    )

    return(new_mildsvm(res, method = "heuristic"))
}

