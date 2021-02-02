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
#' will compute a kernel on the instances and pass that to `kernlab::ksvm` to
#' train the appropriate SVM model.
#'
#' @inheritParams mildsvm
#' @param formula a formula that defines the outcome `y` and covariates `x`.
#'   This argument is an alternative to the `x, y, bags, instances ` arguments,
#'   but requires the `data` argument. See examples.
#' @param data If `formula` is provided, a data.frame or similar from which
#'   formula elements will be extracted.  Otherwise, a 'MilData' object from
#'   which `x, y, instances` are automatically extracted. If a 'MilData'
#'   object is used, all columns will be used as predictors.
#' @param cost The cost parameter in SVM, fed to the `C` argument in
#'   `kernlab::ksvm`
#' @param weights named vector, or TRUE, to control the weight of the cost
#'   parameter for each possible y value.  Weights multiply against the cost
#'   vector. If TRUE, weights are calculated based on inverse counts of
#'   instances with given label. Otherwise, names must match the levels of `y`.
#' @param control list of additional parameters passed to the method that
#'   control computation with the following components:
#'   - `kernel` either a character the describes the kernel ('radial') or a
#'   kernel matrix at the instance level.
#'   - `sigma` argument needed for radial basis kernel.
#'   - `scale` Logical; whether to rescale the input before fitting.
#'
#' @return an object of class 'smm'.  The object contains the following
#'   components, if applicable:
#'   - `model`: an SVM model fit with `kernlab::ksvm`.
#'   - `call_type`: the call type, which specifies whether `smm()`
#'   was called via the formula, data.frame, of MilData method.
#'   - `sigma`: argument used for radial basis kernel.
#'   - `traindata`: training data from the underlying fitting.  This data will
#'   get used when computing the kernel matrix for prediction.
#'   - `cost`: argument used for SVM cost parameter.
#'   - `levels`: levels of `y` that are recorded for future prediction.
#'   - `features`: the features used for prediction.
#'   - `instance_name`: the name of the column used for instances, if the
#'   formula or MilData method is applied.
#'   - `center`: values used to center x, if `scale` = TRUE.
#'   - `scale`: values used to scale x, if `scale` = TRUE.
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
#' df <- data.frame(instance_name = instances, y = y, x)
#'
#' mdl <- smm(x, y, instances)
#' mdl2 <- smm(y ~ ., data = df)
#'
#' # instance level predictions
#' df %>%
#'   bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
#'   bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
#'   distinct(instance_name, y, .pred, .pred_class)
#'
#' @author Sean Kent, Yifei Liu
#' @name smm
NULL

#' @export
smm <- function(x, y, instances, ...) {
    UseMethod("smm")
}

#' @describeIn smm Method for data.frame-like objects
#' @export
smm.default <- function(x,
                        y,
                        instances,
                        cost = 1,
                        weights = TRUE,
                        control = list(kernel = "radial",
                                       sigma = if (is.vector(x)) 1 else 1 / ncol(x),
                                       scale = TRUE))
{
    if ("kernel" %ni% names(control)) control$kernel <- "radial"
    if ("sigma" %ni% names(control)) control$sigma <- if (is.vector(x)) 1 else 1 / ncol(x)
    if ("scale" %ni% names(control) && is.matrix(control$kernel)) {
        message("Since `kernel` was passed as a matrix, defaulting to `scale` = FALSE.")
        control$scale <- FALSE
    } else if ("scale" %ni% names(control)) {
        control$scale <- TRUE
    }

    col_x <- colnames(x)
    if (control$scale) {
        x <- scale(x)
        center <- attr(x, "scaled:center")
        scale <- attr(x, "scaled:scale")
        x <- as.data.frame(x)
    }
    x <- data.frame(instance_name = instances, x)

    # store the levels of y and convert to 0,1 numeric format.
    y_info <- convert_y(y)
    y <- 2*classify_bags(y_info$y, instances) - 1
    y <- factor(y)
    lev <- y_info$lev

    ## weights
    if (is.numeric(weights)) {
        stopifnot(names(weights) == lev | names(weights) == rev(lev))
        weights <- weights[lev]
        names(weights) <- c("-1", "1")
    } else if (isTRUE(weights)) {
        weights <- c("-1" = sum(y == 1) / sum(y == 0), "1" = 1)
    } else {
        weights <- NULL
    }

    ## kernel
    is_matrix_kernel <- inherits(control$kernel, "matrix")
    n_instances <- length(unique(instances))
    if (control$kernel != "radial" && !is_matrix_kernel) {
        warning("control$kernel must either be 'radial' or a square matrix.  Defaulting to 'radial'.")
        control$kernel <- "radial"
    } else if (is_matrix_kernel) {
        if (all(dim(control$kernel) != c(n_instances, n_instances))) {
            warning("Matrix passed to control$kernel is not of correct size. Defaulting to 'radial'.")
            control$kernel <- "radial"
        }
    }
    if (all(control$kernel == "radial")) {
        control$kernel <- kme(x, sigma = control$sigma)
    }

    fit <- kernlab::ksvm(x = control$kernel,
                         y = y,
                         kernel = "matrix",
                         C = cost,
                         class.weights = weights)

    res <- list(
        model = fit,
        call_type = "smm.default",
        sigma = control$sigma,
        traindata = x,
        cost = cost,
        levels = lev,
        features = col_x,
        instance_name = NULL
    )
    if (control$scale) {
        res$center <- center
        res$scale <- scale
    }
    return(new_smm(res))
}

#' @describeIn smm Method for passing formula
#' @export
smm.formula <- function(formula, data, instances = "instance_name", ...)
{
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

#' @describeIn smm Method for MilData objects
#' @export
smm.MilData <- function(data, ...)
{
    x <- as.data.frame(subset(data, select = -c(bag_label, bag_name, instance_name)))
    y <- data$bag_label
    instances <- data$instance_name

    res <- smm.default(x, y, instances, ...)
    res$call_type <- "smm.MilData"
    res$bag_name <- "bag_name"

    res$instance_name <- "instance_name"
    return(res)
}


#' Predict method for 'smm' object
#'
#' @inheritParams predict.mildsvm
#' @param object an object of class smm
#' @param new_data matrix to predict from.  Needs to have the same number of
#'   columns as the X that trained the 'smm' object
#' @param new_instances character or character vector.  Can specify a singular
#'   character that provides the column name for the instance names in
#'   `new_data`, default = "instance_name".  Can also specify a vector of length
#'   `nrow(new_data)` that has instance name for each row.  When `object` was
#'   fitted with `smm.formula`, this parameter is not necessary as the bag name
#'   can be pulled directly from new_data, if available.
#'
#' @return tibble with `nrow(new_data)` rows.  If type = 'class', the tibble
#'   will have a column named '.pred_class'.  If type = 'raw', the tibble will
#'   have a column name '.pred'.
#'
#' @examples
#' # Some fake data
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
#'     distinct(instance_name, y, .pred, .pred_class)
#'
#' @export
#' @importFrom stats predict
#' @author Sean Kent
predict.smm <- function(object,
                        new_data,
                        type = c("class", "raw"),
                        new_instances = "instance_name",
                        kernel = NULL,
                        ...)
{
    type <- match.arg(type)

    if (is.matrix(kernel) && !is.null(object$center)) {
        message("Model was fit using scaling, make sure that kernel matrix was similarly scaled.")
    }

    traindata <- object$traindata
    model <- object$model

    # instance information
    if (object$call_type == "smm.formula" & new_instances[1] == "instance_name" & length(new_instances) == 1) {
        new_instances <- object$instance_name
    }
    if (length(new_instances) == 1 & new_instances[1] %in% colnames(new_data)) {
        instances <- new_data[[new_instances]]
    } else {
        instances <- new_instances
    }

    # new_x
    if (object$call_type == "smm.formula") {
        new_x <- x_from_formula(object$formula, new_data, skip = object$instance_name)
    } else {
        new_x <- new_data[, object$features, drop = FALSE]
    }
    if (!is.null(new_x) && "center" %in% names(object)) {
        new_x <- as.data.frame(scale(new_x, center = object$center, scale = object$scale))
    }

    # kernel_m
    sv_ind <- kernlab::SVindex(model)
    if (is.matrix(kernel)) {
        kernel_m <- kernel[, sv_ind] # future note, I don't think this actually filters anything out...
    } else {
        used_instance_names <- unique(traindata$instance_name)[sv_ind]
        used_instances <- which(traindata$instance_name %in% used_instance_names)
        kernel_m <- kme(df = data.frame(instance_name = instances, new_x),
                        df2 = traindata[used_instances, ],
                        sigma = object$sigma)
    }
    kernel_m <- kernlab::as.kernelMatrix(kernel_m)

    raw <- kernlab::predict(model, kernel_m, type = "decision")
    raw <- as.numeric(raw)
    names(raw) <- unique(instances)

    pos <- 2*(raw > 0) - 1
    pos <- factor(pos, levels = c(-1, 1), labels = object$levels)

    res <- switch(
        type,
        "raw" = tibble::tibble(.pred = raw[instances]),
        "class" = tibble::tibble(.pred_class = pos[instances])
    )
    return(res)
}
