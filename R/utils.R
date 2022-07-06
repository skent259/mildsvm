`%ni%` <- Negate(`%in%`)

#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default
#' value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @noRd
`%||%` <- function(x, y) {
  if (rlang::is_null(x)) y else x
}

#' Version of `base::max()` that keeps the names attribute
#' @noRd
.max <- function(x) {
  x[which.max(x)]
}

#' Version of `base::min()` that keeps the names attribute
#' @noRd
.min <- function(x) {
  x[which.min(x)]
}

#' Safer version of sample
#' @noRd
.resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Function to reorder the data by bag label, bag number, and then first data
#' column
#' @param y A vector of labels.
#' @param b A vector of bags.
#' @param x A data.frame of covariates.
#' @param i A vector of instances.
#' @noRd
.reorder <- function(y, b, x, i = NULL) {
  b <- as.numeric(as.factor(b))
  x <- as.data.frame(x)
  # order by bag label (negative first), and then order data by bag
  if (is.null(i)) {
    data_order <- order(y, b, x[, 1])
  } else {
    i <- as.numeric(as.factor(i))
    data_order <- order(y, b, i, x[, 1])
  }

  list(y = y[data_order],
       b = b[data_order],
       X = as.matrix(x[data_order, , drop = FALSE]),
       inst = i,
       order = data_order)
}

#' Classify y from bags
#'
#' Formally, this function applies `max()` on `y` for each level of `bags`.
#'
#' @inheritParams misvm
#' @param condense A logical (default `TRUE`) for whether to return
#'   classification at the level of unique bags or not.
#'
#' @return a named vector of length `length(unique(b))` which gives the
#'   classification for each bag.  Names come from `bags`.
#'
#' @examples
#' y <- c(1, 0, 0, 1, 1, 1, 0, 0, 0)
#' bags <- rep(1:3, each = 3)
#'
#' classify_bags(y, bags)
#' classify_bags(y, bags, condense = FALSE)
#'
#' # works with regular vector too
#' scores <- 1:9
#' classify_bags(scores, bags)
#'
#' @export
#' @author Sean Kent
classify_bags <- function(y, bags, condense = TRUE) {
  # works whether y is {-1, 1} or {0, 1} as long as 1 reflects a positive instance
  res <- sapply(unique(bags), function(b) max(y[b == bags]))
  names(res) <- unique(bags)
  if (!condense) {
    res <- res[as.character(bags)]
  }
  return(res)
}

#' Take the average of a data frame over the instances
#' @noRd
average_over_instances <- function(x, instances) {
  instances <- factor(instances, levels = unique(instances))
  x <- as.data.frame(x)
  x <- split(x, instances)
  x <- lapply(x, colMeans)
  as.data.frame(do.call(rbind, x))
}

#' Compute kernel matrix based on type
#' @param x A matrix
#' @param x2 A matrix, or `NULL` (default `NULL`).
#' @param type The type of kernel to compute.  Valid options are `'linear'` or
#'   `'radial'`.
#' @param sigma The parameter for `'radial'` kernel (default `NULL`).
#' @noRd
compute_kernel <- function(x, x2 = NULL, type = "linear", sigma = NULL) {
  if (is.null(x2)) x2 <- x
  type <- match.arg(type, c("linear", "radial"))

  if (type == "linear") {
    k <- x %*% t(x2)
  } else if (type == "radial") {
    if (!is.numeric(sigma)) {
      sigma <- 1 / ncol(x)
      rlang::inform(c(
        "Argument `sigma` was not provided.",
        i = paste0("Defaulting to `sigma` = ", sigma)
      ))
    }
    k <- rbf_kernel_matrix(sigma, x, x2)
  }
  return(k)
}

#' Sample `mild_df` object by bags and instances
#'
#' From a `mild_df` object, return a sample that evenly pulls from the unique
#' bags and unique instances from each bag as much as possible.  This is a form
#' of stratified sampling to avoid randomly sampling many rows from a few bags.
#'
#' @param data A `mild_df` object containing the data.
#' @param size A non-negative integer giving the number of rows to choose from
#'   `data`.
#'
#' @return A numeric vector of length `size` indicating which rows were sampled.
#'
#' @examples
#' mil_data <- generate_mild_df(positive_dist = "mvnormal",
#'                              nbag = 2,
#'                              ninst = 2,
#'                              nsample = 2)
#'
#' rows <- bag_instance_sampling(mil_data, 6)
#' table(mil_data$bag_name[rows])
#' table(mil_data$instance_name[rows])
#'
#' rows <- bag_instance_sampling(mil_data, 4)
#' table(mil_data$bag_name[rows])
#' table(mil_data$instance_name[rows])
#'
#' @export
#' @author Sean Kent
bag_instance_sampling <- function(data, size) {
  stopifnot(inherits(data, "mild_df"))

  bags <- unique(data$bag_name)
  sampled_bags <- .resample(c(rep(bags, size %/% length(bags)),
                              .resample(bags, size %% length(bags))))
  sampled_instances <- character(size)
  sampled_rows <- numeric(size)

  for (bag in unique(sampled_bags)) {
    ind <- which(bag == sampled_bags)
    k <- length(ind)
    instances <- unique(data$instance_name[which(data$bag_name == bag)])
    sampled_instances[ind] <- .resample(c(rep(instances, k %/% length(instances)),
                                          .resample(instances, k %% length(instances))))

    for (instance in instances) {
      ind2 <- which(instance == sampled_instances)
      l <- length(ind2)
      rows <- which(data$instance_name == instance)
      sampled_rows[ind2] <- .resample(c(rep(rows, l %/% length(rows)),
                                        .resample(rows, l %% length(rows))))
    }
  }
  return(sampled_rows)
}

