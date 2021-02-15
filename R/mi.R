new_mi <- function(x = matrix()) {
  stopifnot(is.matrix(x))
  structure(x, class = "mi")
}

validate_mi <- function(x) {
  message("No validations currently in place for object of class 'mi'.")
  x
}

#' Create a mi object
#'
#' Create a `mi` object, usually used as a response variable in a model formula.
#'
#' @param bag_label the bag label or response, recorded as 0 = negative, 1 = positive
#' @param bag_name a unique bag identifier for each instance.
#' @return An object of class `mi`.  Currently, no methods are implemented for this
#' @examples
#'
#' mil_data <- generate_mild_df(
#'   positive_dist = 'mvt',
#'   negative_dist = 'mvnormal',
#'   remainder_dist = 'mvnormal',
#'   nbag = 50,
#'   nsample = 20,
#'   positive_degree = 3,
#'   positive_prob = 0.15,
#'   positive_mean = rep(0, 5)
#' )
#' with(mil_data, mi(bag_label, bag_name))
#' df <- get_all_vars(mi(bag_label, bag_name) ~ X1 + X2, data = mil_data)
#' head(df)
#'
#' @export
#' @author Sean Kent
mi <- function(bag_label, bag_name) {
  # TODO: figure out if the character matrix that gets returned will be a problem
  new_mi(cbind(bag_label, bag_name))
}
