new_mild <- function(x = matrix()) {
  stopifnot(is.matrix(x))
  structure(x, class = "mild")
}

validate_mild <- function(x) {
  message("No validations currently in place for object of class 'mi'.")
  x
}

#' Create a mild object
#'
#' Create a `mild` object, usually used as a response variable in a model
#' formula.
#'
#' @param bag_label the bag label or response, recorded as 0 = negative, 1 =
#'   positive
#' @param bag_name a unique bag identifier for each instance.
#' @param instance_name a unique instance identifier for each sample.
#' @return An object of class `mild`.  Currently, no methods are implemented for
#'   this
#' @examples
#'
#' mild_data <- generate_mild_df(
#'   positive_dist = 'mvt',
#'   negative_dist = 'mvnormal',
#'   remainder_dist = 'mvnormal',
#'   nbag = 50,
#'   nsample = 20,
#'   positive_degree = 3,
#'   positive_prob = 0.15,
#'   positive_mean = rep(0, 5)
#' )
#' with(mild_data, mild(bag_label, bag_name, instance_name))
#' df <- get_all_vars(mild(bag_label, bag_name) ~ X1 + X2, data = mild_data)
#' head(df)
#'
#' @export
#' @author Sean Kent
mild <- function(bag_label, bag_name, instance_name) {
  # TODO: figure out if the character matrix that gets returned will be a problem
  new_mild(cbind(bag_label, bag_name, instance_name))
}
