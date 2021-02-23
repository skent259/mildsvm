new_mi <- function(x = matrix()) {
  stopifnot(is.matrix(x))
  structure(x, class = "mi")
}

validate_mi <- function(x) {
  message("No validations currently in place for object of class 'mi'.")
  x
}

#' Create an `mi` object
#'
#' Create an `mi` object, usually used as a response variable in a model
#' formula.
#'
#' @param bag_label The bag label or response, recorded as 0 = negative, 1 =
#'   positive.
#' @param bag_name A unique bag identifier for each instance.
#' @return An object of class `mi`.  Currently, no methods are implemented for
#'   this.
#' @examples
#' mil_data <- generate_mild_df(positive_degree = 3, nbag = 10)
#' with(mil_data, head(mi(bag_label, bag_name)))
#' df <- get_all_vars(mi(bag_label, bag_name) ~ X1 + X2, data = mil_data)
#' head(df)
#'
#' @export
#' @family multiple instance formula helper functions
#' @author Sean Kent
mi <- function(bag_label, bag_name) {
  # TODO: figure out if the character matrix that gets returned will be a problem
  new_mi(cbind(bag_label, bag_name))
}
