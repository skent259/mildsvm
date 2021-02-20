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
#' @inheritParams mi
#' @param instance_name A unique instance identifier for each sample.
#'
#' @return An object of class `mild`.  Currently, no methods are implemented for
#'   this.
#'
#' @examples
#'
#' mil_data <- generate_mild_df(positive_degree = 3, nbag = 10)
#' with(mil_data, head(mild(bag_label, bag_name, instance_name)))
#' df <- get_all_vars(mild(bag_label, bag_name) ~ X1 + X2, data = mil_data)
#' head(df)
#'
#' @export
#' @family multiple instance formula helper functions
#' @author Sean Kent
mild <- function(bag_label, bag_name, instance_name) {
  # TODO: figure out if the character matrix that gets returned will be a problem
  new_mild(cbind(bag_label, bag_name, instance_name))
}
