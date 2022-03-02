#' Skip test when gurobi package not present
#'
#' @noRd
skip_if_no_gurobi <- function() {
  if (!requireNamespace("gurobi", quietly = TRUE)) {
    skip("The package gurobi is not available.")
  }
}

#' Skip test when MilDistribution package not present
#'
#' @noRd
skip_if_no_MilDistrubution <- function() {
  if (!requireNamespace("MilDistribution", quietly = TRUE)) {
    skip("The package MilDistribution is not available.")
  }
}
