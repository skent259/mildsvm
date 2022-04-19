#' Skip test when gurobi package not present
#'
#' @noRd
skip_if_no_gurobi <- function() {
  if (!requireNamespace("gurobi", quietly = TRUE)) {
    testthat::skip("The package gurobi is not available.")
  }
}

