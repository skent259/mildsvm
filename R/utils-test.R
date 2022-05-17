#' Skip test when gurobi package not present
#'
#' @noRd
skip_if_no_gurobi <- function() {
  if (!requireNamespace("gurobi", quietly = TRUE)) {
    testthat::skip("The package gurobi is not available.")
  }
}


#' Evaluate ROC, MZOE, MAE from predictions
#' @noRd
.evaluate_ordinal_predictions <- function(true, pred, roc_cutoff, mzoe_cutoff, mae_cutoff) {
  # roc
  suppressMessages({
    roc <- pROC::multiclass.roc(response = true, predictor = pred)
  })
  expect_gt(roc$auc, roc_cutoff)

  # mean zero-one error
  mzoe <- mean(true != pred)
  expect_lte(mzoe, mzoe_cutoff)

  # mean absolute error
  mae <- mean(abs(true - pred))
  expect_lte(mae, mae_cutoff)

  expect_snapshot({
    print(roc$auc)
    print(mzoe)
    print(mae)
    print(table(true, pred))
  })
}
