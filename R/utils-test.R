#' Evaluate ROC, MZOE, MAE from predictions
#' @noRd
.evaluate_ordinal_predictions <- function(true, pred, roc_cutoff, mzoe_cutoff, mae_cutoff) {
  # roc
  suppressMessages({
    roc <- pROC::multiclass.roc(response = true, predictor = pred)
  })
  testthat::expect_gt(roc$auc, roc_cutoff)

  # mean zero-one error
  mzoe <- mean(true != pred)
  testthat::expect_lte(mzoe, mzoe_cutoff)

  # mean absolute error
  mae <- mean(abs(true - pred))
  testthat::expect_lte(mae, mae_cutoff)

  testthat::expect_snapshot({
    print(roc$auc)
    print(mzoe)
    print(mae)
    print(table(true, pred))
  })
}
