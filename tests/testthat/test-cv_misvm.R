
test_that("cv_misvm() works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")
  df_train <- readRDS(test_path("fixtures", "misvm-train_df.rds"))
  df_test <- readRDS(test_path("fixtures", "misvm-test_df.rds"))

  for (method in c("heuristic", "mip", "qp-heuristic")) {
    set.seed(8)
    model <- .run_cv_misvm(df_train, method = method,
                          control = list(sigma = 1/3))

    expect_equal(names(model), c("misvm_fit", "cost_seq", "cost_aucs", "best_cost"))
    expect_s3_class(model, "cv_misvm")
    expect_s3_class(model$misvm_fit, "misvm")
    if (method == "heuristic") {
      expect_s3_class(model$misvm_fit$svm_fit, "svm")
    }

    pred <- .get_pred_matrix(df_test, model)
    pred_bag <- .summarize_preds(pred, bag_name)

    expect_equal(dim(pred_bag), c(40, 4))
    expect_snapshot({
      pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred) %>%
        suppressMessages()
    })
  }
})

test_that("cv_misvm() works with formula method", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "misvm-train_df.rds"))

  set.seed(8)
  fit1 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                   data = df,
                   n_fold = 3,
                   cost_seq = 2^c(-2, 4))
  set.seed(8)
  fit2 <- .run_cv_misvm(df)

  expect_equal(fit1$model$model, fit2$model$model)
  expect_equal(fit1$model$features, fit2$model$features)
  expect_equal(fit1$cost_seq, fit2$cost_seq)
  expect_equal(fit1$cost_aucs, fit2$cost_aucs)
  expect_equal(fit1$best_cost, fit2$best_cost)

  # predictions should match
  expect_equal(predict(fit1, df, type = "raw"),
               predict(fit2, df, type = "raw"))
  expect_equal(predict(fit1, df, type = "class"),
               predict(fit2, df, type = "class"))


  # check only 1 predictor works
  fit1 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean,
                   data = df,
                   n_fold = 3,
                   cost_seq = 2^c(-2, 4))
  predict(fit1, df, type = "raw")

  # check some obscure formulas
  fit1 <- cv_misvm(mi(bag_label, bag_name) ~ 0 + X1_mean:X2_mean + X2_mean*X3_mean,
                   data = df,
                   n_fold = 3,
                   cost_seq = 2^c(-2, 4))
  expect_equal(fit1$misvm_fit$features,
               colnames(model.matrix(~ 0 + X1_mean:X2_mean + X2_mean*X3_mean, data = df)))
  predict(fit1, df, type = "raw")

  # check for mip method
  fit1 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                   data = df,
                   method = "mip",
                   n_fold = 3,
                   cost_seq = 2^c(-2, 4))
  predict(fit1, df, type = "raw")
})

test_that("predict.cv_misvm returns labels that match the input labels", {
  skip_if_not_installed("gurobi")
  test_prediction_levels_equal <- function(df, method, class = "default") {
    fit <- switch(class,
                  "default" = .run_cv_misvm(df, method = method, n_fold = 2),
                  "formula" = cv_misvm(
                    mi(bag_label, bag_name) ~ X1_mean + X2_mean,
                    data = df,
                    method = method,
                    n_fold = 2,
                    cost_seq = 2^c(-2, 4)
                  )
    )
    preds <- predict(fit, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  df1 <- readRDS(test_path("fixtures", "misvm-train_df.rds")) %>%
    dplyr::filter(bag_name %in% c("bag1", "bag12", "bag2", "bag4"))

  # 0/1
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # 1/0
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # TRUE/FALSE
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c(TRUE, FALSE)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # Yes/No
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  expect_message(test_prediction_levels_equal(df2, method = "heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "mip"))
  expect_message(test_prediction_levels_equal(df2, method = "heuristic", class = "formula"))

  # check that 0/1 and 1/0 return the same predictions
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  fit2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df2)
  fit3 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df3)
  expect_equal(predict(fit2, df2, type = "class"),
               predict(fit3, df3, type = "class"))

})


test_that("Dots work in cv_misvm() formula", {
  skip_if_not_installed("gurobi")
  set.seed(8)
  df1 <- readRDS(test_path("fixtures", "misvm-train_df.rds")) %>%
    dplyr::filter(bag_name %in% c("bag1", "bag12", "bag2", "bag4"))

  set.seed(8)
  misvm_dot <- cv_misvm(mi(bag_label, bag_name) ~ .,
                        data = df1,
                        n_fold = 2,
                        cost_seq = 2^c(-2, 4))
  set.seed(8)
  misvm_nodot <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                          data = df1,
                          n_fold = 2,
                          cost_seq = 2^c(-2, 4))

  expect_equal(misvm_dot$model$model, misvm_nodot$model$model)
  expect_equal(misvm_dot$model$features, misvm_nodot$model$features)
  expect_equal(misvm_dot$model$bag_name, misvm_nodot$model$bag_name)
  expect_equal(misvm_dot$cost_aucs, misvm_nodot$cost_aucs)

  expect_equal(predict(misvm_dot, new_data = df1), predict(misvm_nodot, new_data = df1))

})

test_that("`cv_misvm()` works with `mi_df` method", {

  df1 <- readRDS(test_path("fixtures", "misvm-train_df.rds"))
  df1_mi <- as_mi_df(df1, instance_label = NULL)

  set.seed(8)
  fit1 <- cv_misvm(df1_mi, n_fold = 2, cost_seq = 2^c(-2, 4))
  set.seed(8)
  fit2 <- .run_cv_misvm(df1)

  expect_equal(fit1$model, fit2$model)
  expect_equal(fit1$total_step, fit2$total_step)
  expect_equal(fit1$call_type, "cv_misvm.mi_df")
  expect_equal(fit1$misvm_fit$features, c("X1_mean", "X2_mean", "X3_mean"))
  expect_equal(fit1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(fit1, df1, type = "raw"), predict(fit2, df1, type = "raw"))
  expect_equal(predict(fit1, df1, type = "class"), predict(fit2, df1, type = "class"))
})



