set.seed(8)
mil_data <- mildsvm::generate_mild_df(nbag = 20,
                                      positive_prob = 0.15,
                                      sd_of_mean = rep(0.15, 3))
df1 <- mildsvm::build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

set.seed(9)
mil_data_test <- mildsvm::generate_mild_df(nbag = 20,
                                           positive_prob = 0.15,
                                           sd_of_mean = rep(0.15, 3))

df_test <- mildsvm::build_instance_feature(mil_data_test, seq(0.05, 0.95, length.out = 10))

cost_seq = 2^c(-2, 4)

test_that("cv_misvm() works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")
  set.seed(8)
  model <- cv_misvm(x = df1[, 4:123],
                    y = df1$bag_label,
                    bags = df1$bag_name,
                    n_fold = 3,
                    cost_seq = 2^seq(-5, 7, length.out = 5),
                    control = list(kernel = "radial",
                                   sigma = 1 / length(4:123))
  )

  expect_equal(names(model), c("misvm_fit", "cost_seq", "cost_aucs", "best_cost"))
  expect_equal(class(model), "cv_misvm")
  expect_equal(class(model$misvm_fit), "misvm")
  expect_equal(class(model$misvm_fit$svm_fit), "svm")

  pred <-
    df_test %>%
    dplyr::bind_cols(predict(model, new_data = df_test)) %>%
    dplyr::bind_cols(predict(model, new_data = df_test, type = "raw"))

  pred_bag <-
    pred %>%
    dplyr::group_by(bag_name) %>%
    dplyr::distinct(bag_label, .pred, .pred_class)
  expect_equal(dim(pred_bag), c(20, 4))
  expect_snapshot({
    pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred) %>%
      suppressMessages()
  })

  set.seed(8)
  model <- cv_misvm(x = df1[, 4:123],
                    y = df1$bag_label,
                    bags = df1$bag_name,
                    n_fold = 3,
                    cost_seq = 2^seq(-5, 7, length.out = 5),
                    method = "mip")

  pred <-
    df_test %>%
    dplyr::bind_cols(predict(model, new_data = df_test)) %>%
    dplyr::bind_cols(predict(model, new_data = df_test, type = "raw"))

  pred_bag <-
    pred %>%
    dplyr::group_by(bag_name) %>%
    dplyr::distinct(bag_label, .pred, .pred_class)

  expect_equal(dim(pred_bag), c(20, 4))
  expect_snapshot({
    pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred) %>%
      suppressMessages()
  })

  # qp-heuristic
  set.seed(8)
  model <- cv_misvm(x = df1[, 4:123],
                    y = df1$bag_label,
                    bags = df1$bag_name,
                    n_fold = 3,
                    cost_seq = 2^seq(-5, 7, length.out = 5),
                    method = "qp-heuristic")

  pred <-
    df_test %>%
    dplyr::bind_cols(predict(model, new_data = df_test)) %>%
    dplyr::bind_cols(predict(model, new_data = df_test, type = "raw"))

  pred_bag <-
    pred %>%
    dplyr::group_by(bag_name) %>%
    dplyr::distinct(bag_label, .pred, .pred_class)

  expect_equal(dim(pred_bag), c(20, 4))
  expect_snapshot({
    pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred) %>%
      suppressMessages()
  })

})

test_that("cv_misvm() works with formula method", {
  skip_if_not_installed("gurobi")
  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(nbag = 20,
                                        nsample = 20,
                                        positive_prob = 0.15,
                                        sd_of_mean = rep(0.15, 3))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  set.seed(8)
  mdl1 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                   data = df1,
                   n_fold = 3,
                   cost_seq = 2^seq(-5, 7, length.out = 5))
  set.seed(8)
  mdl2 <- cv_misvm(x = df1[, c("X1_mean", "X2_mean", "X3_mean")],
                   y = df1$bag_label,
                   bags = df1$bag_name,
                   n_fold = 3,
                   cost_seq = 2^seq(-5, 7, length.out = 5))

  expect_equal(mdl1$model$model, mdl2$model$model)
  expect_equal(mdl1$model$features, mdl2$model$features)
  expect_equal(mdl1$cost_seq, mdl2$cost_seq)
  expect_equal(mdl1$cost_aucs, mdl2$cost_aucs)
  expect_equal(mdl1$best_cost, mdl2$best_cost)

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))


  # check only 1 predictor works
  mdl1 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean,
                   data = df1,
                   n_fold = 3,
                   cost_seq = 2^seq(-5, 7, length.out = 5))
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  mdl1 <- cv_misvm(mi(bag_label, bag_name) ~ 0 + X1_mean:X2_mean + X2_mean*X3_mean,
                   data = df1,
                   n_fold = 3,
                   cost_seq = 2^seq(-5, 7, length.out = 5))
  expect_equal(mdl1$misvm_fit$features,
               colnames(model.matrix(~ 0 + X1_mean:X2_mean + X2_mean*X3_mean, data = df1)))
  predict(mdl1, df1, type = "raw")

  # check for mip method
  mdl1 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                   data = df1,
                   method = "mip",
                   n_fold = 3,
                   cost_seq = 2^seq(-5, 7, length.out = 5))
  predict(mdl1, df1, type = "raw")
})

test_that("predict.cv_misvm returns labels that match the input labels", {
  skip_if_not_installed("gurobi")
  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(class,
                  "default" = cv_misvm(x = df[, 4:123],
                                       y = df$bag_label,
                                       bags = df$bag_name,
                                       method = method,
                                       n_fold = 3,
                                       cost_seq = 2^seq(-5, 7, length.out = 5)),
                  "formula" = cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean,
                                       data = df2,
                                       method = method,
                                       n_fold = 3,
                                       cost_seq = 2^seq(-5, 7, length.out = 5)))
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(nbag = 7,
                                        positive_prob = 0.15,
                                        sd_of_mean = rep(0.15, 3))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

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
  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df2)
  mdl3 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df3)
  expect_equal(predict(mdl2, df2, type = "class"),
               predict(mdl3, df3, type = "class"))

})


test_that("Dots work in cv_misvm() formula", {
  skip_if_not_installed("gurobi")
  set.seed(8)
  mil_data <- generate_mild_df(nbag = 7,
                               positive_prob = 0.15,
                               sd_of_mean = rep(0.15, 3))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
    dplyr::select(bag_label, bag_name, X1_mean, X2_mean, X3_mean)

  set.seed(8)
  misvm_dot <- cv_misvm(mi(bag_label, bag_name) ~ .,
                        data = df1,
                        n_fold = 3,
                        cost_seq = 2^seq(-5, 7, length.out = 5))
  set.seed(8)
  misvm_nodot <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                          data = df1,
                          n_fold = 3,
                          cost_seq = 2^seq(-5, 7, length.out = 5))

  expect_equal(misvm_dot$model$model, misvm_nodot$model$model)
  expect_equal(misvm_dot$model$features, misvm_nodot$model$features)
  expect_equal(misvm_dot$model$bag_name, misvm_nodot$model$bag_name)
  expect_equal(misvm_dot$cost_aucs, misvm_nodot$cost_aucs)

  expect_equal(predict(misvm_dot, new_data = df1), predict(misvm_nodot, new_data = df1))

})

test_that("`cv_misvm()` works with `mi_df` method", {
  predictors <- c("X1_mean", "X2_mean", "X3_mean")
  df1_mi <- as_mi_df(df1[, c("bag_label", "bag_name", predictors)], instance_label = NULL)
  set.seed(8)
  mdl1 <- cv_misvm(df1_mi, n_fold = 3, cost_seq = cost_seq)
  set.seed(8)
  mdl2 <- cv_misvm(x = df1[, predictors],
                   y = df1$bag_label,
                   bags = df1$bag_name,
                   n_fold = 3, cost_seq = cost_seq)

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "cv_misvm.mi_df")
  expect_equal(mdl1$misvm_fit$features, predictors)
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
})



