context("Testing the functions in cv_misvm.R")

test_that("cv_misvm() works for data-frame-like inputs", {
  set.seed(8)
  mil_data <- mildsvm::GenerateMilData(positive_dist = 'mvt',
                                       negative_dist = 'mvnormal',
                                       remainder_dist = 'mvnormal',
                                       nbag = 20,
                                       nsample = 50,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))
  df1 <- mildsvm::build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  set.seed(9)
  mil_data_test <- mildsvm::GenerateMilData(positive_dist = 'mvt',
                                            negative_dist = 'mvnormal',
                                            remainder_dist = 'mvnormal',
                                            nbag = 20,
                                            nsample = 50,
                                            positive_degree = 3,
                                            positive_prob = 0.15,
                                            positive_mean = rep(0, 5))
  df_test <- mildsvm::build_instance_feature(mil_data_test, seq(0.05, 0.95, length.out = 10))


  set.seed(8)
  model <- cv_misvm(x = df1[, 4:123],
                    y = df1$bag_label,
                    bags = df1$bag_name,
                    n_fold = 3,
                    cost_seq = 2^seq(-5, 7, length.out = 5),
                    control = list(kernel = "radial",
                                   sigma = 1 / length(4:123))
                    )

  expect_equal(names(model), c("model", "cost_seq", "cost_aucs", "best_cost"))
  expect_equal(class(model), "cv_misvm")
  expect_equal(class(model$model), "misvm")
  expect_equal(class(model$model$model), "svm")

  pred <-
    df_test %>%
    bind_cols(predict(model, new_data = df_test)) %>%
    bind_cols(predict(model, new_data = df_test, type = "raw"))

  pred_bag <-
    pred %>%
    group_by(bag_name) %>%
    distinct(bag_label, .pred, .pred_class)
  expect_equal(dim(pred_bag), c(20, 4))
  expect_equal(round(pred_bag$.pred[1:5], 4),
               c(0.7899, 0.6934, 0.7366, 0.7662, 0.7914))
  expect_equal(round(as.numeric(pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred)), 4),
               0.8791)

  set.seed(8)
  model <- cv_misvm(x = df1[, 4:123],
                    y = df1$bag_label,
                    bags = df1$bag_name,
                    n_fold = 3,
                    cost_seq = 2^seq(-5, 7, length.out = 5),
                    method = "mip")

  pred <-
    df_test %>%
    bind_cols(predict(model, new_data = df_test)) %>%
    bind_cols(predict(model, new_data = df_test, type = "raw"))

  pred_bag <-
    pred %>%
    group_by(bag_name) %>%
    distinct(bag_label, .pred, .pred_class)

  pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred)
  expect_equal(dim(pred_bag), c(20, 4))
  expect_equal(round(pred_bag$.pred[1:5], 4),
               c( 0.6293, -0.6668, -0.6176,  1.8590,  0.4452))
  expect_equal(round(as.numeric(pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred)), 4),
               1.000)

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
    bind_cols(predict(model, new_data = df_test)) %>%
    bind_cols(predict(model, new_data = df_test, type = "raw"))

  pred_bag <-
    pred %>%
    group_by(bag_name) %>%
    distinct(bag_label, .pred, .pred_class)

  pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred)
  expect_equal(dim(pred_bag), c(20, 4))
  expect_equal(round(pred_bag$.pred[1:5], 4),
               c(-0.1722, -1.0722, -0.4241,  0.4497, -0.3856))
  expect_equal(round(as.numeric(pROC::auc(response = pred_bag$bag_label, predictor = pred_bag$.pred)), 4),
               0.7912)

})




test_that("cv_misvm() works with formula method", {
  set.seed(8)
  mil_data <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 20,
                              nsample = 20,
                              positive_degree = 3,
                              positive_prob = 0.15,
                              positive_mean = rep(0, 5))

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
  expect_equal(mdl1$model$features,
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
  mil_data <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 7,
                              nsample = 20,
                              positive_degree = 3,
                              positive_prob = 0.15,
                              positive_mean = rep(0, 5))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  # 0/1
  df2 <- df1 %>% mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # 1/0
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # TRUE/FALSE
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c(TRUE, FALSE)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # Yes/No
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  expect_message(test_prediction_levels_equal(df2, method = "heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "mip"))
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # check that 0/1 and 1/0 return the same predictions
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df2)
  mdl3 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df3)
  expect_equal(predict(mdl2, df2, type = "class"),
               predict(mdl3, df3, type = "class"))

})


test_that("Dots work in cv_misvm() formula", {
  set.seed(8)
  mil_data <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 7,
                              nsample = 20,
                              positive_degree = 3,
                              positive_prob = 0.15,
                              positive_mean = rep(0, 5))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
    select(bag_label, bag_name, X1_mean, X2_mean, X3_mean)

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





