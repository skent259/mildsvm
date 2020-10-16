context("Testing the functions in misvm.R")

test_that("misvm() works for data-frame-like inputs", {
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
  mdl1 <- misvm.default(x = df1[, 4:123],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "mip")

  # mdl1
  expect_equal(
    predict(mdl1, new_data = df1, type = "class", layer = "bag"),
    predict(mdl1, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )
  predict(mdl1, new_data = df1, type = "class", layer = "bag")
  predict(mdl1, new_data = df1, type = "class", layer = "instance")
  predict(mdl1, new_data = df1, type = "raw", layer = "bag")
  predict(mdl1, new_data = df1, type = "raw", layer = "instance")


  mdl2 <- misvm.default(x = df1[, 4:123],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "heuristic")

  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )

  predict(mdl2, new_data = df1, type = "class", layer = "bag")
  predict(mdl2, new_data = df1, type = "class", layer = "instance")
  predict(mdl2, new_data = df1, type = "raw", layer = "bag")
  predict(mdl2, new_data = df1, type = "raw", layer = "instance")

  bag_preds <-
    df1 %>%
    bind_cols(predict(mdl2, df1, type = "class")) %>%
    group_by(bag_name) %>%
    summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(df1$bag_name))

})


test_that("misvm() works with formula method", {
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

  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1)
  mdl2 <- misvm(x = df1[, c("X1_mean", "X2_mean", "X3_mean")],
                y = df1$bag_label,
                bags = df1$bag_name)

  expect_equal(mdl1$svm_mdl, mdl2$svm_mdl)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "misvm.formula")
  expect_equal(mdl1$features, c("X1_mean", "X2_mean", "X3_mean"))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
  predict(mdl1, df1, type = "raw")
  predict(mdl1, df1, type = "class")

  # check only 1 predictor works
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean, data = df1)
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  mdl1 <- misvm(mi(bag_label, bag_name) ~ 0 + X1_mean:X2_mean + X2_mean*X3_mean, data = df1)
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + X1_mean:X2_mean + X2_mean*X3_mean, data = df1)))
  predict(mdl1, df1, type = "raw")

  # check for mip method
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1, method = "mip")


})

test_that("predict.misvm returns labels that match the input labels", {
  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(class,
                  "default" = misvm(x = df[, 4:123],
                                    y = df$bag_label,
                                    bags = df$bag_name,
                                    method = method),
                  "formula" = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean,
                                    data = df2,
                                    method = method))
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

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

test_that("Dots work in misvm() formula", {
  set.seed(8)
  mil_data <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 20,
                              nsample = 20,
                              positive_degree = 3,
                              positive_prob = 0.15,
                              positive_mean = rep(0, 5))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
    select(bag_label, bag_name, X1_mean, X2_mean, X3_mean)

  misvm_dot <- misvm(mi(bag_label, bag_name) ~ ., data = df1)
  misvm_nodot <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1)

  expect_equal(misvm_dot$svm_mdl, misvm_nodot$svm_mdl)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

})


