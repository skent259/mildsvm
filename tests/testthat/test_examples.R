context("Check that examples in mildsvm package work.")
suppressWarnings({
  library(mildsvm)
  # library(MilDistribution)
})


test_that("predict.mild function works without error", {
  set.seed(8)
  MilData1 <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 10,
                              positive_degree = 3,
                              nsample = 20
  )
  foo <- mil_distribution(data = MilData1, cost = 1) ## uses about 10 seconds.
  predictions_mild <- predict(object = foo, newdata = MilData1)
  # predictions_mild <- predict.mild(object = foo, newdata = MilData1)

  expect_equal(attributes(predictions_mild), list(names = c("final_pred", "AUC", "ROC")))

})

test_that("mildsvm example works", {
  MilData1 <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 15,
                              positive_degree = 3,
                              nsample = 20
  )
  foo <- mildsvm(MilData1, method = "mip", m = 10)
  # predict(foo, MilData1)

})



test_that("misvm.R examples work", {
  set.seed(8)
  mil_data <- GenerateMilData(
    positive_dist = 'mvt',
    negative_dist = 'mvnormal',
    remainder_dist = 'mvnormal',
    nbag = 20,
    nsample = 20,
    positive_degree = 3,
    positive_prob = 0.15,
    positive_mean = rep(0, 5)
  )
  df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  # Heuristic method
  mdl1 <- misvm(x = df[, 4:123], y = df$bag_label,
                bags = df$bag_name, method = "heuristic")
  expect_equal(round(mdl1$svm_mdl$coefs[8:10], 4),
               c(-0.1687, -0.1339, -0.1082))


  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)
  expect_equal(round(mdl2$svm_mdl$coefs[1:10],4),
               c(1.0000,  1.0000,  1.0000,  1.0000,  1.0000,  1.0000,  1.0000,
                 -0.1377, -0.5385, -0.4025))

  if (require(gurobi)) {
    # solve using the MIP method
    mdl3 <- misvm(x = df[, 4:123], y = df$bag_label,
                  bags = df$bag_name, method = "mip")
    expect_equivalent(round(mdl3$model$w[1:5], 4), c(0.0318, 0.1657, 0.1020, 0.0558, -0.0933))
  }

  predict(mdl1, new_data = df, type = "raw", layer = "bag")

  # summarize predictions at the bag layer
  library(dplyr)
  preds <- df %>%
    bind_cols(predict(mdl2, df, type = "class")) %>%
    bind_cols(predict(mdl2, df, type = "raw")) %>%
    distinct(bag_name, bag_label, .pred_class, .pred)

  expect_equal(preds$.pred[1:5], c(0.08103285,  0.25842261, -0.98292239, -0.01807634, -0.99997987))
  expect_equal(dim(preds), c(20, 4))
})


test_that("cv_misvm.R examples work", {
  set.seed(8)
  mil_data <- GenerateMilData(
    positive_dist = 'mvt',
    negative_dist = 'mvnormal',
    remainder_dist = 'mvnormal',
    nbag = 20,
    nsample = 20,
    positive_degree = 3,
    positive_prob = 0.15,
    positive_mean = rep(0, 5)
  )
  df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
  cost_seq <- 2^seq(-5, 7, length.out = 5)

  # Heuristic method
  mdl1 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
                   bags = df$bag_name, cost_seq = cost_seq,
                   n_fold = 3, method = "heuristic")
  mdl2 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df,
                   cost_seq = cost_seq, n_fold = 3)

  if (suppressWarnings(require(gurobi))) {
    # solve using the MIP method
    mdl3 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
                     bags = df$bag_name, cost_seq = cost_seq,
                     n_fold = 3, method = "mip")
  }

  predict(mdl1, new_data = df, type = "raw", layer = "bag")

  # summarize predictions at the bag layer
  library(dplyr)
  df %>%
    bind_cols(predict(mdl2, df, type = "class")) %>%
    bind_cols(predict(mdl2, df, type = "raw")) %>%
    distinct(bag_name, bag_label, .pred_class, .pred)

})

# test_that("build_poly_instance_feature works", {
#   MilData1 <- GenerateMilData(positive_dist = 'mvt',
#                               negative_dist = 'mvnormal',
#                               remainder_dist = 'mvnormal',
#                               nbag = 50,
#                               nsample = 20,
#                               positive_degree = 3,
#                               positive_prob = 0.15,
#                               positive_mean = rep(0, 5))
#   df1 <- build_poly_instance_feature(MilData1, degree = 2)
#
#   expect_equal(dim(df1), c(200, 68))
#   expect_equal(class(df1), "data.frame")
#
# })
