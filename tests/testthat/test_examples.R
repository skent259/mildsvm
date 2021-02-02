context("Check that examples in mildsvm package work.")
suppressWarnings({
  library(mildsvm)
  library(dplyr)
  # library(MilDistribution)
})


# test_that("predict.mild function works without error", {
#   set.seed(8)
#   mil_data <- GenerateMilData(positive_dist = 'mvt',
#                               negative_dist = 'mvnormal',
#                               remainder_dist = 'mvnormal',
#                               nbag = 10,
#                               positive_degree = 3,
#                               nsample = 20
#   )
#   foo <- mil_distribution(data = mil_data, cost = 1) ## uses about 10 seconds.
#   predictions_mild <- predict(object = foo, newdata = mil_data)
#   # predictions_mild <- predict.mild(object = foo, newdata = MilData1)
#
#   expect_equal(attributes(predictions_mild), list(names = c("final_pred", "AUC", "ROC")))
#
# })

test_that("mildsvm example works", {
  set.seed(8)
  mil_data <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 15,
                              positive_degree = 3,
                              nsample = 20
  )
  # Heuristic method
  mdl1 <- mildsvm(mil_data)
  mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data)

  expect_warning({
    if (require(gurobi)) {
      foo <- mildsvm(mil_data, method = "mip", control = list(nystrom_args = list(m = 10, r = 10)))
      predict(foo, mil_data)
    }
  })


  predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")

  # summarize predictions at the bag layer
  mil_data %>%
    bind_cols(predict(mdl2, mil_data, type = "class")) %>%
    bind_cols(predict(mdl2, mil_data, type = "raw")) %>%
    distinct(bag_name, bag_label, .pred_class, .pred)

})

test_that("predict.mildsvm examples work", {
  mil_data <- GenerateMilData(
    positive_dist = 'mvt',
    negative_dist = 'mvnormal',
    remainder_dist = 'mvnormal',
    nbag = 20,
    ncov = 5,
    nsample = 50,
    positive_degree = 3,
    positive_mean = rep(5, 5)
  )

  mdl1 <- mildsvm(mil_data, control = list(sigma = 0.05))

  # bag level predictions
  mil_data %>%
    bind_cols(predict(mdl1, mil_data, type = "class")) %>%
    bind_cols(predict(mdl1, mil_data, type = "raw")) %>%
    distinct(bag_name, bag_label, .pred_class, .pred)

  # instance level prediction
  mil_data %>%
    bind_cols(predict(mdl1, mil_data, type = "class", layer = "instance")) %>%
    bind_cols(predict(mdl1, mil_data, type = "raw", layer = "instance")) %>%
    distinct(bag_name, instance_name, bag_label, .pred_class, .pred)

  expect_s3_class(mdl1, "mildsvm")
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
                bags = df$bag_name, method = "heuristic",
                control = list(kernel = "radial", sigma = 1 / 120))
  expect_equal(round(mdl1$model$coefs[8:10], 4),
               c(-0.1346, -0.1346, -0.1346))


  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)
  expect_equal(round(mdl2$model$coefs[1:10],4),
               c(1.0000,  0.0654,  1.0000,  0.9245, -0.1346, -0.1346, -0.1346,
                 -0.1346, -0.1346, -0.1346))

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

  expect_equal(round(preds$.pred[1:5], 4), c(0.9669,  1.3092, -0.0781, 1.2562, 1.3084))
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
  df %>%
    bind_cols(predict(mdl2, df, type = "class")) %>%
    bind_cols(predict(mdl2, df, type = "raw")) %>%
    distinct(bag_name, bag_label, .pred_class, .pred)

  expect_s3_class(mdl1, "cv_misvm")
})

test_that("smm() examples work", {
  set.seed(8)
  n_instances <- 10
  n_samples <- 20
  y <- rep(c(1, -1), each = n_samples * n_instances / 2)
  instances <- as.character(rep(1:n_instances, each = n_samples))
  x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
                  x2 = rnorm(length(y), mean = 2*(y==1)),
                  x3 = rnorm(length(y), mean = 3*(y==1)))

  df <- data.frame(instance_name = instances, y = y, x)

  mdl <- smm(x, y, instances)
  mdl2 <- smm(y ~ ., data = df)

  # instance level predictions
  df %>%
    bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
    bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
    distinct(instance_name, y, .pred, .pred_class)

  expect_s3_class(mdl, "smm")
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
