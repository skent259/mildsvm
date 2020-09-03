context("Testing the functions in feature_map.R")

test_that("Nystrom method approximates the true kernel on a dataframe.", {
  check_nystrom_approximation <- function(fit, df, max_thresh, mean_thresh) {
    fm <- predict_nystrom(fit, df)
    true_kernel <- rbf_kernel_matrix(fit$kernel_params$sigma, as.matrix(df), as.matrix(df))
    approximate_kernel <- fm %*% t(fm)

    expect_lt(max(abs(true_kernel - approximate_kernel)), max_thresh)
    expect_lt(mean(abs(true_kernel - approximate_kernel)), mean_thresh)
  }

  set.seed(8)
  df <- data.frame(
    X1 = c(2,   3,   4,   5,   6, 7, 8),
    X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
    X3 = rnorm(7)
  )

  set.seed(8)
  ## RBF kernel, full feature map
  fit <- fit_nystrom(df, m = 7, r = 7, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1e-14, 1e-15)
  fit <- fit_nystrom(df, m = 7, r = 7, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1e-14, 1e-15)

  ## RBF kernel, smaller feature map
  fit <- fit_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1, 1e-3)
  fit <- fit_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1, 0.05)

})

test_that("Nystrom method approximates the true kernel on a MilData object", {
  check_nystrom_approximation <- function(fit, df, max_thresh, mean_thresh) {
    X <- subset(df, select = -c(bag_label, bag_name, instance_name))
    fm <- predict_nystrom(fit, df)
    fm <- as.matrix(subset(fm, select = -c(bag_label, bag_name, instance_name)))

    true_kernel <- rbf_kernel_matrix(fit$kernel_params$sigma, as.matrix(X), as.matrix(X))
    approximate_kernel <- fm %*% t(fm)

    expect_lt(max(abs(true_kernel - approximate_kernel)), max_thresh)
    expect_lt(mean(abs(true_kernel - approximate_kernel)), mean_thresh)
  }

  set.seed(8)
  df <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                 negative_dist = "mvnormal",
                                 remainder_dist = "mvnormal",
                                 ncov = 5,
                                 nbag = 7,
                                 nsample = 7,
                                 positive_degree = 3,
                                 positive_prob = 0.15,
                                 positive_mean = rep(0, 5))

  set.seed(8)
  ## RBF kernel, full feature map
  fit <- fit_nystrom(df, m = 196, r = 196, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1e-13, 1e-14)
  fit <- fit_nystrom(df, m = 196, r = 196, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1e-13, 1e-14)

  ## RBF kernel, smaller feature map
  fit <- fit_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1, 0.05)
  fit <- fit_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1, 0.06)

})

test_that("Nystrom methods have correct output dimensions", {

  ## test Nystrom on data frame
  set.seed(8)
  df <- data.frame(
    X1 = c(2,   3,   4,   5,   6, 7, 8),
    X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
    X3 = rnorm(7)
  )

  fit <- fit_nystrom(df, m = 7, r = 7, kernel = "rbf", sigma = 0.05)
  fm <- predict_nystrom(fit, df)
  expect_equal(dim(fm), c(7,7))

  fit <- fit_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
  fm <- predict_nystrom(fit, df)
  expect_equal(dim(fm), c(7,6))
  fm <- predict_nystrom(fit, df[1:3, ])
  expect_equal(dim(fm), c(3,6))

  fit <- fit_nystrom(df, m = 5, r = 3, kernel = "rbf", sigma = 0.05)
  fm <- predict_nystrom(fit, df)
  expect_equal(dim(fm), c(7,3))

  ## test Nystrom on MilData
  mil_data <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                       negative_dist = "mvnormal",
                                       remainder_dist = "mvnormal",
                                       ncov = 5,
                                       nbag = 7,
                                       nsample = 7,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))

  fit <- fit_nystrom(mil_data, m = nrow(mil_data), r = nrow(mil_data), kernel = "rbf", sigma = 0.05)
  fm <- predict_nystrom(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), nrow(mil_data)+3))
  fm <- predict_nystrom(fit, mil_data[1:13, ])
  expect_equal(dim(fm), c(13, nrow(mil_data)+3))

  fit <- fit_nystrom(mil_data, m = 7, r = 7, kernel = "rbf", sigma = 0.05)
  fm <- predict_nystrom(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), 7+3))

  fit <- fit_nystrom(mil_data, m = 7, r = 3, kernel = "rbf", sigma = 0.05)
  fm <- predict_nystrom(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), 3+3))

})

test_that("fit_kernel_feature_map works for appropriate methods", {

  set.seed(8)
  df <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                 negative_dist = "mvnormal",
                                 remainder_dist = "mvnormal",
                                 ncov = 5,
                                 nbag = 7,
                                 nsample = 7,
                                 positive_degree = 3,
                                 positive_prob = 0.15,
                                 positive_mean = rep(0, 5))

  fit <- fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", m = 100, r = 50, sigma = 0.05)
  expect_equal(names(fit), c("df_sub", "dv", "method", "kernel", "kernel_params"))
  expect_message(fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", sigma = 0.05),
                 "Using parameter m = 196")
  expect_message(fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", sigma = 0.05),
                 "Using parameter r = 196")
  expect_error(fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", m = 100, r = 50))

  fit <- fit_kernel_feature_map(df, method = "exact", kernel = "polynomial", degree = 2, const = 1)
  expect_equal(names(fit), c("method", "kernel", "kernel_params"))
  expect_message(fit_kernel_feature_map(df, method = "exact", kernel = "polynomial"),
                 "Using parameter degree = 2")
  expect_message(fit_kernel_feature_map(df, method = "exact", kernel = "polynomial"),
                 "Using parameter const = 1")



})

test_that("build_kernel_feature_map works for appropriate methods", {
  set.seed(8)
  df <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                 negative_dist = "mvnormal",
                                 remainder_dist = "mvnormal",
                                 ncov = 5,
                                 nbag = 7,
                                 nsample = 7,
                                 positive_degree = 3,
                                 positive_prob = 0.15,
                                 positive_mean = rep(0, 5))

  fit <- fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", m = 100, r = 50, sigma = 0.05)
  fm <- build_kernel_feature_map(fit, df[1:75, ])
  expect_equal(build_kernel_feature_map(fit, df[1:10, ]), build_kernel_feature_map(fit, df)[1:10, ])
  expect_equal(dim(fm), c(75, 50+3))

  fit <- fit_kernel_feature_map(df, method = "exact", kernel = "polynomial", degree = 2, const = 1)
  fm <- build_kernel_feature_map(fit, df[1:75, ])
  expect_equal(dim(fm), c(75, 20+3))

})


test_that("build_kernel_mean_map works for appropriate methods", {
  set.seed(8)
  df <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                 negative_dist = "mvnormal",
                                 remainder_dist = "mvnormal",
                                 ncov = 5,
                                 nbag = 7,
                                 nsample = 7,
                                 positive_degree = 3,
                                 positive_prob = 0.15,
                                 positive_mean = rep(0, 5))

  ## Nystrom, rbf
  set.seed(8)
  fit <- fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", m = 100, r = 50, sigma = 0.05)
  fm <- build_kernel_mean_map(fit, df)

  expect_equal(dim(fm), c(7*4, 50+3))
  expect_equal(fm$instance_name, unique(fm$instance_name))
  expect_equal(unique(fm$instance_name), unique(df$instance_name))

  set.seed(8)
  fm2 <- fit_kernel_feature_map(df, method = "nystrom", kernel = "rbf", output = "mean_map",
                                m = 100, r = 50, sigma = 0.05)
  expect_equal(fm, fm2)

  ## Exact, polynomial
  fit <- fit_kernel_feature_map(df, method = "exact", kernel = "polynomial", degree = 2, const = 1)
  fm <- build_kernel_mean_map(fit, df)
  expect_equal(dim(fm), c(7*4, 20+3))

})
