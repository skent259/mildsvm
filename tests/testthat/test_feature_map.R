context("Testing the functions in bkfm_functions.R")

test_that("Nystrom method approximates the true kernel on a dataframe.", {
  check_nystrom_approximation <- function(fit, df, max_thresh, mean_thresh) {
    fm <- predict_kfm_nystrom(fit, df)
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
  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1e-14, 1e-15)
  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1e-14, 1e-15)

  ## RBF kernel, smaller feature map
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1, 1e-3)
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1, 0.05)

})

test_that("Nystrom method approximates the true kernel on a MilData object", {
  check_nystrom_approximation <- function(fit, df, max_thresh, mean_thresh) {
    X <- subset(df, select = -c(bag_label, bag_name, instance_name))
    fm <- predict_kfm_nystrom(fit, df)
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
  fit <- kfm_nystrom(df, m = 196, r = 196, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1e-13, 1e-14)
  fit <- kfm_nystrom(df, m = 196, r = 196, kernel = "rbf", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1e-13, 1e-14)

  ## RBF kernel, smaller feature map
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1, 0.05)
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.5)
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

  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "rbf", sigma = 0.05)
  fm <- predict_kfm_nystrom(fit, df)
  expect_equal(dim(fm), c(7,7))

  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "rbf", sigma = 0.05)
  fm <- predict_kfm_nystrom(fit, df)
  expect_equal(dim(fm), c(7,6))
  fm <- predict_kfm_nystrom(fit, df[1:3, ])
  expect_equal(dim(fm), c(3,6))

  fit <- kfm_nystrom(df, m = 5, r = 3, kernel = "rbf", sigma = 0.05)
  fm <- predict_kfm_nystrom(fit, df)
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

  fit <- kfm_nystrom(mil_data, m = nrow(mil_data), r = nrow(mil_data), kernel = "rbf", sigma = 0.05)
  fm <- predict_kfm_nystrom(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), nrow(mil_data)+3))
  fm <- predict_kfm_nystrom(fit, mil_data[1:13, ])
  expect_equal(dim(fm), c(13, nrow(mil_data)+3))

  fit <- kfm_nystrom(mil_data, m = 7, r = 7, kernel = "rbf", sigma = 0.05)
  fm <- predict_kfm_nystrom(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), 7+3))

  fit <- kfm_nystrom(mil_data, m = 7, r = 3, kernel = "rbf", sigma = 0.05)
  fm <- predict_kfm_nystrom(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), 3+3))

})

test_that("Nystrom method works with various sampling parameters", {

  ## test Nystrom on data frame
  set.seed(8)
  df <- data.frame(
    X1 = c(2,   3,   4,   5,   6, 7, 8),
    X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
    X3 = rnorm(7)
  )

  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "rbf", sampling = 1:7, sigma = 0.05)
  expect_equivalent(fit$df_sub, as.matrix(df[1:7, ]))

  expect_warning({
    fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "rbf", sampling = 1:6, sigma = 0.05)
  })

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


  fit <- kfm_nystrom(mil_data, m = 50, r = 50,
                     kernel = "rbf", sampling = 'stratified', sigma = 0.05)

  rows <- as.numeric(rownames(fit$df_sub))
  expect(length(unique(table(mil_data$bag_name[rows]))) %in% c(1,2),
         "Expect counts for each bag to be the same (+/- 1)")
  expect(length(unique(table(mil_data$instance_name[rows]))) %in% c(1,2),
         "Expect counts for each instance to be the same (+/- 1)")
  expect(all(unique(rows) == rows), "Rows are sampled at most once")

  expect_warning({
    fit <- kfm_nystrom(mil_data, m = 50, r = 50, kernel = "rbf", sampling = 1:10, sigma = 0.05)
  })

  fit <- kfm_nystrom(mil_data, m = 10, r = 10, kernel = "rbf", sampling = 1:10, sigma = 0.05)
  expect_equivalent(fit$df_sub, as.matrix(mil_data[1:10, -c(1:3)]))

  fit <- kfm_nystrom(mil_data, m = 10, r = 10, kernel = "rbf", sampling = "random", sigma = 0.05)

})

test_that("Stratified sampling works with bag structure", {
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


  rows <- bag_instance_sampling(df, size = 10)
  expect(length(unique(table(df$bag_name[rows]))) %in% c(1,2),
         "Expect counts for each bag to be the same (+/- 1)")
  expect(length(unique(table(df$instance_name[rows]))) %in% c(1,2),
         "Expect counts for each instance to be the same (+/- 1)")
  expect(all(unique(rows) == rows), "Rows are sampled at most once")

  rows <- bag_instance_sampling(df, size = 50)
  expect(length(unique(table(df$bag_name[rows]))) %in% c(1,2),
         "Expect counts for each bag to be the same (+/- 1)")
  expect(length(unique(table(df$instance_name[rows]))) %in% c(1,2),
         "Expect counts for each instance to be the same (+/- 1)")
  expect(all(unique(rows) == rows), "Rows are sampled at most once")


  rows <- bag_instance_sampling(df, size = 196)
  expect(length(unique(table(df$bag_name[rows]))) %in% c(1,2),
         "Expect counts for each bag to be the same (+/- 1)")
  expect(length(unique(table(df$instance_name[rows]))) %in% c(1,2),
         "Expect counts for each instance to be the same (+/- 1)")
  expect(all(unique(rows) == rows), "Rows are sampled at most once")


  df <- mildsvm::GenerateMilData(positive_dist = "mvnormal",
                                 negative_dist = "mvnormal",
                                 remainder_dist = "mvnormal",
                                 nbag = 2,
                                 ninst = 2,
                                 nsample = 4)
  df <- df[-c(1:3), ]

  rows <- bag_instance_sampling(df, size = 13)
  expect(length(unique(table(df$bag_name[rows]))) %in% c(1,2),
         "Expect counts for each bag to be the same (+/- 1)")
  expect(length(unique(table(df$instance_name[rows]))) %in% c(1,2),
         "Expect counts for each instance to be the same (+/- 1)")
  # in this case, not all rows will be unique, as row 1 gets sampled multiple times



  # how to do stratified sampling

  # how to incorporate into kfm_nystrom function
  # - add parameter 'random_rows' to override variable in kfm_nystrom.default
  # - add parameter 'sampling' which indicates which sampling method to use
  #   - sampling = 'random' or sampling = 'stratified'

})

test_that("Nystrom sampling works with duplicated data", {
  set.seed(8)
  df <- data.frame(
    X1 = c(2,   3,   4,   5,   6, 7, 8),
    X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
    X3 = rnorm(7)
  )
  df <- rbind(df, df[1, ])

  expect_warning({
    fit <- kfm_nystrom(df, m = 8, r = 8, kernel = "rbf", sampling = 1:8, sigma = 0.05)
  })
  fm <- predict_kfm_nystrom(fit, df)
  expect_equal(dim(fm), c(8,7))

})
