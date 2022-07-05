test_that("Nystrom method approximates the true kernel on a dataframe.", {
  check_nystrom_approximation <- function(fit, df, max_thresh, mean_thresh) {
    fm <- build_fm(fit, df)
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
  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "radial", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1e-14, 1e-15)
  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "radial", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1e-14, 1e-15)

  ## RBF kernel, smaller feature map
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1, 1e-3)
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1, 0.05)

})

test_that("Nystrom method approximates the true kernel on a `mild_df` object", {
  check_nystrom_approximation <- function(fit, df, max_thresh, mean_thresh) {
    X <- subset(df, select = -c(bag_label, bag_name, instance_name)) %>%
      suppressWarnings()
    fm <- build_fm(fit, df)
    fm <- as.matrix(subset(fm, select = -c(bag_label, bag_name, instance_name)))

    true_kernel <- rbf_kernel_matrix(fit$kernel_params$sigma, as.matrix(X), as.matrix(X))
    approximate_kernel <- fm %*% t(fm)

    expect_lt(max(abs(true_kernel - approximate_kernel)), max_thresh)
    expect_lt(mean(abs(true_kernel - approximate_kernel)), mean_thresh)
  }

  set.seed(8)
  df <- mildsvm::generate_mild_df(ncov = 5,
                                  nbag = 7,
                                  nsample = 7,
                                  positive_prob = 0.15)

  set.seed(8)
  ## RBF kernel, full feature map
  fit <- kfm_nystrom(df, m = 196, r = 196, kernel = "radial", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1e-13, 1e-14)
  fit <- kfm_nystrom(df, m = 196, r = 196, kernel = "radial", sigma = 0.5)
  check_nystrom_approximation(fit, df, 1e-13, 1e-14)

  ## RBF kernel, smaller feature map
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.05)
  check_nystrom_approximation(fit, df, 1, 0.05)
  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.5)
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

  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "radial", sigma = 0.05)
  fm <- build_fm(fit, df)
  expect_equal(dim(fm), c(7,7))

  fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.05)
  fm <- build_fm(fit, df)
  expect_equal(dim(fm), c(7,6))
  fm <- build_fm(fit, df[1:3, ])
  expect_equal(dim(fm), c(3,6))

  fit <- kfm_nystrom(df, m = 5, r = 3, kernel = "radial", sigma = 0.05)
  fm <- build_fm(fit, df)
  expect_equal(dim(fm), c(7,3))

  ## test Nystrom on MilData
  mil_data <- mildsvm::generate_mild_df(ncov = 5,
                                        nbag = 7,
                                        nsample = 7,
                                        positive_prob = 0.15)

  fit <- kfm_nystrom(mil_data, m = nrow(mil_data), r = nrow(mil_data), kernel = "radial", sigma = 0.05)
  fm <- build_fm(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), nrow(mil_data)+3))
  fm <- build_fm(fit, mil_data[1:13, ])
  expect_equal(dim(fm), c(13, nrow(mil_data)+3))

  fit <- kfm_nystrom(mil_data, m = 7, r = 7, kernel = "radial", sigma = 0.05)
  fm <- build_fm(fit, mil_data)
  expect_equal(dim(fm), c(nrow(mil_data), 7+3))

  fit <- kfm_nystrom(mil_data, m = 7, r = 3, kernel = "radial", sigma = 0.05)
  fm <- build_fm(fit, mil_data)
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

  fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "radial", sampling = 1:7, sigma = 0.05)
  expect_equal(fit$df_sub, as.matrix(df[1:7, ]), ignore_attr = TRUE)

  expect_warning({
    fit <- kfm_nystrom(df, m = 7, r = 7, kernel = "radial", sampling = 1:6, sigma = 0.05)
  })

  ## test Nystrom on MilData
  mil_data <- mildsvm::generate_mild_df(ncov = 5,
                                        nbag = 7,
                                        nsample = 7,
                                        positive_prob = 0.15)


  fit <- kfm_nystrom(mil_data, m = 50, r = 50,
                     kernel = "radial", sampling = 'stratified', sigma = 0.05)

  rows <- as.numeric(rownames(fit$df_sub))
  expect(length(unique(table(mil_data$bag_name[rows]))) %in% c(1,2),
         "Expect counts for each bag to be the same (+/- 1)")
  expect(length(unique(table(mil_data$instance_name[rows]))) %in% c(1,2),
         "Expect counts for each instance to be the same (+/- 1)")
  expect(all(unique(rows) == rows), "Rows are sampled at most once")

  expect_warning({
    fit <- kfm_nystrom(mil_data, m = 50, r = 50, kernel = "radial", sampling = 1:10, sigma = 0.05)
  })

  fit <- kfm_nystrom(mil_data, m = 10, r = 10, kernel = "radial", sampling = 1:10, sigma = 0.05)
  expect_equal(fit$df_sub, as.matrix(mil_data[1:10, -c(1:3)]), ignore_attr = TRUE) %>%
    expect_warning()
  fit <- kfm_nystrom(mil_data, m = 10, r = 10, kernel = "radial", sampling = "random", sigma = 0.05)

})

test_that("Stratified sampling works with bag structure", {
  set.seed(8)
  df <- mildsvm::generate_mild_df(ncov = 5,
                                  nbag = 7,
                                  nsample = 7,
                                  positive_prob = 0.15)


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


  df <- mildsvm::generate_mild_df(positive_dist = "mvnormal",
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
    fit <- kfm_nystrom(df, m = 8, r = 8, kernel = "radial", sampling = 1:8, sigma = 0.05)
  })
  fm <- build_fm(fit, df)
  expect_equal(dim(fm), c(8,7))

})

test_that("Nystrom feature map prints correctly", {
  df <- data.frame(
    X1 = c(2,   3,   4,   5,   6, 7, 8),
    X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
    X3 = rnorm(7)
  )
  set.seed(8)
  df2 <- mildsvm::generate_mild_df(ncov = 5,
                                  nbag = 7,
                                  nsample = 7,
                                  positive_prob = 0.15)

  expect_snapshot({
    kfms <- list(
      "default" = kfm_nystrom(df),
      "supplied_sample" = kfm_nystrom(df, sampling = 1:7),
      "stratified_sample" = kfm_nystrom(df2, sampling = "stratified"),
      "low m" = kfm_nystrom(df, m = 5),
      "low r" = kfm_nystrom(df, r = 5),
      "sigma" = kfm_nystrom(df, sigma = 0.05)
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(kfms)
  })
  expect_true(TRUE)
})

