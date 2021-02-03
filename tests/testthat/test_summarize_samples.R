context("Testing the functions in summarize_samples.R")

## MilData set to work with
mil_df <- GenerateMilData(positive_dist = 'mvnormal',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 20,
                          positive_prob = 0.15,
                          positive_mean = rep(4, 5))
k <- ncol(mil_df) - 3 # number of features in mil_df

test_that("summarize_samples() works with various `.fns` inputs", {
  # default method
  s <- summarize_samples(mtcars, group_cols = "cyl", .fns = list(mean = mean, sd = sd, qtl25 = ~quantile(.x, 0.25)))
  expect_equal(colnames(s)[1], "cyl")
  expect_equal(nrow(s), length(unique(mtcars$cyl)))
  expect_equal(ncol(s), 1 + 3*10)

  # MilData method
  s <- summarize_samples(mil_df)
  expect_equal(colnames(s)[1:3], c("bag_label", "bag_name", "instance_name"))
  expect_equal(nrow(s), length(unique(mil_df$instance_name)))
  expect_equal(ncol(s), 3 + 1*k)

  s <- summarize_samples(mil_df, .fns = list(mean = mean, sd = sd, qtl25 = ~quantile(.x, 0.25)))
  expect_equal(nrow(s), length(unique(mil_df$instance_name)))
  expect_equal(ncol(s), 3 + 3*k)

})

test_that("summarize_samples() works with `cor` = TRUE", {
  # default method
  s <- summarize_samples(mtcars, group_cols = "cyl", cor = TRUE)
  expect_equal(nrow(s), length(unique(mtcars$cyl)))
  expect_equal(ncol(s), 56)

  # MilData method
  s <- summarize_samples(mil_df, cor = TRUE)
  expect_equal(nrow(s), length(unique(mil_df$instance_name)))
  expect_equal(ncol(s), 3 + k + choose(k, 2))

})
