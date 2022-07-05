
## MilData set to work with
mil_df <- generate_mild_df(nbag = 10,
                           nsample = 20,
                           positive_prob = 0.15,
                           nimp_pos = 1:5, nimp_neg = 1:5,
                           dist = rep("mvnormal", 3),
                           mean = list(rep(4, 5), rep(0, 5), 0))
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
