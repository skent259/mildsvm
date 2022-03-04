test_that("select_cv_folds2() is the same as select_cv_folds()", {
  set.seed(8)
  mil_data <- generate_mild_df(nbag = 20,
                               nsample = 20,
                               positive_prob = 0.15)

  ## Data at the instance level
  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
  set.seed(9)
  fold1 <- select_cv_folds(df1, n_fold = 5)
  set.seed(9)
  fold2 <- select_cv_folds2(df1$bag_label, df1$bag_name, n_fold = 5)

  expect_equal(fold1, fold2)

  ## NOTE: this doesn't work for data at the sample level...the following code
  ## will fail
  # set.seed(9)
  # fold1 <- select_cv_folds(mil_data, n_fold = 5)
  # set.seed(9)
  # fold2 <- select_cv_folds2(mil_data$bag_label, mil_data$bag_name, n_fold = 5)
  #
  # expect_equal(fold1, fold2)

})


test_that("`classify_bags()` works quickly on large input", {

  nbag <- 50
  n_inst <- 5000
  bags <- paste0("bag", 1:nbag)
  bag_name <- rep(bags, each = n_inst)
  bag_name <- sample(bag_name)
  y <- rnorm(length(bag_name))

  tm <- system.time({
    classify_bags(y, bag_name, condense = FALSE)
  })

  expect_lt(tm["elapsed"], 3)

})

test_that("`classify_bags()` works on integer bag input", {

  set.seed(8)
  scores <- 1:20
  bags <- rep(c(1, 4, 6, 9, 2), each = 4)
  correct <- seq(4, 20, length.out = 5)

  expect_equivalent(classify_bags(scores, bags),
                    correct)
  expect_equivalent(classify_bags(scores, bags, condense = FALSE),
                    rep(correct, each = 4))

  bags <- rep(c("a", "b", "d", "c", "e"), each = 4)
  expect_equivalent(classify_bags(scores, bags),
                    correct)
  expect_equivalent(classify_bags(scores, bags, condense = FALSE),
                    rep(correct, each = 4))

})





