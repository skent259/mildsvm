context("Testing the functions in mildsvm.R")
suppressWarnings(library(dplyr))

set.seed(8)
mil_data <- generate_mild_df(positive_dist = "mvnormal",
                             negative_dist = "mvnormal",
                             remainder_dist = "mvnormal",
                             nbag = 10,
                             nsample = 5,
                             positive_mean = rep(2, 5))

mil_data_test <- generate_mild_df(positive_dist = "mvnormal",
                                  negative_dist = "mvnormal",
                                  remainder_dist = "mvnormal",
                                  nbag = 20,
                                  nsample = 5,
                                  positive_mean = rep(2, 5))

test_that("mildsvm() works for data-frame-like inputs", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  # mip method
  # df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
  mdl1 <- mildsvm.default(x = df1[, 4:13],
                          y = df1$bag_label,
                          bags = df1$bag_name,
                          instances = df1$instance_name,
                          method = "mip")

  expect_equal(
    predict(mdl1, new_data = df1, type = "class", layer = "bag"),
    predict(mdl1, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )
  expect_equal(
    predict(mdl1, new_data = df1, type = "class", layer = "bag"),
    predict(mdl1, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name, new_instances = df1$instance_name)
  )

  predict(mdl1, new_data = df1, type = "class", layer = "bag")
  predict(mdl1, new_data = df1, type = "class", layer = "instance")
  predict(mdl1, new_data = df1, type = "raw", layer = "bag")
  predict(mdl1, new_data = df1, type = "raw", layer = "instance")

  # heuristic method
  mdl2 <- mildsvm.default(x = df1[, 4:13],
                          y = df1$bag_label,
                          bags = df1$bag_name,
                          instances = df1$instance_name,
                          method = "heuristic")

  expect_equal(
    predict(mdl2, new_data = df1, type = "raw", layer = "bag"),
    predict(mdl2, new_data = df1, type = "raw", layer = "bag", new_bags = df1$bag_name)
  )
  expect_equal(
    predict(mdl2, new_data = df1, type = "raw", layer = "bag"),
    predict(mdl2, new_data = df1, type = "raw", layer = "bag", new_bags = df1$bag_name, new_instances = df1$instance_name)
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


test_that("mildsvm() works with formula method", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  # df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  mdl1 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = df1)
  mdl2 <- mildsvm.default(x = df1[, c("X1", "X2", "X3")],
                          y = df1$bag_label,
                          bags = df1$bag_name,
                          instances = df1$instance_name)

  expect_equal(mdl1$ksvm_fit, mdl2$ksvm_fit)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mildsvm.formula")
  expect_equal(mdl1$features, c("X1", "X2", "X3"))
  expect_equal(mdl1$bag_name, "bag_name")
  expect_equal(mdl1$instance_name, "instance_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
  predict(mdl1, df1, type = "raw")
  predict(mdl1, df1, type = "class")

  # check only 1 predictor works
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1, data = df1)
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  mdl1 <- misvm(mi(bag_label, bag_name) ~ 0 + X1:X2 + X2*X3, data = df1)
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + X1:X2 + X2*X3, data = df1)))
  predict(mdl1, df1, type = "raw")

  # check for mip method
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1 + X2 + X3, data = df1, method = "mip")

})

test_that("mildsvm() works with mild_df method", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  mdl1 <- mildsvm(df1)
  mdl2 <- mildsvm.default(x = df1[, 4:13],
                          y = df1$bag_label,
                          bags = df1$bag_name,
                          instances = df1$instance_name)

  expect_equal(mdl1$ksvm_fit, mdl2$ksvm_fit)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mildsvm.mild_df")
  expect_equal(mdl1$features, paste0("X", 1:10))
  expect_equal(mdl1$bag_name, "bag_name")
  expect_equal(mdl1$instance_name, "instance_name")

  predict(mdl1, new_data = df1)

})

test_that("predict.mildsvm returns labels that match the input labels", {
  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(class,
                  "default" = mildsvm(x = df[, 4:13],
                                      y = df$bag_label,
                                      bags = df$bag_name,
                                      instances = df$instance_name,
                                      method = method),
                  "formula" = mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2,
                                      data = df,
                                      method = method))
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 8,
                          nsample = 5,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))
  class(df1) <- "data.frame"

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
  expect_message(test_prediction_levels_equal(df2, method = "heuristic", class = "formula"))

  # check that 0/1 and 1/0 return the same predictions
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2, data = df2)
  mdl3 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2, data = df3)
  expect_equal(predict(mdl2, df2, type = "class"),
               predict(mdl3, df3, type = "class"))

})

test_that("Dots work in mildsvm() formula", {
  set.seed(8)
  mil_data <- generate_mild_df(positive_dist = 'mvt',
                               negative_dist = 'mvnormal',
                               remainder_dist = 'mvnormal',
                               nbag = 20,
                               nsample = 20,
                               positive_degree = 3,
                               positive_prob = 0.15,
                               positive_mean = rep(0, 5))
  mil_data2 <- mil_data %>% select(bag_label, bag_name, instance_name, X1, X2, X3)

  mildsvm_dot <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = mil_data2)
  mildsvm_nodot <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data2)

  expect_equal(mildsvm_dot$ksvm_fit, mildsvm_nodot$ksvm_fit)
  expect_equal(mildsvm_dot$features, mildsvm_nodot$features)
  expect_equal(mildsvm_dot$bag_name, mildsvm_nodot$bag_name)

  expect_equal(predict(mildsvm_dot, new_data = mil_data2), predict(mildsvm_nodot, new_data = mil_data2))

})

test_that("misvm() has correct argument handling", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  ## weights
  mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = TRUE)
  mdl1 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 1, "1" = 1))
  mdl1$weights <- NULL
  expect_equal(
    mdl1,
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = FALSE)
  )

  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  dimnames(df2) <- dimnames(df1)
  expect_equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1)),
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1))
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  tmp1 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  tmp2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1), method = "mip")
  expect_equal(tmp1, tmp2)

  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  dimnames(df2) <- dimnames(df1)
  expect_equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1))$ksvm_fit,
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1))$ksvm_fit
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  tmp1 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  tmp2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1), method = "mip")
  expect_equal(tmp1$gurobi_fit, tmp2$gurobi_fit)

  expect_false(isTRUE(all.equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")$gurobi_fit,
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "mip")$gurobi_fit
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 200, "1" = 1), method = "heuristic")$ksvm_fit,
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "heuristic")$ksvm_fit
  )))

  ## kernel
  # there isn't a "linear" kernel option for mildsvm
  expect_warning(expect_equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "heuristic", control = list(kernel = "radial")),
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "heuristic", control = list(kernel = "linear"))
  ))
  # TODO: try passing in the kernel as a matrix into this
  expect_warning(expect_false(isTRUE(all.equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "mip", control = list(kernel = "radial")),
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "mip", control = list(kernel = "linear"))
  ))))

  ## scale
  expect_false(isTRUE(all.equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "heuristic", control = list(scale = TRUE)),
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "heuristic", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "mip", control = list(scale = TRUE)),
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "mip", control = list(scale = FALSE))
  )))


  ## nystrom_args
  mdl <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, method = "mip",
                 control = list(nystrom_args = list(m = 100, r = 50)))

  expect_equal(length(mdl$gurobi_fit$w), 50)
  expect_equal(dim(mdl$kfm_fit$dv), c(50, 100))
  expect_equal(dim(mdl$kfm_fit$df_sub), c(100, ncol(df1) - 3))

  ## minimal arguments
  mildsvm.mild_df(df1)
  mildsvm.formula(mild(bag_label, bag_name, instance_name) ~ ., data = df1)
  mildsvm.default(df1[, 4:13], df1$bag_label, df1$bag_name, df1$instance_name)

})


test_that("mildsvm mip can warm start", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  verbose <- interactive()

  # manually check that the output says "User MIP start produced solution with objective ..."
  mdl1 <- mildsvm(x = df1[, 4:13] %>% as.data.frame(),
                  y = df1$bag_label,
                  bags = df1$bag_name,
                  instances = df1$instance_name,
                  method = "mip",
                  control = list(start = TRUE, verbose = verbose))

  mdl2 <- mildsvm(x = df1[, 4:13] %>% as.data.frame(),
                  y = df1$bag_label,
                  bags = df1$bag_name,
                  instances = df1$instance_name,
                  method = "mip",
                  control = list(start = FALSE, verbose = verbose))

  expect_equal(mdl1$gurobi_fit[c("b", "xi", "z")],
               mdl2$gurobi_fit[c("b", "xi", "z")])
  expect_equal(abs(mdl1$gurobi_fit$w), abs(mdl2$gurobi_fit$w))

  pred1 <- predict(mdl1, new_data = df1, type = "raw", layer = "instance")
  pred2 <- predict(mdl2, new_data = df1, type = "raw", layer = "instance")
  expect_equal(pred1, pred2)

  # Hard to test whether the warm start improves the time to reach a solution without testing large problems

})


test_that("mildsvm mip works with radial kernel", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  mdl1 <- mildsvm.default(x = df1[, 4:12],
                          y = df1$bag_label,
                          bags = df1$bag_name,
                          instances = df1$instance_name,
                          method = "mip",
                          control = list(kernel = "radial",
                                         sigma = 1))
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  predict(mdl1, new_data = df1, type = "class", layer = "bag")
  predict(mdl1, new_data = df1, type = "class", layer = "instance")
  predict(mdl1, new_data = df1, type = "raw", layer = "bag")
  predict(mdl1, new_data = df1, type = "raw", layer = "instance")

  expect_warning({
    mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3,
                    data = df1,
                    method = "mip",
                    control = list(kernel = "radial",
                                   sigma = 1))
  })
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  m <- 20
  r <- 10
  mdl2 <- mildsvm.default(x = df1[, 4:12],
                          y = df1$bag_label,
                          bags = df1$bag_name,
                          instances = df1$instance_name,
                          method = "mip",
                          control = list(kernel = "radial",
                                         sigma = 1,
                                         nystrom_args = list(m = m, r = r)))

  expect_equal(dim(mdl2$kfm_fit$dv), c(r, m))
  expect_equal(dim(mdl2$kfm_fit$df_sub), c(m, length(4:12)))

  # Running with linear kernel shouldn't have the kfm_fit element
  expect_warning({
    mdl1 <- mildsvm.default(x = df1[, 4:12],
                            y = df1$bag_label,
                            bags = df1$bag_name,
                            instances = df1$instance_name,
                            method = "mip",
                            control = list(kernel = "linear"))
  })
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

})

test_that("Passing kernel matrix into mildsvm works", {
  set.seed(8)
  df1 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 10,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  df2 <- generate_mild_df(positive_dist = 'mvt',
                          negative_dist = 'mvnormal',
                          remainder_dist = 'mvnormal',
                          nbag = 20,
                          nsample = 10,
                          positive_degree = 3,
                          positive_prob = 0.15,
                          positive_mean = rep(0, 5))

  df1 <- df1[sample(1:nrow(df1)), ]

  mdl1 <- mildsvm(df1, control = list(kernel = kme(df1, sigma = 0.05), sigma = 0.05))
  pred1 <- predict(mdl1, new_data = df2, type = "raw", kernel = kme(df2, df1, sigma = 0.05))

  mdl2 <- mildsvm(df1, control = list(sigma = 0.05, scale = FALSE))
  pred2 <- predict(mdl2, new_data = df2, type = "raw")

  expect_equal(mdl1, mdl2)
  expect_equal(pred1, pred2)

})

test_that("Re-ordering data doesn't reduce performance", {

  set.seed(8)
  mdl1 <- mildsvm(mil_data, control = list(sigma = 0.1))
  mdl2 <- mildsvm(mil_data[sample(1:nrow(mil_data)), ], control = list(sigma = 0.1))

  pred1 <- predict(mdl1, mil_data_test, type = "raw")
  pred2 <- predict(mdl2, mil_data_test, type = "raw")

  auc1 <- with(mil_data_test,
               pROC::auc(response = classify_bags(bag_label, bag_name),
                         predictor = classify_bags(pred1$.pred, bag_name)))
  auc2 <- with(mil_data_test,
               pROC::auc(response = classify_bags(bag_label, bag_name),
                         predictor = classify_bags(pred2$.pred, bag_name)))

  # the auc2 should be in the neighborhood of auc1
  auc1; auc2
  eps <- 0.01
  expect_gte(auc2, auc1 - eps)
  expect_lte(auc2, auc1 + eps)
})

test_that("`mildsvm()` value returns make sense", {

  # different methods
  names(mildsvm(mil_data, method = "heuristic"))
  names(mildsvm(mil_data, method = "mip", control = list(nystrom_args = list(m = 10))))

  # # different S3 methods
  names(mildsvm(x = as.data.frame(mil_data[, 4:13]),
                y = mil_data$bag_label,
                bags = mil_data$bag_name,
                instances = mil_data$instance_name))
  names(mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = mil_data))
  names(mildsvm(mil_data))

  # shouldn't have `x_scale`
  names(mildsvm(mil_data, method = "heuristic", control = list(scale = FALSE)))
  names(mildsvm(mil_data, method = "mip", control = list(scale = FALSE, nystrom_args = list(m = 10))))

  # shouldn't have `weights`
  names(mildsvm(mil_data, method = "heuristic", weights = FALSE))
  expect_true(TRUE)
})

test_that("`predict.mildsvm()` works without new_data", {

  mdl1 <- mildsvm(mil_data, method = "heuristic",
                  control = list(scale = FALSE, sigma = 1/10))

  pred1 <- predict(mdl1, mil_data_test, type = "raw", layer = "instance")
  pred2 <- predict(mdl1, NULL, "raw", "instance",
                   new_bags = mil_data_test$bag_label,
                   new_instances = mil_data_test$instance_name,
                   kernel = kme(mil_data_test, mil_data, sigma = 1/10))

  expect_equal(pred1, pred2)
})

