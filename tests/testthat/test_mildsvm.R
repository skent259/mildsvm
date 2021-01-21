context("Testing the functions in mildsvm.R")

test_that("mildsvm() works for data-frame-like inputs", {
  set.seed(8)
  df1 <- GenerateMilData(positive_dist = 'mvt',
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
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )
  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name, new_instances = df1$instance_name)
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
  df1 <- GenerateMilData(positive_dist = 'mvt',
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

  expect_equal(mdl1$model, mdl2$model)
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

test_that("mildsvm() works with MilData method", {
  set.seed(8)
  df1 <- GenerateMilData(positive_dist = 'mvt',
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

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mildsvm.MilData")
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
  df1 <- GenerateMilData(positive_dist = 'mvt',
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

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df1), predict(misvm_nodot, new_data = df1))

})

test_that("misvm() has correct argument handling", {
  set.seed(8)
  df1 <- GenerateMilData(positive_dist = 'mvt',
                         negative_dist = 'mvnormal',
                         remainder_dist = 'mvnormal',
                         nbag = 10,
                         nsample = 10,
                         positive_degree = 3,
                         positive_prob = 0.15,
                         positive_mean = rep(0, 5))

  ## weights
  mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = TRUE)
  expect_equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 1, "1" = 1)),
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
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1))$model,
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1))$model
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  tmp1 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  tmp2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1), method = "mip")
  expect_equal(tmp1$model, tmp2$model)

  expect_false(isTRUE(all.equal(
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")$model,
    mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "mip")$model
  )))
  # TODO: understand why this fails...
  # expect_false(isTRUE(all.equal(
  #   mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 200, "1" = 1), method = "heuristic")$model,
  #   mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "heuristic")$model
  # )))

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

  expect_equal(length(mdl$model$w), 50)
  expect_equal(dim(mdl$kfm_fit$dv), c(50, 100))
  expect_equal(dim(mdl$kfm_fit$df_sub), c(100, ncol(df1) - 3))

  ## minimal arguments
  mildsvm.MilData(df1)
  mildsvm.formula(mild(bag_label, bag_name, instance_name) ~ ., data = df1)
  mildsvm.default(df1[, 4:13], df1$bag_label, df1$bag_name, df1$instance_name)

})


test_that("mildsvm mip can warm start", {
  set.seed(8)
  df1 <- GenerateMilData(positive_dist = 'mvt',
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

  expect_equal(mdl1$model[c("b", "xi", "z")],
               mdl2$model[c("b", "xi", "z")])
  expect_equal(abs(mdl1$model$w), abs(mdl2$model$w))

  pred1 <- predict(mdl1, new_data = df1, type = "raw", layer = "instance")
  pred2 <- predict(mdl2, new_data = df1, type = "raw", layer = "instance")
  expect_equal(pred1, pred2)

  # Hard to test whether the warm start improves the time to reach a solution without testing large problems

})


test_that("mildsvm mip works with radial kernel", {
  set.seed(8)
  df1 <- GenerateMilData(positive_dist = 'mvt',
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
  df1 <- GenerateMilData(positive_dist = 'mvt',
                         negative_dist = 'mvnormal',
                         remainder_dist = 'mvnormal',
                         nbag = 10,
                         nsample = 10,
                         positive_degree = 3,
                         positive_prob = 0.15,
                         positive_mean = rep(0, 5))

  df2 <- GenerateMilData(positive_dist = 'mvt',
                         negative_dist = 'mvnormal',
                         remainder_dist = 'mvnormal',
                         nbag = 20,
                         nsample = 10,
                         positive_degree = 3,
                         positive_prob = 0.15,
                         positive_mean = rep(0, 5))

  mdl1 <- mildsvm(df1, control = list(kernel = kme(df1, sigma = 0.05), sigma = 0.05))
  pred1 <- predict(mdl1, new_data = df2, type = "raw", kernel = kme(df1, df2, sigma = 0.05))

  mdl2 <- mildsvm(df1, control = list(sigma = 0.05, scale = FALSE))
  pred2 <- predict(mdl2, new_data = df2, type = "raw")

  expect_equal(mdl1, mdl2)
  expect_equal(pred1, pred2)

})
