suppressWarnings(library(dplyr))


set.seed(8)
mil_data <- generate_mild_df(nbag = 10,
                             nsample = 5,
                             ninst = 3,
                             nimp_pos = 1:5, nimp_neg = 1:5,
                             dist = rep("mvnormal", 3),
                             mean = list(rep(15, 5), rep(0, 5), 0))

mil_data_test <- generate_mild_df(nbag = 20,
                                  nsample = 5,
                                  ninst = 3,
                                  nimp_pos = 1:5, nimp_neg = 1:5,
                                  dist = rep("mvnormal", 3),
                                  mean = list(rep(15, 5), rep(0, 5), 0))

test_that("mildsvm() works for data-frame-like inputs", {
  skip_if_no_gurobi()
  # mip method
  # df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
  mdl1 <- mildsvm.default(x = mil_data[, 4:13],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name,
                          method = "mip")

  expect_equal(
    predict(mdl1, new_data = mil_data, type = "class", layer = "bag"),
    predict(mdl1, new_data = mil_data, type = "class", layer = "bag", new_bags = mil_data$bag_name)
  )
  expect_equal(
    predict(mdl1, new_data = mil_data, type = "class", layer = "bag"),
    predict(mdl1, new_data = mil_data, type = "class", layer = "bag",
            new_bags = mil_data$bag_name, new_instances = mil_data$instance_name)
  )

  predict(mdl1, new_data = mil_data, type = "class", layer = "bag")
  predict(mdl1, new_data = mil_data, type = "class", layer = "instance")
  predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")
  predict(mdl1, new_data = mil_data, type = "raw", layer = "instance")

  # heuristic method
  mdl2 <- mildsvm.default(x = mil_data[, 4:13],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name,
                          method = "heuristic")

  expect_equal(
    predict(mdl2, new_data = mil_data, type = "raw", layer = "bag"),
    predict(mdl2, new_data = mil_data, type = "raw", layer = "bag", new_bags = mil_data$bag_name)
  )
  expect_equal(
    predict(mdl2, new_data = mil_data, type = "raw", layer = "bag"),
    predict(mdl2, new_data = mil_data, type = "raw", layer = "bag", new_bags = mil_data$bag_name, new_instances = mil_data$instance_name)
  )

  predict(mdl2, new_data = mil_data, type = "class", layer = "bag")
  predict(mdl2, new_data = mil_data, type = "class", layer = "instance")
  predict(mdl2, new_data = mil_data, type = "raw", layer = "bag")
  predict(mdl2, new_data = mil_data, type = "raw", layer = "instance")

  bag_preds <-
    mil_data %>%
    bind_cols(predict(mdl2, mil_data, type = "class")) %>%
    group_by(bag_name) %>%
    summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(mil_data$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(mil_data$bag_name))

  # qp-heuristic method
  mdl3 <- mildsvm.default(x = mil_data[, 4:13],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name,
                          method = "qp-heuristic")

  expect_equal(
    predict(mdl3, new_data = mil_data, type = "raw", layer = "bag"),
    predict(mdl3, new_data = mil_data, type = "raw", layer = "bag", new_bags = mil_data$bag_name)
  )
  expect_equal(
    predict(mdl3, new_data = mil_data, type = "raw", layer = "bag"),
    predict(mdl3, new_data = mil_data, type = "raw", layer = "bag",
            new_bags = mil_data$bag_name, new_instances = mil_data$instance_name)
  )

  predict(mdl3, new_data = mil_data, type = "class", layer = "bag")
  predict(mdl3, new_data = mil_data, type = "class", layer = "instance")
  predict(mdl3, new_data = mil_data, type = "raw", layer = "bag")
  predict(mdl3, new_data = mil_data, type = "raw", layer = "instance")

  # manually check the bag level predictions
  bag_level_pred <- function(object, df) {
    df %>%
      bind_cols(predict(object, df, type = "class")) %>%
      bind_cols(predict(object, df, type = "raw")) %>%
      distinct(bag_name, bag_label, .pred, .pred_class)
  }

  bag_level_pred(mdl1, mil_data)
  bag_level_pred(mdl2, mil_data)
  bag_level_pred(mdl3, mil_data)
  bag_level_pred(mdl3, mil_data_test)
  # with(bag_level_pred(mdl3, mil_data_test), plot(.pred, bag_label))

})


test_that("mildsvm() works with formula method", {
  skip_if_no_gurobi()
  mdl1 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data)
  mdl2 <- mildsvm.default(x = mil_data[, c("X1", "X2", "X3")],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name)

  expect_equal(mdl1$ksvm_fit, mdl2$ksvm_fit)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mildsvm.formula")
  expect_equal(mdl1$features, c("X1", "X2", "X3"))
  expect_equal(mdl1$bag_name, "bag_name")
  expect_equal(mdl1$instance_name, "instance_name")

  # predictions should match
  expect_equal(predict(mdl1, mil_data, type = "raw"), predict(mdl2, mil_data, type = "raw"))
  expect_equal(predict(mdl1, mil_data, type = "class"), predict(mdl2, mil_data, type = "class"))
  predict(mdl1, mil_data, type = "raw")
  predict(mdl1, mil_data, type = "class")

  # check only 1 predictor works
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1, data = mil_data)
  predict(mdl1, mil_data, type = "raw")

  # check some obscure formulas
  mdl1 <- misvm(mi(bag_label, bag_name) ~ 0 + X1:X2 + X2*X3, data = mil_data)
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + X1:X2 + X2*X3, data = mil_data)))
  predict(mdl1, mil_data, type = "raw")

  # check for mip method
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1 + X2 + X3, data = mil_data, method = "mip")

  # check for qp-heuristic method
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1 + X2 + X3, data = mil_data, method = "qp-heuristic")
})

test_that("mildsvm() works with mild_df method", {
  skip_if_no_gurobi()
  mdl1 <- mildsvm(mil_data)
  mdl2 <- mildsvm.default(x = mil_data[, 4:13],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name)

  expect_equal(mdl1$ksvm_fit, mdl2$ksvm_fit)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mildsvm.mild_df")
  expect_equal(mdl1$features, paste0("X", 1:10))
  expect_equal(mdl1$bag_name, "bag_name")
  expect_equal(mdl1$instance_name, "instance_name")

  predict(mdl1, new_data = mil_data)


  mdl1 <- mildsvm(mil_data, method = "qp-heuristic")

})

test_that("predict.mildsvm returns labels that match the input labels", {
  skip_if_no_gurobi()
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

  # 0/1
  df1 <- mil_data %>% as.data.frame() %>% mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df1, method = "heuristic")
  test_prediction_levels_equal(df1, method = "mip")
  test_prediction_levels_equal(df1, method = "qp-heuristic")
  test_prediction_levels_equal(df1, method = "heuristic", class = "formula")

  # 1/0
  df1 <- mil_data %>% as.data.frame() %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  test_prediction_levels_equal(df1, method = "heuristic")
  test_prediction_levels_equal(df1, method = "mip")
  test_prediction_levels_equal(df1, method = "qp-heuristic")
  test_prediction_levels_equal(df1, method = "heuristic", class = "formula")

  # TRUE/FALSE
  df1 <- mil_data %>% as.data.frame() %>% mutate(bag_label = factor(bag_label, labels = c(TRUE, FALSE)))
  test_prediction_levels_equal(df1, method = "heuristic")
  test_prediction_levels_equal(df1, method = "mip")
  test_prediction_levels_equal(df1, method = "qp-heuristic")
  test_prediction_levels_equal(df1, method = "heuristic", class = "formula")

  # Yes/No
  df1 <- mil_data %>% as.data.frame() %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  expect_message(test_prediction_levels_equal(df1, method = "heuristic"))
  expect_message(test_prediction_levels_equal(df1, method = "mip"))
  test_prediction_levels_equal(df1, method = "qp-heuristic")
  expect_message(test_prediction_levels_equal(df1, method = "heuristic", class = "formula"))

  # check that 0/1 and 1/0 return the same predictions
  df1 <- mil_data %>% as.data.frame() %>% mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- mil_data %>% as.data.frame() %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2, data = df1)
  mdl3 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2, data = df3)
  expect_equal(predict(mdl2, df1, type = "class"),
               predict(mdl3, df3, type = "class"))

})

test_that("Dots work in mildsvm() formula", {
  skip_if_no_gurobi()
  mil_data2 <- mil_data %>% select(bag_label, bag_name, instance_name, X1, X2, X3)

  mildsvm_dot <- mildsvm(mild(bag_label, bag_name, instance_name) ~ ., data = mil_data2)
  mildsvm_nodot <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data2)

  expect_equal(mildsvm_dot$ksvm_fit, mildsvm_nodot$ksvm_fit)
  expect_equal(mildsvm_dot$features, mildsvm_nodot$features)
  expect_equal(mildsvm_dot$bag_name, mildsvm_nodot$bag_name)

  expect_equal(predict(mildsvm_dot, new_data = mil_data2), predict(mildsvm_nodot, new_data = mil_data2))

})

test_that("mildsvm() has correct argument handling", {
  skip_if_no_gurobi()
  ## weights
  mildsvm(mil_data, weights = TRUE)
  mdl1 <- mildsvm(mil_data, weights = c("0" = 1, "1" = 1))
  mdl1$weights <- NULL
  expect_equal(
    mdl1,
    mildsvm(mil_data, weights = FALSE)
  )

  mil_data_test <- mil_data %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  dimnames(mil_data_test) <- dimnames(mil_data)
  expect_equal(
    mildsvm(mil_data, weights = c("0" = 2, "1" = 1)),
    mildsvm(mil_data_test, weights = c("0" = 2, "1" = 1))
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  tmp1 <- mildsvm(mil_data, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  tmp2 <- mildsvm(mil_data_test, weights = c("0" = 2, "1" = 1), method = "mip")
  expect_equal(tmp1, tmp2)

  mil_data_test <- mil_data %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  dimnames(mil_data_test) <- dimnames(mil_data)
  expect_equal(
    mildsvm(mil_data, weights = c("0" = 2, "1" = 1))$ksvm_fit,
    mildsvm(mil_data_test, weights = c("No" = 2, "Yes" = 1))$ksvm_fit
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  tmp1 <- mildsvm(mil_data, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  tmp2 <- mildsvm(mil_data_test, weights = c("No" = 2, "Yes" = 1), method = "mip")
  expect_equal(tmp1$gurobi_fit, tmp2$gurobi_fit)

  expect_false(isTRUE(all.equal(
    mildsvm(mil_data, weights = c("0" = 2, "1" = 1), method = "mip")$gurobi_fit,
    mildsvm(mil_data, weights = c("0" = 1e-6, "1" = 1), method = "mip")$gurobi_fit
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mil_data, weights = c("0" = 200, "1" = 1), method = "heuristic")$ksvm_fit,
    mildsvm(mil_data, weights = c("0" = 1e-6, "1" = 1), method = "heuristic")$ksvm_fit
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mil_data, weights = c("0" = 200, "1" = 1), method = "qp-heuristic")$gurobi_fit,
    mildsvm(mil_data, weights = c("0" = 1e-6, "1" = 1), method = "qp-heuristic")$gurobi_fit
  )))

  ## kernel
  # there isn't a "linear" kernel option for mildsvm
  expect_warning(expect_equal(
    mildsvm(mil_data, method = "heuristic", control = list(kernel = "radial")),
    mildsvm(mil_data, method = "heuristic", control = list(kernel = "linear"))
  ))
  # TODO: try passing in the kernel as a matrix into this
  expect_warning(expect_false(isTRUE(all.equal(
    mildsvm(mil_data, method = "mip", control = list(kernel = "radial")),
    mildsvm(mil_data, method = "mip", control = list(kernel = "linear"))
  ))))
  expect_warning(expect_false(isTRUE(all.equal(
    mildsvm(mil_data, method = "qp-heuristic", control = list(kernel = "radial")),
    mildsvm(mil_data, method = "qp-heuristic", control = list(kernel = "linear"))
  ))))

  ## scale
  expect_false(isTRUE(all.equal(
    mildsvm(mil_data,  method = "heuristic", control = list(scale = TRUE)),
    mildsvm(mil_data, method = "heuristic", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mil_data, method = "mip", control = list(scale = TRUE)),
    mildsvm(mil_data, method = "mip", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mil_data, method = "qp-heuristic", control = list(scale = TRUE)),
    mildsvm(mil_data, method = "qp-heuristic", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mildsvm(mil_data, method = "qp-heuristic", control = list(scale = TRUE)),
    mildsvm(mil_data, method = "qp-heuristic", control = list(scale = FALSE))
  )))


  ## nystrom_args
  mdl <- mildsvm(mil_data, method = "mip",
                 control = list(nystrom_args = list(m = 100, r = 50)))

  expect_equal(length(mdl$gurobi_fit$w), 50)
  expect_equal(dim(mdl$kfm_fit$dv), c(50, 100))
  expect_equal(dim(mdl$kfm_fit$df_sub), c(100, ncol(mil_data) - 3))

  ## minimal arguments
  mildsvm.mild_df(mil_data)
  mildsvm.formula(mild(bag_label, bag_name, instance_name) ~ ., data = mil_data)
  mildsvm.default(mil_data[, 4:13], mil_data$bag_label, mil_data$bag_name, mil_data$instance_name)

})


test_that("mildsvm mip can warm start", {
  skip_if_no_gurobi()
  verbose <- interactive()

  # manually check that the output says "User MIP start produced solution with objective ..."
  mdl1 <- mildsvm(x = mil_data[, 4:13] %>% as.data.frame(),
                  y = mil_data$bag_label,
                  bags = mil_data$bag_name,
                  instances = mil_data$instance_name,
                  method = "mip",
                  control = list(start = TRUE, verbose = verbose))

  mdl2 <- mildsvm(x = mil_data[, 4:13] %>% as.data.frame(),
                  y = mil_data$bag_label,
                  bags = mil_data$bag_name,
                  instances = mil_data$instance_name,
                  method = "mip",
                  control = list(start = FALSE, verbose = verbose))

  expect_equal(mdl1$gurobi_fit[c("xi", "z")],
               mdl2$gurobi_fit[c("xi", "z")])
  expect_lte(abs(mdl1$gurobi_fit$b - mdl2$gurobi_fit$b), 1e-7)
  expect_lte(max(abs(mdl1$gurobi_fit$w) - abs(mdl2$gurobi_fit$w)), 1e-5)

  pred1 <- predict(mdl1, new_data = mil_data, type = "raw", layer = "instance")
  pred2 <- predict(mdl2, new_data = mil_data, type = "raw", layer = "instance")
  expect_equal(pred1, pred2, tolerance = 1e-7)

  # Hard to test whether the warm start improves the time to reach a solution without testing large problems

})


test_that("mildsvm mip works with radial kernel", {
  skip_if_no_gurobi()
  mdl1 <- mildsvm.default(x = mil_data[, 4:12],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name,
                          method = "mip",
                          control = list(kernel = "radial",
                                         sigma = 1))
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  predict(mdl1, new_data = mil_data, type = "class", layer = "bag")
  predict(mdl1, new_data = mil_data, type = "class", layer = "instance")
  predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")
  predict(mdl1, new_data = mil_data, type = "raw", layer = "instance")

  mdl2 <- mildsvm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3,
                  data = mil_data,
                  method = "mip",
                  control = list(kernel = "radial",
                                 sigma = 1))
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  m <- 20
  r <- 10
  mdl2 <- mildsvm.default(x = mil_data[, 4:12],
                          y = mil_data$bag_label,
                          bags = mil_data$bag_name,
                          instances = mil_data$instance_name,
                          method = "mip",
                          control = list(kernel = "radial",
                                         sigma = 1,
                                         nystrom_args = list(m = m, r = r)))

  expect_equal(dim(mdl2$kfm_fit$dv), c(r, m))
  expect_equal(dim(mdl2$kfm_fit$df_sub), c(m, length(4:12)))

  # Running with linear kernel shouldn't have the kfm_fit element
  expect_warning({
    mdl1 <- mildsvm.default(x = mil_data[, 4:12],
                            y = mil_data$bag_label,
                            bags = mil_data$bag_name,
                            instances = mil_data$instance_name,
                            method = "mip",
                            control = list(kernel = "linear"))
  })
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

})

test_that("Passing kernel matrix into mildsvm works", {
  skip_if_no_gurobi()
  set.seed(8)
  mil_data_shuf <- mil_data[sample(1:nrow(mil_data)), ]

  check_kernel_matrix_works <- function(method) {
    set.seed(8)
    mdl1 <- mildsvm(mil_data_shuf, method = method, control = list(kernel = kme(mil_data_shuf, sigma = 0.05), sigma = 0.05))
    pred1 <- predict(mdl1, new_data = mil_data_test, type = "raw", kernel = kme(mil_data_test, mil_data_shuf, sigma = 0.05))
    set.seed(8)
    mdl2 <- mildsvm(mil_data_shuf, method = method, control = list(sigma = 0.05, scale = FALSE))
    pred2 <- predict(mdl2, new_data = mil_data_test, type = "raw")

    expect_equal(mdl1, mdl2)
    expect_equal(pred1, pred2)
  }

  check_kernel_matrix_works(method = "heuristic")
  check_kernel_matrix_works(method = "qp-heuristic")

})

test_that("Re-ordering data doesn't reduce performance", {
  skip_if_no_gurobi()
  check_auc_after_reordering <- function(method) {
    set.seed(8)
    mdl1 <- mildsvm(mil_data, method = method, control = list(sigma = 0.1))
    mdl2 <- mildsvm(mil_data[sample(1:nrow(mil_data)), ], method = method, control = list(sigma = 0.1))

    pred1 <- predict(mdl1, mil_data_test, type = "raw")
    pred2 <- predict(mdl2, mil_data_test, type = "raw")

    auc1 <- with(mil_data_test,
                 pROC::auc(response = classify_bags(bag_label, bag_name),
                           predictor = classify_bags(pred1$.pred, bag_name),
                           levels = c(0,1), direction = "<"))
    auc2 <- with(mil_data_test,
                 pROC::auc(response = classify_bags(bag_label, bag_name),
                           predictor = classify_bags(pred2$.pred, bag_name),
                           levels = c(0,1), direction = "<"))

    # the auc2 should be in the neighborhood of auc1
    auc1; auc2
    eps <- 0.01
    expect_gte(auc2, auc1 - eps)
    expect_lte(auc2, auc1 + eps)
  }


  check_auc_after_reordering(method = "heuristic")
  check_auc_after_reordering(method = "qp-heuristic")

})

test_that("`mildsvm()` value returns make sense", {
  skip_if_no_gurobi()
  # different methods
  names(mildsvm(mil_data, method = "heuristic"))
  names(mildsvm(mil_data, method = "mip", control = list(nystrom_args = list(m = 10))))
  names(mildsvm(mil_data, method = "qp-heuristic"))

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
  names(mildsvm(mil_data, method = "qp-heuristic", control = list(scale = FALSE)))

  # shouldn't have `weights`
  names(mildsvm(mil_data, method = "heuristic", weights = FALSE))
  expect_true(TRUE)
})

test_that("`predict.mildsvm()` works without new_data", {
  skip_if_no_gurobi()
  check_prediction_no_data <- function(method) {
    mdl1 <- mildsvm(mil_data, method = method,
                    control = list(scale = FALSE, sigma = 1/10))

    pred1 <- predict(mdl1, mil_data_test, type = "raw", layer = "instance")
    pred2 <- predict(mdl1, NULL, "raw", "instance",
                     new_bags = mil_data_test$bag_label,
                     new_instances = mil_data_test$instance_name,
                     kernel = kme(mil_data_test, mil_data, sigma = 1/10))
    expect_equal(pred1, pred2)
  }

  check_prediction_no_data(method = "heuristic")
  check_prediction_no_data(method = "qp-heuristic")

})

