context("Testing the functions in misvm.R")
suppressWarnings(library(dplyr))

set.seed(8)
mil_data <- generate_mild_df(positive_dist = 'mvnormal',
                             negative_dist = 'mvnormal',
                             remainder_dist = 'mvnormal',
                             nbag = 20,
                             nsample = 20,
                             positive_prob = 0.15,
                             positive_mean = rep(2, 5))

mil_data_test <- generate_mild_df(positive_dist = 'mvnormal',
                                  negative_dist = 'mvnormal',
                                  remainder_dist = 'mvnormal',
                                  nbag = 40,
                                  nsample = 20,
                                  positive_prob = 0.15,
                                  positive_mean = rep(2, 5))

df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
  select(-instance_name)

df1_test <- build_instance_feature(mil_data_test, seq(0.05, 0.95, length.out = 10)) %>%
  select(-instance_name)

test_that("misvm() works for data-frame-like inputs", {
  mdl1 <- misvm.default(x = df1[, 3:122],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "mip")

  expect_equal(
    predict(mdl1, new_data = df1, type = "class", layer = "bag"),
    predict(mdl1, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )
  predict(mdl1, new_data = df1, type = "class", layer = "bag")
  predict(mdl1, new_data = df1, type = "class", layer = "instance")
  predict(mdl1, new_data = df1, type = "raw", layer = "bag")
  predict(mdl1, new_data = df1, type = "raw", layer = "instance")

  # heuristic method
  mdl2 <- misvm.default(x = df1[, 3:122],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "heuristic")

  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
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

  # qp-heuristic method
  mdl3 <- misvm.default(x = df1[, 3:122],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "qp-heuristic")

  expect_equal(
    predict(mdl3, new_data = df1, type = "class", layer = "bag"),
    predict(mdl3, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )

  predict(mdl3, new_data = df1, type = "class", layer = "bag")
  predict(mdl3, new_data = df1, type = "class", layer = "instance")
  predict(mdl3, new_data = df1, type = "raw", layer = "bag")
  predict(mdl3, new_data = df1, type = "raw", layer = "instance")

  df1 %>%
    bind_cols(predict(mdl3, new_data = df1, type = "raw", layer = "bag")) %>%
    bind_cols(predict(mdl3, new_data = df1, type = "class", layer = "bag")) %>%
    distinct(bag_label, bag_name, .pred, .pred_class) %>%
    as_tibble()

})

test_that("misvm() works with formula method", {
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1)
  mdl2 <- misvm(x = df1[, c("X1_mean", "X2_mean", "X3_mean")],
                y = df1$bag_label,
                bags = df1$bag_name)

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "misvm.formula")
  expect_equal(mdl1$features, c("X1_mean", "X2_mean", "X3_mean"))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
  predict(mdl1, df1, type = "raw")
  predict(mdl1, df1, type = "class")

  # check only 1 predictor works
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean, data = df1)
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  mdl1 <- misvm(mi(bag_label, bag_name) ~ 0 + X1_mean:X2_mean + X2_mean*X3_mean, data = df1)
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + X1_mean:X2_mean + X2_mean*X3_mean, data = df1)))
  predict(mdl1, df1, type = "raw")

  # check for mip method
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1, method = "mip")

  # check for qp-heuristic method
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1, method = "qp-heuristic")

})

test_that("predict.misvm returns labels that match the input labels", {
  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(class,
                  "default" = misvm(x = df[, 3:122],
                                    y = df$bag_label,
                                    bags = df$bag_name,
                                    method = method),
                  "formula" = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean,
                                    data = df2,
                                    method = method))
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 0/1
  df2 <- df1 %>% mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # 1/0
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # TRUE/FALSE
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c(TRUE, FALSE)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # Yes/No
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  expect_message(test_prediction_levels_equal(df2, method = "heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "mip"))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # check that 0/1 and 1/0 return the same predictions
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df2)
  mdl3 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df3)
  expect_equal(predict(mdl2, df2, type = "class"),
               predict(mdl3, df3, type = "class"))

})

test_that("Dots work in misvm() formula", {
  df2 <- df1 %>% select(bag_label, bag_name, X1_mean, X2_mean, X3_mean)

  misvm_dot <- misvm(mi(bag_label, bag_name) ~ ., data = df2)
  misvm_nodot <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df2)

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("misvm() has correct argument handling", {
  ## weights
  misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE)
  mdl1 <- misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 1, "1" = 1))
  mdl1$weights <- NULL
  mdl2 <- misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = FALSE)
  expect_equal(mdl1, mdl2)

  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  dimnames(df2) <- dimnames(df1)
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1)),
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1))
  )
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip"),
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1), method = "mip")
  )

  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  dimnames(df2) <- dimnames(df1)
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1))$svm_fit,
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1))$svm_fit
  )
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")$gurobi_fit,
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1), method = "mip")$gurobi_fit
  )

  expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")$gurobi_fit,
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "mip")$gurobi_fit
  )))
  expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "qp-heuristic")$gurobi_fit,
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "qp-heuristic")$model
  )))

  ## kernel
  expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, method = "heuristic", control = list(kernel = "radial")),
    misvm(mi(bag_label, bag_name) ~ ., data = df1, method = "heuristic", control = list(kernel = "linear"))
  )))
  expect_warning(expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1, method = "mip", control = list(kernel = "radial")),
    misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1, method = "mip", control = list(kernel = "linear"))
  ))))
  expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic", control = list(kernel = "radial")),
    misvm(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic", control = list(kernel = "linear"))
  )))

  ## scale
  expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, method = "heuristic", control = list(scale = TRUE)),
    misvm(mi(bag_label, bag_name) ~ ., data = df1, method = "heuristic", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1, method = "mip", control = list(scale = TRUE)),
    misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1, method = "mip", control = list(scale = FALSE))
  )))

  # `start`
  expect_warning(expect_message({
    fit <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1,
                 method = "mip", control = list(kernel = "radial", start = TRUE))
  }))


})

test_that("misvm mip can warm start", {
  verbose <- interactive()

  # manually check that the output says "User MIP start produced solution with objective ..."
  mdl1 <- misvm(x = df1[, 3:122],
                y = df1$bag_label,
                bags = df1$bag_name,
                method = "mip",
                control = list(start = TRUE, verbose = verbose))

  mdl2 <- misvm(x = df1[, 3:122],
                y = df1$bag_label,
                bags = df1$bag_name,
                method = "mip",
                control = list(start = FALSE, verbose = verbose))

  expect_equal(mdl1$model[c("w", "b", "xi", "z")],
               mdl2$model[c("w", "b", "xi", "z")])

  # Hard to test whether the warm start improves the time to reach a solution without testing large problems

})

test_that("misvm mip works with radial kernel", {
  set.seed(8)
  mil_data <- generate_mild_df(positive_dist = 'mvt',
                               negative_dist = 'mvnormal',
                               remainder_dist = 'mvnormal',
                               nbag = 10,
                               nsample = 20,
                               positive_degree = 3,
                               positive_prob = 0.15,
                               positive_mean = rep(0, 5))

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  mdl1 <- misvm.default(x = df1[, 4:12],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "mip",
                        control = list(kernel = "radial",
                                       sigma = 1))
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  predict(mdl1, new_data = df1, type = "class", layer = "bag")
  predict(mdl1, new_data = df1, type = "class", layer = "instance")
  predict(mdl1, new_data = df1, type = "raw", layer = "bag")
  predict(mdl1, new_data = df1, type = "raw", layer = "instance")

  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean,
                data = df1,
                method = "mip",
                control = list(kernel = "radial",
                               sigma = 1))
  expect(!is.null(mdl1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  m <- 20
  r <- 10
  mdl2 <- misvm.default(x = df1[, 4:12],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "mip",
                        control = list(kernel = "radial",
                                       sigma = 1,
                                       nystrom_args = list(m = m, r = r)))

  expect_equal(dim(mdl2$kfm_fit$dv), c(r, m))
  expect_equal(dim(mdl2$kfm_fit$df_sub), c(m, length(4:12)))

  # Running with defaults (linear kernel) shouldn't have the kfm_fit element
  mdl1 <- misvm.default(x = df1[, 4:12],
                        y = df1$bag_label,
                        bags = df1$bag_name,
                        method = "mip")
  expect_null(mdl1$kfm_fit)

})

test_that("misvm() works on 'mild_df' objects", {
  # minimal arguments
  mdl <- misvm(mil_data)
  expect_equal(mdl$call_type, "misvm.mild_df")
  expect_equal(mdl$summary_fns, list(mean = mean, sd = sd))
  expect_equal(mdl$features, paste0(colnames(mil_data)[4:13], rep(c("_mean", "_sd"), each = 10)))

  # bag level predictions
  pred <- predict(mdl, new_data = mil_data, type = "raw")
  expect_equal(nrow(pred), nrow(mil_data))
  expect_lte(length(unique(pred$.pred)), length(unique(mil_data$bag_name)))

  # instance level predictions
  pred <- predict(mdl, new_data = mil_data, type = "raw", layer = "instance")
  expect_equal(nrow(pred), nrow(mil_data))
  expect_lte(length(unique(pred$.pred)), length(unique(mil_data$instance_name)))

  # predictions on new data
  pred <- predict(mdl, new_data = mil_data_test, type = "raw", layer = "instance")
  expect_equal(nrow(pred), nrow(mil_data_test))
  expect_lte(length(unique(pred$.pred)), length(unique(mil_data_test$instance_name)))

  # make sure when new data is scrambled things don't get out of order
  scrambled <- mil_data_test[sample(1:nrow(mil_data_test)), ]

  instance_level <- scrambled %>%
    bind_cols(predict(mdl, scrambled, type = "raw", layer = "instance")) %>%
    distinct(bag_label, instance_name, .pred)
  expect_equal(nrow(instance_level), length(unique(scrambled$instance_name)))

  bag_level <- scrambled %>%
    bind_cols(predict(mdl, scrambled, type = "raw", layer = "bag")) %>%
    distinct(bag_label, bag_name, .pred)
  expect_equal(nrow(bag_level), length(unique(scrambled$bag_name)))

  # cor = TRUE
  mdl <- misvm(mil_data, cor = TRUE)
  expect_equal(mdl$call_type, "misvm.mild_df")
  expect_equal(mdl$summary_fns, list(mean = mean, sd = sd))
  expect_equal(mdl$summary_cor, TRUE)
  expect_equal(length(mdl$features), 2*10 + choose(10, 2))

  pred <- predict(mdl, new_data = mil_data, type = "raw")
  expect_equal(nrow(pred), nrow(mil_data))
  expect_lte(length(unique(pred$.pred)), length(unique(mil_data$bag_name)))

  # alternative methods
  mdl <- misvm(mil_data, method = "mip")
  pred <- predict(mdl, new_data = mil_data_test, type = "raw")

  mdl <- misvm(mil_data, method = "qp-heuristic")
  pred <- predict(mdl, new_data = mil_data_test, type = "raw")

  # different summary functions
  mdl <- misvm(mil_data, .fns = list(mean = mean, med = median, qtl25 = ~quantile(.x, 0.25)))
  expect_equal(length(mdl$features), 3*10)
  expect_equal(mdl$call_type, "misvm.mild_df")
  expect_equal(mdl$summary_fns, list(mean = mean, med = median, qtl25 = ~quantile(.x, 0.25)))
  pred <- predict(mdl, new_data = mil_data_test, type = "raw")
})

test_that("`misvm()` value returns make sense", {

  # different methods
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "heuristic"))
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "mip"))
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic"))

  # different S3 methods
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "heuristic"))
  names(misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, method = "heuristic", data = df1))
  names(misvm(mil_data))

  # shouldn't have `x_scale`
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "heuristic", control = list(scale = FALSE)))
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "mip", control = list(scale = FALSE)))
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic", control = list(scale = FALSE)))

  # should have `kfm_fit`
  expect_warning({
    names(misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1, method = "mip", control = list(kernel = "radial")))
  })

  # shoudln't have `weights`
  names(misvm(x = df1[, 3:122], y = df1$bag_label, bags = df1$bag_name, method = "heuristic", weights = FALSE))
  names(misvm(mil_data))

  expect_true(TRUE)
})

test_that("Ordering of data doesn't change `misvm()` results", {
  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"))
  }

  # heuristic
  form <- mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean
  mdl1 <- misvm(form , data = df1, method = "heuristic")
  mdl2 <- misvm(form, data = df1[sample(1:nrow(df1)), ], method = "heuristic")
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  with(df1_test, {
       pred <- predict(mdl2, df1_test, type = "raw")$.pred
       pROC::auc(classify_bags(bag_label, bag_name),
                 classify_bags(pred, bag_name))
  })

  # qp-heuristic
  mdl1 <- misvm(form, data = df1, method = "qp-heuristic")
  mdl2 <- misvm(form, data = df1[sample(1:nrow(df1)), ], method = "qp-heuristic")
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  with(df1_test, {
    pred <- predict(mdl2, df1_test, type = "raw")$.pred
    pROC::auc(classify_bags(bag_label, bag_name),
              classify_bags(pred, bag_name))
  })

})

test_that("`misvm()` works even when there are Nan columns or idential columns", {

  df2 <- df1
  df2$nan_feature <- NaN

  expect_warning({
    mdl1 <- misvm(x = df2[, 3:123],
                  y = df2$bag_label,
                  bags = df2$bag_name,
                  method = "qp-heuristic")
  })

  expect_warning({
    mdl2 <- misvm(x = df2[, c(3:123, 123)],
                  y = df2$bag_label,
                  bags = df2$bag_name,
                  method = "qp-heuristic")
  })

  pred <- predict(mdl1, new_data = df1_test, layer = "instance", type = "raw")
  pred <- predict(mdl2, new_data = df1_test, layer = "instance", type = "raw")

  df3 <- df2
  df3$ident_feature <- 50

  expect_warning({
    mdl1 <- misvm(x = df3[, 3:124],
                  y = df3$bag_label,
                  bags = df3$bag_name,
                  method = "qp-heuristic")
  })
  pred <- predict(mdl1, new_data = df1_test, layer = "instance", type = "raw")

})

test_that("`misvm()` works with a few bags with only one instance", {
  df2 <- df1[-(2:4), ]
  table(df2$bag_name)

  mdl <- misvm(x = df2[, 3:10],
               y = df2$bag_label,
               bags = df2$bag_name,
               method = "qp-heuristic")

  expect_s3_class(mdl, "misvm")
})

test_that("`misvm()` works fine with matrices", {
  for (method in c("heuristic", "mip", "qp-heuristic")) {
    mdl <- misvm(x = as.matrix(df1[, 3:10]),
                 y = df1$bag_label,
                 bags = df1$bag_name,
                 method = method)
    expect_s3_class(mdl, "misvm")
  }
})
