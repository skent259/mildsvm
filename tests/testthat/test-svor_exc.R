data("ordmvnorm")
df <- ordmvnorm
df$y <- attr(ordmvnorm, "instance_label")
df <- df[, c(1, 2, 8, 3:7)]
df <- tibble::as_tibble(df)

train <- df$bag_name %in% 1:30
df1 <- df[train, ]
df1$bag_label <- df1$bag_name <- NULL

df1_test <- df[!train, ]

# Tests ------------------------------------------------------------------------

test_that("svor_exc() internal functions work on simple examples", {
  y <- df1$y
  x <- as.matrix(df1[, 2:6])

  res <- svor_exc_fit(y, x, c = 1, rescale = FALSE) %>%
    suppressMessages()

  f <- .calculate_f(res$smo_fit$alpha, compute_kernel(x, type = "linear"))

  # ggplot2::qplot(f, y) +
  #   ggplot2::geom_vline(xintercept = res$smo_fit$b, linetype = "dotted")

  tmp <- outer(as.vector(f), res$smo_fit$b, `-`)
  y_pred <- rowSums(tmp > 0) + 1

  expect_snapshot({
    # evaluation measures
    table(y, y_pred)
    pROC::multiclass.roc(response = y,
                         predictor = f) %>%
      suppressMessages()
    mzoe <- mean(y != y_pred)
    mae <- mean(y - y_pred)

    mzoe; mae
  })
  expect_true(TRUE)
})

test_that("svor_exc() has reasonable performance", {
  skip_on_cran()
  skip_on_ci()

  mdl1 <- svor_exc(y ~ ., data = df1, weights = NULL) %>%
    suppressMessages()

  check_performance <- function(model, df, roc_cutoff, mzoe_cutoff, mae_cutoff) {
    preds <- predict(model, new_data = df)
    pred_scores <- predict(model, new_data = df, type = "raw")

    resp <- df$y
    pred <- as.numeric(as.character(preds$.pred_class))

    # roc
    suppressMessages({
      roc <- pROC::multiclass.roc(response = resp, predictor = pred)
    })
    expect_gt(roc$auc, roc_cutoff)

    # mean zero-one error
    mzoe <- mean(resp != pred)
    expect_lte(mzoe, mzoe_cutoff)

    # mean absolute error
    mae <- mean(abs(resp - pred))
    expect_lte(mae, mae_cutoff)

    expect_snapshot({
      print(roc$auc)
      print(mzoe)
      print(mae)
    })
  }

  check_performance(mdl1, df1, 0.93, 0.20, 0.20)
  check_performance(mdl1, df1_test, 0.94, 0.22, 0.22) # a bit worse on testing data, but not bad

})

test_that("svor_exc() works for data-frame-like inputs", {

  mdl2 <- svor_exc(x = df1[, paste0("V", 1:5)], y = df1$y) %>%
    suppressMessages()

  predict(mdl2, new_data = df1, type = "class")
  predict(mdl2, new_data = df1, type = "raw")

  pred <- predict(mdl2, new_data = df1, type = "class")
  expect_equal(mdl2$levels, levels(pred$.pred_class))

})

test_that("svor_exc() works with formula method", {
  suppressMessages({
    mdl1 <- svor_exc(y ~ V1 + V2 + V3 + V4 + V5, data = df1)
    mdl2 <- svor_exc(x = df1[, paste0("V", 1:5)],
                     y = df1$y)
  })

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "svor_exc.formula")
  expect_equal(mdl1$features, paste0("V", 1:5))

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
  predict(mdl1, df1, type = "raw")
  predict(mdl1, df1, type = "class")

  # check only 1 predictor works
  mdl1 <- svor_exc(y ~ V1, data = df1)
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  mdl1 <- svor_exc(y ~ 0 + V1:V2 + V2*V3, data = df1) %>%
    suppressMessages()
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + V1:V2 + V2*V3, data = df1)))
  predict(mdl1, df1, type = "raw")

})

test_that("`svor_exc()` works with `mi_df` method", {
  df2 <- df1
  df2$bag_name <- seq_len(nrow(df2))
  predictors <- paste0("V", 1:5)
  df1_mi <- as_mi_df(df2[, c("y", "bag_name", predictors)],
                     bag_label = "y",
                     instance_label = NULL)
  suppressMessages({
    mdl1 <- svor_exc(df1_mi)
    mdl2 <- svor_exc(x = df1[, predictors],
                     y = df1$y)
  })

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "svor_exc.mi_df")
  expect_equal(mdl1$features, predictors)
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
})

test_that("predict.svor_exc() returns labels that match the input labels", {
  test_prediction_levels_equal <- function(df, class = "default") {
    suppressMessages({
      mdl <- switch(class,
                    "default" = svor_exc(x = df[, 2:6],
                                         y = df$y),
                    "formula" = svor_exc(y ~ V1 + V2 + V3,
                                         data = df))
    })
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$y))
  }

  # 1:5
  df2 <- df1 %>% dplyr::mutate(y = factor(y))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, class = "formula")

  # 1/0
  df2 <- df1 %>% dplyr::mutate(y = factor(y, levels = 5:1))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, class = "formula")

  # Characters
  df2 <- df1 %>% dplyr::mutate(y = factor(y, labels = c("A", "B", "C", "D", "E")))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, class = "formula")

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% dplyr::mutate(y = ordered(y, labels = letters[1:5]))
  mdl2 <- svor_exc(y ~ V1 + V2, data = df2, weights = NULL)
  expect_message(mdl3 <- svor_exc(y ~ V1 + V2, data = df3, weights = NULL))
  expect_equal(predict(mdl2, df2, type = "class") %>%
                 dplyr::mutate(.pred_class = ordered(.pred_class, labels = letters[1:5])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in svor_exc() formula", {
  df2 <- df1 %>% dplyr::select(y, V1, V2, V3)

  suppressMessages({
    misvm_dot <- svor_exc(y ~ ., data = df2)
    misvm_nodot <- svor_exc(y ~ V1 + V2 + V3, data = df2)
  })

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("svor_exc() has correct argument handling", {
  # `weights`
  suppressMessages({
    expect_warning(svor_exc(y ~ ., data = df1, weights = TRUE))
    svor_exc(y ~ ., data = df1, weights = NULL)
  })

  # `kernel`
  expect_false(isTRUE(all.equal(
    svor_exc(y ~ ., data = df1, weights = NULL, control = list(kernel = "radial")),
    svor_exc(y ~ ., data = df1, weights = NULL, control = list(kernel = "linear"))
  ))) %>%
    suppressMessages()

  # `scale`
  expect_false(isTRUE(all.equal(
    svor_exc(y ~ ., data = df1, weights = NULL, control = list(scale = TRUE)),
    svor_exc(y ~ ., data = df1, weights = NULL, control = list(scale = FALSE))
  ))) %>%
    suppressMessages()

})

test_that("`svor_exc()` value returns make sense", {
  skip_on_covr()
  skip_on_ci()

  df2 <- df1
  df2$bag_name <- seq_len(nrow(df2))

  expect_snapshot({
    models <- list(
      "xy" = svor_exc(x = df1[, 2:6], y = df1$y, weights = NULL),
      "formula" = svor_exc(y ~ V1 + V2, data = df1, weights = NULL),
      "mi_df" = svor_exc(as_mi_df(df2, bag_label = "y", instance_label = NULL)),
      "no-scale" = svor_exc(x = df1[, 2:6], y = df1$y,
                            weights = NULL, control = list(scale = FALSE))
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)

})

test_that("`svor_exc() works with bag input", {
  mdl1 <- svor_exc(x = df1[, 2:6], y = df1$y, weights = NULL) %>%
    suppressMessages()
  bags_test <- df1_test$bag_name

  pred_inst <- predict(mdl1, new_data = df1_test, type = "raw")
  pred_bag <- predict(mdl1, new_data = df1_test, new_bags = bags_test,
                      type = "raw", layer = "bag")

  expect_lte(length(unique(pred_bag[[1]])), length(unique(bags_test)))

  pred_inst <- predict(mdl1, new_data = df1_test, type = "class")
  pred_bag <- predict(mdl1, new_data = df1_test, new_bags = bags_test,
                      type = "class", layer = "bag")

  expect_lte(length(unique(pred_bag[[1]])), length(unique(bags_test)))

})

test_that("Ordering of data doesn't change `svor_exc()` results", {
  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw"),
                 predict(model2, data, type = "raw"))
  }

  form <- y ~ V1 + V2 + V3
  suppressMessages({
    mdl1 <- svor_exc(form, data = df1, weights = NULL)
    mdl2 <- svor_exc(form, data = df1[sample(seq_len(nrow(df1))), ], weights = NULL)
  })
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

})

test_that("Can pass sigma to `svor_exc()`", {
  mdl1 <- svor_exc(y ~ V1 + V2, data = df1, control = list(kernel = "radial", sigma = 1/3))
  predict(mdl1, df1)
  expect_true(TRUE)
})
