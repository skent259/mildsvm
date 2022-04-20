suppressMessages(suppressWarnings({
  library(dplyr)
  library(tibble)
}))

# Build a sample data set ------------------------------------------------------
# - 4 columns where two of them have means related to outcome and the other two are noise
set.seed(8)
n <- 400
y <- sample(1:5, size = n, prob = (1 / 1:5), replace = TRUE)

X <- matrix(NA, nrow = length(y), ncol = 5)
for (y_ in unique(y)) {
  to_fill <- which(y_ == y)
  X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(2*y_, -1*y_, 1*y_, 0, 0))
}
colnames(X) <- paste0("V", 1:ncol(X))

# build into data frames
df <- bind_cols(y = y, as.data.frame(X)) %>%
  as_tibble()
train <- rownames(df) %in% 1:150
df1 <- df[train, ]
df1_test <- df[!train, ]

# Tests ------------------------------------------------------------------------

test_that("svor_exc() internal functions work on simple examples", {


  res <- svor_exc_fit(y, X, c = 1, rescale = FALSE) %>%
    suppressMessages()

  f <- .calculate_f(res$smo_fit$alpha, compute_kernel(X, type = "linear"))

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

  check_performance(mdl1, df1, 0.95, 0.18, 0.18)
  check_performance(mdl1, df1_test, 0.94, 0.19, 0.19) # a bit worse on testing data, but not bad

})

test_that("svor_exc() works for data-frame-like inputs", {

  mdl2 <- svor_exc(x = X, y = y) %>%
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
  df2 <- df1 %>% mutate(y = factor(y))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, class = "formula")

  # 1/0
  df2 <- df1 %>% mutate(y = factor(y, levels = 5:1))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, class = "formula")

  # Characters
  df2 <- df1 %>% mutate(y = factor(y, labels = c("A", "B", "C", "D", "E")))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, class = "formula")

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% mutate(y = ordered(y, labels = letters[1:5]))
  mdl2 <- svor_exc(y ~ V1 + V2, data = df2, weights = NULL)
  expect_message(mdl3 <- svor_exc(y ~ V1 + V2, data = df3, weights = NULL))
  expect_equal(predict(mdl2, df2, type = "class") %>% mutate(.pred_class = ordered(.pred_class, labels = letters[1:5])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in svor_exc() formula", {
  df2 <- df1 %>% select(y, V1, V2, V3)

  misvm_dot <- svor_exc(y ~ ., data = df2)
  misvm_nodot <- svor_exc(y ~ V1 + V2 + V3, data = df2)

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
  # svor_exc(y ~ ., data = df1, weights = TRUE)
  # mdl1 <- svor_exc(y ~ ., data = df1, weights = c("0" = 1, "1" = 1))
  # mdl1$weights <- NULL
  # mdl2 <- svor_exc(y ~ ., data = df1, weights = FALSE)
  # expect_equal(mdl1, mdl2)
  #
  # df2 <- df1 %>% mutate(y = factor(y, levels = c(1, 0)))
  # dimnames(df2) <- dimnames(df1)
  # expect_equal(
  #   svor_exc(y ~ ., data = df1, weights = c("0" = 2, "1" = 1)),
  #   svor_exc(y ~ ., data = df2, weights = c("0" = 2, "1" = 1))
  # )
  #
  # df2 <- df1 %>% mutate(y = factor(y, labels = c("No", "Yes")))
  # dimnames(df2) <- dimnames(df1)
  # expect_equal(
  #   svor_exc(y ~ ., data = df1, weights = c("0" = 2, "1" = 1))$svm_fit,
  #   svor_exc(y ~ ., data = df2, weights = c("No" = 2, "Yes" = 1))$svm_fit
  # )
  #
  # expect_false(isTRUE(all.equal(
  #   svor_exc(y ~ ., data = df1, weights = c("0" = 2, "1" = 1))$gurobi_fit,
  #   svor_exc(y ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1))$model
  # )))

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

  expect_snapshot({
    models <- list(
      "xy" = svor_exc(x = df1[, 2:6], y = df1$y, weights = NULL),
      "formula" = svor_exc(y ~ V1 + V2, data = df1, weights = NULL),
      "no-scale" = svor_exc(x = df1[, 2:6], y = df1$y,
                            weights = NULL, control = list(scale = FALSE))
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
  })
  expect_true(TRUE)

})

test_that("`svor_exc() works with bag input", {
  mdl1 <- svor_exc(x = df1[, 2:6], y = df1$y, weights = NULL) %>%
    suppressMessages()
  bags_test <- rep(1:50, each = 5)

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
  mdl1 <- svor_exc(form, data = df1, weights = NULL)
  mdl2 <- svor_exc(form, data = df1[sample(1:nrow(df1)), ], weights = NULL)
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

})

