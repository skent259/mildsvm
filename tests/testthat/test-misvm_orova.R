# Build a sample data set ------------------------------------------------------
# - 4 columns where two of them have means related to outcome and the other two are noise
# - bags are aggregated randomly
set.seed(8)
n <- 1000
y <- sample(1:5, size = n, prob = (1 / 1:5)^2, replace = TRUE)
bags <- rep(1:(n/5), each = 5)

classify_bags(y, bags) %>% table()
y <- classify_bags(y, bags, condense = FALSE)

X <- matrix(NA, nrow = length(y), ncol = 5)
for (y_ in unique(y)) {
  to_fill <- which(y_ == y)
  X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(2*y_, -1*y_, 1*y_, 0, 0))
}
colnames(X) <- paste0("V", seq_len(ncol(X)))

# build into data frames
df <- dplyr::bind_cols(bag_label = classify_bags(y, bags, condense = FALSE), bag_name = bags, as.data.frame(X)) %>%
  tibble::as_tibble()
train <- bags %in% 1:100
df1 <- df[train, ]
df1_test <- df[!train, ]

# Tests ------------------------------------------------------------------------

test_that("misvm_orova() has reasonable performance", {
  skip_if_not_installed("gurobi")

  set.seed(8)
  mdl1 <- misvm_orova(mi(bag_label, bag_name) ~ .,
                      data = df1,
                      weights = FALSE,
                      method = "qp-heuristic")

  check_performance <- function(model, df, mzoe_cutoff, mae_cutoff) {
    preds <- predict(model, new_data = df)
    pred_scores <- predict(model, new_data = df, type = "raw")
    pred_vec <- as.numeric(as.character(preds$.pred_class))

    bag_resp <- with(df, classify_bags(bag_label, bag_name))
    bag_pred <- with(df, classify_bags(pred_vec, bag_name))

    # mean zero-one error
    mzoe <- mean(bag_resp != bag_pred)
    expect_lte(mzoe, mzoe_cutoff)

    # mean absolute error
    mae <- mean(abs(bag_resp - bag_pred))
    expect_lte(mae, mae_cutoff)

    expect_snapshot({
      print(mzoe)
      print(mae)
      print(table(bag_resp, bag_pred))
    })
  }

  check_performance(mdl1, df1, 0.28, 0.28)
  check_performance(mdl1, df1_test, 0.40, 0.40) # a bit worse on testing data, but not bad

})

test_that("misvm_orova() works for data-frame-like inputs", {

  # heuristic method
  mdl2 <- misvm_orova(x = as.data.frame(X),
                      y = y,
                      bags = bags,
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
    dplyr::bind_cols(predict(mdl2, df1, type = "class")) %>%
    dplyr::group_by(bag_name) %>%
    dplyr::summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(df1$bag_name))

})

test_that("misvm_orova() works with formula method", {
  mdl1 <- misvm_orova(mi(bag_label, bag_name) ~ V1 + V2 + V3 + V4 + V5, data = df1)
  mdl2 <- misvm_orova(x = df1[, paste0("V", 1:5)],
                      y = df1$bag_label,
                      bags = df1$bag_name)

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "misvm_orova.formula")
  expect_equal(mdl1$features, paste0("V", 1:5))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
  predict(mdl1, df1, type = "raw")
  predict(mdl1, df1, type = "class")

  # check only 1 predictor works
  mdl1 <- misvm_orova(mi(bag_label, bag_name) ~ V1, data = df1)
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  mdl1 <- misvm_orova(mi(bag_label, bag_name) ~ 0 + V1:V2 + V2*V3, data = df1)
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + V1:V2 + V2*V3, data = df1)))
  predict(mdl1, df1, type = "raw")

})

test_that("`misvm_orova()` works with `mi_df` method", {
  predictors <- paste0("V", 1:5)
  df1_mi <- as_mi_df(df1[, c("bag_label", "bag_name", predictors)], instance_label = NULL)
  mdl1 <- misvm_orova(df1_mi)
  mdl2 <- misvm_orova(x = df1[, predictors],
                      y = df1$bag_label,
                      bags = df1$bag_name)

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "misvm_orova.mi_df")
  expect_equal(mdl1$features, predictors)
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
})

test_that("predict.misvm_orova() returns labels that match the input labels", {
  skip_if_not_installed("gurobi")

  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(class,
                  "default" = misvm_orova(x = df[, 3:7],
                                          y = df$bag_label,
                                          bags = df$bag_name,
                                          method = method),
                  "formula" = misvm_orova(mi(bag_label, bag_name) ~ V1 + V2 + V3,
                                          data = df,
                                          method = method))
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 1:5
  df2 <- df1[1:100, ] %>% dplyr::mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # 1/0
  df2 <- df1[1:100, ] %>% dplyr::mutate(bag_label = factor(bag_label, levels = 5:1))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # Characters
  df2 <- df1[1:100, ] %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("A", "B", "C", "D", "E")))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # check re-naming of factors returns the same predictions
  df2 <- df1[1:100, ]
  df3 <- df1[1:100, ] %>% dplyr::mutate(bag_label = ordered(bag_label, labels = letters[1:5]))
  mdl2 <- misvm_orova(mi(bag_label, bag_name) ~ V1 + V2, data = df2, weights = FALSE)
  expect_message({
    mdl3 <- misvm_orova(mi(bag_label, bag_name) ~ V1 + V2, data = df3, weights = FALSE)
  })
  expect_equal(predict(mdl2, df2, type = "class") %>% dplyr::mutate(.pred_class = ordered(.pred_class, labels = letters[1:5])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in misvm_orova() formula", {
  df2 <- df1 %>% dplyr::select(bag_label, bag_name, V1, V2, V3)

  misvm_dot <- misvm_orova(mi(bag_label, bag_name) ~ ., data = df2)
  misvm_nodot <- misvm_orova(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df2)

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("misvm_orova() has correct argument handling", {
  set.seed(8)
  df2 <- df1[1:100, ]
  # `weights`
  # misvm_orova(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE)
  # misvm_orova(mi(bag_label, bag_name) ~ ., data = df1, weights = FALSE)
  mdl1 <- misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 1, "1" = 1))
  mdl1$weights <- NULL
  for (i in seq_along(mdl1$fits)) {
    mdl1$fits[[i]]$weights <- NULL
  }
  mdl2 <- misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, weights = FALSE)
  expect_equal(mdl1, mdl2)

  # `kernel`
  expect_false(isTRUE(all.equal(
    misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, method = "heuristic", control = list(kernel = "radial")),
    misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, method = "heuristic", control = list(kernel = "linear"))
  )))

  skip_if_not_installed("gurobi")

  # `weights`
  expect_false(isTRUE(all.equal(
    misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1), method = "qp-heuristic")$fits,
    misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 1e-6, "1" = 1), method = "qp-heuristic")$fits
  )))

  # `scale`
  expect_false(isTRUE(all.equal(
    misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, method = "qp-heuristic", control = list(scale = TRUE)),
    misvm_orova(mi(bag_label, bag_name) ~ ., data = df2, method = "qp-heuristic", control = list(scale = FALSE))
  )))

})

test_that("`misvm_orova()` value returns make sense", {
  skip_if_not_installed("gurobi")

  df2 <- df1[1:100, ]

  expect_snapshot({
    models <- list(
      "heur" = misvm_orova(x = df2[, 3:7], y = df2$bag_label, bags = df2$bag_name, method = "heuristic"),
      "qp" = misvm_orova(x = df2[, 3:7], y = df2$bag_label, bags = df2$bag_name, method = "qp-heuristic"),
      "mip" = misvm_orova(x = df2[, 3:7], y = df2$bag_label, bags = df2$bag_name, method = "mip"),
      "formula" = misvm_orova(mi(bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic", data = df2),
      "mi_df" = misvm_orova(as_mi_df(df2, instance_label = NULL))
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)
})

test_that("Ordering of data doesn't change `misvm_orova()` results", {
  skip_if_not_installed("gurobi")

  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"))
  }

  # heuristic
  form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
  set.seed(8)
  mdl1 <- misvm_orova(form, data = df1, method = "qp-heuristic")
  ind <- sample(seq_len(nrow(df1)))
  set.seed(8)
  mdl2 <- misvm_orova(form, data = df1[ind, ], method = "qp-heuristic")
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

})

