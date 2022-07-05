data("ordmvnorm")
train <- ordmvnorm$bag_name %in% 1:100
df1 <- ordmvnorm[train, ]
df1_test <- ordmvnorm[!train, ]

set.seed(8)

# Tests ------------------------------------------------------------------------

test_that("omisvm() has reasonable performance", {
  skip_if_not_installed("gurobi")

  check_performance <- function(model, df, roc_cutoff, mzoe_cutoff, mae_cutoff) {
    preds <- predict(model, new_data = df)
    pred_scores <- predict(model, new_data = df, type = "raw")
    pred_vec <- as.numeric(as.character(preds$.pred_class))

    bag_resp <- with(df, classify_bags(bag_label, bag_name))
    bag_pred <- with(df, classify_bags(pred_vec, bag_name))

    .evaluate_ordinal_predictions(bag_resp, bag_pred, roc_cutoff, mzoe_cutoff, mae_cutoff)
  }

  set.seed(9)
  mdl1 <- omisvm(mi(bag_label, bag_name) ~ ., data = df1, weights = NULL)
  check_performance(mdl1, df1, 0.95, 0.2, 0.21)
  check_performance(mdl1, df1_test, 0.93, 0.3, 0.3)

  # Slight performance drop, but not too bad
  set.seed(11)
  mdl2 <- omisvm(mi(bag_label, bag_name) ~ ., data = df1[1:250, ], weights = TRUE,
                 control = list(kernel = "radial"))
  check_performance(mdl2, df1, 0.85, 0.35, 0.35)
  check_performance(mdl2, df1_test, 0.80, 0.50, 0.55)

  # With smaller s, very similar
  set.seed(11)
  mdl3 <- omisvm(mi(bag_label, bag_name) ~ ., data = df1[1:250, ],
                 s = 3,
                 weights = TRUE, control = list(kernel = "radial"))
  check_performance(mdl3, df1, 0.85, 0.35, 0.35)
  check_performance(mdl3, df1_test, 0.80, 0.50, 0.55)

})

# make data smaller for fast testing
train <- ordmvnorm$bag_name %in% 1:15
df1 <- ordmvnorm[train, ]
df1$inst_label <- NULL
df1_test <- ordmvnorm[!train, ]

test_that("omisvm() works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")

  # qp-heuristic method
  expect_warning({
    mdl2 <- omisvm(x = as.data.frame(df1)[, paste0("V", 1:5)],
                   y = df1$bag_label,
                   bags = df1$bag_name,
                   method = "qp-heuristic")
  })

  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )

  bag_preds <-
    df1 %>%
    dplyr::bind_cols(predict(mdl2, df1, type = "class")) %>%
    dplyr::group_by(bag_name) %>%
    dplyr::summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(df1$bag_name))

  # qp-heuristic, radial kernel
  mdl2 <- omisvm(x = as.data.frame(df1)[, paste0("V", 1:5)],
                 y = df1$bag_label,
                 bags = df1$bag_name,
                 method = "qp-heuristic",
                 control = list(kernel = "radial"))

  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )


})

test_that("omisvm() works with formula method", {
  skip_if_not_installed("gurobi")

  # qp-heuristic
  expect_warning(expect_warning({
    set.seed(8)
    mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3 + V4 + V5, data = df1)
    set.seed(8)
    mdl2 <- omisvm(x = as.data.frame(df1)[, paste0("V", 1:5)],
                   y = df1$bag_label,
                   bags = df1$bag_name)
  }))

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "omisvm.formula")
  expect_equal(mdl1$features, paste0("V", 1:5))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))

  # qp-heuristic, radial kernel

  set.seed(8)
  mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3 + V4 + V5, data = df1,
                 control = list(kernel = "radial"))
  set.seed(8)
  mdl2 <- omisvm(x = as.data.frame(df1)[, paste0("V", 1:5)],
                 y = df1$bag_label,
                 bags = df1$bag_name,
                 control = list(kernel = "radial"))

  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))

  # check only 1 predictor works
  expect_warning({
    mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1, data = df1)
  })
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  expect_warning({
    mdl1 <- omisvm(mi(bag_label, bag_name) ~ 0 + V1:V2 + V2*V3, data = df1)
  })
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + V1:V2 + V2*V3, data = df1)))
  predict(mdl1, df1, type = "raw")

})

test_that("`misvm()` works with `mi_df` method", {
  skip_if_not_installed("gurobi")

  predictors <- paste0("V", 1:5)
  suppressWarnings({
    mdl1 <- omisvm(df1)
    mdl2 <- omisvm(x = as.data.frame(df1)[, predictors],
                   y = df1$bag_label,
                   bags = df1$bag_name)
  })

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "omisvm.mi_df")
  expect_equal(mdl1$features, predictors)
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))

  suppressWarnings({
    set.seed(8)
    mdl1 <- omisvm(df1, control = list(kernel = "radial"))
    set.seed(8)
    mdl2 <- omisvm(x = as.data.frame(df1)[, predictors],
                   y = df1$bag_label,
                   bags = df1$bag_name,
                   control = list(kernel = "radial"))
  })
  expect_equal(mdl1$model, mdl2$model)
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))

})

test_that("predict.omisvm() returns labels that match the input labels", {
  skip_if_not_installed("gurobi")

  set.seed(9)
  test_prediction_levels_equal <- function(df, method,
                                           class = "default",
                                           kernel = "linear") {
    suppressWarnings({
      mdl <- switch(class,
                    "default" = omisvm(x = df[, 3:7],
                                       y = df$bag_label,
                                       bags = df$bag_name,
                                       method = method,
                                       control = list(kernel = kernel)),
                    "formula" = omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3,
                                       data = df,
                                       method = method,
                                       control = list(kernel = kernel)))
    })
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 1:5
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # 1 0
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = 5:1))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # Characters
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("A", "B", "C", "D", "E")))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% dplyr::mutate(bag_label = ordered(bag_label, labels = letters[1:5]))
  mdl2 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df2, weights = NULL)
  expect_message({
    mdl3 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df3, weights = NULL)
  })
  expect_equal(predict(mdl2, df2, type = "class") %>%
                 dplyr::mutate(.pred_class = ordered(.pred_class, levels = 1:5, labels = letters[1:5])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in omisvm() formula", {
  skip_if_not_installed("gurobi")

  df2 <- df1 %>% dplyr::select(bag_label, bag_name, V1, V2, V3)

  suppressWarnings({
    set.seed(8)
    misvm_dot <- omisvm(mi(bag_label, bag_name) ~ ., data = df2)
    set.seed(8)
    misvm_nodot <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df2)
  })

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("omisvm() has correct argument handling", {
  skip_if_not_installed("gurobi")

  # `weights`
  expect_warning(omisvm(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE))
  omisvm(mi(bag_label, bag_name) ~ ., data = df1, weights = NULL)

  # `kernel`
  expect_false(isTRUE(all.equal(
    omisvm(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
    weights = NULL, control = list(kernel = "radial")),
    omisvm(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
    weights = NULL, control = list(kernel = "linear"))
  )))

  # `scale`
  expect_false(isTRUE(all.equal(
    omisvm(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
    weights = NULL, control = list(scale = TRUE)),
    omisvm(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
    weights = NULL, control = list(scale = FALSE))
  )))

})

test_that("`omisvm()` value returns make sense", {
  skip_if_not_installed("gurobi")

  df1 <- as.data.frame(df1)
  expect_snapshot({
    models <- list(
      "xy" = omisvm(x = df1[, 3:7], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic", weights = NULL),
      "formula" = omisvm(mi(bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic", data = df1, weights = NULL),
      "mi_df" = omisvm(as_mi_df(df1, instance_label = NULL)),
      "no-scale" = omisvm(x = df1[, 3:7], y = df1$bag_label, bags = df1$bag_name,
                          method = "qp-heuristic", weights = NULL, control = list(scale = FALSE))
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)
})

test_that("Ordering of data doesn't change `omisvm()` results", {
  skip_if_not_installed("gurobi")

  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"),
                 tolerance = 1e-4)
  }

  ind <- sample(seq_len(nrow(df1)))

  # qp-heuristic
  suppressMessages(suppressWarnings({
    form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
    set.seed(8)
    mdl1 <- omisvm(form, data = df1, method = "qp-heuristic", weights = NULL)
    set.seed(8)
    mdl2 <- omisvm(form, data = df1[ind, ], method = "qp-heuristic", weights = NULL)
  }))
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  expect_snapshot({
    with(df1_test, suppressWarnings({
      pred <- predict(mdl2, df1_test, type = "raw")$.pred
      pROC::auc(classify_bags(bag_label, bag_name),
                classify_bags(pred, bag_name))
    }))
  })

  # NOTE: even on same input data, model output can be unstable with a small
  # number of predictors.  This seems to only happen in the qp-heuristic case,
  # so I'm omitting this test for now

})

test_that("`omisvm()` can use all values of `s`", {
  skip_if_not_installed("gurobi")

  form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
  omisvm(form, data = df1, s = 4, weights = NULL, control = list(kernel = "radial"))
  omisvm(form, data = df1, s = 1, weights = NULL, control = list(kernel = "radial"))
  omisvm(form, data = df1, s = -1, weights = NULL, control = list(kernel = "radial")) %>%
    expect_warning()
  omisvm(form, data = df1, s = 20, weights = NULL, control = list(kernel = "radial")) %>%
    expect_warning()
})

test_that("`omisvm()` works when passing label with 2 levels", {
  skip_if_not_installed("gurobi")

  ind <- df1$bag_label %in% c(2, 5)
  expect_warning({mdl1 <- omisvm(df1[ind, ])},
                   "Only 2 levels detected") %>%
    suppressWarnings() %>%
    suppressMessages()

  preds <- predict(mdl1, new_data = df1)$.pred_class
  expect_equal(length(table(preds)), 2)
})

