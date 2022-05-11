suppressMessages(suppressWarnings({
  library(dplyr)
  library(tibble)
}))

data("ordmvnorm")
train <- ordmvnorm$bag_name %in% 1:100
df1 <- ordmvnorm[train, ]
df1$inst_label <- NULL
df1_test <- ordmvnorm[!train, ]

set.seed(8)

# Tests ------------------------------------------------------------------------

test_that("omisvm() has reasonable performance", {

  check_performance <- function(model, df, roc_cutoff, mzoe_cutoff, mae_cutoff) {
    preds <- predict(model, new_data = df)
    pred_scores <- predict(model, new_data = df, type = "raw")
    pred_vec <- as.numeric(as.character(preds$.pred_class))

    bag_resp <- with(df, classify_bags(bag_label, bag_name))
    bag_pred <- with(df, classify_bags(pred_vec, bag_name))

    # roc
    suppressMessages({
      roc <- pROC::multiclass.roc(response = bag_resp, predictor = bag_pred)
    })
    expect_gt(roc$auc, roc_cutoff)

    # mean zero-one error
    mzoe <- mean(bag_resp != bag_pred)
    expect_lte(mzoe, mzoe_cutoff)

    # mean absolute error
    mae <- mean(abs(bag_resp - bag_pred))
    expect_lte(mae, mae_cutoff)

    expect_snapshot({
      print(roc$auc)
      print(mzoe)
      print(mae)
      print(table(bag_resp, bag_pred))
    })
  }

  set.seed(9)
  mdl1 <- omisvm(mi(bag_label, bag_name) ~ ., data = df1, weights = NULL)
  check_performance(mdl1, df1, 0.95, 0.2, 0.2)
  check_performance(mdl1, df1_test, 0.93, 0.3, 0.3)

  # Note: performance drops with radial kernel, and similar for linear kernel
  # using dual (currently not used for this reason)
  set.seed(9)
  mdl2 <- omisvm(mi(bag_label, bag_name) ~ ., data = df1[1:250, ], weights = NULL,
                 control = list(kernel = "radial"))
  check_performance(mdl2, df1, 0.85, 0.85, 1.05)
  check_performance(mdl2, df1_test, 0.85, 0.80, 0.95)

  # With smaller s, slightly worse
  set.seed(9)
  mdl3 <- omisvm(mi(bag_label, bag_name) ~ ., data = df1[1:250, ],
                 s = 3,
                 weights = NULL, control = list(kernel = "radial"))
  check_performance(mdl3, df1, 0.75, 0.85, 1.05)
  check_performance(mdl3, df1_test, 0.75, 0.80, 0.95)

})

# make data smaller for fast testing
train <- ordmvnorm$bag_name %in% 1:20
df1 <- ordmvnorm[train, ]
df1$inst_label <- NULL
df1_test <- ordmvnorm[!train, ]

test_that("omisvm() works for data-frame-like inputs", {

  # qp-heuristic method
  expect_warning({
    mdl2 <- omisvm(x = df1[, paste0("V", 1:5)],
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
    bind_cols(predict(mdl2, df1, type = "class")) %>%
    group_by(bag_name) %>%
    summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(df1$bag_name))

  # qp-heuristic, radial kernel
  expect_warning({
    mdl2 <- omisvm(x = df1[, paste0("V", 1:5)],
                   y = df1$bag_label,
                   bags = df1$bag_name,
                   method = "qp-heuristic",
                   control = list(kernel = "radial"))
  })

  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )


})

test_that("omisvm() works with formula method", {
  # qp-heuristic
  expect_warning(expect_warning({
    set.seed(8)
    mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3 + V4 + V5, data = df1)
    set.seed(8)
    mdl2 <- omisvm(x = df1[, paste0("V", 1:5)],
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
  expect_warning(expect_warning({
    set.seed(8)
    mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3 + V4 + V5, data = df1,
                   control = list(kernel = "radial"))
    set.seed(8)
    mdl2 <- omisvm(x = df1[, paste0("V", 1:5)],
                   y = df1$bag_label,
                   bags = df1$bag_name,
                   control = list(kernel = "radial"))
  }))
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

test_that("predict.omisvm() returns labels that match the input labels", {
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
  df2 <- df1 %>% mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # 1 0
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = 5:1))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # Characters
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("A", "B", "C", "D", "E")))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% mutate(bag_label = ordered(bag_label, labels = letters[1:5]))
  mdl2 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df2, weights = NULL)
  expect_message({
    mdl3 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df3, weights = NULL)
  })
  expect_equal(predict(mdl2, df2, type = "class") %>% mutate(.pred_class = ordered(.pred_class, labels = letters[1:5])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in omisvm() formula", {
  df2 <- df1 %>% select(bag_label, bag_name, V1, V2, V3)

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

  expect_snapshot({
    models <- list(
      "xy" = omisvm(x = df1[, 3:7], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic", weights = NULL),
      "formula" = omisvm(mi(bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic", data = df1, weights = NULL),
      "no-scale" = omisvm(x = df1[, 3:7], y = df1$bag_label, bags = df1$bag_name,
                          method = "qp-heuristic", weights = NULL, control = list(scale = FALSE))
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
  })
  expect_true(TRUE)
})

test_that("Ordering of data doesn't change `omisvm()` results", {
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

  # qp-heuristic, radial kernel
  suppressWarnings({
    form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
    set.seed(9)
    mdl1 <- omisvm(form, data = df1, method = "qp-heuristic",
                   weights = NULL, control = list(kernel = "radial"))
    set.seed(9)
    mdl2 <- omisvm(form, data = df1[ind, ], method = "qp-heuristic",
                   weights = NULL, control = list(kernel = "radial"))
  })

  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  expect_snapshot({
    with(df1_test, suppressWarnings({
      pred <- predict(mdl2, df1_test, type = "raw")$.pred
      pROC::auc(classify_bags(bag_label, bag_name),
                classify_bags(pred, bag_name))
    }))
  })

})

test_that("`omisvm()` can use all values of `s`", {

  form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
  omisvm(form, data = df1, s = 4, weights = NULL, control = list(kernel = "radial"))
  omisvm(form, data = df1, s = 1, weights = NULL, control = list(kernel = "radial"))
  omisvm(form, data = df1, s = -1, weights = NULL, control = list(kernel = "radial")) %>%
    expect_warning()
  omisvm(form, data = df1, s = 20, weights = NULL, control = list(kernel = "radial")) %>%
    expect_warning()
})

