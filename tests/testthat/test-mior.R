# Build a sample data set ------------------------------------------------------
# - 3 columns, where there is a representative point and other points that are irrelevant

# start with representative points
set.seed(8)
x <- rbind(
  mvtnorm::rmvnorm(100, mean = c(4, -2, 0)),
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0)),
  mvtnorm::rmvnorm(100, mean = c(-2, 1, 0))
)
score <- x %*% c(2, -1, 0)
y <- as.numeric(cut(score, c(-Inf, quantile(score, probs = 1:2 / 3), Inf)))
bags <- seq_along(y)

# add in points outside boundaries
x <- rbind(
  x,
  mvtnorm::rmvnorm(300, mean = c(6, -3, 0)),
  mvtnorm::rmvnorm(300, mean = c(-6, 3, 0))
)
y <- c(y, rep(-1, 600))
bags <- rep(bags, 3)
repr <- c(rep(1, 300), rep(0, 600))

y_bag <- classify_bags(y, bags, condense = FALSE)

df <- dplyr::bind_cols(bag_label = y_bag,
                bag_name = bags,
                repr = repr,
                as.data.frame(x))

train <- bags %in% sample(unique(df$bag_name), 150)
df1 <- df[train, ]
df1_test <- df[!train, ]

# Tests ------------------------------------------------------------------------

test_that("`mior()` has reasonable performance", {
  skip_if_not_installed("gurobi")

  check_performance <- function(model, df, roc_cutoff, mzoe_cutoff, mae_cutoff) {
    preds <- predict(model, new_data = df)
    pred_scores <- predict(model, new_data = df, type = "raw")
    pred_vec <- as.numeric(as.character(preds$.pred_class))

    bag_resp <- df %>% dplyr::filter(repr == 1) %>% dplyr::select(bag_name, bag_label) %>% tibble::deframe()
    bag_pred <- with(df, classify_bags(pred_vec, bag_name))

    .evaluate_ordinal_predictions(bag_resp, bag_pred, roc_cutoff, mzoe_cutoff, mae_cutoff)
  }

  set.seed(8)
  mdl1 <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df1,
               cost = 1e5, cost_eta = 1e5, weights = NULL) %>%
    suppressWarnings() %>%
    suppressMessages()
  check_performance(mdl1, df1, 0.65, 0.9, 1.4)
  check_performance(mdl1, df1_test, 0.55, 0.8, 1.2)

  # using dual
  set.seed(8)
  mdl2 <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df1,
               cost = 1e5, cost_eta = 1e5, weights = NULL,
               control = list(kernel = "radial")) %>%
    suppressWarnings() %>%
    suppressMessages()
  check_performance(mdl2, df1, 0.60, 0.75, 0.80)
  check_performance(mdl2, df1_test, 0.50, 0.70, 0.80)

})

test_that("`mior()` works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")

  # qp-heuristic method
  mdl2 <- mior(x = df1[, paste0("V", 1:3)],
               y = df1$bag_label,
               bags = df1$bag_name,
               method = "qp-heuristic") %>%
    suppressWarnings() %>%
    suppressMessages()

  expect_equal(
    predict(mdl2, new_data = df1, type = "class", layer = "bag"),
    predict(mdl2, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )

  expect_snapshot({
    predict(mdl2, new_data = df1, type = "class", layer = "bag")
    predict(mdl2, new_data = df1, type = "class", layer = "instance")
    predict(mdl2, new_data = df1, type = "raw", layer = "bag")
    predict(mdl2, new_data = df1, type = "raw", layer = "instance")
  })

  bag_preds <-
    df1 %>%
    dplyr::bind_cols(predict(mdl2, df1, type = "class")) %>%
    dplyr::group_by(bag_name) %>%
    dplyr::summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(df1$bag_name))

  # dual, radial kernel
  mdl3 <- mior(x = df1[, paste0("V", 1:3)],
               y = df1$bag_label,
               bags = df1$bag_name,
               method = "qp-heuristic",
               control = list(kernel = "radial")) %>%
    suppressWarnings() %>%
    suppressMessages()

  expect_equal(
    predict(mdl3, new_data = df1, type = "class", layer = "bag"),
    predict(mdl3, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )
})

test_that("`mior()` works with formula method", {
  skip_if_not_installed("gurobi")

  suppressMessages(suppressWarnings({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df1)
    set.seed(8)
    mdl2 <- mior(x = df1[, paste0("V", 1:3)],
                 y = df1$bag_label,
                 bags = df1$bag_name)
  }))

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mior.formula")
  expect_equal(mdl1$features, paste0("V", 1:3))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
  predict(mdl1, df1, type = "raw")
  predict(mdl1, df1, type = "class")

  # dual, radial kernel
  suppressMessages(suppressWarnings({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df1,
                 control = list(kernel = "radial"))
    set.seed(8)
    mdl2 <- mior(x = df1[, paste0("V", 1:3)],
                 y = df1$bag_label,
                 bags = df1$bag_name,
                 control = list(kernel = "radial"))
  }))
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))

  # check only 1 predictor works
  mdl1 <- mior(mi(bag_label, bag_name) ~ V1, data = df1) %>%
      suppressWarnings() %>%
      suppressMessages()
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas

  mdl1 <- mior(mi(bag_label, bag_name) ~ 0 + V1:V2 + V2*V3, data = df1) %>%
    suppressWarnings() %>%
    suppressMessages()
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + V1:V2 + V2*V3, data = df1)))
  predict(mdl1, df1, type = "raw")

})

test_that("`misvm()` works with `mi_df` method", {
  skip_if_not_installed("gurobi")

  predictors <- paste0("V", 1:3)
  df1_mi <- as_mi_df(df1[, c("bag_label", "bag_name", predictors)], instance_label = NULL)
  suppressWarnings(suppressMessages({
    set.seed(8)
    mdl1 <- mior(df1_mi)
    set.seed(8)
    mdl2 <- mior(x = df1[, predictors],
                 y = df1$bag_label,
                 bags = df1$bag_name)
  }))

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "mior.mi_df")
  expect_equal(mdl1$features, predictors)
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
})

test_that("`predict.mior()` returns labels that match the input labels", {
  skip_if_not_installed("gurobi")

  test_prediction_levels_equal <- function(df, method,
                                           class = "default",
                                           kernel = "linear") {
    suppressMessages(suppressWarnings({
      mdl <- switch(class,
                    "default" = mior(x = df[, 4:6],
                                     y = df$bag_label,
                                     bags = df$bag_name,
                                     method = method,
                                     control = list(kernel = kernel)),
                    "formula" = mior(mi(bag_label, bag_name) ~ V1 + V2 + V3,
                                     data = df,
                                     method = method,
                                     control = list(kernel = kernel)))
    }))
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 1:5
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # 1/0
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = 3:1))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # Characters
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("A", "B", "C")))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", kernel = "radial")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% dplyr::mutate(bag_label = ordered(bag_label, labels = letters[1:3]))
  suppressMessages(suppressWarnings({
    set.seed(8)
    mdl2 <- mior(mi(bag_label, bag_name) ~ V1 + V2, data = df2, weights = NULL)
    set.seed(8)
    mdl3 <- mior(mi(bag_label, bag_name) ~ V1 + V2, data = df3, weights = NULL)
  }))
  expect_equal(predict(mdl2, df2, type = "class") %>% dplyr::mutate(.pred_class = ordered(.pred_class, labels = letters[1:3])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in `mior()` formula", {
  skip_if_not_installed("gurobi")

  df2 <- df1 %>% dplyr::select(bag_label, bag_name, V1, V2, V3)

  suppressMessages(suppressWarnings({
    set.seed(8)
    misvm_dot <- mior(mi(bag_label, bag_name) ~ ., data = df2)
    set.seed(8)
    misvm_nodot <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df2)
  }))

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("`mior()` has correct argument handling", {
  skip_if_not_installed("gurobi")

  set.seed(8)
  # `weights`
  expect_warning(mior(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE), "Weights") %>%
    suppressWarnings() %>%
    suppressMessages()
  expect_warning(mior(mi(bag_label, bag_name) ~ ., data = df1, weights = NULL), "Step") %>%
    suppressWarnings() %>%
    suppressMessages()

  # # `kernel`
  suppressMessages(suppressWarnings({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
                 weights = NULL, control = list(kernel = "radial"))
    set.seed(8)
    mdl2 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
                 weights = NULL, control = list(kernel = "linear"))
  }))
  expect_false(isTRUE(all.equal(mdl1, mdl2)))

  # `scale`
  suppressMessages(suppressWarnings({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
                 weights = NULL, control = list(scale = TRUE))
    set.seed(8)
    mdl2 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic",
                 weights = NULL, control = list(scale = FALSE))
  }))
  expect_false(isTRUE(all.equal(mdl1, mdl2)))

})

test_that("`mior()` value returns make sense", {
  skip_if_not_installed("gurobi")

  expect_snapshot({
    models <- list(
      "xy" = mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name,
           method = "qp-heuristic", weights = NULL),
      "formula" = mior(mi(bag_label, bag_name) ~ V1 + V2,
           method = "qp-heuristic", data = df1, weights = NULL),
      "mi_df" = mior(as_mi_df(df1, instance_label = NULL)),
      "no-scale" = mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name,
           method = "qp-heuristic", weights = NULL, control = list(scale = FALSE))
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)

})

test_that("Ordering of data doesn't change `mior()` results", {
  skip_if_not_installed("gurobi")

  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"))
  }

  ind <- sample(seq_len(nrow(df1)))
  # heuristic
  suppressMessages(suppressWarnings({
    form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
    set.seed(8)
    mdl1 <- mior(form, data = df1, method = "qp-heuristic", weights = NULL)
    set.seed(8)
    mdl2 <- mior(form, data = df1[ind, ], method = "qp-heuristic", weights = NULL)
  }))
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  with(df1_test, suppressWarnings({
    pred <- predict(mdl2, df1_test, type = "raw")$.pred
    suppressMessages({
      roc <- pROC::multiclass.roc(response = bag_label, predictor = pred)
      pROC::auc(roc)
    })
  }))

  # qp-heuristic, dual
  suppressMessages(suppressWarnings({
    form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
    set.seed(8)
    mdl1 <- mior(form, data = df1, method = "qp-heuristic",
                 weights = NULL, control = list(kernel = "radial"))
    set.seed(8)
    mdl2 <- mior(form, data = df1[ind, ], method = "qp-heuristic",
                 weights = NULL, control = list(kernel = "radial"))
  }))
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)
})


