
test_that("omisvm() has reasonable performance", {
  skip_if_not_installed("gurobi")
  data("ordmvnorm")
  train <- ordmvnorm$bag_name %in% 1:100
  df1 <- ordmvnorm[train, ]
  df1_test <- ordmvnorm[!train, ]


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

test_that("omisvm() works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  {mdl1 <- .run_omisvm(df1)} %>%
    expect_warning()
  mdl2 <- .run_omisvm(df1, control = list(kernel = "radial"))

  for (fit in list(mdl1, mdl2)) {
    expect_equal(
      predict(fit, new_data = df1, type = "class", layer = "bag"),
      predict(fit, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
    )

    bag_preds <- df1 %>%
      .get_pred_matrix(fit) %>%
      .summarize_preds()

    expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
    expect_setequal(bag_preds$bag_name, unique(df1$bag_name))
  }
})

test_that("omisvm() works with formula method", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  # linear kernel
  set.seed(8)
  {mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3 + V4 + V5, data = df1)} %>%
    expect_warning()
  set.seed(8)
  {mdl2 <- .run_omisvm(df1)} %>%
    expect_warning()

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "omisvm.formula")
  expect_equal(mdl1$features, paste0("V", 1:5))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))

  # radial kernel
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
  {mdl1 <- omisvm(mi(bag_label, bag_name) ~ V1, data = df1)} %>%
    expect_warning()
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  {mdl1 <- omisvm(mi(bag_label, bag_name) ~ 0 + V1:V2 + V2*V3, data = df1)} %>%
    expect_warning()
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + V1:V2 + V2*V3, data = df1)))
  predict(mdl1, df1, type = "raw")

})

test_that("`misvm()` works with `mi_df` method", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  predictors <- paste0("V", 1:5)
  suppressWarnings({
    mdl1 <- omisvm(df1)
    mdl2 <- .run_omisvm(df1)
  })

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "omisvm.mi_df")
  expect_equal(mdl1$features, paste0("V", 1:5))
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))

  # radial kernel
  set.seed(8)
  mdl1 <- omisvm(df1, control = list(kernel = "radial"))
  mdl2 <- .run_omisvm(df1, control = list(kernel = "radial"), seed = 8)

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
})

test_that("predict.omisvm() returns labels that match the input labels", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  test_prediction_levels_equal <- function(df,
                                           class = "default",
                                           kernel = "linear") {
    suppressWarnings({
      mdl <- switch(
        class,
        "default" = .run_omisvm(df, control = list(kernel = kernel)),
        "formula" = omisvm(mi(bag_label, bag_name) ~ V1 + V2 + V3,
                           data = df,
                           control = list(kernel = kernel)))
    })
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 1:3
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2)
  test_prediction_levels_equal(df2, kernel = "radial")
  test_prediction_levels_equal(df2, class = "formula")

  # 3:1
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = 3:1))
  expect_message(test_prediction_levels_equal(df2))
  expect_message(test_prediction_levels_equal(df2, class = "formula"))

  # Characters
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("A", "B", "C")))
  expect_message(test_prediction_levels_equal(df2))
  expect_message(test_prediction_levels_equal(df2, kernel = "radial"))
  expect_message(test_prediction_levels_equal(df2, class = "formula"))

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% dplyr::mutate(bag_label = ordered(bag_label, labels = letters[1:3]))
  mdl2 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df2, weights = NULL)
  {mdl3 <- omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df3, weights = NULL)} %>%
    expect_message()
  expect_equal(predict(mdl2, df2, type = "class") %>%
                 dplyr::mutate(.pred_class = ordered(.pred_class, levels = 1:3, labels = letters[1:3])),
               predict(mdl3, df3, type = "class"),
               ignore_attr = TRUE)
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in omisvm() formula", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

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
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  # `weights`
  {.run_omisvm(df1, weights = TRUE)} %>%
    expect_warning()
  .run_omisvm(df1, weights = NULL)

  # `kernel`
  expect_false(isTRUE(all.equal(
    .run_omisvm(df1, weights = NULL, control = list(kernel = "radial")),
    .run_omisvm(df1, weights = NULL, control = list(kernel = "linear"))
  )))

  # `scale`
  expect_false(isTRUE(all.equal(
    .run_omisvm(df1, weights = NULL, control = list(scale = TRUE)),
    .run_omisvm(df1, weights = NULL, control = list(scale = FALSE))
  )))

})

test_that("`omisvm()` value returns make sense", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  expect_snapshot({
    models <- list(
      "xy" = .run_omisvm(df1, weights = NULL),
      "formula" = omisvm(mi(bag_label, bag_name) ~ V1 + V2, data = df1, weights = NULL),
      "mi_df" = omisvm(as_mi_df(df1, instance_label = NULL)),
      "no-scale" = .run_omisvm(df1, weights = NULL, control = list(scale = FALSE))
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
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))
  df1_test <- readRDS(test_path("fixtures", "omisvm-test_df.rds"))

  expect_predictions_equal <- function(model1, model2, data) {
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"),
                 tolerance = 1e-4)
  }

  features <- paste0("V", 1:3)
  ind <- sample(seq_len(nrow(df1)))
  mdl1 <- .run_omisvm(df1, features, weights = NULL)
  mdl2 <- .run_omisvm(df1[ind, ], features, weights = NULL)

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
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  features = paste0("V", 1:3)
  .run_omisvm(df1, s = 2, weights = NULL, control = list(kernel = "radial"))
  .run_omisvm(df1, s = 1, weights = NULL, control = list(kernel = "radial"))
  .run_omisvm(df1, s = -1, weights = NULL, control = list(kernel = "radial")) %>%
    expect_warning()
  .run_omisvm(df1, s = 20, weights = NULL, control = list(kernel = "radial")) %>%
    expect_warning()

})

test_that("`omisvm()` works when passing label with 2 levels", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "omisvm-train_df.rds"))

  ind <- df1$bag_label %in% c(2, 3)
  {mdl1 <- omisvm(df1[ind, ])} %>%
    expect_warning("Only 2 levels detected") %>%
    suppressWarnings() %>%
    suppressMessages()

  preds <- predict(mdl1, new_data = df1)$.pred_class
  expect_equal(length(table(preds)), 2)
})

