test_that("mismm() works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  for (method in c("mip", "heuristic", "qp-heuristic")) {
    set.seed(8)
    fit1 <- .run_mismm(df, method = method)

    expect_s3_class(fit1, "mismm")
    expect_equal(
      predict(fit1, new_data = df, type = "raw", layer = "bag"),
      predict(fit1, new_data = df, type = "raw", layer = "bag", new_bags = df$bag_name)
    )
    expect_equal(
      predict(fit1, new_data = df, type = "raw", layer = "instance"),
      predict(fit1, new_data = df, type = "raw", layer = "instance",
              new_bags = df$bag_name, new_instances = df$instance_name)
    )

    bag_preds <-
      df %>%
      .get_pred_matrix(fit1) %>%
      .summarize_preds(by = bag_name)

    expect_equal(nrow(bag_preds), length(unique(df$bag_name)))
    expect_setequal(bag_preds$bag_name, unique(df$bag_name))
    expect_snapshot(bag_preds)

    predict(fit1, new_data = df, type = "class", layer = "bag")
    predict(fit1, new_data = df, type = "class", layer = "instance")
    predict(fit1, new_data = df, type = "raw", layer = "bag")
    predict(fit1, new_data = df, type = "raw", layer = "instance")

  }
})

test_that("mismm() works with formula method", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  fit1 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = df)
  fit2 <- .run_mismm(df)

  expect_equal(fit1$ksvm_fit, fit2$ksvm_fit)
  expect_equal(fit1$total_step, fit2$total_step)
  expect_equal(fit1$call_type, "mismm.formula")
  expect_equal(fit1$features, c("X1", "X2", "X3"))
  expect_equal(fit1$bag_name, "bag_name")
  expect_equal(fit1$instance_name, "instance_name")

  # predictions should match
  expect_equal(predict(fit1, df, type = "raw"), predict(fit2, df, type = "raw"))
  expect_equal(predict(fit1, df, type = "class"), predict(fit2, df, type = "class"))

  # check only 1 predictor works
  fit1 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1, data = df)
  predict(fit1, df, type = "raw")

  # check some obscure formulas
  fit1 <- mismm(mild(bag_label, bag_name, instance_name) ~ 0 + X1:X2 + X2*X3,
                data = df)
  expect_equal(fit1$features,
               colnames(model.matrix(~ 0 + X1:X2 + X2*X3, data = df)))
  predict(fit1, df, type = "raw")

  # check other methods
  for (method in c("heuristic", "mip", "qp-heuristic")) {
    set.seed(8)
    formula <- mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3
    fit1 <- mismm(formula, data = df, method = method)
    expect_s3_class(fit1, "mismm")
  }

})

test_that("mismm() works with mild_df method", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  fit1 <- mismm(df)
  fit2 <- .run_mismm(df) # default method

  expect_s3_class(fit1, "mismm")
  expect_equal(fit1$ksvm_fit, fit2$ksvm_fit)
  expect_equal(fit1$total_step, fit2$total_step)
  expect_equal(fit1$call_type, "mismm.mild_df")
  expect_equal(fit1$features, paste0("X", 1:3))
  expect_equal(fit1$bag_name, "bag_name")
  expect_equal(fit1$instance_name, "instance_name")

  predict(fit1, new_data = df)

  fit1 <- mismm(df, method = "qp-heuristic")
  expect_s3_class(fit1, "mismm")
})

test_that("predict.mismm returns labels that match the input labels", {
  skip_if_not_installed("gurobi")
  df1 <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds")) %>%
    tibble::as_tibble() %>%
    dplyr::filter(bag_name %in% c("bag1", "bag2", "bag4", "bag8"))

  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(
      class,
      "default" = .run_mismm(df, method = method),
      "formula" = mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2,
                        data = df,
                        method = method)
    )
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 0/1
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # 1/0
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # TRUE/FALSE
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c(TRUE, FALSE)))
  test_prediction_levels_equal(df2, method = "heuristic")
  test_prediction_levels_equal(df2, method = "mip")
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "heuristic", class = "formula")

  # Yes/No
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  expect_message(test_prediction_levels_equal(df2, method = "heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "mip"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))

  # check that 0/1 and 1/0 return the same predictions
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  fit2 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2, data = df2)
  mdl3 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2, data = df3)
  expect_equal(predict(fit2, df2, type = "class"),
               predict(mdl3, df3, type = "class"))

})

test_that("Dots work in mismm() formula", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  mismm_dot <- mismm(mild(bag_label, bag_name, instance_name) ~ ., data = df)
  mismm_nodot <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = df)

  expect_equal(mismm_dot$ksvm_fit, mismm_nodot$ksvm_fit)
  expect_equal(mismm_dot$features, mismm_nodot$features)
  expect_equal(mismm_dot$bag_name, mismm_nodot$bag_name)

  expect_equal(predict(mismm_dot, new_data = df), predict(mismm_nodot, new_data = df))
})

test_that("mismm() has correct argument handling", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds")) %>%
    dplyr::filter(bag_name %in% c("bag1", "bag2", "bag4", "bag8"))

  ## weights
  mismm(df, weights = TRUE)
  fit1 <- mismm(df, weights = c("0" = 1, "1" = 1))
  fit1$weights <- NULL
  expect_equal(
    fit1,
    mismm(df, weights = FALSE)
  )

  df2 <- df %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  expect_equal(dimnames(df2), dimnames(df))
  expect_equal(
    mismm(df, weights = c("0" = 2, "1" = 1)),
    mismm(df2, weights = c("0" = 2, "1" = 1))
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  fit1 <- mismm(df, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  fit2 <- mismm(df2, weights = c("0" = 2, "1" = 1), method = "mip")
  expect_equal(fit1, fit2)

  df2 <- df %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  expect_equal(dimnames(df2), dimnames(df))
  expect_equal(
    mismm(df, weights = c("0" = 2, "1" = 1))$ksvm_fit,
    suppressMessages(mismm(df2, weights = c("No" = 2, "Yes" = 1))$ksvm_fit)
  )
  set.seed(8) # nystrom sampling may change, need to set seed for each
  fit1 <- mismm(df, weights = c("0" = 2, "1" = 1), method = "mip")
  set.seed(8)
  fit2 <- mismm(df2, weights = c("No" = 2, "Yes" = 1), method = "mip") %>%
    suppressMessages()
  expect_equal(fit1$gurobi_fit, fit2$gurobi_fit)

  expect_false(isTRUE(all.equal(
    mismm(df, weights = c("0" = 2, "1" = 1), method = "mip")$gurobi_fit,
    mismm(df, weights = c("0" = 1e-6, "1" = 1), method = "mip")$gurobi_fit
  )))
  expect_false(isTRUE(all.equal(
    mismm(df, weights = c("0" = 200, "1" = 1), method = "heuristic")$ksvm_fit,
    mismm(df, weights = c("0" = 1e-6, "1" = 1), method = "heuristic")$ksvm_fit
  )))
  expect_false(isTRUE(all.equal(
    mismm(df, weights = c("0" = 200, "1" = 1), method = "qp-heuristic")$gurobi_fit,
    mismm(df, weights = c("0" = 1e-6, "1" = 1), method = "qp-heuristic")$gurobi_fit
  )))

  ## kernel
  # there isn't a "linear" kernel option for mismm
  expect_warning(expect_equal(
    mismm(df, method = "heuristic", control = list(kernel = "radial")),
    mismm(df, method = "heuristic", control = list(kernel = "linear"))
  ))
  # TODO: try passing in the kernel as a matrix into this
  expect_warning(expect_false(isTRUE(all.equal(
    mismm(df, method = "mip", control = list(kernel = "radial")),
    mismm(df, method = "mip", control = list(kernel = "linear"))
  ))))
  expect_warning(expect_false(isTRUE(all.equal(
    mismm(df, method = "qp-heuristic", control = list(kernel = "radial")),
    mismm(df, method = "qp-heuristic", control = list(kernel = "linear"))
  ))))

  ## scale
  expect_false(isTRUE(all.equal(
    mismm(df,  method = "heuristic", control = list(scale = TRUE)),
    mismm(df, method = "heuristic", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mismm(df, method = "mip", control = list(scale = TRUE)),
    mismm(df, method = "mip", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mismm(df, method = "qp-heuristic", control = list(scale = TRUE)),
    mismm(df, method = "qp-heuristic", control = list(scale = FALSE))
  )))
  expect_false(isTRUE(all.equal(
    mismm(df, method = "qp-heuristic", control = list(scale = TRUE)),
    mismm(df, method = "qp-heuristic", control = list(scale = FALSE))
  )))


  ## nystrom_args
  mdl <- mismm(df, method = "mip",
               control = list(nystrom_args = list(m = 16, r = 8)))

  expect_equal(length(mdl$gurobi_fit$w), 8)
  expect_equal(dim(mdl$kfm_fit$dv), c(8, 16))
  expect_equal(dim(mdl$kfm_fit$df_sub), c(16, ncol(df) - 3))

  ## minimal arguments
  mismm.mild_df(df)
  mismm.formula(mild(bag_label, bag_name, instance_name) ~ ., data = df)
  .run_mismm(df)

})

test_that("mismm mip can warm start", {
  skip_if_not_installed("gurobi")
  verbose <- interactive()
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  # manually check that the output says "User MIP start produced solution with objective ..."
  set.seed(8)
  fit1 <- .run_mismm(df, method = "mip",
                     control = list(start = TRUE, verbose = verbose))
  fit2 <- .run_mismm(df, method = "mip",
                     control = list(start = FALSE, verbose = verbose))

  expect_equal(fit1$gurobi_fit[c("xi", "z")],
               fit2$gurobi_fit[c("xi", "z")])
  expect_lte(abs(fit1$gurobi_fit$b - fit2$gurobi_fit$b), 1e-7)
  expect_lte(max(abs(fit1$gurobi_fit$w) - abs(fit2$gurobi_fit$w)), 1e-4)

  pred1 <- predict(fit1, new_data = df, type = "raw", layer = "instance")
  pred2 <- predict(fit2, new_data = df, type = "raw", layer = "instance")
  expect_equal(pred1, pred2, tolerance = 1e-7)

  # Hard to test whether the warm start improves the time to reach a solution
  # without testing large problems
})


test_that("mismm mip works with radial kernel", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  fit1 <- .run_mismm(df, method = "mip",
                     control = list(kernel = "radial", sigma = 1))
  expect(!is.null(fit1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  predict(fit1, new_data = df, type = "class", layer = "bag")
  predict(fit1, new_data = df, type = "class", layer = "instance")
  predict(fit1, new_data = df, type = "raw", layer = "bag")
  predict(fit1, new_data = df, type = "raw", layer = "instance")

  fit2 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3,
                data = df,
                method = "mip",
                control = list(kernel = "radial",
                               sigma = 1))
  expect(!is.null(fit1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

  m <- 20
  r <- 10
  fit2 <- .run_mismm(df, method = "mip",
                     control = list(kernel = "radial",
                                    sigma = 1,
                                    nystrom_args = list(m = m, r = r)))
  expect_equal(dim(fit2$kfm_fit$dv), c(r, m))
  expect_equal(dim(fit2$kfm_fit$df_sub), c(m, length(4:6)))

  # Running with linear kernel shouldn't have the kfm_fit element
  expect_warning({
    fit1 <- .run_mismm(df, method = "mip", control = list(kernel = "linear"))
  })
  expect(!is.null(fit1$kfm_fit), failure_message = "Kfm_fit was not found in the model")

})

test_that("Passing kernel matrix into mismm works", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))
  df_test <- readRDS(test_path("fixtures", "mimmm-test_mild_df.rds"))

  set.seed(8)
  df_shuf <- df[sample(seq_len(nrow(df))), ]

  check_kernel_matrix_works <- function(method) {
    set.seed(8)
    mdl1 <- mismm(df_shuf, method = method, control = list(kernel = kme(df_shuf, sigma = 0.05), sigma = 0.05))
    pred1 <- predict(mdl1, new_data = df_test, type = "raw", kernel = kme(df_test, df_shuf, sigma = 0.05))
    set.seed(8)
    mdl2 <- mismm(df_shuf, method = method, control = list(sigma = 0.05, scale = FALSE))
    pred2 <- predict(mdl2, new_data = df_test, type = "raw")

    shared <- c(
      "ksvm_fit", "call_type", "x", "features", "levels", "cost",
      "sigma", "weights", "repr_inst", "n_step", "useful_inst_idx",
      "inst_order", "bag_name", "instance_name"
    )
    expect_equal(mdl1[shared], mdl2[shared])
    expect_equal(pred1, pred2)
  }

  check_kernel_matrix_works(method = "heuristic") %>%
    expect_message()
  check_kernel_matrix_works(method = "qp-heuristic") %>%
    expect_message()

})

test_that("Re-ordering data doesn't reduce performance", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))
  df_test <- readRDS(test_path("fixtures", "mimmm-test_mild_df.rds"))

  check_auc_after_reordering <- function(method) {
    set.seed(8)
    mdl1 <- mismm(df, method = method, control = list(sigma = 0.1))
    mdl2 <- mismm(df[sample(seq_len(nrow(df))), ], method = method, control = list(sigma = 0.1))

    pred1 <- predict(mdl1, df_test, type = "raw")
    pred2 <- predict(mdl2, df_test, type = "raw")

    auc1 <- with(df_test,
                 pROC::auc(response = classify_bags(bag_label, bag_name),
                           predictor = classify_bags(pred1$.pred, bag_name),
                           levels = c(0,1), direction = "<"))
    auc2 <- with(df_test,
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

test_that("`mismm()` value returns make sense", {
  skip_if_not_installed("gurobi")
  df <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))

  expect_snapshot({
    models <- list(
      "mildata-heur" = mismm(df, method = "heuristic"),
      "mildata-mip" = mismm(df, method = "mip", control = list(nystrom_args = list(m = 10))),
      "mildata-qp" = mismm(df, method = "qp-heuristic"),
      "xy" = mismm(x = as.data.frame(df[, 4:6]),
                   y = df$bag_label,
                   bags = df$bag_name,
                   instances = df$instance_name),
      "formula" = mismm(mild(bag_label, bag_name, instance_name) ~ ., data = df),
      "no-scale-heur" = mismm(df, method = "heuristic", control = list(scale = FALSE)),
      "no-scale-mip" = mismm(df, method = "mip", control = list(scale = FALSE, nystrom_args = list(m = 10))),
      "no-scale-qp" = mismm(df, method = "qp-heuristic", control = list(scale = FALSE)),
      "no-weights" = mismm(df, method = "heuristic", weights = FALSE)
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)
})

test_that("`predict.mismm()` works without new_data", {
  skip_if_not_installed("gurobi")
  df_train <- readRDS(test_path("fixtures", "mimmm-train_mild_df.rds"))
  df_test <- readRDS(test_path("fixtures", "mimmm-test_mild_df.rds"))

  check_prediction_no_data <- function(method) {
    fit1 <- mismm(df_train, method = method,
                  control = list(scale = FALSE, sigma = 1/10))

    pred1 <- predict(fit1, df_test, type = "raw", layer = "instance")
    pred2 <- predict(fit1, NULL, "raw", "instance",
                     new_bags = df_test$bag_label,
                     new_instances = df_test$instance_name,
                     kernel = kme(df_test, df_train, sigma = 1/10))
    expect_equal(pred1, pred2)
  }

  check_prediction_no_data(method = "heuristic")
  check_prediction_no_data(method = "qp-heuristic")

})

