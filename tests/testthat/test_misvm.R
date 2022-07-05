set.seed(8)
mil_data <- generate_mild_df(nbag = 20,
                             nsample = 20,
                             nimp_pos = 1:5, nimp_neg = 1:5,
                             positive_prob = 0.15,
                             dist = rep("mvnormal", 3),
                             mean = list(rep(2, 5), rep(0, 5), 0),
                             sd_of_mean = rep(0.1, 3))

mil_data_test <- generate_mild_df(nbag = 40,
                                  nsample = 20,
                                  nimp_pos = 1:5, nimp_neg = 1:5,
                                  positive_prob = 0.15,
                                  dist = rep("mvnormal", 3),
                                  mean = list(rep(2, 5), rep(0, 5), 0),
                                  sd_of_mean = rep(0.1, 3))

df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
  dplyr::select(-instance_name)

df1_test <- build_instance_feature(mil_data_test, seq(0.05, 0.95, length.out = 10)) %>%
  dplyr::select(-instance_name)

run_misvm <- function(df = df1, features = 3:122, ...) {
  misvm.default(x = df[, features],
                y = df$bag_label,
                bags = df$bag_name,
                ...)
}

test_that("misvm() works for data-frame-like inputs", {
  skip_if_not_installed("gurobi")
  mdl1 <- run_misvm(method = "mip")

  expect_equal(
    predict(mdl1, new_data = df1, type = "class", layer = "bag"),
    predict(mdl1, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )
  predict(mdl1, new_data = df1, type = "class", layer = "bag")
  predict(mdl1, new_data = df1, type = "class", layer = "instance")
  predict(mdl1, new_data = df1, type = "raw", layer = "bag")
  predict(mdl1, new_data = df1, type = "raw", layer = "instance")

  # heuristic method
  mdl2 <- run_misvm(method = "heuristic")

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

  # qp-heuristic method
  mdl3 <- run_misvm(method = "qp-heuristic")

  expect_equal(
    predict(mdl3, new_data = df1, type = "class", layer = "bag"),
    predict(mdl3, new_data = df1, type = "class", layer = "bag", new_bags = df1$bag_name)
  )

  predict(mdl3, new_data = df1, type = "class", layer = "bag")
  predict(mdl3, new_data = df1, type = "class", layer = "instance")
  predict(mdl3, new_data = df1, type = "raw", layer = "bag")
  predict(mdl3, new_data = df1, type = "raw", layer = "instance")

  df1 %>%
    dplyr::bind_cols(predict(mdl3, new_data = df1, type = "raw", layer = "bag")) %>%
    dplyr::bind_cols(predict(mdl3, new_data = df1, type = "class", layer = "bag")) %>%
    dplyr::distinct(bag_label, bag_name, .pred, .pred_class) %>%
    tibble::as_tibble()

})

test_that("misvm() works with formula method", {
  skip_if_not_installed("gurobi")
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df1)
  mdl2 <- run_misvm(features = c("X1_mean", "X2_mean", "X3_mean"))

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
  skip_if_not_installed("gurobi")
  test_prediction_levels_equal <- function(df, method, class = "default") {
    mdl <- switch(class,
                  "default" = run_misvm(df, method = method),
                  "formula" = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean,
                                    data = df2,
                                    method = method))
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
  expect_message(test_prediction_levels_equal(df2, method = "heuristic", class = "formula"))

  # check that 0/1 and 1/0 return the same predictions
  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(0, 1)))
  df3 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df2)
  mdl3 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df3)
  expect_equal(predict(mdl2, df2, type = "class"),
               predict(mdl3, df3, type = "class"))

})

test_that("Dots work in misvm() formula", {
  skip_if_not_installed("gurobi")
  df2 <- df1 %>% dplyr::select(bag_label, bag_name, X1_mean, X2_mean, X3_mean)

  misvm_dot <- misvm(mi(bag_label, bag_name) ~ ., data = df2)
  misvm_nodot <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df2)

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("misvm() has correct argument handling", {
  skip_if_not_installed("gurobi")
  ## weights
  misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE)
  mdl1 <- misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 1, "1" = 1))
  mdl1$weights <- NULL
  mdl2 <- misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = FALSE)
  expect_equal(mdl1, mdl2)

  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  dimnames(df2) <- dimnames(df1)
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1)),
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1))
  )
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip"),
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1), method = "mip")
  )

  df2 <- df1 %>% dplyr::mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  dimnames(df2) <- dimnames(df1)
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1))$svm_fit,
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1))$svm_fit
  ) %>%
    expect_message()
  expect_equal(
    misvm(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "mip")$gurobi_fit,
    misvm(mi(bag_label, bag_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1), method = "mip")$gurobi_fit
  ) %>%
    expect_message()

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
  skip_if_not_installed("gurobi")
  verbose <- interactive()

  # manually check that the output says "User MIP start produced solution with objective ..."
  mdl1 <- run_misvm(method = "mip",
                    control = list(start = TRUE, verbose = verbose))

  mdl2 <- run_misvm(method = "mip",
                    control = list(start = FALSE, verbose = verbose))

  expect_equal(mdl1$model[c("w", "b", "xi", "z")],
               mdl2$model[c("w", "b", "xi", "z")])

  # Hard to test whether the warm start improves the time to reach a solution without testing large problems

})

test_that("misvm mip works with radial kernel", {
  skip_if_not_installed("gurobi")
  set.seed(8)
  mil_data <- generate_mild_df(nbag = 10,
                               nsample = 20,
                               positive_prob = 0.15)

  df1 <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

  mdl1 <- run_misvm(df1, features = 4:12,
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
  mdl2 <- run_misvm(features = 4:12,
                    method = "mip",
                    control = list(kernel = "radial",
                                   sigma = 1,
                                   nystrom_args = list(m = m, r = r)))

  expect_equal(dim(mdl2$kfm_fit$dv), c(r, m))
  expect_equal(dim(mdl2$kfm_fit$df_sub), c(m, length(4:12)))

  # Running with defaults (linear kernel) shouldn't have the kfm_fit element
  mdl1 <- run_misvm(features = 4:12, method = "mip")
  expect_null(mdl1$kfm_fit)

})

test_that("`misvm()` works with `mi_df` method", {
  predictors <- c("X1_mean", "X2_mean", "X3_mean")
  df1_mi <- as_mi_df(df1[, c("bag_label", "bag_name", predictors)], instance_label = NULL)
  mdl1 <- misvm(df1_mi)
  mdl2 <- run_misvm(df1, predictors)

  expect_equal(mdl1$model, mdl2$model)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$call_type, "misvm.mi_df")
  expect_equal(mdl1$features, predictors)
  expect_equal(mdl1$bag_name, "bag_name")

  # predictions should match
  expect_equal(predict(mdl1, df1, type = "raw"), predict(mdl2, df1, type = "raw"))
  expect_equal(predict(mdl1, df1, type = "class"), predict(mdl2, df1, type = "class"))
})

test_that("misvm() works on 'mild_df' objects", {
  skip_if_not_installed("gurobi")
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
  scrambled <- mil_data_test[sample(seq_len(nrow(mil_data_test))), ]

  instance_level <- scrambled %>%
    dplyr::bind_cols(predict(mdl, scrambled, type = "raw", layer = "instance")) %>%
    dplyr::distinct(bag_label, instance_name, .pred)
  expect_equal(nrow(instance_level), length(unique(scrambled$instance_name)))

  bag_level <- scrambled %>%
    dplyr::bind_cols(predict(mdl, scrambled, type = "raw", layer = "bag")) %>%
    dplyr::distinct(bag_label, bag_name, .pred)
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
  skip_if_not_installed("gurobi")

  expect_snapshot({
    models <- list(
      "xy-heur" = run_misvm(method = "heuristic"),
      "xy-mip" = run_misvm(method = "mip"),
      "xy-qp" = run_misvm(method = "qp-heuristic"),
      "formula" = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, method = "heuristic", data = df1),
      "mi_df" = misvm(as_mi_df(df1, instance_label = NULL)),
      "mildata" = misvm(mil_data),
      "no-scale-heur" = run_misvm(method = "heuristic", control = list(scale = FALSE)),
      "no-scale-mip" = run_misvm(method = "mip", control = list(scale = FALSE)),
      "no-scale-qp" = run_misvm(method = "qp-heuristic", control = list(scale = FALSE)),
      "kfm_fit" = misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df1, method = "mip", control = list(kernel = "radial")),
      "no-weights-heur" = run_misvm(method = "heuristic", weights = FALSE),
      "no-weights-mildata" = misvm(mil_data)
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)
})

test_that("Ordering of data doesn't change `misvm()` results", {
  skip_if_not_installed("gurobi")
  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"))
  }

  ind <- sample(seq_len(nrow(df1)))
  # heuristic
  form <- mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean
  mdl1 <- misvm(form, data = df1, method = "heuristic")
  mdl2 <- misvm(form, data = df1[ind, ], method = "heuristic")
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  expect_snapshot({
    with(df1_test, {
      pred <- predict(mdl2, df1_test, type = "raw")$.pred
      pROC::auc(classify_bags(bag_label, bag_name),
                classify_bags(pred, bag_name))
    })
  })

  # qp-heuristic
  mdl1 <- misvm(form, data = df1, method = "qp-heuristic")
  mdl2 <- misvm(form, data = df1[ind, ], method = "qp-heuristic")
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  expect_snapshot({
    with(df1_test, {
      pred <- predict(mdl2, df1_test, type = "raw")$.pred
      pROC::auc(classify_bags(bag_label, bag_name),
                classify_bags(pred, bag_name))
    })
  })

  # mild_df object
  mdl1 <- misvm(mil_data)
  mdl2 <- misvm(mil_data[sample(seq_len(nrow(mil_data))), ])
  expect_predictions_equal(mdl1, mdl2, mil_data)
  expect_predictions_equal(mdl1, mdl2, mil_data_test)

  expect_snapshot({
    with(mil_data_test, {
      pred <- predict(mdl2, mil_data_test, type = "raw")$.pred
      pROC::auc(classify_bags(bag_label, bag_name),
                classify_bags(pred, bag_name))
    })
  })

})

test_that("`misvm()` works even when there are Nan columns or idential columns", {
  skip_if_not_installed("gurobi")
  df2 <- df1
  df2$nan_feature <- NaN

  expect_warning({
    mdl1 <- run_misvm(df2, features = 3:123, method = "qp-heuristic")
  })

  expect_warning({
    mdl2 <- run_misvm(df2, features = c(3:123, 123), method = "qp-heuristic")
  })

  pred <- predict(mdl1, new_data = df1_test, layer = "instance", type = "raw")
  pred <- predict(mdl2, new_data = df1_test, layer = "instance", type = "raw")

  df3 <- df2
  df3$ident_feature <- 50

  expect_warning(expect_warning({
    mdl1 <- run_misvm(df3, features = 3:124, method = "qp-heuristic")
  }))
  pred <- predict(mdl1, new_data = df1_test, layer = "instance", type = "raw")

})

test_that("`misvm()` works with a few bags with only one instance", {
  skip_if_not_installed("gurobi")
  df2 <- df1[-(2:4), ]
  table(df2$bag_name)

  mdl <- run_misvm(df2, features = 3:10, method = "qp-heuristic")

  expect_s3_class(mdl, "misvm")
})

test_that("`misvm()` works fine with matrices", {
  skip_if_not_installed("gurobi")
  for (method in c("heuristic", "mip", "qp-heuristic")) {
    mdl <- misvm(x = as.matrix(df1[, 3:10]),
                 y = df1$bag_label,
                 bags = df1$bag_name,
                 method = method)
    expect_s3_class(mdl, "misvm")
  }
})


test_that("`misvm.formula()` handles identical columns correctly.", {
  skip_if_not_installed("gurobi")
  df2 <- df1
  df2$constant <- 1

  for (method in c("heuristic", "qp-heuristic", "mip")) {
    expect_warning({
      mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + constant,
                    data = df2,
                    method = method)
    }, "Cannot use columns"  )

    expect_warning({
      mdl2 <- misvm(x = df2[, c("X1_mean", "X2_mean", "constant")],
                    y = df2$bag_label,
                    bags = df2$bag_name,
                    method = method)
    })

    expect_equal(mdl1$features, c("X1_mean", "X2_mean"))

    expect_equal(
      predict(mdl1, new_data = df2),
      predict(mdl2, new_data = df2)
    )
  }


})

test_that("Extra factor levels don't cause problems for `misvm()`", {
  skip_if_not_installed("gurobi")
  df2 <- df1
  df2$bag_name <- as.factor(as.numeric(factor(df2$bag_name)))
  # all sorts of factor mutilations
  levels(df2$bag_name) <- c(levels(df2$bag_name), "bag1001")
  df2$bag_name <- relevel(df2$bag_name, 10)
  df2$bag_name

  method <- "qp-heuristic"
  mdl1 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean, data = df2, method = method)
  mdl2 <- misvm(df2[, 3:10], df2$bag_label, df2$bag_name, method = method)

  pred <- predict(mdl1, new_data = df1)
  pred <- predict(mdl2, new_data = df1)

  expect_true(TRUE)
})

test_that("Formulas with spaces in names work for `misvm()`", {
  skip_if_not_installed("gurobi")
  df2 <- df1[, 1:10]

  colnames(df2)[3] <- "space name"

  method <- "qp-heuristic"
  mdl1 <- misvm(mi(bag_label, bag_name) ~ ., data = df2, method = method)
  mdl2 <- misvm(mi(bag_label, bag_name) ~ `space name` + X1_0.15, data = df2, method = method)

  mdl2$features
  predict(mdl1, df2)
  predict(mdl2, df2)

  expect_true(TRUE)
})

