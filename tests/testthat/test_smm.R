## Generic data set to work with
set.seed(8)
n_instances <- 10
n_samples <- 20
y <- rep(c(1, -1), each = n_samples * n_instances / 2)
instances <- as.character(rep(1:n_instances, each = n_samples))
x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
                x2 = rnorm(length(y), mean = 2*(y==1)),
                x3 = rnorm(length(y), mean = 3*(y==1)))

new_inst <- rep(c("11", "12"), each = 30)
new_y <- rep(c(1, -1), each = 30)
new_x <- data.frame(x1 = rnorm(length(new_inst), mean = 1*(new_inst=="11")),
                    x2 = rnorm(length(new_inst), mean = 2*(new_inst=="11")),
                    x3 = rnorm(length(new_inst), mean = 3*(new_inst=="11")))

## mild_df set to work with
mil_df <- generate_mild_df(nbag = 10,
                           nsample = 20,
                           nimp_pos = 1:5, nimp_neg = 1:5,
                           positive_prob = 0.15,
                           dist = rep("mvnormal", 3),
                           mean = list(rep(2, 5), rep(0, 5), 0))

mil_df_test <- generate_mild_df(nbag = 20,
                                nsample = 20,
                                nimp_pos = 1:5, nimp_neg = 1:5,
                                positive_prob = 0.15,
                                dist = rep("mvnormal", 3),
                                mean = list(rep(2, 5), rep(0, 5), 0))

test_that("smm() works for data-frame-like inputs", {

  mdl <- smm(x, y, instances, control = list(sigma = 1/3))
  expect_s3_class(mdl, "smm")
  expect_equal(mdl$call_type, "smm.default")
  expect_s4_class(mdl$ksvm_fit, "ksvm")

  pred <- predict(mdl, type = "raw", new_data = rbind(x, new_x), new_instances = c(instances, new_inst))
  expect_equal(nrow(pred), nrow(rbind(x, new_x)))
  expect_equal(colnames(pred), ".pred")
  expect_equal(length(unique(pred$.pred)), length(unique(c(instances, new_inst))))

  # data.frame(instance_name = instances, y = y, x) %>%
  #   dplyr::bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
  #   dplyr::bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
  #   dplyr::distinct(instance_name, y, .pred, .pred_class)
  #
  # data.frame(instance_name = new_inst, y = new_y, new_x) %>%
  #   dplyr::bind_cols(predict(mdl, type = "raw", new_data = new_x, new_instances = new_inst)) %>%
  #   dplyr::bind_cols(predict(mdl, type = "class", new_data = new_x, new_instances = new_inst)) %>%
  # dplyr::distinct(instance_name, y, .pred, .pred_class)
})

test_that("smm() works with formula method", {

  df <- data.frame(y = y, instance_name = instances, x)

  mdl <- smm(y ~ x1 + x2 + x3, data = df)
  expect_s3_class(mdl, "smm")
  expect_equal(mdl$call_type, "smm.formula")
  expect_equal(mdl$instance_name, "instance_name")
  expect_s4_class(mdl$ksvm_fit, "ksvm")
  expect_s3_class(mdl$formula, "formula")

  pred <- predict(mdl, type = "raw", new_data = rbind(x, new_x), new_instances = c(instances, new_inst))
  expect_equal(nrow(pred), nrow(rbind(x, new_x)))
  expect_equal(colnames(pred), ".pred")
  expect_equal(length(unique(pred$.pred)), length(unique(c(instances, new_inst))))

  # check equivalence to default method
  mdl1 <- smm(x, y, instances)
  common_components <- c("ksvm_fit", "sigma", "cost", "levels", "features")
  expect_equal(mdl[common_components], mdl1[common_components])
  expect_equal(mdl$traindata, mdl1$traindata, ignore_attr = TRUE)

  # predictions should also match
  expect_equal(predict(mdl, df, type = "raw"), predict(mdl1, df, type = "raw"))
  expect_equal(predict(mdl, df, type = "class"), predict(mdl1, df, type = "class"))

  # check only 1 predictor works
  mdl <- smm(y ~ x1, data = df)
  predict(mdl, df, type = "raw")

  # check some obscure formulas
  mdl <- smm(y ~ 0 + x1 + x1:x2 + x1*x3, data = df)
  expect_equal(mdl$features,
               colnames(model.matrix(~ 0 + x1 + x1:x2 + x1*x3, data = df)))
  predict(mdl, df, type = "raw")

})

test_that("smm() works with mild_df method", {

  mdl <- smm(mil_df)
  expect_s3_class(mdl, "smm")
  expect_equal(mdl$call_type, "smm.mild_df")
  expect_s4_class(mdl$ksvm_fit, "ksvm")

  pred <- predict(mdl, mil_df, type = "raw")
  expect_equal(nrow(pred), nrow(mil_df))
  expect_equal(colnames(pred), ".pred")
  expect_equal(length(unique(pred$.pred)), length(unique(mil_df$instance_name)))

  mdl <- smm(mil_df, weights = FALSE)
  pred <- predict(mdl, mil_df, type = "class", layer = "bag")
})

test_that("smm() has correct argument handling", {

  # `cost`
  expect_false(isTRUE(all.equal(
    smm(x, y, instances, cost = 1),
    smm(x, y, instances, cost = 1000)
  )))

  # `weights`
  mdl1 <- smm(x, y, instances, weights = c("-1" = 1, "1" = 1))
  mdl1$weights <- NULL
  mdl2 <- smm(x, y, instances, weights = FALSE)
  expect_equal(mdl1, mdl2)

  y2 <- factor(y, levels = c(1, -1))
  expect_equal(
    smm(x, y, instances, weights = c("-1" = 2, "1" = 1)),
    smm(x, y2, instances, weights = c("-1" = 2, "1" = 1))
  )

  y2 <- factor(y, labels = c("No", "Yes"))
  expect_message(expect_equal(
    smm(x, y, instances, weights = c("-1" = 2, "1" = 1))$ksvm_fit,
    smm(x, y2, instances, weights = c("No" = 2, "Yes" = 1))$ksvm_fit
  ))

  y <- factor(y)
  # ctrl <- list(sigma = 1/3)
  expect_false(isTRUE(all.equal(
    smm(x, y, instances, weights = c("-1" = 2e6, "1" = 1))$ksvm_fit,
    smm(x, y, instances, weights = c("-1" = 1e-6, "1" = 1))$ksvm_fit
  )))

  # `kernel`
  shared <- c("ksvm_fit", "call_type", "x", "features", "levels",
              "cost", "sigma", "weights")
  kernel_mat <- kme(data.frame(instance_name = instances, x), sigma = 1/3)
  expect_message(expect_equal(
    smm(x, y, instances, control = list(kernel = "radial", sigma = 1/3, scale = FALSE))[shared],
    smm(x, y, instances, control = list(kernel = kernel_mat))[shared]
  ))
  suppressMessages({
    mdl1 <- smm(x, y, instances, control = list(kernel = kernel_mat))
  })
  predict(mdl1, type = "raw", new_data = new_x, new_instances = new_inst)

  # `sigma`
  expect_false(isTRUE(all.equal(
    smm(x, y, instances, control = list(sigma = 1/10)),
    smm(x, y, instances, control = list(sigma = 1/2))
  )))

  expect_equal(
    smm(x, y, instances),
    smm(x, y, instances, control = list(sigma = 1/3))
  )

  df <- data.frame(y = y, instance_name = instances, x)
  expect_equal(
    smm(y ~ x1 + x2 + x3, data = df),
    smm(y ~ x1 + x2 + x3, data = df, control = list(sigma = 1/3))
  )

  df <- generate_mild_df(nbag = 10,
                         nsample = 20,
                         positive_prob = 0.15)
  expect_equal(
    smm(df),
    smm(df, control = list(sigma = 1/10))
  )

  # `scale`
  expect_false(isTRUE(all.equal(
    smm(x, y, instances, control = list(scale = TRUE)),
    smm(x, y, instances, control = list(scale = FALSE))
  )))
  mdl <- smm(x, y, instances, control = list(scale = TRUE))
  expect_type(mdl$x_scale$center, "double")
  expect_type(mdl$x_scale$scale, "double")
  expect_equal(length(mdl$x_scale$center), ncol(x))

})

test_that("Dots work in smm() formula", {
  df <- data.frame(y = y, instance_name = instances, x)

  smm_dot <- smm(y ~ ., data = df)
  smm_nodot <- smm(y ~ x1 + x2 + x3, data = df)

  common_components <- setdiff(names(smm_dot), "formula")
  expect_equal(smm_dot[common_components], smm_nodot[common_components])
  expect_equal(predict(smm_dot, new_data = df), predict(smm_nodot, new_data = df))

})

test_that("predict.smm has correct argument handling", {
  df <- data.frame(y = y, instance_name = instances, x)
  mdl_x <- smm(x, y, instances)
  mdl_f <- smm(y ~ x1 + x2 + x3, data = df)

  new_df <- data.frame(y = NA, instance_name = new_inst, new_x)

  # `new_data` and `new_instances`
  # adding extra columns to new_data shouldn't change predictions
  expect_equal(
    predict(mdl_x, new_data = new_df, type = "raw"),
    predict(mdl_x, new_data = cbind(new_df, useless = "blah"), type = "raw")
  )
  expect_equal(
    predict(mdl_f, new_data = new_df, type = "raw"),
    predict(mdl_f, new_data = cbind(new_df, useless = "blah"), type = "raw")
  )

  # passing new_instances separately shouldn't change predictions
  expect_equal(
    predict(mdl_x, new_data = new_df, type = "raw"),
    predict(mdl_x, new_data = new_x, new_instances = new_inst, type = "raw")
  )
  expect_equal(
    predict(mdl_f, new_data = new_df, type = "raw"),
    predict(mdl_f, new_data = new_x, new_instances = new_inst, type = "raw")
  )

  # re-naming the instances column shouldn't change predictions
  new_df2 <- new_df
  colnames(new_df2) <- c("y", "my_inst", "x1", "x2", "x3")
  expect_equal(
    predict(mdl_x, new_data = new_df, type = "raw"),
    predict(mdl_x, new_data = new_df2, new_instances = "my_inst", type = "raw")
  )
  expect_equal(
    predict(mdl_f, new_data = new_df, type = "raw"),
    predict(mdl_f, new_data = new_df2, new_instances = "my_inst", type = "raw")
  )

  # `type`
  pred <- predict(mdl_x, new_data = new_df, type = "raw")
  expect_equal(colnames(pred), ".pred")
  expect_s3_class(pred, "tbl_df")
  expect_type(pred$.pred, "double")

  pred <- predict(mdl_f, new_data = new_df, type = "raw")
  expect_equal(colnames(pred), ".pred")
  expect_s3_class(pred, "tbl_df")
  expect_type(pred$.pred, "double")

  pred <- predict(mdl_x, new_data = new_df, type = "class")
  expect_equal(colnames(pred), ".pred_class")
  expect_s3_class(pred, "tbl_df")
  expect_s3_class(pred$.pred_class, "factor")

  pred <- predict(mdl_f, new_data = new_df, type = "class")
  expect_equal(colnames(pred), ".pred_class")
  expect_s3_class(pred, "tbl_df")
  expect_s3_class(pred$.pred_class, "factor")

  # `kernel`
  # passing kernel in shouldn't change predictions
  kernel_mat <- kme(data.frame(instance_name = instances, scale(x)), sigma = 1/3)
  expect_message(expect_equal(
    predict(mdl_x, new_data = df, type = "raw"),
    predict(mdl_x, new_data = NULL, new_instances = instances, type = "raw", kernel = kernel_mat)
  ))

  new_km <- kme(data.frame(instance_name = new_inst, scale(new_x, mdl_x$x_scale$center, mdl_x$x_scale$scale)),
                data.frame(instance_name = instances, scale(x)),
                sigma = 1/3)
  expect_message(expect_equal(
    predict(mdl_x, new_data = new_x, new_instances = new_inst, type = "raw"),
    predict(mdl_x, new_data = NULL, new_instances = new_inst, type = "raw", kernel = new_km)
  ))

  kernel_mat <- kme(data.frame(instance_name = instances, x), sigma = 1/3)
  expect_message(expect_false(isTRUE(all.equal(
    predict(mdl_x, new_data = df, type = "raw"),
    predict(mdl_x, new_data = NULL, new_instances = instances, type = "raw", kernel = kernel_mat)
  ))))


})

test_that("predict.smm() works when fit with smm.mild_df()", {
  mdl <- smm(mil_df)

  expect_equal(
    predict(mdl, new_data = mil_df, type = "raw", layer = "bag"),
    predict(mdl, new_data = mil_df, type = "raw", layer = "bag", new_bags = "bag_name")
  )
  expect_equal(
    predict(mdl, new_data = mil_df, type = "raw", layer = "bag"),
    predict(mdl, new_data = mil_df %>% dplyr::select(-bag_name),
            type = "raw", layer = "bag", new_bags = mil_df$bag_name)
  )

  pred <- predict(mdl, new_data = mil_df, type = "raw", layer = "bag")
  expect_lte(nrow(dplyr::distinct(pred)), length(unique(mil_df$bag_name)))

  pred <- predict(mdl, new_data = mil_df, type = "raw", layer = "instance")
  expect_lte(nrow(dplyr::distinct(pred)), length(unique(mil_df$instance_name)))

})

test_that("predict.smm returns labels that match the input labels", {
  test_prediction_levels_equal <- function(df, class = "default") {
    mdl <- switch(class,
                  "default" = smm(x = df[, 3:5],
                                  y = df$y,
                                  instances = df$instance_name),
                  "formula" = smm(y ~ x1 + x2 + x3, data = df)
    )
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$y))
  }

  df <- data.frame(y = y, instance_name = instances, x)

  # -1/1
  df1 <- df %>% dplyr::mutate(y = factor(y, labels = c(-1, 1)))
  test_prediction_levels_equal(df1)
  test_prediction_levels_equal(df1, class = "formula")

  # 0/1
  df1 <- df %>% dplyr::mutate(y = factor(y, labels = c(0, 1)))
  test_prediction_levels_equal(df1)
  test_prediction_levels_equal(df1, class = "formula")

  # 1/0
  df1 <- df %>% dplyr::mutate(y = factor(y, labels = c(1, 0)))
  test_prediction_levels_equal(df1)
  test_prediction_levels_equal(df1, class = "formula")

  # TRUE/FALSE
  df1 <- df %>% dplyr::mutate(y = factor(y, labels = c(TRUE, FALSE)))
  test_prediction_levels_equal(df1)
  test_prediction_levels_equal(df1, class = "formula")

  # Yes/No
  df1 <- df %>% dplyr::mutate(y = factor(y, labels = c("No", "Yes")))
  expect_message(test_prediction_levels_equal(df1))
  expect_message(test_prediction_levels_equal(df1, class = "formula"))

  # check that -1/1 and 1/-1 return the same predictions
  df1 <- df %>% dplyr::mutate(y = factor(y, levels = c(-1, 1)))
  df2 <- df %>% dplyr::mutate(y = factor(y, levels = c(1, -1)))
  mdl1 <- smm(y ~ ., data = df1)
  mdl2 <- smm(y ~ ., data = df2)
  expect_equal(predict(mdl1, df1, type = "class"),
               predict(mdl2, df2, type = "class"))

})

test_that("Re-ordering data doesn't reduce performance", {

  set.seed(8)
  mdl1 <- smm(mil_df, control = list(sigma = 0.1))
  mdl2 <- smm(mil_df[sample(seq_len(nrow(mil_df))), ], control = list(sigma = 0.1))

  pred1 <- predict(mdl1, mil_df_test, type = "raw", layer = "bag")
  pred2 <- predict(mdl2, mil_df_test, type = "raw", layer = "bag")

  expect_snapshot({
    auc1 <- with(mil_df_test,
                 pROC::auc(response = classify_bags(bag_label, bag_name),
                           predictor = classify_bags(pred1$.pred, bag_name)))
    auc2 <- with(mil_df_test,
                 pROC::auc(response = classify_bags(bag_label, bag_name),
                           predictor = classify_bags(pred2$.pred, bag_name)))

    # the auc2 should be in the neighborhood of auc1
    auc1; auc2
    eps <- 0.05
  })

  expect_gte(auc2, auc1 - eps)
  expect_lte(auc2, auc1 + eps)
})

test_that("`smm()` value returns make sense", {

  df <- data.frame(y = y, instance_name = instances, x)

  expect_snapshot({
    models <- list(
      "xy" = smm(x, y, instances),
      "formula" = smm(y ~ x1 + x2 + x3, data = df),
      "mildata" = smm(mil_df),
      "no-scale-xy" = smm(x, y, instances, control = list(scale = FALSE)),
      "no-scale-mildata" = smm(mil_df, control = list(scale = FALSE)),
      "no-weights-xy" = smm(x, y, instances, weights = FALSE),
      "no-weights-mildata" = smm(mil_df, weights = FALSE)
    ) %>%
      suppressWarnings() %>%
      suppressMessages()

    print(lapply(models, names))
    print(models)
  })
  expect_true(TRUE)



  expect_true(TRUE)
})

