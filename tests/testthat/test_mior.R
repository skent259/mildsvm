context("Testing the functions in mior.R")
suppressWarnings({
  library(dplyr)
  library(tibble)
})

# Build a sample data set ------------------------------------------------------
# - 3 columns, where there is a representative point and other points that are irrelevant

# start with representative points
set.seed(8)
X <- rbind(
  mvtnorm::rmvnorm(100, mean = c(4, -2, 0)),
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0)),
  mvtnorm::rmvnorm(100, mean = c(-2, 1, 0))
)
score <- X %*% c(2, -1, 0)
y <- as.numeric(cut(score, c(-Inf, quantile(score, probs = 1:2 / 3), Inf)))
bags <- 1:length(y)

# add in points outside boundaries
X <- rbind(
  X,
  mvtnorm::rmvnorm(300, mean = c(6, -3, 0)),
  mvtnorm::rmvnorm(300, mean = c(-6, 3, 0))
)
y <- c(y, rep(-1, 600))
bags <- rep(bags, 3)
repr <- c(rep(1, 300), rep(0, 600))

y_bag <- classify_bags(y, bags, condense = FALSE)

df <- bind_cols(bag_label = y_bag,
                bag_name = bags,
                repr = repr,
                as.data.frame(X))

train <- bags %in% 1:150
df1 <- df[train, ]
df1_test <- df[!train, ]

# Tests ------------------------------------------------------------------------

test_that("mior() internal functions work on simple examples", {
  set.seed(8)
  cost <- 1
  suppressWarnings({
    res <- mior_dual_fit(df$bag_label, df$bag_name, df[, 3:5], c0 = cost, c1 = cost,
                         rescale = TRUE, verbose = FALSE, option = "corrected")
  })

  res$gurobi_fit$w
  res$gurobi_fit$w / max(abs(res$gurobi_fit$w)) # relative
  # res$gurobi_fit$a
  f <- as.matrix(X) %*% res$gurobi_fit$w # + b_

  b_ <- res$gurobi_fit$b
  ind <- 2:length(b_)
  midpoints <- (b_[ind-1] + b_[ind]) / 2

  # plot(f, y)
  # abline(v = midpoints)

  tmp <- abs(outer(as.vector(f), midpoints, `-`))
  y_pred <- sapply(unique(bags), function(b) {
    instance_min <- apply(tmp[which(bags == b), ], 2, min)
    which.min(instance_min)
  })
  names(y_pred) <- NULL

  # evaluation measures
  table(y_pred, y[1:300]) # confusion matrix
  pROC::multiclass.roc(response = y[1:300],
                       predictor = y_pred)
  mzoe <- mean(y[1:300] != y_pred)
  mae <- mean(abs(y[1:300] - y_pred))

  mzoe; mae
})

test_that("`mior()` has reasonable performance", {

  set.seed(8)
  expect_warning({
    mdl1 <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df1,
                 cost = 1e5, cost_eta = 1e5, weights = NULL)
  })

  check_performance <- function(model, df, roc_cutoff, mzoe_cutoff, mae_cutoff) {
    preds <- predict(model, new_data = df)
    pred_scores <- predict(model, new_data = df, type = "raw")
    pred_vec <- as.numeric(as.character(preds$.pred_class))

    bag_resp <- df %>% filter(repr == 1) %>% select(bag_name, bag_label) %>% deframe()
    # bag_resp <- with(df, classify_bags(bag_label, bag_name))
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

    if (interactive()) {
      print(roc$auc)
      print(mzoe)
      print(mae)
    }
  }

  # NOTE: performance seems to be quite bad,
  check_performance(mdl1, df1, 0.5, 1, 1.5)
  check_performance(mdl1, df1_test, 0.5, 1, 1.5)

})

test_that("mior() works for data-frame-like inputs", {

  # qp-heuristic method
  expect_warning({
    mdl2 <- mior(x = X,
                 y = y,
                 bags = bags,
                 method = "qp-heuristic")
  })

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
    bind_cols(predict(mdl2, df1, type = "class")) %>%
    group_by(bag_name) %>%
    summarize(bag_label = unique(bag_label),
              .pred = unique(.pred_class))

  expect_equal(nrow(bag_preds), length(unique(df1$bag_name)))
  expect_setequal(bag_preds$bag_name, unique(df1$bag_name))

})

test_that("mior() works with formula method", {
  expect_warning({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df1)
    set.seed(8)
    mdl2 <- mior(x = df1[, paste0("V", 1:3)],
                 y = df1$bag_label,
                 bags = df1$bag_name)
  })

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

  # check only 1 predictor works
  expect_warning({
    mdl1 <- mior(mi(bag_label, bag_name) ~ V1, data = df1)
  })
  predict(mdl1, df1, type = "raw")

  # check some obscure formulas
  expect_warning({
    mdl1 <- mior(mi(bag_label, bag_name) ~ 0 + V1:V2 + V2*V3, data = df1)
  })
  expect_equal(mdl1$features,
               colnames(model.matrix(~ 0 + V1:V2 + V2*V3, data = df1)))
  predict(mdl1, df1, type = "raw")

})

test_that("predict.mior() returns labels that match the input labels", {
  test_prediction_levels_equal <- function(df, method, class = "default") {
    suppressWarnings({
      mdl <- switch(class,
                    "default" = mior(x = df[, 4:6],
                                     y = df$bag_label,
                                     bags = df$bag_name,
                                     method = method),
                    "formula" = mior(mi(bag_label, bag_name) ~ V1 + V2 + V3,
                                     data = df,
                                     method = method))
    })
    preds <- predict(mdl, df, type = "class")
    expect_setequal(levels(preds$.pred_class), levels(df$bag_label))
  }

  # 1:5
  df2 <- df1 %>% mutate(bag_label = factor(bag_label))
  test_prediction_levels_equal(df2, method = "qp-heuristic")
  test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula")

  # 1/0
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = 3:1))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # Characters
  df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("A", "B", "C")))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic"))
  expect_message(test_prediction_levels_equal(df2, method = "qp-heuristic", class = "formula"))

  # check re-naming of factors returns the same predictions
  df2 <- df1
  df3 <- df1 %>% mutate(bag_label = ordered(bag_label, labels = letters[1:3]))
  suppressWarnings({
    set.seed(8)
    mdl2 <- mior(mi(bag_label, bag_name) ~ V1 + V2, data = df2, weights = NULL)
    set.seed(8)
    mdl3 <- mior(mi(bag_label, bag_name) ~ V1 + V2, data = df3, weights = NULL)
  })
  expect_equivalent(predict(mdl2, df2, type = "class") %>% mutate(.pred_class = ordered(.pred_class, labels = letters[1:3])),
                    predict(mdl3, df3, type = "class"))
  # NOTE: re-ordering of the factors in this case WILL NOT return the same model, and this is expected

})

test_that("Dots work in mior() formula", {
  df2 <- df1 %>% select(bag_label, bag_name, V1, V2, V3)

  expect_warning({
    set.seed(8)
    misvm_dot <- mior(mi(bag_label, bag_name) ~ ., data = df2)
    set.seed(8)
    misvm_nodot <- mior(mi(bag_label, bag_name) ~ V1 + V2 + V3, data = df2)
  })

  expect_equal(misvm_dot$model, misvm_nodot$model)
  expect_equal(misvm_dot$features, misvm_nodot$features)
  expect_equal(misvm_dot$bag_name, misvm_nodot$bag_name)

  expect_equal(predict(misvm_dot, new_data = df2), predict(misvm_nodot, new_data = df2))

})

test_that("mior() has correct argument handling", {
  # `weights`
  expect_warning(mior(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE), "Weights")
  expect_warning(mior(mi(bag_label, bag_name) ~ ., data = df1, weights = NULL), "Step")
  # mior(mi(bag_label, bag_name) ~ ., data = df1, weights = TRUE)
  # mdl1 <- mior(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 1, "1" = 1))
  # mdl1$weights <- NULL
  # mdl2 <- mior(mi(bag_label, bag_name) ~ ., data = df1, weights = FALSE)
  # expect_equal(mdl1, mdl2)
  #
  # df2 <- df1 %>% mutate(bag_label = factor(bag_label, levels = c(1, 0)))
  # dimnames(df2) <- dimnames(df1)
  # expect_equal(
  #   mior(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1)),
  #   mior(mi(bag_label, bag_name) ~ ., data = df2, weights = c("0" = 2, "1" = 1))
  # )
  #
  # df2 <- df1 %>% mutate(bag_label = factor(bag_label, labels = c("No", "Yes")))
  # dimnames(df2) <- dimnames(df1)
  # expect_equal(
  #   mior(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1))$svm_fit,
  #   mior(mi(bag_label, bag_name) ~ ., data = df2, weights = c("No" = 2, "Yes" = 1))$svm_fit
  # )
  #
  # expect_false(isTRUE(all.equal(
  #   mior(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 2, "1" = 1), method = "qp-heuristic")$gurobi_fit,
  #   mior(mi(bag_label, bag_name) ~ ., data = df1, weights = c("0" = 1e-6, "1" = 1), method = "qp-heuristic")$model
  # )))

  # `kernel`
  # NOTE: currently only "linear" kernel should work
  suppressWarnings({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic", weights = NULL, control = list(kernel = "radial"))
    set.seed(8)
    mdl2 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic", weights = NULL, control = list(kernel = "linear"))
  })
  expect_equal(mdl1, mdl2)

  # `scale`
  suppressWarnings({
    set.seed(8)
    mdl1 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic", weights = NULL, control = list(scale = TRUE))
    set.seed(8)
    mdl2 <- mior(mi(bag_label, bag_name) ~ ., data = df1, method = "qp-heuristic", weights = NULL, control = list(scale = FALSE))
  })
  expect_false(isTRUE(all.equal(mdl1, mdl2)))

})

test_that("`mior()` value returns make sense", {

  suppressWarnings({
    # different methods
    names(mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic", weights = NULL))

    # different S3 methods
    names(mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name, method = "qp-heuristic", weights = NULL))
    names(mior(mi(bag_label, bag_name) ~ V1 + V2, method = "qp-heuristic", data = df1, weights = NULL))

    # shouldn't have `x_scale`
    names(mior(x = df1[, 4:6], y = df1$bag_label, bags = df1$bag_name,
               method = "qp-heuristic", weights = NULL, control = list(scale = FALSE)))
  })

  expect_true(TRUE)
})

test_that("Ordering of data doesn't change `mior()` results", {
  expect_predictions_equal <- function(model1, model2, data) {
    # If predictions match for `type = 'raw` and `layer = 'instance'`, they will
    # match for all other options.
    expect_equal(predict(model1, data, type = "raw", layer = "instance"),
                 predict(model2, data, type = "raw", layer = "instance"))
  }

  # heuristic
  suppressWarnings({
    form <- mi(bag_label, bag_name) ~ V1 + V2 + V3
    set.seed(8)
    mdl1 <- mior(form, data = df1, method = "qp-heuristic", weights = NULL)
    set.seed(8)
    mdl2 <- mior(form, data = df1[sample(1:nrow(df1)), ], method = "qp-heuristic", weights = NULL)
  })
  expect_predictions_equal(mdl1, mdl2, df1)
  expect_predictions_equal(mdl1, mdl2, df1_test)

  with(df1_test, suppressWarnings({
    pred <- predict(mdl2, df1_test, type = "raw")$.pred
    suppressMessages({
      roc <- pROC::multiclass.roc(response = bag_label, predictor = pred)
      pROC::auc(roc)
    })
    # pROC::auc(classify_bags(bag_label, bag_name),
    #           classify_bags(pred, bag_name))
  }))
})


# # really simple example
# set.seed(8)
# y <- c(rep(1, 5), rep(2, 2), rep(3, 2), rep(4, 3))
# bags <- c(rep(1, 2), rep(2, 3), rep(3, 2), rep(4, 2), rep(5, 3))
#
# X <- matrix(NA, nrow = length(y), ncol = 5)
# for (y_ in unique(y)) {
#   to_fill <- which(y_ == y)
#   X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(3*y_, -3*y_, 1*y_, 0, 0))
# }
# colnames(X) <- paste0("V", 1:ncol(X))
# x <- as.matrix(X)
#
#
#
# set.seed(8)
# # tmp <- mior_dual_fit(y, bags, x, 1, 1, verbose = TRUE)
#
# K <- max(y)
# w_t <- rnorm(ncol(x)) # check to see if there is a suggested initialization
# b_t <- sort(rnorm(K+1))
#
# scores <- as.matrix(x) %*% w_t - (b_t[y] + b_t[y+1]) / 2
# scores <- as.numeric(scores)
# # g <- abs(scores)
# # h <- -classify_bags(-abs(scores), bags, condense = FALSE)
# theta <- compute_theta(g = abs(scores), bags)
# lambda <- sign(scores)
#
# delta <- theta*lambda
# # decompose h, obtain problem 10, dual form of problem 10
# model <- mior_dual_model(x, y, bags, delta, 1, 1)
# colnames(model$A) <- model$varnames
# model$A
#
# gurobi_result <- gurobi::gurobi(model)
#
# compute_b(gurobi_result, model, delta, y, bags, 1, 1)

