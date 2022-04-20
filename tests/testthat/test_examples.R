suppressMessages(suppressWarnings({
  library(mildsvm)
  library(dplyr)
}))


test_that("`mismm()` example works", {

  expect_snapshot({
    set.seed(8)
    mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
                                 sd_of_mean = rep(0.1, 3))

    # Heuristic method
    mdl1 <- mismm(mil_data)
    mdl2 <- mismm(mild(bag_label, bag_name, instance_name) ~ X1 + X2 + X3, data = mil_data)

    # MIP method
    if (require(gurobi)) {
      mdl3 <- mismm(mil_data, method = "mip", control = list(nystrom_args = list(m = 10, r = 10)))
      predict(mdl3, mil_data)
    }

    predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")

    # summarize predictions at the bag layer
    mil_data %>%
      bind_cols(predict(mdl2, mil_data, type = "class")) %>%
      bind_cols(predict(mdl2, mil_data, type = "raw")) %>%
      distinct(bag_name, bag_label, .pred_class, .pred)
  })

  expect_s3_class(mdl1, "mismm")
  expect_s3_class(mdl2, "mismm")
})

test_that("`predict.mismm()` examples work", {
  set.seed(8)
  expect_snapshot({
    mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
                                 sd_of_mean = rep(0.1, 3))

    mdl1 <- mismm(mil_data, control = list(sigma = 1/5))

    # bag level predictions
    mil_data %>%
      bind_cols(predict(mdl1, mil_data, type = "class")) %>%
      bind_cols(predict(mdl1, mil_data, type = "raw")) %>%
      distinct(bag_name, bag_label, .pred_class, .pred)

    # instance level prediction
    mil_data %>%
      bind_cols(predict(mdl1, mil_data, type = "class", layer = "instance")) %>%
      bind_cols(predict(mdl1, mil_data, type = "raw", layer = "instance")) %>%
      distinct(bag_name, instance_name, bag_label, .pred_class, .pred)
  })

  expect_s3_class(mdl1, "mismm")
})

test_that("`misvm()` examples work", {
  expect_snapshot({
    set.seed(8)
    mil_data <- generate_mild_df(nbag = 20,
                                 positive_prob = 0.15,
                                 sd_of_mean = rep(0.1, 3))
    df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))

    # Heuristic method
    mdl1 <- misvm(x = df[, 4:123], y = df$bag_label,
                  bags = df$bag_name, method = "heuristic")
    mdl2 <- misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df)

    # MIP method
    if (require(gurobi)) {
      mdl3 <- misvm(x = df[, 4:123], y = df$bag_label,
                    bags = df$bag_name, method = "mip")
    }

    predict(mdl1, new_data = df, type = "raw", layer = "bag")

    # summarize predictions at the bag layer
    # library(dplyr)
    df %>%
      bind_cols(predict(mdl2, df, type = "class")) %>%
      bind_cols(predict(mdl2, df, type = "raw")) %>%
      distinct(bag_name, bag_label, .pred_class, .pred)

  })

  expect_s3_class(mdl1, "misvm")
  expect_s3_class(mdl2, "misvm")
})

test_that("`cv_misvm()` examples work", {
  expect_snapshot({
    set.seed(8)
    mil_data <- generate_mild_df(nbag = 20,
                                 positive_prob = 0.15,
                                 dist = rep("mvnormal", 3),
                                 mean = list(rep(1, 10), rep(2, 10)),
                                 sd_of_mean = rep(0.1, 3))
    df <- build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))
    cost_seq <- 2^seq(-5, 7, length.out = 3)

    # Heuristic method
    mdl1 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
                     bags = df$bag_name, cost_seq = cost_seq,
                     n_fold = 3, method = "heuristic")
    mdl2 <- cv_misvm(mi(bag_label, bag_name) ~ X1_mean + X2_mean + X3_mean, data = df,
                     cost_seq = cost_seq, n_fold = 3)

    if (require(gurobi)) {
      # solve using the MIP method
      mdl3 <- cv_misvm(x = df[, 4:123], y = df$bag_label,
                       bags = df$bag_name, cost_seq = cost_seq,
                       n_fold = 3, method = "mip")
    }

    predict(mdl1, new_data = df, type = "raw", layer = "bag")

    # summarize predictions at the bag layer
    df %>%
      bind_cols(predict(mdl2, df, type = "class")) %>%
      bind_cols(predict(mdl2, df, type = "raw")) %>%
      distinct(bag_name, bag_label, .pred_class, .pred)
  })

  expect_s3_class(mdl1, "cv_misvm")
  expect_s3_class(mdl2, "cv_misvm")
  expect_s3_class(mdl3, "cv_misvm")
})

test_that("`smm()` examples work", {
  expect_snapshot({
    set.seed(8)
    n_instances <- 10
    n_samples <- 20
    y <- rep(c(1, -1), each = n_samples * n_instances / 2)
    instances <- as.character(rep(1:n_instances, each = n_samples))
    x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
                    x2 = rnorm(length(y), mean = 2*(y==1)),
                    x3 = rnorm(length(y), mean = 3*(y==1)))

    df <- data.frame(instance_name = instances, y = y, x)

    mdl <- smm(x, y, instances)
    mdl2 <- smm(y ~ ., data = df)

    # instance level predictions
    df %>%
      dplyr::bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) %>%
      dplyr::bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) %>%
      dplyr::distinct(instance_name, y, .pred, .pred_class)
  })

  expect_s3_class(mdl, "smm")
  expect_s3_class(mdl2, "smm")
})

test_that("`generate_mild_df()` examples work", {
  expect_snapshot({
    set.seed(8)
    mild_data <- generate_mild_df(nbag = 7, ninst = 3, nsample = 20,
                                  ncov = 2,
                                  nimp_pos = 1,
                                  dist = rep("mvnormal", 3),
                                  mean = list(
                                    rep(5, 1),
                                    rep(15, 2),
                                    0
                                  ))

    library(dplyr)
    distinct(mild_data, bag_label, bag_name, instance_name)
    split(mild_data[, 4:5], mild_data$instance_name) %>%
      sapply(colMeans) %>%
      round(2) %>%
      t()
  })

  expect_s3_class(mild_data, "mild_df")
})
