test_that("`build_fm()`, `kfm_exact()`, `kfm_nystrom()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    df <- data.frame(
      X1 = c(2,   3,   4,   5,   6, 7, 8),
      X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
      X3 = rnorm(7)
    )

    fit1 <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.05)
    fm <- build_fm(fit1, df)

    fit2 <- kfm_exact(kernel = "polynomial", degree = 2, const = 1)
    fm <- build_fm(fit2, df)
  })

  expect_s3_class(fit1, "kfm_nystrom")
  expect_s3_class(fit2, "kfm_exact")
})

test_that("`cv_misvm()` examples work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("gurobi")

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
      dplyr::bind_cols(predict(mdl2, df, type = "class")) %>%
      dplyr::bind_cols(predict(mdl2, df, type = "raw")) %>%
      dplyr::distinct(bag_name, bag_label, .pred_class, .pred)
  })

  expect_s3_class(mdl1, "cv_misvm")
  expect_s3_class(mdl2, "cv_misvm")
  expect_s3_class(mdl3, "cv_misvm")
})

test_that("`generate_mild_df()` examples work", {
  skip_on_cran()
  skip_on_ci()

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

    dplyr::distinct(mild_data, bag_label, bag_name, instance_name)
    split(mild_data[, 4:5], mild_data$instance_name) %>%
      sapply(colMeans) %>%
      round(2) %>%
      t()
  })

  expect_s3_class(mild_data, "mild_df")
})

test_that("`kme()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    x = data.frame('instance_name' = c('inst_1', 'inst_2', 'inst_1'),
                   'X1' = c(-0.4, 0.5, 2))
    kme(x)

    mild_df1 <- generate_mild_df(nbag = 10, positive_degree = 3)
    kme(mild_df1)
  })

  expect_true(TRUE)
})

test_that("`mi_df()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    mi_df('bag_label' = factor(c(1, 1, 0)),
          'bag_name' = c(rep('bag_1', 2), 'bag_2'),
          'X1' = c(-0.4, 0.5, 2),
          'instance_label' = c(0, 1, 0))
  })

  expect_true(TRUE)
})

test_that("`mi()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    mil_data <- generate_mild_df(positive_degree = 3, nbag = 10)
    with(mil_data, head(mi(bag_label, bag_name)))
    df <- get_all_vars(mi(bag_label, bag_name) ~ X1 + X2, data = mil_data)
    head(df)
  })

  expect_true(TRUE)
})

test_that("`mild_df()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    mild_df('bag_label' = factor(c(1, 1, 0)),
            'bag_name' = c(rep('bag_1', 2), 'bag_2'),
            'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
            'X1' = c(-0.4, 0.5, 2),
            'instance_label' = c(0, 1, 0))
  })

  expect_true(TRUE)
})

test_that("`mild()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    mil_data <- generate_mild_df(positive_degree = 3, nbag = 10)
    with(mil_data, head(mild(bag_label, bag_name, instance_name)))
    df <- get_all_vars(mild(bag_label, bag_name) ~ X1 + X2, data = mil_data)
    head(df)

  })

  expect_true(TRUE)
})

test_that("`mior()` examples work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("gurobi")

  expect_snapshot({
    if (require(gurobi)) {
      set.seed(8)
      # make some data
      n <- 15
      X <- rbind(
        mvtnorm::rmvnorm(n/3, mean = c(4, -2, 0)),
        mvtnorm::rmvnorm(n/3, mean = c(0, 0, 0)),
        mvtnorm::rmvnorm(n/3, mean = c(-2, 1, 0))
      )
      score <- X %*% c(2, -1, 0)
      y <- as.numeric(cut(score, c(-Inf, quantile(score, probs = 1:2 / 3), Inf)))
      bags <- seq_along(y)

      # add in points outside boundaries
      X <- rbind(
        X,
        mvtnorm::rmvnorm(n, mean = c(6, -3, 0)),
        mvtnorm::rmvnorm(n, mean = c(-6, 3, 0))
      )
      y <- c(y, rep(-1, 2*n))
      bags <- rep(bags, 3)
      repr <- c(rep(1, n), rep(0, 2*n))

      y_bag <- classify_bags(y, bags, condense = FALSE)

      mdl1 <- mior(X, y_bag, bags)
      predict(mdl1, X, new_bags = bags)

      # summarize predictions at the bag layer
      df1 <- dplyr::bind_cols(y = y_bag, bags = bags, as.data.frame(X))
      df1 %>%
        dplyr::bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
        dplyr::bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
        dplyr::distinct(y, bags, .pred_class, .pred)
    }
  })

  expect_s3_class(mdl1, "mior")
})

test_that("`mismm()` example works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("gurobi")

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
      dplyr::bind_cols(predict(mdl2, mil_data, type = "class")) %>%
      dplyr::bind_cols(predict(mdl2, mil_data, type = "raw")) %>%
      dplyr::distinct(bag_name, bag_label, .pred_class, .pred)
  })

  expect_s3_class(mdl1, "mismm")
  expect_s3_class(mdl2, "mismm")
})

test_that("`predict.mismm()` examples work", {
  skip_on_cran()
  skip_on_ci()

  set.seed(8)
  expect_snapshot({
    mil_data <- generate_mild_df(nbag = 15, nsample = 20, positive_prob = 0.15,
                                 sd_of_mean = rep(0.1, 3))

    mdl1 <- mismm(mil_data, control = list(sigma = 1/5))

    # bag level predictions
    mil_data %>%
      dplyr::bind_cols(predict(mdl1, mil_data, type = "class")) %>%
      dplyr::bind_cols(predict(mdl1, mil_data, type = "raw")) %>%
      dplyr::distinct(bag_name, bag_label, .pred_class, .pred)

    # instance level prediction
    mil_data %>%
      dplyr::bind_cols(predict(mdl1, mil_data, type = "class", layer = "instance")) %>%
      dplyr::bind_cols(predict(mdl1, mil_data, type = "raw", layer = "instance")) %>%
      dplyr::distinct(bag_name, instance_name, bag_label, .pred_class, .pred)
  })

  expect_s3_class(mdl1, "mismm")
})

test_that("`misvm_orova()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    data("ordmvnorm")
    x <- ordmvnorm[, 3:7]
    y <- ordmvnorm$bag_label
    bags <- ordmvnorm$bag_name

    mdl1 <- misvm_orova(x, y, bags)
    predict(mdl1, x, new_bags = bags)

    # summarize predictions at the bag layer
    df1 <- dplyr::bind_cols(y = y, bags = bags, as.data.frame(x))
    df1 %>%
      dplyr::bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
      dplyr::bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
      dplyr::select(-starts_with("V")) %>%
      dplyr::distinct()
  })

  expect_s3_class(mdl1, "misvm_orova")
})

test_that("`misvm()` examples work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("gurobi")

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
    df %>%
      dplyr::bind_cols(predict(mdl2, df, type = "class")) %>%
      dplyr::bind_cols(predict(mdl2, df, type = "raw")) %>%
      dplyr::distinct(bag_name, bag_label, .pred_class, .pred)

  })

  expect_s3_class(mdl1, "misvm")
  expect_s3_class(mdl2, "misvm")
})

test_that("`omisvm()` examples work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("gurobi")

  set.seed(8)
  expect_snapshot({
    if (require(gurobi)) {
      data("ordmvnorm")
      x <- ordmvnorm[, 3:7]
      y <- ordmvnorm$bag_label
      bags <- ordmvnorm$bag_name

      mdl1 <- omisvm(x, y, bags, weights = NULL)
      predict(mdl1, x, new_bags = bags)

      df1 <- dplyr::bind_cols(y = y, bags = bags, as.data.frame(x))
      df1 %>%
        dplyr::bind_cols(predict(mdl1, df1, new_bags = bags, type = "class")) %>%
        dplyr::bind_cols(predict(mdl1, df1, new_bags = bags, type = "raw")) %>%
        dplyr::distinct(y, bags, .pred_class, .pred)
    }
  })

  expect_s3_class(mdl1, "omisvm")
})

test_that("`smm()` examples work", {
  skip_on_cran()
  skip_on_ci()

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

test_that("`summarize_samples()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    fns <- list(mean = mean, sd = sd)
    suppressMessages({
      summarize_samples(mtcars, group_cols = c("cyl", "gear"), .fns = fns) %>%
        print()
      summarize_samples(mtcars, group_cols = c("cyl", "gear"), .fns = fns, cor = TRUE) %>%
        print()
    })
  })

  expect_true(TRUE)
})

test_that("`svor_exc()` examples work", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot({
    data("ordmvnorm")
    x <- ordmvnorm[, 3:7]
    y <- attr(ordmvnorm, "instance_label")

    mdl1 <- svor_exc(x, y)
    predict(mdl1, x)
    predict(mdl1, x, type = "raw")
  })

  expect_s3_class(mdl1, "svor_exc")
})

