test_that("GenerateData.R functions have identical output", {
  skip_if_not_installed("MilDistribution") %>%
    suppressMessages()
  # Note: as of updates to 0.3.2 of mildsvm, no longer expect fidelity of
  # `generate_mild_df()`.  Just check for similarity here

  set.seed(8)
  mildsvm_data <- mildsvm::generate_mild_df(nimp_pos = 1:5,
                                            nimp_neg = 1:5,
                                            mean = list(rep(1, 5), rep(2, 5), 0),
                                            sd_of_mean = rep(0, 3))
  set.seed(8)
  MilDistribution_data <-
    MilDistribution::GenerateMilData(positive_dist = "mvt",
                                     negative_dist = "mvnormal",
                                     remainder_dist = "mvnormal",
                                     positive_mean = rep(1, 5),
                                     negative_mean = rep(2, 5),
                                     positive_degree = 3)

  class(MilDistribution_data) <- c("mild_df", "data.frame")
  cols <- 4:13
  suppressWarnings({
    diff <- colMeans(mildsvm_data[, cols]) - colMeans(MilDistribution_data[, cols])
  })
  expect_lte(mean(diff), 0.05)
  expect_equal(dim(mildsvm_data), dim(MilDistribution_data))

})

test_that("kme.R functions have identical output", {
  skip_if_not_installed("MilDistribution")
  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(ncov = 5, nbag = 7, nsample = 7)

  # remove one instance, and one observation to ensure unequal lengths
  ind1 <- which(mil_data$instance_name == unique(mil_data$instance_name)[1])
  ind2 <- which(mil_data$instance_name == unique(mil_data$instance_name)[2])[1]
  mil_data <- mil_data[-c(ind1, ind2), ]
  mil_data2 <- mil_data
  class(mil_data2) <- c("MilData", "data.frame")

  x = data.frame("instance_name" = c("inst_1", "inst_2", "inst_1"),
                 "X1" = c(-0.4, 0.5, 2))
  x2 = data.frame("instance_name" = c("inst_1", "inst_2", "inst_1"),
                  "X1" = c(-1, -2, 3))

  split_ind <- mil_data$bag_name %in% unique(mil_data$bag_name)[1:2]

  expect_equal(mildsvm::kme(mil_data), MilDistribution::kme(mil_data2))
  expect_equal(mildsvm::kme(x), MilDistribution::kme(x))
  expect_equal(mildsvm::kme(x, x2), MilDistribution::kme(x, x2))
  expect_equal(mildsvm::kme(mil_data[split_ind, ], mil_data[!split_ind, ]),
               MilDistribution::kme(mil_data2[split_ind, ], mil_data2[!split_ind, ]))
})

test_that("mismm.R functions have identical output", {
  skip_if_not_installed("gurobi")
  skip_if_not_installed("MilDistribution")
  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(ncov = 5, nbag = 10, nsample = 7,
                                        positive_prob = 0.15,
                                        sd_of_mean = rep(0.1, 3))

  # remove one instance, and one observation to create unequal lengths
  ind1 <- which(mil_data$instance_name == unique(mil_data$instance_name)[1])
  ind2 <- which(mil_data$instance_name == unique(mil_data$instance_name)[2])[1]
  mil_data <- mil_data[-c(ind1, ind2), ]

  mil_data <- mil_data %>%
    dplyr::arrange(bag_label, bag_name, instance_name, X1)
  mil_data2 <- mil_data
  class(mil_data2) <- c("MilData", "data.frame")


  set.seed(8)
  mdl1 <- mildsvm::mismm(mil_data, cost = 1,
                           weights = c("0" = 5 / 19, "1" = 1), # set to n_neg_inst / n_pos_bag
                           control = list(scale = FALSE,
                                          sigma = 0.05))
  mdl2 <- MilDistribution::mil_distribution(mil_data2, cost = 1)

  # models are equal on key components, just differ some naming
  expect_equal(mdl1$ksvm_fit@alpha, mdl2$model$ksvm_res@alpha)
  expect_equal(mdl1$ksvm_fit@b, mdl2$model$ksvm_res@b)
  expect_equal(mdl1$n_step, mdl2$total_step)
  expect_equal(mdl1$repr_inst, mdl2$representative_inst)
  expect_equal(mdl1$x, mdl2$traindata, ignore_attr = TRUE)
  expect_equal(mdl1$useful_inst_idx, mdl2$useful_inst_idx, ignore_attr = TRUE)

  # predictions should match
  expect_equal(
    predict(mdl1, new_data = mil_data, type = "raw", layer = "instance")$.pred %>%
      setNames(NULL) %>%
      suppressMessages(),
    # factor(predict(mdl2, newdata = mil_data_)$final_pred$bag_label)
    predict(mdl2, newdata = mil_data2)$final_pred$instance_score %>%
      suppressMessages(),
    ignore_attr = TRUE
  )

  expect_equal(
    mil_data %>%
      dplyr::bind_cols(predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")) %>%
      dplyr::distinct(bag_name, .pred),
    predict(mdl2, newdata = mil_data2)$final_pred %>%
      dplyr::distinct(bag_name, bag_score) %>%
      suppressMessages(),
    ignore_attr = TRUE
  )

  # it seems that there may have been a bug in Yifei's predict.mild code.  An
  # instance with a bag score of 0.14 gets labels as negative, when it should be
  # positive
  # expect_equal(
  #   mil_data_ %>%
  #     dplyr::bind_cols(predict(mdl1, new_data = mil_data_, type = "raw", layer = "bag")) %>%
  #     dplyr::bind_cols(predict(mdl1, new_data = mil_data_, type = "class", layer = "bag")) %>%
  #     dplyr::distinct(bag_name, .pred, .pred_class),
  #   predict(mdl2, newdata = mil_data_)$final_pred %>%
  #     dplyr::distinct(bag_name, bag_score, bag_label) %>%
  #     dplyr::mutate(bag_label = as.factor(bag_label)),
  #     ignore_attr = TRUE
  # )

  # there is another bug in the mil_distribution code where it will not work
  # when the data is passed out of order.

  # set.seed(8)
  # mildsvm_cv_output <- mildsvm::cv_mild(mil_data, n_fold = 3)
  # set.seed(8)
  # MilDistribution_cv_output <- MilDistribution::cv_mild(mil_data, n_fold = 3)
  # expect_equal(mildsvm_cv_output,
  #              MilDistribution_cv_output)

  # expect_equal(mildsvm::kme(x), MilDistribution::kme(x))
})

test_that("misvm.R functions have identical output.", {
  skip_if_not_installed("gurobi")
  skip_if_not_installed("MilDistribution")
  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(nbag = 10, nsample = 7,
                                        positive_prob = 0.15,
                                        sd_of_mean = rep(0.1, 3))
  df1 <- mildsvm::build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
    dplyr::arrange(desc(bag_label), bag_name, instance_name)

  set.seed(8)
  mildsvm_output <- mildsvm::misvm(x = df1[, 4:123],
                                   y = df1$bag_label,
                                   bags = df1$bag_name,
                                   cost = 1,
                                   method = "heuristic",
                                   control = list(kernel = "radial",
                                                  sigma = 1 / length(4:123)))
  set.seed(8)
  MilDistribution_output <- MilDistribution::MI_SVM(df1, cost = 1)

  expect_equal(mildsvm_output$n_step, MilDistribution_output$total_step)

  # objects are quite different because of different ordering, but as long as predictions match that is okay
  mildsvm_inst_pred <- df1 %>%
    dplyr::select(bag_label, bag_name) %>%
    dplyr::bind_cols(predict(mildsvm_output, new_data = df1, layer = "instance")) %>%
    dplyr::bind_cols(predict(mildsvm_output, new_data = df1, type = "raw", layer = "instance"))

  mildsvm_bag_pred <- df1 %>%
    dplyr::select(bag_label, bag_name) %>%
    dplyr::bind_cols(predict(mildsvm_output, new_data = df1, type = "raw", layer = "bag")) %>%
    dplyr::bind_cols(predict(mildsvm_output, new_data = df1, layer = "bag")) %>%
    dplyr::distinct()

  MilDistribution_pred <- predict(MilDistribution_output, newdata = df1)

  expect_equal(
    mildsvm_inst_pred,
    MilDistribution_pred$instance_level_prediction,
    ignore_attr = TRUE
  )
  expect_equal(
    mildsvm_bag_pred %>% dplyr::arrange(.pred) %>% dplyr::pull(.pred),
    MilDistribution_pred$bag_level_prediction %>% dplyr::arrange(bag_score_pred) %>% dplyr::pull(bag_score_pred),
    ignore_attr = TRUE
  )


})

test_that("cv_misvm.R functions have identical output.", {
  skip_if_not_installed("gurobi")
  skip_if_not_installed("MilDistribution")
  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(nbag = 10, nsample = 7,
                                        positive_prob = 0.15,
                                        sd_of_mean = rep(0.1, 3))
  df1 <- mildsvm::build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
    dplyr::arrange(desc(bag_label), bag_name, instance_name)


  set.seed(8)
  mildsvm_cv_output <- mildsvm::cv_misvm(x = df1[, 4:123],
                                         y = df1$bag_label,
                                         bags = df1$bag_name,
                                         cost_seq = 2^(-2:2),
                                         n_fold = 3,
                                         # weights = c("0" = 5 / 19, "1" = 1), # set to n_neg_inst / n_pos_bag
                                         method = "heuristic",
                                         control = list(kernel = "radial",
                                                        sigma = 1 / length(4:123)))
  set.seed(8)
  MilDistribution_cv_output <-
    MilDistribution::cv_MI_SVM(df1, n_fold = 3, cost_seq = 2^(-2:2)) %>%
    suppressMessages()

  mildsvm_cv_output$model$model$call <- NULL
  MilDistribution_cv_output$BestMdl$svm_mdl$call <- NULL
  expect_equal(mildsvm_cv_output$misvm_fit$n_step, MilDistribution_cv_output$BestMdl$total_step)

  mildsvm_inst_pred <- df1 %>%
    dplyr::select(bag_label, bag_name) %>%
    dplyr::bind_cols(predict(mildsvm_cv_output, new_data = df1, layer = "instance")) %>%
    dplyr::bind_cols(predict(mildsvm_cv_output, new_data = df1, type = "raw", layer = "instance"))

  mildsvm_bag_pred <- df1 %>%
    dplyr::select(bag_label, bag_name) %>%
    dplyr::bind_cols(predict(mildsvm_cv_output, new_data = df1, type = "raw", layer = "bag")) %>%
    dplyr::bind_cols(predict(mildsvm_cv_output, new_data = df1, layer = "bag")) %>%
    dplyr::distinct()

  MilDistribution_pred <- predict(MilDistribution_cv_output$BestMdl, newdata = df1) %>%
    suppressMessages()

  expect_equal(
    mildsvm_inst_pred,
    MilDistribution_pred$instance_level_prediction,
    ignore_attr = TRUE
  )
  expect_equal(
    mildsvm_bag_pred %>% dplyr::arrange(.pred) %>% dplyr::pull(.pred),
    MilDistribution_pred$bag_level_prediction %>% dplyr::arrange(bag_score_pred) %>% dplyr::pull(bag_score_pred),
    ignore_attr = TRUE
  )

})

test_that("smm.R functions have identical output", {
  skip_if_not_installed("gurobi")
  skip_if_not_installed("MilDistribution")
  set.seed(8)
  n_instances <- 10
  n_samples <- 20
  y <- rep(c(1, -1), each = n_samples * n_instances / 2)
  instances <- as.character(rep(1:n_instances, each = n_samples))
  x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
                  x2 = rnorm(length(y), mean = 2*(y==1)),
                  x3 = rnorm(length(y), mean = 3*(y==1)))

  # NOTE: yifei's code has a bug in `SMM()` where instance_names are re-ordered
  # in y, but not in the x.  This comes from a dplyr::summarize ordering by the
  # instance_name, which might not align with the way the data was ordered.
  # Thus, we order by the instance_name here.

  # NOTE: also, we need to pass a factor as `y` for `ksvm`, which wasn't made
  # explicit in yifei's code
  df <- data.frame(instance_label = factor(y), instance_name = instances, x) %>%
    dplyr::arrange(instance_name)

  mdl1 <- mildsvm::smm(x = df[, c("x1", "x2", "x3")],
                       y = df$instance_label,
                       instances = df$instance_name,
                       cost = 1,
                       weights = FALSE,
                       control = list(sigma = 0.05, scale = FALSE))
  mdl2 <- MilDistribution::SMM(df %>% dplyr::arrange(instance_name))

  expect_equal(mdl1$ksvm_fit, mdl2$ksvm_res)
  common_components <- c("sigma", "cost")
  expect_equal(mdl1[common_components], mdl2[common_components])
  expect_equal(mdl1$x,
               mdl2$traindata %>% dplyr::select(-instance_label),
               ignore_attr = TRUE)

})

test_that("misvm.R functions have identical output on MilData object.", {
  skip_if_not_installed("gurobi")
  skip_if_not_installed("MilDistribution")
  set.seed(8)
  mil_data <- mildsvm::generate_mild_df(nbag = 10, nsample = 7,
                                        positive_prob = 0.15,
                                        sd_of_mean = rep(0.1, 3)) %>%
    dplyr::arrange(desc(bag_label), bag_name, instance_name)

  # make the quantile functions
  qtls <- seq(0.05, 0.95, length.out = 10)
  q_funs <- purrr::map(qtls,
                       function(qtl) { return(~quantile(.x, qtl)) })
  names(q_funs) <- paste0("q", qtls)


  set.seed(8)
  mdl1 <- mildsvm::misvm(mil_data, cost = 1,
                         .fns = c(list(mean = base::mean, sd = stats::sd), q_funs),
                         method = "heuristic",
                         control = list(kernel = "radial",
                                        sigma = 1 / 120))

  suppressWarnings({
    mdl2 <- MilDistribution::mil_with_feature(mil_data, cost = 1)
  })


  expect_equal(mdl1$svm_fit$coefs, mdl2$svm_mdl$coefs)
  expect_equal(mdl1$n_step, mdl2$total_step)

  # objects are quite different because of different ordering, but as long as predictions match that is okay
  mildsvm_bag_pred <- mil_data %>%
    dplyr::select(bag_label, bag_name) %>%
    dplyr::bind_cols(predict(mdl1, new_data = mil_data, type = "raw", layer = "bag")) %>%
    dplyr::bind_cols(predict(mdl1, new_data = mil_data, layer = "bag")) %>%
    dplyr::distinct()

  # Note prediction doesn't work in MilDistribution, but this is what it should do
  suppressWarnings({
    MilDistribution_pred <- predict(mdl2, newdata = MilDistribution::build_instance_feature(mil_data))
  })

  expect_equal(
    mildsvm_bag_pred %>% dplyr::arrange(.pred) %>% dplyr::pull(.pred),
    MilDistribution_pred$bag_level_prediction %>% dplyr::arrange(bag_score_pred) %>% dplyr::pull(bag_score_pred),
    ignore_attr = TRUE
  )

})

