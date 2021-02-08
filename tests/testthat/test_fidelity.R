context("Fidelity to MilDistribution package")
suppressWarnings({
  # library(mildsvm)
  library(MilDistribution)
  library(dplyr)
})


test_that("GenerateData.R functions have identical output", {
  mil_data <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                       negative_dist = "mvnormal",
                                       remainder_dist = "mvnormal",
                                       ncov = 5,
                                       nbag = 7,
                                       nsample = 7,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))

  set.seed(8)
  mildsvm_data <-
    mildsvm::GenerateMilData(positive_dist = "mvt",
                             negative_dist = "mvnormal",
                             remainder_dist = "mvnormal",
                             positive_degree = 3)
  set.seed(8)
  MilDistribution_data <-
    MilDistribution::GenerateMilData(positive_dist = "mvt",
                                     negative_dist = "mvnormal",
                                     remainder_dist = "mvnormal",
                                     positive_degree = 3)

  expect_equal(mildsvm_data, MilDistribution_data)
})

test_that("kme.R functions have identical output", {
  mil_data <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                       negative_dist = "mvnormal",
                                       remainder_dist = "mvnormal",
                                       ncov = 5,
                                       nbag = 7,
                                       nsample = 7,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))

  # remove one instance, and one observation to ensure unequal lengths
  ind1 <- which(mil_data$instance_name == unique(mil_data$instance_name)[1])
  ind2 <- which(mil_data$instance_name == unique(mil_data$instance_name)[2])[1]
  mil_data <- mil_data[-c(ind1, ind2), ]

  x = data.frame("instance_name" = c("inst_1", "inst_2", "inst_1"),
                 "X1" = c(-0.4, 0.5, 2))
  x2 = data.frame("instance_name" = c("inst_1", "inst_2", "inst_1"),
                  "X1" = c(-1, -2, 3))

  split_ind <- mil_data$bag_name %in% unique(mil_data$bag_name)[1:2]
  sub1 <- mil_data[split_ind, ]
  sub2 <- mil_data[!split_ind, ]

  expect_equal(mildsvm::kme(mil_data), MilDistribution::kme(mil_data))
  expect_equal(mildsvm::kme(x), MilDistribution::kme(x))
  expect_equal(mildsvm::kme(x, x2), MilDistribution::kme(x, x2))
  expect_equal(mildsvm::kme(sub1, sub2), MilDistribution::kme(sub1, sub2))
})

test_that("mildsvm.R functions have identical output", {
  set.seed(8)
  mil_data <- mildsvm::GenerateMilData(positive_dist = "mvt",
                                       negative_dist = "mvnormal",
                                       remainder_dist = "mvnormal",
                                       ncov = 5,
                                       nbag = 10,
                                       nsample = 7,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))

  # remove one instance, and one observation to create unequal lengths
  ind1 <- which(mil_data$instance_name == unique(mil_data$instance_name)[1])
  ind2 <- which(mil_data$instance_name == unique(mil_data$instance_name)[2])[1]
  mil_data_ <- mil_data[-c(ind1, ind2), ]

  mil_data_ <- mil_data_ %>%
    arrange(bag_label, bag_name, instance_name, X1)

  set.seed(8)
  mdl1 <- mildsvm::mildsvm(mil_data_, cost = 1,
                           weights = c("0" = 0.375, "1" = 1),
                           control = list(scale = FALSE,
                                          sigma = 0.05))
  mdl2 <- MilDistribution::mil_distribution(mil_data_, cost = 1)

  # models are equal on key components, just differ some naming
  expect_equal(mdl1$model$model@alpha, mdl2$model$ksvm_res@alpha)
  expect_equal(mdl1$model$model@b, mdl2$model$ksvm_res@b)
  expect_equal(mdl1$total_step, mdl2$total_step)
  expect_equal(mdl1$representative_inst, mdl2$representative_inst)
  expect_equal(mdl1$traindata, mdl2$traindata)
  expect_equivalent(mdl1$useful_inst_idx, mdl2$useful_inst_idx)

  # predictions should match
  expect_equivalent(
    predict(mdl1, new_data = mil_data_, type = "raw", layer = "instance")$.pred %>% setNames(NULL),
    # factor(predict(mdl2, newdata = mil_data_)$final_pred$bag_label)
    predict(mdl2, newdata = mil_data_)$final_pred$instance_score
  )

  expect_equivalent(
    mil_data_ %>%
      bind_cols(predict(mdl1, new_data = mil_data_, type = "raw", layer = "bag")) %>%
      distinct(bag_name, .pred),
    predict(mdl2, newdata = mil_data_)$final_pred %>%
      distinct(bag_name, bag_score)
  )

  # it seems that there may have been a bug in Yifei's predict.mild code.  An
  # instance with a bag score of 0.14 gets labels as negative, when it should be
  # positive
  # expect_equivalent(
  #   mil_data_ %>%
  #     bind_cols(predict(mdl1, new_data = mil_data_, type = "raw", layer = "bag")) %>%
  #     bind_cols(predict(mdl1, new_data = mil_data_, type = "class", layer = "bag")) %>%
  #     distinct(bag_name, .pred, .pred_class),
  #   predict(mdl2, newdata = mil_data_)$final_pred %>%
  #     distinct(bag_name, bag_score, bag_label) %>%
  #     dplyr::mutate(bag_label = as.factor(bag_label))
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
  set.seed(8)
  mil_data <- mildsvm::GenerateMilData(positive_dist = 'mvt',
                                       negative_dist = 'mvnormal',
                                       remainder_dist = 'mvnormal',
                                       nbag = 10,
                                       nsample = 7,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))
  df1 <- mildsvm::build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10)) %>%
    arrange(desc(bag_label), bag_name, instance_name)

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

  expect_equal(mildsvm_output$total_step, MilDistribution_output$total_step)

  # objects are quite different because of different ordering, but as long as predictions match that is okay
  mildsvm_inst_pred <- df1 %>%
    select(bag_label, bag_name) %>%
    bind_cols(predict(mildsvm_output, new_data = df1, layer = "instance")) %>%
    bind_cols(predict(mildsvm_output, new_data = df1, type = "raw", layer = "instance"))

  mildsvm_bag_pred <- df1 %>%
    select(bag_label, bag_name) %>%
    bind_cols(predict(mildsvm_output, new_data = df1, type = "raw", layer = "bag")) %>%
    bind_cols(predict(mildsvm_output, new_data = df1, layer = "bag")) %>%
    distinct()

  MilDistribution_pred <- predict(MilDistribution_output, newdata = df1)

  expect_equivalent(
    mildsvm_inst_pred,
    MilDistribution_pred$instance_level_prediction
  )
  expect_equivalent(
    mildsvm_bag_pred %>% arrange(.pred) %>% pull(.pred),
    MilDistribution_pred$bag_level_prediction %>% arrange(bag_score_pred) %>% pull(bag_score_pred)
  )


})

test_that("cv_misvm.R functions have identical output.", {
  set.seed(8)
  mil_data <- mildsvm::GenerateMilData(positive_dist = 'mvt',
                                       negative_dist = 'mvnormal',
                                       remainder_dist = 'mvnormal',
                                       nbag = 10,
                                       nsample = 7,
                                       positive_degree = 3,
                                       positive_prob = 0.15,
                                       positive_mean = rep(0, 5))
  df1 <- mildsvm::build_instance_feature(mil_data, seq(0.05, 0.95, length.out = 10))


  set.seed(8)
  mildsvm_cv_output <- mildsvm::cv_misvm(x = df1[, 4:123],
                                         y = df1$bag_label,
                                         bags = df1$bag_name,
                                         cost_seq = 2^(-2:2),
                                         n_fold = 3,
                                         method = "heuristic",
                                         control = list(kernel = "radial",
                                                        sigma = 1 / length(4:123)))
  set.seed(8)
  MilDistribution_cv_output <- MilDistribution::cv_MI_SVM(df1, n_fold = 3, cost_seq = 2^(-2:2))

  mildsvm_cv_output$model$model$call <- NULL
  MilDistribution_cv_output$BestMdl$svm_mdl$call <- NULL
  expect_equal(mildsvm_cv_output$model$total_step, MilDistribution_cv_output$BestMdl$total_step)

  mildsvm_inst_pred <- df1 %>%
    select(bag_label, bag_name) %>%
    bind_cols(predict(mildsvm_cv_output, new_data = df1, layer = "instance")) %>%
    bind_cols(predict(mildsvm_cv_output, new_data = df1, type = "raw", layer = "instance"))

  mildsvm_bag_pred <- df1 %>%
    select(bag_label, bag_name) %>%
    bind_cols(predict(mildsvm_cv_output, new_data = df1, type = "raw", layer = "bag")) %>%
    bind_cols(predict(mildsvm_cv_output, new_data = df1, layer = "bag")) %>%
    distinct()

  MilDistribution_pred <- predict(MilDistribution_cv_output$BestMdl, newdata = df1)

  expect_equivalent(
    mildsvm_inst_pred,
    MilDistribution_pred$instance_level_prediction
  )
  expect_equivalent(
    mildsvm_bag_pred %>% arrange(.pred) %>% pull(.pred),
    MilDistribution_pred$bag_level_prediction %>% arrange(bag_score_pred) %>% pull(bag_score_pred)
  )

})

test_that("smm.R functions have identical output", {
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
    arrange(instance_name)

  mdl1 <- mildsvm::smm(x = df[, c("x1", "x2", "x3")],
                       y = df$instance_label,
                       instances = df$instance_name,
                       cost = 1,
                       weights = FALSE,
                       control = list(sigma = 0.05, scale = FALSE))
  mdl2 <- MilDistribution::SMM(df %>% arrange(instance_name))

  expect_equal(mdl1$model, mdl2$ksvm_res)
  common_components <- c("sigma", "cost")
  expect_equal(mdl1[common_components], mdl2[common_components])
  expect_equivalent(mdl1$traindata,
                    mdl2$traindata %>% select(-instance_label))

})

test_that("misvm.R functions have identical output.", {
  set.seed(8)
  mil_data <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 50,
                              nsample = 20,
                              positive_degree = 3,
                              positive_prob = 0.15,
                              positive_mean = rep(0, 5))


  expect_equal(mildsvm::mil_with_feature(data = mil_data),
               MilDistribution::mil_with_feature(data = mil_data))
})

