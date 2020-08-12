context("Fidelity to MilDistribution package")
suppressWarnings({
  library(mildsvm)
  library(MilDistribution)
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


test_that("mil_distribution.R functions have identical output", {
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
  mil_data <- mil_data[-c(ind1, ind2), ]

  set.seed(8)
  expect_equal(mildsvm::mil_distribution(mil_data, cost = 1),
               MilDistribution::mil_distribution(mil_data, cost = 1))
  expect_equal(mildsvm::cv_mild(mil_data, n_fold = 3),
               MilDistribution::cv_mild(mil_data, n_fold = 3))

  # expect_equal(mildsvm::kme(x), MilDistribution::kme(x))
})




# TODO: Found some bug when ncov can't be less than 5
