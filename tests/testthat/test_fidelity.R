context("Fidelity to MilDistribution package")
suppressWarnings({
  library(mildsvm)
  library(MilDistribution)
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

  expect_equal(mildsvm::kme(mil_data), MilDistribution::kme(mil_data))
  expect_equal(mildsvm::kme(x), MilDistribution::kme(x))
})




# TODO: Found some bug when ncov can't be less than 5
