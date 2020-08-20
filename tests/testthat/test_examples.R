context("Check that examples in mildsvm package work.")
suppressWarnings({
  library(mildsvm)
  # library(MilDistribution)
})


test_that("predict.mild function works without error", {
  MilData1 <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 10,
                              positive_degree = 3,
                              nsample = 20
  )
  foo <- mil_distribution(data = MilData1, cost = 1) ## uses about 10 seconds.
  predictions_mild <- predict(object = foo, newdata = MilData1)
  # predictions_mild <- predict.mild(object = foo, newdata = MilData1)

  expect_equal(attributes(predictions_mild), list(names = c("final_pred", "AUC", "ROC")))

})




test_that("build_poly_instance_feature works", {
  MilData1 <- GenerateMilData(positive_dist = 'mvt',
                              negative_dist = 'mvnormal',
                              remainder_dist = 'mvnormal',
                              nbag = 50,
                              nsample = 20,
                              positive_degree = 3,
                              positive_prob = 0.15,
                              positive_mean = rep(0, 5))
  df1 <- build_poly_instance_feature(MilData1, degree = 2)

  expect_equal(dim(df1), c(200, 68))
  expect_equal(class(df1), "data.frame")

})
