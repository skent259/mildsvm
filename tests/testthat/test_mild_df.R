context("Testing the functions in mild_df.R")

test_that("`as_mild_df()` works for several data.frames.", {

  x = data.frame('bag_label' = factor(c(1, 1, 0)),
                 'bag_name' = c(rep('bag_1', 2), 'bag_2'),
                 'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
                 'X1' = c(-0.4, 0.5, 2),
                 'instance_label' = c(0, 1, 0))

  df <- as_mild_df(x)
  expect_s3_class(df, "mild_df")
  expect(is.vector(attr(df, "instance_label")), "`df` does not have attribute instance_label")
  expect_equivalent(df, x[, -5])


  x = data.frame('bag_LABEL' = factor(c(1, 1, 0)),
                'bag_name' = c(rep('bag_1', 2), 'bag_2'),
                'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
                'X1' = c(-0.4, 0.5, 2),
                'instance_label' = c(0, 1, 0))

  expect_warning(df <- as_mild_df(x))
  expect_equivalent(df, x[, -5])


  x = data.frame('a' = factor(c(1, 1, 0)),
                 'b' = c(rep('bag_1', 2), 'bag_2'),
                 'c' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
                 'd' = c(-0.4, 0.5, 2))

  expect_warning(expect_message(df <- as_mild_df(x)))
  expect_equivalent(df, x)


  x = data.frame('bag_name' = c(rep('bag_1', 2), 'bag_2'),
                 'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
                 'X1' = c(-0.4, 0.5, 2),
                 'instance_label' = c(0, 1, 0),
                 'bag_label' = factor(c(1, 1, 0)))

  df <- as_mild_df(x)
  expect_equal(colnames(df), c("bag_label", "bag_name", "instance_name", "X1"))

})


test_that("`mild_df()` works for typical input", {
  df <- mild_df(
    'bag_label' = factor(c(1, 1, 0)),
    'bag_name' = c(rep('bag_1', 2), 'bag_2'),
    'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
    'X1' = c(-0.4, 0.5, 2),
    'instance_label' = c(0, 1, 0)
  )

  expect_s3_class(df, "mild_df")
  expect(is.vector(attr(df, "instance_label")), "`df` does not have attribute instance_label")


  expect_warning(df <- mild_df())
  expect_s3_class(df, "mild_df")
  expect_null(attr(df, "instance_label"))
})
