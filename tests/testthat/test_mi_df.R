
test_that("`as_mi_df()` works for several data.frames.", {

  x <- data.frame("bag_label" = factor(c(1, 1, 0)),
                 "bag_name" = c(rep("bag_1", 2), "bag_2"),
                 "X1" = c(-0.4, 0.5, 2),
                 "instance_label" = c(0, 1, 0))

  df <- as_mi_df(x)
  expect_s3_class(df, "mi_df")
  expect(is.vector(attr(df, "instance_label")), "`df` does not have attribute instance_label")
  expect_equal(df, x[, -4], ignore_attr = TRUE)


  x <- data.frame("bag_LABEL" = factor(c(1, 1, 0)),
                "bag_name" = c(rep("bag_1", 2), "bag_2"),
                "X1" = c(-0.4, 0.5, 2),
                "instance_label" = c(0, 1, 0))

  expect_warning(df <- as_mi_df(x))
  expect_equal(df, x[, -4], ignore_attr = TRUE)


  x <- data.frame("a" = factor(c(1, 1, 0)),
                 "b" = c(rep("bag_1", 2), "bag_2"),
                 "d" = c(-0.4, 0.5, 2))

  expect_message(df <- as_mi_df(x)) %>%
    expect_warning() %>%
    expect_warning()
  expect_equal(df, x, ignore_attr = TRUE)


  x <- data.frame("bag_name" = c(rep("bag_1", 2), "bag_2"),
                 "X1" = c(-0.4, 0.5, 2),
                 "instance_label" = c(0, 1, 0),
                 "bag_label" = factor(c(1, 1, 0)))

  df <- as_mi_df(x)
  expect_equal(colnames(df), c("bag_label", "bag_name", "X1"))

})


test_that("`mi_df()` works for typical input", {
  df <- mi_df(
    "bag_label" = factor(c(1, 1, 0)),
    "bag_name" = c(rep("bag_1", 2), "bag_2"),
    "X1" = c(-0.4, 0.5, 2),
    "instance_label" = c(0, 1, 0)
  )

  expect_s3_class(df, "mi_df")
  expect(is.vector(attr(df, "instance_label")), "`df` does not have attribute instance_label")


  expect_warning(df <- mi_df())
  expect_s3_class(df, "mi_df")
  expect_null(attr(df, "instance_label"))
})
