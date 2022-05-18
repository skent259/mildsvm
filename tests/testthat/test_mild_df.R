suppressMessages(suppressWarnings(library(tibble)))

x_main <- data.frame("bag_label" = factor(c(1, 1, 0)),
                     "bag_name" = c(rep("bag_1", 2), "bag_2"),
                     'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
                     "instance_label" = c(0, 1, 0))
x_main <- rbind(x_main, x_main) %>% dplyr::arrange(bag_name, instance_name)
x_main$X1 <- c(-0.4, -0.35, 0.4, 0.5, 2, 2.1)

test_that("`as_mild_df()` works for several data.frames.", {
  x <- x_main
  df <- as_mild_df(x)
  expect_s3_class(df, "mild_df")
  expect(is.vector(attr(df, "instance_label")), "`df` does not have attribute instance_label")
  expect_equal(df, x[, -4], ignore_attr = TRUE)

  colnames(x)[1] <- "bag_LABEL"
  expect_warning(df <- as_mild_df(x))
  expect_equal(df, x[, -4], ignore_attr = TRUE)

  x$instance_label <- NULL
  colnames(x)[1:4] <- c("a", "b", "c", "d")
  expect_message(df <- as_mild_df(x)) %>%
    expect_warning() %>%
    expect_warning() %>%
    expect_warning()
  expect_equal(df, x, ignore_attr = TRUE)

  x <- x_main[c(2, 3, 5, 4, 1)]
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

test_that("`as_mild_df()` retains tibble typing", {
  x <- as_tibble(x_main)

  expect_s3_class(as_mild_df(x), "mild_df")
  expect_s3_class(as_mild_df(x), "tbl")
  expect_s3_class(as_mild_df(x), "tbl_df")

  expect_false(inherits(as_mild_df(x_main), "tbl"))
})


test_that("Printing methods work as expected", {
  x <- x_main

  df <- as_mild_df(x)
  expect_snapshot(print(df))

  x$instance_label <- NULL
  df <- as_mild_df(x) %>%
    suppressMessages()
  expect_snapshot(print(df))

  x <- as_tibble(x)
  df <- as_mild_df(x) %>%
    suppressMessages()
  expect_snapshot(print(df))
  expect_snapshot(print(df, n = 2))

  expect_s3_class(df, "mild_df")
})

test_that("Subsetting `mild_df` gives correct warnings and classes", {
  df <- as_mild_df(x_main)

  expect_s3_class(df[, c(1:3)], "mild_df")
  expect_s3_class(df[, c(1:4)], "mild_df")
  expect_false(inherits(df[, 1], "mild_df"))
  expect_s3_class(df[, 1], "factor")

  expect_warning(df2 <- df[, c(2:3)], "Dropping 'mild_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mild_df"))

  expect_warning(df2 <- df[, c(1,4)], "Dropping 'mild_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mild_df"))


  df <- as_mild_df(tibble::as_tibble(x_main))

  expect_s3_class(df[, c(1:3)], "mild_df")
  expect_s3_class(df[, c(1:4)], "mild_df")
  expect_false(inherits(df[, 1], "mild_df"))
  expect_s3_class(df[, 1], "data.frame") # different for tibbles

  expect_warning(df2 <- df[, c(2:3)], "Dropping 'mild_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mild_df"))

  expect_warning(df2 <- df[, c(1,4)], "Dropping 'mild_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mild_df"))
})

