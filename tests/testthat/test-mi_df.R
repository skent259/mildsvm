x_main <- data.frame("bag_label" = factor(c(1, 1, 0)),
                     "bag_name" = c(rep("bag_1", 2), "bag_2"),
                     "X1" = c(-0.4, 0.5, 2),
                     "instance_label" = c(0, 1, 0))

test_that("`as_mi_df()` works for several data.frames.", {

  x <- x_main
  df <- as_mi_df(x)
  expect_s3_class(df, "mi_df")
  expect(is.vector(attr(df, "instance_label")), "`df` does not have attribute instance_label")
  expect_equal(df, x[, -4], ignore_attr = TRUE)

  x <- x_main
  colnames(x)[1] <- "bag_LABEL"
  expect_warning(df <- as_mi_df(x))
  expect_equal(df, x[, -4], ignore_attr = TRUE)

  x <- x_main
  colnames(x)[1:3] <- c("a", "b", "d")
  x$instance_label <- NULL
  expect_message(df <- as_mi_df(x)) %>%
    expect_warning() %>%
    expect_warning()
  expect_equal(df, x, ignore_attr = TRUE)

  x <- x_main[, c(2, 3, 4, 1)]
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

test_that("`as_mi_df()` retains tibble typing", {
  x <- tibble::as_tibble(x_main)

  expect_s3_class(as_mi_df(x), "mi_df")
  expect_s3_class(as_mi_df(x), "tbl")
  expect_s3_class(as_mi_df(x), "tbl_df")

})

test_that("`as_mi_df()` converts data.frame to tibble", {

  expect_s3_class(as_mi_df(x_main), "mi_df")
  expect_s3_class(as_mi_df(x_main), "tbl")
  expect_s3_class(as_mi_df(x_main), "tbl_df")

})

test_that("Printing methods work as expected", {
  x <- x_main

  df <- as_mi_df(x)
  expect_snapshot(print(df))

  df <- as_mi_df(rbind(x, x))
  expect_snapshot(print(df))


  x$instance_label <- NULL
  df <- as_mi_df(x) %>%
    suppressMessages()
  expect_snapshot(print(df))

  x <- tibble::as_tibble(x)
  df <- as_mi_df(x) %>%
    suppressMessages()
  expect_snapshot(print(df))
  expect_snapshot(print(df, n = 2))

  expect_s3_class(df, "mi_df")
})

test_that("Subsetting `mi_df` gives correct warnings and classes", {
  df <- as_mi_df(x_main)

  expect_s3_class(df[, c(1:2)], "mi_df")
  expect_s3_class(df[, c(1:3)], "mi_df")
  expect_false(inherits(df[, 1], "mi_df"))
  expect_s3_class(df[, 1], "tbl_df")

  expect_warning(df2 <- df[, c(2:3)], "Dropping 'mi_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mi_df"))

  expect_warning({df2 <- df[, c(1, 3)]}, "Dropping 'mi_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mi_df"))


  df <- as_mi_df(tibble::as_tibble(x_main))

  expect_s3_class(df[, c(1:2)], "mi_df")
  expect_s3_class(df[, c(1:3)], "mi_df")
  expect_false(inherits(df[, 1], "mi_df"))
  expect_s3_class(df[, 1], "data.frame") # different for tibbles

  expect_warning(df2 <- df[, c(2:3)], "Dropping 'mi_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mi_df"))

  expect_warning(df2 <- df[, c(1, 3)], "Dropping 'mi_df'")
  expect_s3_class(df2, "data.frame")
  expect_false(inherits(df2, "mi_df"))
})

test_that("Subsetting `mild_df` rows works as expected", {
  df <- as_mi_df(x_main)

  check_row_subset <- function(df, ind) {
    df2 <- df[ind, ]
    n <- length(ind)
    expect_equal(nrow(df2), n)
    expect_equal(ncol(df2), ncol(df))
    expect_equal(length(df_instance_label(df2)), n)
    expect_equal(length(rownames(df2)), n)
  }

  check_row_subset(df, 1:2) # fewer rows
  check_row_subset(df, 1:3) # same rows
  check_row_subset(df, 1:4) %>%
    suppressWarnings() # extra rows
  check_row_subset(df, c(1, 1, 2)) # different order

  # list subsetting
  expect_equal(nrow(df[1]), nrow(df))
  expect_equal(nrow(df[1:2]), nrow(df))
  expect_equal(nrow(df[1:3]), nrow(df))

  # dplyr::filter
  df2 <- dplyr::filter(df, bag_label == 1)
  # expect_equal(length(df_instance_label(df2)), nrow(df2)) # fails
})


