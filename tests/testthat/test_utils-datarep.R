test_that("data repliation with s works as expected.", {

  check_length <- function(y, s, k) {
    ind <- .include_datarep(y, s, k)
    expect_equal((k - 1) * length(y), length(ind))
  }

  check_length(1:3, s = 1, k = 3)
  check_length(1:3, s = 1, k = 4)
  check_length(1:5, s = 1, k = 5)
  check_length(1:5, s = 2, k = 5)
  check_length(1:5, s = 4, k = 5)
  check_length(c(rep(1, 5), rep(2, 3), rep(3, 2)), s = 2, k = 3)

  # At most 2 * s points included for replication
  check_n_points <- function(y, s, k) {
    ind <- .include_datarep(y, s, k)
    n_points <- rowSums(matrix(ind, ncol = (k-1)))
    expect_lte(max(n_points), 2 * s)
  }

  check_n_points(1:7, s = 1, k = 7)
  check_n_points(1:6, s = 1, k = 6)
  check_n_points(1:6, s = 2, k = 6)
  check_n_points(1:6, s = 4, k = 6)
  check_n_points(1:6, s = 5, k = 6)
  check_n_points(1:3, s = 2, k = 3)
  check_n_points(c(rep(1, 5), rep(2, 3), rep(3, 2)), s = 1, k = 3)
  check_n_points(c(rep(1, 5), rep(2, 3), rep(3, 2)), s = 2, k = 3)
})

test_that("data repliation on y works as expected.", {

  check_length <- function(y, k) {
    y_new <- .y_datarep(y, k)

    expect_equal((k - 1) * length(y), length(y_new))
  }

  check_length(1:3, k = 3)
  check_length(1:3, k = 4)
  check_length(1:5, k = 5)
  check_length(c(rep(1, 5), rep(2, 3), rep(3, 2)), k = 3)

  check_counting_rule <- function(y, k) {
    y_new <- .y_datarep(y, k)

    counts <- rowSums(matrix(y_new, ncol = (k - 1)) == 1) + 1
    expect_equal(y, counts)
  }

  check_counting_rule(1:3, k = 5)
  check_counting_rule(1:5, k = 5)
  check_length(c(rep(1, 5), rep(2, 3), rep(3, 2)), k = 3)

})


test_that("data replication on x matches kernel in linear case", {

  check_datarep_match <- function(x, k, h) {
    kernel <- x %*% t(x)
    x_new <- .x_datarep(x, k, h)
    kernel_new <- .kernel_datarep(kernel, k, h)

    expect_equal(
      x_new %*% t(x_new),
      kernel_new,
      ignore_attr = TRUE
    )
  }

  check_datarep_match(matrix(1:9, ncol = 3), k = 3, h = 1)
  check_datarep_match(matrix(1:9, ncol = 3), k = 4, h = 1)
  check_datarep_match(matrix(1:9, ncol = 3), k = 5, h = 1)
  check_datarep_match(matrix(1:9, ncol = 3), k = 5, h = 3)
  check_datarep_match(matrix(1:10, ncol = 2), k = 5, h = 2)

})

# It's important to recognize that we do NOT want data replication
# to match exactly in the 'rbf' case.  This is because the non-linear
# transformation should exist on the x only, not on the features that
# indicate the replication. However, we should be able to recover it with a
# separate function.
test_that("data replication on x matches kernel in rbf case", {

  check_datarep_no_match <- function(x, k, h) {
    kernel <- compute_kernel(x, type = "radial", sigma = 0.5)
    x_new <- .x_datarep(x, k, h)
    kernel_new <- .kernel_datarep(kernel, k, h)

    # shouldn't match overall
    expect_true(isFALSE({
      all(compute_kernel(x_new, type = "radial", sigma = 0.5) == kernel_new)
    }))

    # but should be able to calculate with `.compute_kernel_datarep()`
    expect_equal(
      .compute_kernel_datarep(x_new, x_new, k, h, type = "radial", sigma = 0.5),
      kernel_new,
      ignore_attr = TRUE
    )
  }

  check_datarep_no_match(matrix(1:9, ncol = 3), k = 3, h = 1)
  check_datarep_no_match(matrix(1:9, ncol = 3), k = 4, h = 1)
  check_datarep_no_match(matrix(1:9, ncol = 3), k = 5, h = 1)
  check_datarep_no_match(matrix(1:9, ncol = 3), k = 5, h = 3)
  check_datarep_no_match(matrix(1:10, ncol = 2), k = 5, h = 2)

})
