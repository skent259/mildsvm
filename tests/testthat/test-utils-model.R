test_that("`.warn_omisvm_s()` ensures 1 <= s <= k-1.", {

  check_s_bounds <- function(s, k, warn, correct_s) {
    if (warn) {
      expect_warning({
        s <- .warn_omisvm_s(s, k, "", "")
      })
    } else {
      s <- .warn_omisvm_s(s, k, "", "")
    }
    expect_equal(s, correct_s)
  }

  check_s_bounds(s = Inf, k = 5, warn = FALSE, correct_s = 4)
  check_s_bounds(s = Inf, k = 3, warn = FALSE, correct_s = 2)
  check_s_bounds(s = 0, k = 5, warn = TRUE, correct_s = 1)
  check_s_bounds(s = -1, k = 5, warn = TRUE, correct_s = 1)
  check_s_bounds(s = 20, k = 5, warn = TRUE, correct_s = 4)
  check_s_bounds(s = 3, k = 5, warn = FALSE, correct_s = 3)

})
