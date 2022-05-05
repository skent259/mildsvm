
#' Vector of 0's with a 1 in the qth spot
#' @noRd
.e <- function(q, len) {
  x <- rep(0, len)
  x[q] <- 1
  return(x)
}

#' Data replication on `y`
#' @inheritParams omisvm
#' @param k The number of ordinal levels in `y`.
#' @noRd
.y_datarep <- function(y, k) {
  # y_i -> { -1 for y_i = 1, ..., q; +1 for y_i = q+1, ..., K }
  out <- lapply(seq_len(k-1), FUN = function(q) {
    y_new <- 1*(y >= q+1)
    y_new <- 2*y_new - 1
  })

  unlist(out)
}

#' Data replication on `X`
#' @inheritParams y_datarep
#' @param h The hyperparameter in replication which determines how far to
#'   separate the new `x` points.
#' @noRd
.x_datarep <- function(x, k, h = 1) {
  # row x_i -> [x_i, h*e_{q-1}]
  # e_{q-1} is a k-2 length vector that is 0 except in the q-1 position
  out <- lapply(seq_len(k-1), FUN = function(q) {
    x_h <- replicate(nrow(x), h * .e(q-1, k-2), simplify = FALSE)
    x_h <- do.call(rbind, x_h)
    cbind(x, x_h)
  })

  do.call(rbind, out)
}

#' Data replication on the kernel
#' @param kernel The kernel, which in the linear case is X X^T
#' @inheritParams x_datarep
#' @noRd
.kernel_datarep <- function(kernel, k, h) {
  h_mat <- matrix(rep(h^2, nrow(kernel)^2), nrow = nrow(kernel))
  h_mat <- Matrix::bdiag(c(
    rep(0, nrow(kernel)),
    rep(list(h_mat), k-2))
  )

  rep_ind <- rep(seq_len(nrow(kernel)), k-1)
  as.matrix(kernel[rep_ind, rep_ind] + h_mat)
}


# TODO: do the data replication calculations within omisvm, then pass to `misvm_dualqpheuristic_fit()`
