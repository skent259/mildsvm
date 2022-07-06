

.include_datarep <- function(y, s, k) {
  # include when max(1, q-s+1) <= y <= q and
  # include when q+1 <= y <= min(K, q+s)
  out <- lapply(seq_len(k-1), FUN = function(q) {
    include <-
      (max(1, q-s+1) <= y & y <= q) |
      (q+1 <= y & y <= min(k, q+s))
  })
  unlist(out)
}

#' Vector of 0's with a 1 in the qth spot
#' @noRd
.e <- function(q, len) {
  x <- rep(0, len)
  x[q] <- 1
  if (length(x) > 0) {
    names(x) <- paste0("dr", seq_len(len))
  }
  return(x)
}

#' Data replication on `y`
#' @inheritParams omisvm
#' @param k The number of ordinal levels in `y`.
#' @noRd
.y_datarep <- function(y, k) {
  # y_i -> { -1 for y_i = 1, ..., q; +1 for y_i = q+1, ..., K }
  out <- lapply(seq_len(k-1), FUN = function(q) {
    y_new <- 1 * (y >= q + 1)
    y_new <- 2 * y_new - 1
  })

  unlist(out)
}

#' Data replication on `bags`
#'
#' Returns a data.frame that maps original bags (`.orig`) to replicated bags
#' (`.repl`) in the replicated data.
#' @inheritParams omisvm
#' @inheritParams y_datarep
#' @noRd
.bags_datarep <- function(bags, k) {
  out <- lapply(seq_len(k-1), FUN = function(q) {
    data.frame(
      .orig = bags,
      .repl = paste0(bags, "_rep", q),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

#' Data replication on `X`
#' @param x A data.frame, matrix, or similar object of covariates.
#' @param h The hyperparameter in replication which determines how far to
#'   separate the new `x` points.
#' @inheritParams y_datarep
#' @noRd
.x_datarep <- function(x, k, h = 1) {
  # row x_i -> [x_i, h*e_{q-1}]
  # e_{q-1} is a k-2 length vector that is 0 except in the q-1 position
  out <- lapply(seq_len(k-1), FUN = function(q) {
    x_h <- replicate(nrow(x), h * .e(q - 1, k - 2), simplify = FALSE)
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
  n <- nrow(kernel)
  h_mat <- matrix(rep(h^2, n^2), nrow = n)
  h_mat <- Matrix::bdiag(c(
    rep(0, n),
    rep(list(h_mat), k - 2))
  )

  rep_ind <- rep(seq_len(nrow(kernel)), k-1)
  as.matrix(kernel[rep_ind, rep_ind] + h_mat)
}

#' Compute kernel from data in replication space
#'
#' Note: this is less efficient than running `.kernel_datarep()` on a
#' non-replicated version of `x`, because the replicated `x` will have much
#' larger size.  However, this function is needed when we are working in the
#' replicated space.
#'
#' @param x A matrix of data, which lies in a data replication space
#' @param x2 A matrix of data which lies in a data replication space, or `NULL`
#'   to compute using `x`.
#' @param ... Arguments passed to `compute_kernel()` including `type` and
#'   `sigma`
#' @inheritParams x_datarep
#' @noRd
.compute_kernel_datarep <- function(x, x2, k, h = 1, ...) {
  if (is.null(x2)) {
    x2 <- x
  }

  dr_cols <- rev(ncol(x) - seq_len(k - 2) + 1)
  x_kernel <- compute_kernel(
    x[, -dr_cols, drop = FALSE],
    x2[, -dr_cols, drop = FALSE],
    ...
  )

  h_kernel <- compute_kernel(
    x[, dr_cols, drop = FALSE],
    x2[, dr_cols, drop = FALSE],
    type = "linear"
  )

  as.matrix(x_kernel + h_kernel)
}


