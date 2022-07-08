new_kfm_nystrom <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "kfm_nystrom")
}

validate_kfm_nystrom <- function(x) {
  message("No validations currently in place for object of class 'kfm_nystrom'.")
  x
}

#' Fit a Nyström kernel feature map approximation
#'
#' Use the Nyström method to fit a feature map that approximates a given kernel.
#'
#' For the `...` argument, the additional parameters depend on which kernel is
#' used:
#' * For `kernel = 'radial'`, specify `sigma` to define kernel bandwidth.
#'
#' @param df An object containing covariates for training.  Usually a data.frame
#'   or matrix.
#' @param m The number of examples from `df` to sample in fitting.
#' @param r The rank of matrix approximation to use. Must be less than or equal
#'   to `m`, the default.
#' @param kernel A character determining the kernel to use.  Currently, only
#'   `'radial'` is implemented.
#' @param sampling A character determining how to sample instances.  Default is
#'   `'random'`. For `kfm_nystrom.mild_df()`, one can specify `sampling =
#'   'stratified'` to ensure that samples are chosen evenly from bags and
#'   instances. `sampling` can also be a numeric vector of length `m` of
#'   pre-determined samples.
#' @param ... additional parameters needed for the kernels.  See details.
#'
#' @return an object of class `kfm_nystrom` with the following components:
#'   * `df_sub` the sub-sampled version of `df`
#'   * `dv` pre-multiplication matrix which contains information on the
#'   eigenvalues and eigenvectors of `df_sub`
#'   * `method` `'nystrom'`
#'   * `kernel` the input parameter `kernel`
#'   * `kernel_params` parameters passed to `...`
#'
#' @references Williams, C., & Seeger, M. (2001). Using the Nyström Method to
#'   Speed Up Kernel Machines. *Advances in Neural Information Processing
#'   Systems*, *13*, 682–688.
#'
#'   Kent, S., & Yu, M. (2022). Non-convex SVM for cancer diagnosis based on
#'   morphologic features of tumor microenvironment *arXiv preprint*
#'   [arXiv:2206.14704](https://arxiv.org/abs/2206.14704)
#'
#' @examples
#' df <- data.frame(
#'   X1 = c(2,   3,   4,   5,   6, 7, 8),
#'   X2 = c(1, 1.2, 1.3, 1.4, 1.1, 7, 1),
#'   X3 = rnorm(7)
#' )
#'
#' fit <- kfm_nystrom(df, m = 7, r = 6, kernel = "radial", sigma = 0.05)
#' fm <- build_fm(fit, df)
#'
#' @family kernel feature map functions
#' @export
#' @author Sean Kent
kfm_nystrom <- function(df, m, r, kernel, sampling, ...) {
  UseMethod("kfm_nystrom", df)
}

#' @describeIn kfm_nystrom For use on objects of class `data.frame` or `matrix`.
#' @export
kfm_nystrom.default <- function(df,
                                m = nrow(df),
                                r = m,
                                kernel = "radial",
                                sampling = "random",
                                ...) {
  # TODO: check all columns are numeric
  `%ni%` <- Negate(`%in%`)
  kernel_params <- list(...)
  sampling_arg <- .set_sampling_arg_passed(sampling)

  df <- as.matrix(df)
  # `sampling`
  if (is.numeric(sampling)) {
    if (length(sampling) != m)  {
      warning("Length of input 'sampling' is not equal to 'm', reverting to sampling = 'random'.")
      sampling <- .resample(seq_len(nrow(df)), m)
    }
  } else if (sampling == "random") {
    sampling <- .resample(seq_len(nrow(df)), m)
  } else {
    stop("parameter 'sampling' must be a numeric vector or the character 'random'. ")
  }
  df_sub <- df[sampling, , drop = FALSE]
  rownames(df_sub) <- sampling

  k_hat <- compute_kernel(df_sub, type = kernel, sigma = kernel_params$sigma)

  e <- eigen(k_hat)
  # sometimes when data is duplicated we get 0 eigenvalues and NA columns in the output
  n_rep <- sum(e$values[1:r] < 1e-10)
  if (n_rep > 0) {
    r <- r - n_rep
    warning(paste0("Data chosen in subsample appears to be duplicated, reducing number of features to ", r))
  }
  d_mat <- diag(1 / sqrt(e$values[1:r]))
  v_mat <- t(e$vectors[, 1:r, drop = FALSE])

  return(new_kfm_nystrom(list(
    df_sub = df_sub,
    dv = d_mat %*% v_mat,
    method = "nystrom",
    params = list(
      m = m,
      r = r,
      sampling = sampling_arg
    ),
    kernel = kernel,
    kernel_params = kernel_params
  )))
}

#' @describeIn kfm_nystrom Ignore the information columns `'bag_label'`,
#'   `'bag_name'`, and `'instance_name'` when calculating kernel approximation.
#' @export
kfm_nystrom.mild_df <- function(df,
                                m = nrow(df),
                                r = m,
                                kernel = "radial",
                                sampling = "random",
                                ...) {

  if (all(sampling == "stratified")) {
    sampling <- bag_instance_sampling(df, m)
  }
  df$bag_label <- df$bag_name <- df$instance_name <- NULL

  return(kfm_nystrom.default(df, m, r, kernel, sampling, ...))
}

#' @export
print.kfm_nystrom <- function(x, digits = getOption("digits"), ...) {
  kernel_param <- .get_kernel_param_str(x, digits)

  cat("A Nystrom kernel feature map object", "\n")
  cat("", "\n")
  cat("Parameters:", "\n")
  cat("  m:", x$params$m, "\n")
  cat("  r:", x$params$r, "\n")
  cat("  kernel:", x$kernel, kernel_param, "\n")
  cat("  sampling:", x$params$sampling, "\n")
  cat("\n")
}

#' @describeIn build_fm Method for `kfm_nystrom` class.
#' @export
build_fm.kfm_nystrom <- function(kfm_fit, new_data, ...) {
  if (inherits(new_data, "mild_df")) {
    info <- new_data[, c("bag_label", "bag_name", "instance_name"), drop = FALSE]
    new_data$bag_label <- new_data$bag_name <- new_data$instance_name <- NULL
  } else {
    info <- NULL
  }
  stopifnot("newx must have the same columns as kfm_fit$df_sub." = colnames(new_data) == colnames(kfm_fit$df_sub))
  new_data <- as.matrix(new_data)

  k <- compute_kernel(new_data,
                      kfm_fit$df_sub,
                      type = kfm_fit$kernel,
                      sigma = kfm_fit$kernel_params$sigma)

  fm <- k %*% t(kfm_fit$dv)
  if (!is.null(info)) {
    fm <- cbind(info, fm)
  }
  return(fm)
}
