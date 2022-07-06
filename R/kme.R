#' Calculate the kernel mean embedding matrix
#'
#' Function to calculate the kernel mean embedding for to distributional data
#' sets. It uses the empirical approximation for the integral
#' \deqn{\int_{\mathcal X} \int_{\mathcal Y} K(x, y) d P_X d Q_Y } for a given
#' kernel \eqn{K(\cdot, \cdot)}. Currently only supports radial basis function
#' kernel for fast computation.
#'
#' If `df2 = NULL`, calculate the kernel mean embedding matrix of (`df`, `df`)
#' otherwise calculate (`df`, `df2`)
#'
#' @param df A data.frame of `mild_df` object, must have column
#'   `'instance_name'` which defines the instances.
#' @param df2 A data.frame, `mild_df` object, or `NULL` (default `NULL`).
#' @param sigma The parameter for `'radial'` kernel (default `0.05`).
#' @param ... Additional arguments passed to methods.
#'
#' @return A matrix of kernel mean embedding at the instance level.
#'
#' @examples
#' x = data.frame('instance_name' = c('inst_1', 'inst_2', 'inst_1'),
#'                'X1' = c(-0.4, 0.5, 2))
#' kme(x)
#'
#' mild_df1 <- generate_mild_df(nbag = 10, positive_degree = 3)
#' kme(mild_df1)
#'
#' @export
#' @author Yifei Liu, Sean Kent
#' @name kme
NULL

#' @export
kme <- function(df, df2 = NULL, sigma = 0.05, ...) {
    UseMethod("kme", df)
}

#' @describeIn kme Default S3 method
#' @export
kme.default <- function(df, df2 = NULL, sigma = 0.05, ...) {

    if (is.null(df2)) {
        if (is.null(df$instance_name)) {
            stop("There should be a column of 'df' called 'instance_name'!")
        } else {
            instances <- df$instance_name
            inst_name_set <- unique(instances)
            df$instance_name <- NULL
            s <- split(df, factor(instances, levels = inst_name_set))
            s <- lapply(s, as.matrix)
            r <- lapply(s, nrow)
            n <- length(inst_name_set)
            k_mat <- matrix(NA, n, n)
            for (i in 1:n) {
                for (j in 1:i) {
                    k_mat[i, j] <- 1 / (r[[i]] * r[[j]]) * sum(rbf_kernel_matrix(sigma, s[[i]], s[[j]]))
                    if (j != i) k_mat[j, i] <- k_mat[i, j]
                }
            }
        }
    } else {
        if (is.null(df$instance_name) || is.null(df2$instance_name)) {
            stop("There should be a column of 'df' and 'df2' called 'instance_name'!")
        } else {
            instances <- df$instance_name
            instances2 <- df2$instance_name
            df$instance_name <- df2$instance_name <- NULL
            s <- split(df, factor(instances, levels = unique(instances)))
            s <- lapply(s, as.matrix)
            s2 <- split(df2, factor(instances2, levels = unique(instances2)))
            s2 <- lapply(s2, as.matrix)
            r <- lapply(s, nrow)
            r2 <- lapply(s2, nrow)
            n <- length(unique(instances))
            n2 <- length(unique(instances2))
            k_mat <- matrix(NA, n, n2)
            for (i in 1:n) {
                for (j in 1:n2) {
                    k_mat[i, j] <- 1 / (r[[i]] * r2[[j]]) * sum(rbf_kernel_matrix(sigma, s[[i]], s2[[j]]))
                }
            }
        }
    }
    return(k_mat)
}

#' @describeIn kme S3 method for class `mild_df`
#' @export
kme.mild_df <- function(df, df2 = NULL, sigma = 0.05, ...) {
    df$bag_label <- df$bag_name <- NULL
    if (!is.null(df2))
        df2$bag_label <- df2$bag_name <- NULL
    kme.default(df, df2, sigma)
}


#' Compute the Radial Basis Kernel from two matrices
#'
#' Compute the Radial Basis Kernel from two matrices.  Follows very closely to
#' kernlab::kernelMatrix.rbfkernel but omits unnecessary checks that slow down
#' the computation.
#'
#' @param sigma Parameter for the radial basis function that controls the width
#' @param x A matrix.
#' @param y A matrix that must have same number of columns as `x`.
#' @noRd
rbf_kernel_matrix <- function(sigma, x, y) {
    n <- dim(x)[1]
    m <- dim(y)[1]
    dota <- rowSums(x*x)/2
    dotb <- rowSums(y*y)/2
    res <- x%*%t(y)
    for (i in 1:m) {
        res[, i]<- exp(2 * sigma * (res[, i] - dota - rep(dotb[i], n)))
    }
    return(res)
}

#' Compute the Linear Kernel from two matrices
#'
#' Compute the Linear Kernel from two matrices.  Follows very closely to
#' kernlab::kernelMatrix.vanilla but omits unnecessary checks that slow down the
#' computation.
#'
#' @inheritParams rbf_kernel_matrix
#' @noRd
linear_kernel_matrix <- function(x, y) {
    crossprod(t(x), t(y))
}
