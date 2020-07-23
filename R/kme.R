#' Compute the Radial Basis Kernel from two matrices
#'
#' @description Compute the Radial Basis Kernel from two matrices.  Follows
#' very closesly to kernlab::kernelMatrix.rbfkernel but omits unnecessary
#' checks that slow down the computation.
#' @param sigma Parameter for the radial basis function that controls the width
#' @param x matrix
#' @param y matrix, must have same number of columns as `x`
#' @author Sean Kent
rbf_kernel_matrix <- function(sigma, x, y) {
    n <- dim(x)[1]
    m <- dim(y)[1]
    dota <- rowSums(x*x)/2
    dotb <- rowSums(y*y)/2
    res <- x%*%t(y)
    for( i in 1:m ) {
        res[,i]<- exp(2*sigma*(res[,i] - dota - rep(dotb[i],n)))
    }
    return(res)
}



##' Default method for kme function
##'
##' Default method for kme function.  This function calculates the kernel mean embedding for a given
##' kernel and a dataset if df2 is NULL, then calculate the kernel mean
##' embedding matrix of (df, df) otherwise calculate (df, df2)
##' @param df Data.frame, must have column 'instance_name' which defines the instances.
##' @param df2 Data.frame
##' @param sigma Parameter for 'rbf'
##' @return A matrix.
##' @examples
##' x = data.frame('instance_name' = c('inst_1', 'inst_2', 'inst_1'),
##'                'X1' = c(-0.4, 0.5, 2))
##' K <- kme(x)
##' @export
##' @author Yifei Liu
kme.default <- function(df, df2 = NULL, sigma = 0.05) {


    if (is.null(df2)) {
        if (is.null(df$instance_name)) {
            stop("There should be a column of 'df' called 'instance_name'!")
        } else {
            inst_name_set <- unique(df$instance_name)
            df_features <- subset(df, select = -c(instance_name))
            s <- split(df_features, factor(df$instance_name, levels = inst_name_set))
            s <- lapply(s, as.matrix)
            r <- lapply(s, nrow)
            n <- length(inst_name_set)
            K <- matrix(NA, n, n)
            for (i in 1:n) {
                for (j in 1:i) {
                    K[i, j] <- 1 / (r[[i]] * r[[j]]) * sum( rbf_kernel_matrix(sigma, s[[i]], s[[j]]) )
                    if(j != i) K[j, i] <- K[i, j]
                }
            }
        }
    } else {
        if (is.null(df$instance_name) | is.null(df2$instance_name)) {
            stop("There should be a column of 'df' and 'df2' called 'instance_name'!")
        } else {
            inst_name_set <- unique(df$instance_name)
            inst_name_set2 <- unique(df2$instance_name)
            n <- length(inst_name_set)
            n2 <- length(inst_name_set2)
            K <- matrix(NA, n, n2)
            for (i in 1:n) {
                X_i <- df[df$instance_name == inst_name_set[i], ]
                X_i$instance_name = NULL
                n_i <- nrow(X_i)
                for (j in 1:n2) {
                  Z_j <- df2[df2$instance_name == inst_name_set2[j], ]
                  Z_j$instance_name = NULL
                  n_j <- nrow(Z_j)
                  dis_ij <- apply(X_i, 1, function(x) apply(Z_j, 1, function(z) sum((x - z)^2)))
                  K[i, j] <- 1/(n_i * n_j) * sum(exp(-sigma * dis_ij))
                  ## K[i, j] <- 1 / (n_i * n_j) * sum(apply(X_i, 1, function(x)
                  ## sum(apply(Z_j, 1, function(z) as.numeric(kernel_mild(x, z)) ) ) ) )
                }
            }
        }
    }
    return(K)
}

##' MilData method for kme function
##'
##' MilData method for kme function
##' @param df A MilData object.
##' @param df2 Data.frame
##' @param sigma Parameter for 'rbf'
##' @return A matrix.
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 10,
##'                             positive_degree = 3)
##' K <- kme(MilData1) ## About 10 seconds.
##' @export
##' @author Yifei Liu
kme.MilData <- function(df, df2 = NULL, sigma = 0.05) {
    df$bag_label <- df$bag_name <- NULL
    if (!is.null(df2))
        df2$bag_label <- df2$bag_name <- NULL
    kme.default(df, df2, sigma)
}

##' Function to calculate the kernel mean embedding matrix for two distributional data with a given kernel
##'
##' Function to calculate the kernel mean embedding for to distributional data sets. It uses the empirical approximation for the integral
##' \deqn{\int_{\mathcal X} \int_{\mathcal Y} K(x, y) d P_X d Q_Y },
##' for a given kernel \eqn{K(\cdot, \cdot)}. Currently only supports radial basis function kernel for fast computation.
##' @param df A data.frame with first column being `instance_name` and the rest being features. Or a MilData object.
##' @param df2 If `df2` is null, the inner embedding of `df` and itself will be calculated. Otherwise `df2` is a data.frame with first column being `instance_name` and the rest being features. Or a MilData object.
##' @param sigma The parameter for rbf kernel.
##' @return A matrix K of number of unique instance labels in df by that in df2.
##' @examples
##' MilData1 <- GenerateMilData(positive_dist = 'mvt',
##'                             negative_dist = 'mvnormal',
##'                             remainder_dist = 'mvnormal',
##'                             nbag = 10,
##'                             positive_degree = 3)
##' K <- kme(MilData1) ## About 10 seconds.
##' @export
##' @author Yifei Liu
##'
kme <- function(df, df2 = NULL, sigma = 0.05) {
    UseMethod("kme", df)
}
