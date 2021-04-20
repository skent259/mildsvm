context("Testing the functions in mior.R")
suppressWarnings({
  library(dplyr)
  library(tibble)
})

# Build a sample data set ------------------------------------------------------
# - 4 columns where two of them have means related to outcome and the other two are noise
# - bags are aggregated randomly
set.seed(9)
n <- 1000
y <- sample(1:5, size = n, prob = (1 / 1:5)^2, replace = TRUE)
bags <- rep(1:(n/5), each = 5)

classify_bags(y, bags) %>% table()

X <- matrix(NA, nrow = length(y), ncol = 5)
for (y_ in unique(y)) {
  to_fill <- which(y_ == y)
  X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(20*y_, -7*y_, 5*y_, 0, 0))
}
colnames(X) <- paste0("V", 1:ncol(X))

y_bag <- classify_bags(y, bags, condense = FALSE)

# build into data frames
df <- bind_cols(bag_label = classify_bags(y, bags, condense = FALSE), bag_name = bags, as.data.frame(X)) %>%
  as_tibble()
train <- bags %in% 1:100
df1 <- df[train, ]
df1_test <- df[!train, ]

# TODO: try adding a data set that follows closely to the picture in Xiao et al.

# Tests ------------------------------------------------------------------------

test_that("mior() internal functions work on simple examples", {
  set.seed(8)
  tmp <- mior_dual_fit(y_bag, bags, X, 1, 1, verbose = TRUE)

  set.seed(9)
  tmp <- mior_dual_fit(y, bags, X, 1, 1, verbose = FALSE)

  # tmp[[1]]
  # tmp[[2]]
  # tmp[[3]]$x %>% round(5)

})


#
#
# # really simple example
# set.seed(8)
# y <- c(rep(1, 5), rep(2, 2), rep(3, 2), rep(4, 3))
# bags <- c(rep(1, 2), rep(2, 3), rep(3, 2), rep(4, 2), rep(5, 3))
#
# X <- matrix(NA, nrow = length(y), ncol = 5)
# for (y_ in unique(y)) {
#   to_fill <- which(y_ == y)
#   X[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(3*y_, -3*y_, 1*y_, 0, 0))
# }
# colnames(X) <- paste0("V", 1:ncol(X))
# x <- as.matrix(X)
#
#
#
# set.seed(8)
# # tmp <- mior_dual_fit(y, bags, x, 1, 1, verbose = TRUE)
#
# K <- max(y)
# w_t <- rnorm(ncol(x)) # check to see if there is a suggested initialization
# b_t <- sort(rnorm(K+1))
#
# scores <- as.matrix(x) %*% w_t - (b_t[y] + b_t[y+1]) / 2
# scores <- as.numeric(scores)
# # g <- abs(scores)
# # h <- -classify_bags(-abs(scores), bags, condense = FALSE)
# theta <- compute_theta(g = abs(scores), bags)
# lambda <- sign(scores)
#
# delta <- theta*lambda
# # decompose h, obtain problem 10, dual form of problem 10
# model <- mior_dual_model(x, y, bags, delta, 1, 1)
# colnames(model$A) <- model$varnames
# model$A
#
# gurobi_result <- gurobi::gurobi(model)
#
# compute_b(gurobi_result, model, delta, y, bags, 1, 1)

