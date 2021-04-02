#
#
# # x <- as_tibble(X)
# x <- as.matrix(X)
#
# # pre-specified parameters
# # x, y, bags,
# max_t <- 500
# c0 <- 1
# c1 <- 1
# threshold <- 0.1
#
# # gurobi parameters
# params <- list()
# params$OutputFlag = 1*verbose
# params$IntFeasTol = 1e-5
# if (time_limit) params$TimeLimit = time_limit
#
#
# K <- max(y)
# # initialize parameters
# t <- 0
#
# delta_j <- 1e-3
# j <- numeric(max_t+2)
# j_ <- function(t) {
#   j[t+2] # let j_ start at index -1, so j[1] = j_(-1), j[2] = j_(0), etc.
# }
# j[1] <- 1e-3 # j(-1)
#
# w_t <- rnorm(ncol(x)) # check to see if there is a suggested initialization
# b_t <- rnorm(K+1)
#
# while (delta_j / j_(t-1) > threshold) {
#   # compute theta and lambda
#   scores <- as.matrix(x) %*% w_t - (b_t[y] + b_t[y+1]) / 2
#   scores <- as.numeric(scores)
#   # g <- abs(scores)
#   # h <- -classify_bags(-abs(scores), bags, condense = FALSE)
#   theta <- compute_theta(g = abs(scores), bags)
#   lambda <- sign(scores)
#
#   delta <- theta*lambda
#   # decompose h, obtain problem 10, dual form of problem 10
#   model <- mior_dual_model(x, y, bags, delta, c0, c1)
#
#   # solve dual form (21) to get w and b
#   gurobi_result <- gurobi::gurobi(model, params = params)
#
#   # update w, b, j
#   w <- 0
#   b <- 0 # need to figure out how to compute b from complementary slackness
#   j[t+2] <- 0
#
#   t <- t + 1
#   w_t <- w
#   b_t <- b
#   delta_j <- j_(t-2) - j_(t-1)
# }


mior_dual_model <- function(x, y, bags, delta, c0, c1) {

  n_a <- length(unique(bags))
  n_mu <- K <- max(y)
  n_rho <- 1

  y_bag <- classify_bags(y, bags) # TODO: make sure that the bag label is passed here still

  # Build constraint matrix
  .e_vec <- function(b, len) {
    # puts a 1 in the `b`th entry of a `len`-length 0 vector
    vec <- rep(0, len)
    vec[b] <- 1
    return(vec)
  }

  alpha_constr <- matrix(0, nrow = n_mu+1, n_a)
  sum_delta_plus <- sapply(unique(bags), function(bag) sum(delta[bags == bag] + 1))
  sum_delta_minus <- sapply(unique(bags), function(bag) sum(delta[bags == bag] - 1))
  for (i in 1:n_a) {
    p <- y_bag[i] + 1 # +1 for zero-indexing
    alpha_constr[p, i] <- alpha_constr[p, i] + sum_delta_plus[i]
    alpha_constr[p-1, i] <- alpha_constr[p-1, i] + sum_delta_minus[i]
  }
  mu_constr <- sapply(1:(K), .e_vec, len = n_mu+1) - sapply(2:(K+1), .e_vec, len = n_mu+1)
  rho_constr <- .e_vec(K+1, n_mu+1) - .e_vec(1, n_mu+1)

  constraints <- cbind(alpha_constr, mu_constr, rho_constr)

  # Quadratic objective matrix
  ind <- delta != 0
  kernel <- x[ind, ] %*% t(x[ind, ])
  alpha_Q <- - 0.5 * (delta[ind] %*% t(delta[ind])) * kernel
  mu_rho_Q <- matrix(0, n_mu + n_rho, n_mu + n_rho)
  Q <- Matrix::bdiag(alpha_Q, mu_rho_Q)
  # worry about bags that are unordered, maybe take care of this at the beginning of the function

  model <- list()
  # Objective
  model$modelsense <- "max"
  model$obj <- c(rep(1, n_a), rep(0, n_mu + n_rho))
  model$Q <- Q # TODO: replace this with kernel at some point
  # Constraints
  model$varnames <- c(paste0("a", 1:n_a), paste0("mu", 1:n_mu), "rho")
  model$A <- constraints
  model$sense <- rep("=", nrow(constraints))
  model$rhs <- rep(0, nrow(constraints))
  model$lb <- rep(0, n_a + n_mu + n_rho)
  model$ub <- c(rep(c1, n_a), rep(Inf, n_mu), c0)

  return(model)
}
#
#
# model <- mior_dual_model(x, y, bags, delta, 1, 1)
# gurobi_result <- gurobi::gurobi(model)
#
# a <- gurobi_result$x[grepl("a", model$varnames)]
#
# w <- - colSums(a * delta[ind] * X[ind, ])
# w
#
# # TODO: try this out with a small example that is easy to compute and see if the model checks out


# helper functions
compute_theta <- function(g, bags) {
  # theta is 1 only for the instance with minimal g in the bag
  theta <- rep(0, length(g))
  for (bag in unique(bags)) {
    ind <- bag == bags
    argmin <- which.min(g[ind])
    theta[ind][argmin] <- 1
  }
  return(theta)
}

