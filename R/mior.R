
# Specific implementation methods below ----------------------------------------

mior_dual_fit <- function(y, bags, x, c0, c1, rescale = TRUE, weights = NULL,
                          kernel = "linear", sigma = NULL,
                          verbose = FALSE, time_limit = FALSE, max_step = 500) {

  r <- .reorder(y, bags, X)
  y <- r$y
  bags <- r$b
  X <- r$X
  if (rescale) x <- scale(x)

  # kernel
  # if (!is.matrix(kernel)) {
  #   K <- compute_kernel(X, type = kernel, sigma = sigma)
  # } else {
  #   K <- kernel
  # }

  # pre-specified parameters
  # x, y, bags,
  # max_step <- 500
  # c0 <- 1
  # c1 <- 1
  threshold <- 0.1
  # verbose <- FALSE

  # gurobi parameters
  params <- list()
  params$OutputFlag = 1*(verbose == 2)
  params$IntFeasTol = 1e-5
  if (time_limit) params$TimeLimit = time_limit

  # initialize parameters
  K <- max(y)
  t <- 0
  delta_j <- 1e-3
  j <- numeric(max_step+2)
  j_ <- function(t) {
    j[t+2] # let j_ start at index -1, so j[1] = j_(-1), j[2] = j_(0), etc.
  }
  j[1] <- 1e-3 # j(-1)

  w_t <- rnorm(ncol(x)) # check to see if there is a suggested initialization
  b_t <- sort(rnorm(K+1))

  while (abs(delta_j / j_(t-1)) > threshold && t < max_step) {
    if (min(abs(j[t+1] - j[-(t+1)])) < 1e-7) {
      # print(min(abs(j[t+1] - j)))
      rlang::warn(c(
        "Optimization appears to be repeating solutions.",
        i = "Stopping with best solution."
      ))
      break
    }

    # compute theta and lambda
    scores <- as.matrix(x) %*% w_t - (b_t[y] + b_t[y+1]) / 2
    scores <- as.numeric(scores)
    # g <- abs(scores)
    # h <- -classify_bags(-abs(scores), bags, condense = FALSE)
    theta <- compute_theta(g = abs(scores), bags)
    lambda <- sign(scores)
    delta <- theta*lambda

    model <- mior_dual_model(x, y, bags, delta, c0, c1)
    gurobi_result <- gurobi::gurobi(model, params = params)

    ind <- delta != 0
    a <- gurobi_result$x[grepl("a", model$varnames)]
    # update w, b, j
    t <- t + 1
    w_t <- - colSums(a * delta[ind] * x[ind, , drop = FALSE])
    b_t <- compute_b(gurobi_result, model, delta, y, bags, c0, c1)
    j[t+1] <- gurobi_result$objval # or, sum(a) + t(gurobi_result$x) %*% model$Q %*% gurobi_result$x
    delta_j <- j_(t-2) - j_(t-1)

    if (verbose) {
      cat(t, ": ", sep = "")
      cat("J: ", j[t+1], "; ", sep = "")
      cat("b: ", paste0(round(b_t, 2), collapse = " "), "\n", sep = "")
    }
  }

  if (t == max_step) {
    rlang::warn(c(
      paste0("The number of iterations of heuristic algorithm reached the threshold of ", max_step, "."),
      i = "Stopping with current selection."
    ))
  }
  # TODO: figure out why this doesn't converge.  Might be an error in my
  # implementation... But it seems to find something that's fairly reasonable.

  res <- list(
    gurobi_fit = list(
      w = w_t,
      b = b_t,
      xmatrix = X[ind, , drop = FALSE],
      # ay = a * y[ind],
      # kernel = kernel,
      # sigma = sigma,
      # xi = gurobi_result$x[grepl("xi", model$varnames)],
      status = gurobi_result$status,
      itercount = gurobi_result$itercount,
      baritercount = gurobi_result$baritercount,
      objval = gurobi_result$objval,
      c0 = c0,
      c1 = c1,
      n_selections = t
    ),
    n_step = t,
    # repr_inst = selected_vec,
    x = NULL
  )
  if (rescale) {
    res$x_scale <- list(
      "center" = attr(X, "scaled:center"),
      "scale" = attr(X, "scaled:scale")
    )
  }
  # names(res$model$w) <- colnames(X)
  return(res)
}

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
  sum_delta_plus <- sapply(unique(bags), function(bag) sum(1 + delta[bags == bag]))
  sum_delta_minus <- sapply(unique(bags), function(bag) sum(1 - delta[bags == bag]))
  # sum_delta_plus <- sapply(unique(bags), function(bag) 1 + sum(delta[bags == bag]))
  # sum_delta_minus <- sapply(unique(bags), function(bag) 1 - sum(delta[bags == bag]))

  for (i in 1:n_a) {
    p <- y_bag[i] + 1 # +1 for zero-indexing
    alpha_constr[p, i] <- alpha_constr[p, i] - sum_delta_plus[i] / 2
    alpha_constr[p-1, i] <- alpha_constr[p-1, i] + sum_delta_minus[i] / 2
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

compute_b <- function(gurobi_result, model, delta, y, bags, c0, c1) {
  # names(gurobi_result$x) <- model$varnames
  a <- gurobi_result$x[grepl("a", model$varnames)]
  mu <- gurobi_result$x[grepl("mu", model$varnames)]
  rho <- gurobi_result$x[grepl("rho", model$varnames)]

  n_b <- length(mu) + 1
  eps <- 1e-5
  ind <- which(a > 0 + eps & a < c1 - eps)
  if (length(ind) == 0) {
    # browser()
    rlang::warn(c(
      "The optimization didn't return any support vectors.",
      i = "Resetting the values of `b` randomly. "
    ))
    return(sort(rnorm(n_b)))
    # return(rep(1, n_b))
  }

  support_bags <- unique(bags)[ind]
  y_support <- classify_bags(y, bags)[ind]

  Q_tilde <- -2 * model$Q[1:length(a), 1:length(a)] # recovers delta * kernel
  b_q <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag] + 1))
  b_q1 <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag] - 1))
  # b_q <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag]) + 1)
  # b_q1 <- - 0.5 * sapply(unique(bags)[ind], function(bag) sum(delta[bags == bag]) - 1)
  # browser()
  # linear model using complementary slackness constraints: resp ~ pred_matrix
  resp <- as.numeric(- a %*% Q_tilde[, ind] ) + 1
  # resp <- as.numeric(- a %*% Q_tilde ) + 1
  resp <- c(resp, rep(0, length(mu) + 1))
  pred_matrix <- matrix(0, nrow = length(ind) + length(mu) + 1, ncol = n_b)
  # information from alpha
  for (i in 1:length(ind)) {
    pred_matrix[i, y_support[i]+1] <- b_q[i]
    pred_matrix[i, y_support[i]] <- b_q1[i]
  }

  # information from mu; b_q = b_{q-1} if \mu_q > 0
  for (q in 1:length(mu)) {
    if (mu[q] > 0 + eps) {
      rlang::warn(c(
        paste0("The optimization solution suggests that two intercepts are equal: b[", q-1, "] == b[", q, "].")
      ))
      start <- length(ind)
      pred_matrix[start + q, q] <- 1
      pred_matrix[start + q, q+1] <- -1
    }
  }
  # information from rho, gamma, eta; b_0 = b_K if \rho < C_0
  if (rho < c0 - eps) {
    rlang::warn(c(
      "The optimization solution suggests that endpoints are equal: b[0] == b[K]."
    ))
    start <- length(ind) + length(mu)
    pred_matrix[start + 1, 1] <- -1
    pred_matrix[start + 1, n_b] <- 1
  }
  # browser()
  b <- coef(lm(resp ~ 0 + pred_matrix))
  if (any(is.na(b))) {
    # If values are NA, that means the particular column isn't needed for
    # prediction, i.e. that the dropped coefficient is 0.
    rlang::warn("There were NA values in `b`.  Replacing with 0.")
    b[which(is.na(b))] <- 0
    # rlang::abort("There are NA values in `b`.")
  }
  return(b)
}
