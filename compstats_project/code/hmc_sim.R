library(mvtnorm)
library(ggplot2)
library(coda)

# Define the log-posterior (unnormalized) for a mixture of Gaussians
log_target <- function(theta) {
  mu1 <- c(-3, -3)
  mu2 <- c(3, 3)
  sigma <- 0.5 * diag(2)
  l1 <- dmvnorm(theta, mean = mu1, sigma = sigma, log = TRUE)
  l2 <- dmvnorm(theta, mean = mu2, sigma = sigma, log = TRUE)
  return(log(exp(l1) + exp(l2)))  # log-sum-exp trick
}


metropolis_sampler <- function(log_target, n_iter, init, proposal_sd) {
  theta <- matrix(NA, nrow = n_iter, ncol = 2)
  theta[1, ] <- init
  accept <- 0
  
  for (i in 2:n_iter) {
    proposal <- theta[i - 1, ] + rnorm(2, 0, proposal_sd)
    log_alpha <- log_target(proposal) - log_target(theta[i - 1, ])
    if (log(runif(1)) < log_alpha) {
      theta[i, ] <- proposal
      accept <- accept + 1
    } else {
      theta[i, ] <- theta[i - 1, ]
    }
  }
  return(list(samples = theta, accept_rate = accept / n_iter))
}

# Gradient of the log-target (approximated numerically)
grad_log_target <- function(theta, eps = 1e-4) {
  grad <- numeric(length(theta))
  for (i in 1:length(theta)) {
    step <- rep(0, length(theta))
    step[i] <- eps
    grad[i] <- (log_target(theta + step) - log_target(theta - step)) / (2 * eps)
  }
  return(grad)
}

hmc_sampler <- function(log_target, grad_log_target, n_iter, init, step_size, L, mass = diag(2)) {
  theta <- matrix(NA, nrow = n_iter, ncol = 2)
  theta[1, ] <- init
  accept <- 0
  
  for (i in 2:n_iter) {
    current_theta <- theta[i - 1, ]
    p <- mvtnorm::rmvnorm(1, sigma = mass)[1, ]
    current_p <- p
    
    # Leapfrog steps
    theta_new <- current_theta
    p <- p + 0.5 * step_size * grad_log_target(theta_new)
    for (j in 1:L) {
      theta_new <- theta_new + step_size * solve(mass, p)
      if (j != L) {
        p <- p + step_size * grad_log_target(theta_new)
      }
    }
    p <- p + 0.5 * step_size * grad_log_target(theta_new)
    p <- -p  # negate for reversibility
    
    # Hamiltonian
    current_H <- -log_target(current_theta) + 0.5 * t(current_p) %*% solve(mass, current_p)
    new_H <- -log_target(theta_new) + 0.5 * t(p) %*% solve(mass, p)
    
    if (log(runif(1)) < (current_H - new_H)) {
      theta[i, ] <- theta_new
      accept <- accept + 1
    } else {
      theta[i, ] <- current_theta
    }
  }
  return(list(samples = theta, accept_rate = accept / n_iter))
}


set.seed(123)
n_iter <- 5000

# Run MH
mh_result <- metropolis_sampler(log_target, n_iter, init = c(0, 0), proposal_sd = 1.0)

# Run HMC
hmc_result <- hmc_sampler(log_target, grad_log_target, n_iter, init = c(0, 0),
                          step_size = 0.25, L = 15)

# Convert to MCMC objects
mh_mcmc <- mcmc(mh_result$samples)
hmc_mcmc <- mcmc(hmc_result$samples)

plot_samples <- function(samples, title) {
  df <- as.data.frame(samples)
  colnames(df) <- c("x", "y")
  ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.3, size = 0.7) +
    theme_minimal() +
    ggtitle(title)
}

plot_samples(mh_result$samples, "Metropolis-Hastings Samples")
plot_samples(hmc_result$samples, "Hamiltonian Monte Carlo Samples")


par(mfrow = c(2, 2))
plot(mh_mcmc[, 1], main = "MH Trace (x)")
acf(mh_mcmc[, 1], main = "MH ACF (x)")
plot(hmc_mcmc[, 1], main = "HMC Trace (x)")
acf(hmc_mcmc[, 1], main = "HMC ACF (x)")

cat("MH acceptance rate:", mh_result$accept_rate, "\n")
cat("HMC acceptance rate:", hmc_result$accept_rate, "\n")
