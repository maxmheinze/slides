library(mvtnorm)
library(ggplot2)
library(gridExtra)



# No-U-Turn sampler
leapfrog <- function(theta, r, grad_log_prob, eps) {
  r_half   <- r + 0.5 * eps * grad_log_prob(theta)
  theta_new<- theta + eps * r_half
  r_new    <- r_half + 0.5 * eps * grad_log_prob(theta_new)
  list(theta = theta_new, r = r_new)
}

find_reasonable_epsilon <- function(theta, grad_log_prob, log_prob) {
  eps <- 1
  r   <- rnorm(length(theta))
  lp0 <- log_prob(theta) - 0.5 * sum(r^2)
  lf  <- leapfrog(theta, r, grad_log_prob, eps)
  lp1 <- log_prob(lf$theta) - 0.5 * sum(lf$r^2)
  a   <- exp(lp1 - lp0)
  dir <- if (a > 0.5) 1 else -1
  while (a^dir > 2^(-dir)) {
    eps <- eps * 2^dir
    lf  <- leapfrog(theta, r, grad_log_prob, eps)
    lp1 <- log_prob(lf$theta) - 0.5 * sum(lf$r^2)
    a   <- exp(lp1 - lp0)
  }
  eps
}

build_tree <- function(theta, r, u, v, j, eps, log_prob, grad_log_prob, delta_max = 1000) {
  if (j == 0) {
    lf <- leapfrog(theta, r, grad_log_prob, v * eps)
    lp <- log_prob(lf$theta) - 0.5 * sum(lf$r^2)
    n  <- as.integer(u <= exp(lp))
    s  <- as.integer(lp - log(u) > -delta_max)
    alpha <- min(1, exp(lp - (log_prob(theta) - 0.5*sum(r^2))))
    list(theta_minus = lf$theta, r_minus = lf$r,
         theta_plus  = lf$theta, r_plus  = lf$r,
         theta_prop  = lf$theta, n_prop  = n,
         s_prop      = s,     alpha    = alpha,
         n_alpha     = 1)
  } else {
    bt1 <- build_tree(theta, r, u, v, j-1, eps, log_prob, grad_log_prob, delta_max)
    if (bt1$s_prop == 1) {
      if (v == -1) {
        bt2 <- build_tree(bt1$theta_minus, bt1$r_minus, u, v, j-1, eps, log_prob, grad_log_prob, delta_max)
        theta_minus <- bt2$theta_minus; r_minus <- bt2$r_minus
        theta_plus  <- bt1$theta_plus;  r_plus  <- bt1$r_plus
      } else {
        bt2 <- build_tree(bt1$theta_plus, bt1$r_plus, u, v, j-1, eps, log_prob, grad_log_prob, delta_max)
        theta_minus <- bt1$theta_minus; r_minus <- bt1$r_minus
        theta_plus  <- bt2$theta_plus;  r_plus  <- bt2$r_plus
      }
      den  <- bt1$n_prop + bt2$n_prop
      if (den > 0) {
        if (runif(1) < bt2$n_prop / den) {
          theta_prop <- bt2$theta_prop
        } else {
          theta_prop <- bt1$theta_prop
        }
      } else {
        theta_prop <- bt1$theta_prop
      }
      r_sum <- bt2$r_plus + bt2$r_minus
      s_prop <- as.integer(bt1$s_prop == 1 && bt2$s_prop == 1 &&
                             sum(r_sum * (bt2$theta_plus - bt2$theta_minus)) >= 0)
      list(theta_minus = theta_minus, r_minus = r_minus,
           theta_plus  = theta_plus,  r_plus  = r_plus,
           theta_prop  = theta_prop,
           n_prop      = bt1$n_prop + bt2$n_prop,
           s_prop      = s_prop,
           alpha       = bt1$alpha + bt2$alpha,
           n_alpha     = bt1$n_alpha + bt2$n_alpha)
    } else {
      bt1
    }
  }
}

nuts <- function(log_prob, grad_log_prob, theta0, n_iter, adapt_steps = floor(n_iter/2),
                 delta = 0.65, max_depth = 10) {
  d <- length(theta0)
  samples <- matrix(NA, n_iter, d)
  eps_hist <- numeric(n_iter)
  theta <- theta0
  eps   <- find_reasonable_epsilon(theta, grad_log_prob, log_prob)
  mu    <- log(10 * eps)
  eps_bar <- 1
  H_bar   <- 0
  gamma <- 0.05; t0 <- 10; kappa <- 0.75
  
  for (i in 1:n_iter) {
    eps_hist[i] <- eps
    r0 <- rnorm(d)
    joint0 <- log_prob(theta) - 0.5 * sum(r0^2)
    u <- exp(joint0) * runif(1)
    theta_minus <- theta; theta_plus <- theta
    r_minus <- r0;       r_plus   <- r0
    theta_prop <- theta
    j <- 0; n_prop <- 1; s_prop <- 1; alpha_sum <- 0; n_alpha_sum <- 0
    
    while (s_prop == 1 && j < max_depth) {
      v <- sample(c(-1,1),1)
      if (v == -1) {
        bt <- build_tree(theta_minus, r_minus, u, v, j, eps, log_prob, grad_log_prob)
        theta_minus <- bt$theta_minus; r_minus <- bt$r_minus
      } else {
        bt <- build_tree(theta_plus, r_plus, u, v, j, eps, log_prob, grad_log_prob)
        theta_plus  <- bt$theta_plus;  r_plus  <- bt$r_plus
      }
      if (bt$s_prop == 1 && runif(1) < bt$n_prop / n_prop) {
        theta_prop <- bt$theta_prop
      }
      n_prop    <- n_prop + bt$n_prop
      s_prop    <- bt$s_prop * as.integer(sum((theta_plus - theta_minus)*r_minus)>=0) *
        as.integer(sum((theta_plus - theta_minus)*r_plus )>=0)
      alpha_sum   <- alpha_sum + bt$alpha
      n_alpha_sum <- n_alpha_sum + bt$n_alpha
      j <- j + 1
    }
    
    acc_rate <- alpha_sum / n_alpha_sum
    if (i <= adapt_steps) {
      H_bar <- (1 - 1/(i + t0)) * H_bar + (delta - acc_rate)/(i + t0)
      log_eps <- mu - sqrt(i)/gamma * H_bar
      eta <- i^(-kappa)
      eps_bar <- exp((1 - eta) * log(eps_bar) + eta * log_eps)
      eps <- exp(log_eps)
    } else {
      eps <- eps_bar
    }
    
    theta <- theta_prop
    samples[i, ] <- theta
    cat(sprintf("Iter %d: eps=%.5f, acc=%.3f\n", i, eps, acc_rate))
    
  }
  samples
}

#NUTS with epsilon
# Wrapper NUTS that records ε at each iteration
nuts_with_eps <- function(log_prob, grad_log_prob, theta0, n_iter,
                          adapt_steps = floor(n_iter/2),
                          delta = 0.65, max_depth = 10) {
  d <- length(theta0)
  samples <- matrix(NA, n_iter, d)
  eps_hist <- numeric(n_iter)
  theta <- theta0
  eps   <- find_reasonable_epsilon(theta, grad_log_prob, log_prob)
  mu    <- log(10 * eps)
  eps_bar <- 1
  H_bar   <- 0
  gamma <- 0.05; t0 <- 10; kappa <- 0.75
  
  for (i in 1:n_iter) {
    eps_hist[i] <- eps
    
    # one NUTS iteration (identical to your nuts() core, minus storage)
    r0 <- rnorm(d)
    joint0 <- log_prob(theta) - 0.5 * sum(r0^2)
    u <- exp(joint0) * runif(1)
    tm <- theta; tp <- theta; rm <- r0; rp <- r0
    prop <- theta; j <- 0; n_prop <- 1; s_prop <- 1
    alpha_sum <- 0; n_alpha_sum <- 0
    
    while (s_prop == 1 && j < max_depth) {
      v <- sample(c(-1,1), 1)
      bt <- if (v == -1)
        build_tree(tm, rm, u, v, j, eps, log_prob, grad_log_prob)
      else
        build_tree(tp, rp, u, v, j, eps, log_prob, grad_log_prob)
      
      if (bt$s_prop == 1 && runif(1) < bt$n_prop / n_prop)
        prop <- bt$theta_prop
      
      n_prop      <- n_prop + bt$n_prop
      s_prop      <- bt$s_prop *
        as.integer(sum((tp - tm)*rm) >= 0) *
        as.integer(sum((tp - tm)*rp) >= 0)
      alpha_sum   <- alpha_sum + bt$alpha
      n_alpha_sum <- n_alpha_sum + bt$n_alpha
      
      if (v == -1) {
        tm <- bt$theta_minus; rm <- bt$r_minus
      } else {
        tp <- bt$theta_plus;  rp <- bt$r_plus
      }
      j <- j + 1
    }
    
    acc_rate <- alpha_sum / n_alpha_sum
    if (i <= adapt_steps) {
      H_bar   <- (1 - 1/(i + t0)) * H_bar + (delta - acc_rate)/(i + t0)
      log_eps <- mu - sqrt(i)/gamma * H_bar
      eta     <- i^(-kappa)
      eps_bar <- exp((1 - eta) * log(eps_bar) + eta * log_eps)
      eps     <- exp(log_eps)
    } else {
      eps <- eps_bar
    }
    
    theta         <- prop
    samples[i, ]  <- theta
  }
  
  list(samples = samples, epsilon = eps_hist)
}



# Metropolis Hastings sampler

mh_sampler <- function(log_prob, initial_theta, n_samples, proposal_sd=1) {
  samples <- numeric(n_samples); theta <- initial_theta
  lp_curr <- log_prob(theta)
  for (i in 1:n_samples) {
    prop <- rnorm(1, theta, proposal_sd); lp_prop <- log_prob(prop)
    if (runif(1) < exp(lp_prop-lp_curr)) { theta <- prop; lp_curr <- lp_prop }
    samples[i] <- theta
  }
  samples
}



# Define target and samplers (assume nuts() and mh_sampler() exist)
log_prob <- function(x) {
  lp1 <- dnorm(x, -3, 1, log = TRUE) + log(0.5)
  lp2 <- dnorm(x,  3, 1, log = TRUE) + log(0.5)
  m   <- max(lp1, lp2)
  m + log(exp(lp1 - m) + exp(lp2 - m))
}
grad_log_prob <- function(x) {
  lp1 <- dnorm(x, -3, 1, log = TRUE)
  lp2 <- dnorm(x,  3, 1, log = TRUE)
  p1  <- exp(lp1  - max(lp1, lp2))
  p2  <- exp(lp2  - max(lp1, lp2))
  w1  <- p1 / (p1 + p2)
  w2  <- p2 / (p1 + p2)
  w1 * (-(x + 3)) + w2 * (-(x - 3))
}

set.seed(123)
n_iter      <- 20000
nuts_samps  <- nuts(log_prob, grad_log_prob, theta0 = 0, n_iter = n_iter)
mh_samps    <- mh_sampler(log_prob, initial_theta = 0, n_samples = n_iter, proposal_sd = 1)
res <- nuts_with_eps(log_prob, grad_log_prob,
                     theta0 = 0, n_iter = n_iter)


# True mixture density for overlay
x_grid <- seq(-6, 6, length.out = 1000)
dens_df <- data.frame(
  x = x_grid,
  y = 0.5 * dnorm(x_grid, -3, 1) + 0.5 * dnorm(x_grid, 3, 1)
)

# Trace data frames
trace_nuts <- data.frame(iter = 1:n_iter, value = nuts_samps, sampler = "NUTS")
trace_mh   <- data.frame(iter = 1:n_iter, value = mh_samps,   sampler = "MH")

# Plot 1: NUTS trace
p1 <- ggplot(trace_nuts, aes(x = iter, y = value)) +
  geom_line() +
  labs(title = "NUTS Trace", x = "Iteration", y = "Sample")

# Plot 2: MH trace
p2 <- ggplot(trace_mh, aes(x = iter, y = value)) +
  geom_line() +
  labs(title = "MH Trace", x = "Iteration", y = "Sample")

# Plot 3: NUTS density
p3 <- ggplot(data.frame(sample = nuts_samps), aes(x = sample)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "grey80", color = "black") +
  geom_line(data = dens_df, aes(x = x, y = y), size = 1) +
  xlim(-6, 6) +
  labs(title = "NUTS Density", x = "Value", y = "Density")

# Plot 4: MH density
p4 <- ggplot(data.frame(sample = mh_samps), aes(x = sample)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "grey80", color = "black") +
  geom_line(data = dens_df, aes(x = x, y = y), size = 1) +
  xlim(-6, 6) +
  labs(title = "MH Density", x = "Value", y = "Density")

# Arrange in 2×2 grid
grid.arrange(p1, p2, p3, p4, nrow = 2)




# Autocorrelation plots up to lag 50
acf_nuts <- acf(nuts_samps, plot = FALSE, lag.max = 50)
nuts_acf_df <- data.frame(lag = acf_nuts$lag[-1], acf = acf_nuts$acf[-1])
acf_mh   <- acf(mh_samps,   plot = FALSE, lag.max = 50)
mh_acf_df   <- data.frame(lag = acf_mh$lag[-1],   acf = acf_mh$acf[-1])
p5 <- ggplot(nuts_acf_df, aes(lag, acf)) + geom_bar(stat="identity") + labs(title="NUTS ACF", x="Lag", y="ACF")
p6 <- ggplot(mh_acf_df,   aes(lag, acf)) + geom_bar(stat="identity") + labs(title="MH ACF",   x="Lag", y="ACF")

p5
p6



# Plot ε evolution
df_eps <- data.frame(iter = 1:n_iter, epsilon = res$epsilon)
p_eps_1 <- ggplot(df_eps, aes(x = iter, y = epsilon)) +
  geom_line() +
  labs(
    title = "NUTS Step-Size ε over Iterations",
    x = "Iteration",
    y = expression(epsilon)
  ) +
  theme_minimal()


# # Multivariate example: 2D banana‐shaped distribution
# log_prob_banana <- function(x) {
#   # x is length‐2 vector
#   y1 <- x[1] / 1
#   y2 <- x[2] + 0.03 * (x[1]^2 - 100)
#   -0.5 * (y1^2 + y2^2)
# }
# grad_log_prob_banana <- function(x) {
#   y1 <- x[1]
#   y2 <- x[2] + 0.03 * (x[1]^2 - 100)
#   d1 <- -y1 - 0.06 * x[1] * y2
#   d2 <- -y2
#   c(d1, d2)
# }
# 
# # assume `nuts` is the function you defined above
# set.seed(123)
# samples2d <- nuts(
#   log_prob      = log_prob_banana,
#   grad_log_prob = grad_log_prob_banana,
#   theta0        = c(0, 0),
#   n_iter        = 2000,
#   delta         = 0.65,
#   max_depth     = 8
# )
# 
# # visualize
# par(mfrow = c(1,2))
# plot(samples2d[,1], type="l", main="Trace of x_1")
# plot(samples2d[,2], type="l", main="Trace of x_2")



mh_sampler <- function(log_prob, initial_theta, n_samples, proposal_sd = 1) {
  d       <- length(initial_theta)
  samples <- matrix(NA, n_samples, d)
  theta   <- initial_theta
  lp_curr <- log_prob(theta)
  for (i in 1:n_samples) {
    prop    <- theta + rnorm(d, 0, proposal_sd)
    lp_prop <- log_prob(prop)
    if (runif(1) < exp(lp_prop - lp_curr)) {
      theta   <- prop
      lp_curr <- lp_prop
    }
    samples[i,] <- theta
  }
  samples
}



# Three-peak 2D Gaussian mixture
# weights <- c(0.3, 0.4, 0.3)
# means   <- list(c(-3, -3), c(0, 3), c(3, -1))
# Sigma   <- diag(2); Sigma_inv <- solve(Sigma)
# 
# log_prob_2d <- function(x) {
#   lp <- sapply(1:3, function(i)
#     log(weights[i]) + dmvnorm(x, means[[i]], Sigma, log = TRUE)
#   )
#   m <- max(lp); m + log(sum(exp(lp - m)))
# }
# grad_log_prob_2d <- function(x) {
#   lp   <- sapply(1:3, function(i)
#     log(weights[i]) + dmvnorm(x, means[[i]], Sigma, log = TRUE)
#   )
#   m    <- max(lp)
#   w    <- exp(lp - m); w <- w / sum(w)
#   grads <- sapply(1:3, function(i) -Sigma_inv %*% (x - means[[i]]))
#   as.numeric(grads %*% w)
# }
# 
# # Run samplers
# set.seed(123)
# n_iter     <- 1000
# nuts_samps <- nuts(log_prob_2d, grad_log_prob_2d, theta0 = c(0,0), n_iter = n_iter)
# mh_samps   <- mh_sampler(log_prob_2d, initial_theta = c(0,0), n_samples = n_iter, proposal_sd = 1)
# res <- nuts_with_eps(log_prob_2d, grad_log_prob_2d,
#                      theta0 = c(0,0), n_iter = n_iter)
# # True density grid
# grid_pts <- 100
# x1 <- seq(-6, 6, length = grid_pts)
# x2 <- seq(-6, 6, length = grid_pts)
# gr <- expand.grid(x1, x2)
# z  <- matrix(
#   apply(gr, 1, function(xx)
#     sum(sapply(1:3, function(i)
#       weights[i] * dmvnorm(xx, means[[i]], Sigma)
#     ))
#   ),
#   nrow = grid_pts, byrow = TRUE
# )
# 
# # Visualization
# par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
# plot(nuts_samps[,1], type="l", main="NUTS Trace x₁", xlab="iter", ylab="x₁")
# plot(nuts_samps[,2], type="l", main="NUTS Trace x₂", xlab="iter", ylab="x₂")
# plot(nuts_samps, pch=20, cex=0.5, main="NUTS Samples", xlab="x₁", ylab="x₂")
# contour(x1, x2, z, add=TRUE, drawlabels=FALSE, lwd=2)
# plot(mh_samps[,1], type="l", main="MH Trace x₁", xlab="iter", ylab="x₁")
# plot(mh_samps[,2], type="l", main="MH Trace x₂", xlab="iter", ylab="x₂")
# plot(mh_samps, pch=20, cex=0.5, main="MH Samples", xlab="x₁", ylab="x₂")
# contour(x1, x2, z, add=TRUE, drawlabels=FALSE, lwd=2)
# 
# 





library(ggplot2)
library(mvtnorm)
library(patchwork)

# --- Define 2D three‐peak mixture ---
weights <- c(0.2, 0.3, 0.3)
means   <- list(c(-3, -3), c(0, 3), c(5, -1))
Sigma   <- diag(2); Sigma_inv <- solve(Sigma)

# target log‐density and grad (for NUTS)
log_prob_2d <- function(x) {
  lp <- sapply(1:3, function(i)
    log(weights[i]) + dmvnorm(x, means[[i]], Sigma, log = TRUE))
  m  <- max(lp); m + log(sum(exp(lp - m)))
}
grad_log_prob_2d <- function(x) {
  lp   <- sapply(1:3, function(i)
    log(weights[i]) + dmvnorm(x, means[[i]], Sigma, log = TRUE))
  m    <- max(lp)
  w    <- exp(lp - m); w <- w / sum(w)
  grads <- sapply(1:3, function(i) -Sigma_inv %*% (x - means[[i]]))
  as.numeric(grads %*% w)
}

# --- Run samplers ---
set.seed(123)
n_iter     <- 1000
nuts_samps <- nuts(log_prob_2d, grad_log_prob_2d, theta0 = c(0,0), n_iter = n_iter)
mh_samps   <- mh_sampler(log_prob_2d, initial_theta = c(0,0),
                         n_samples = n_iter, proposal_sd = 1)

# --- Build true density grid for contour ---
grid_pts <- 200
x1 <- seq(-6, 6, length.out = grid_pts)
x2 <- seq(-6, 6, length.out = grid_pts)
grid_df <- expand.grid(x = x1, y = x2)
grid_df$z <- apply(grid_df, 1, function(r)
  sum(sapply(1:3, function(i)
    weights[i] * dmvnorm(c(r["x"], r["y"]), means[[i]], Sigma)
  ))
)

# --- Data frames for plotting ---
nuts_df <- data.frame(iter = 1:n_iter,
                      x1 = nuts_samps[,1],
                      x2 = nuts_samps[,2],
                      sampler = "NUTS")
mh_df   <- data.frame(iter = 1:n_iter,
                      x1 = mh_samps[,1],
                      x2 = mh_samps[,2],
                      sampler = "MH")

# --- NUTS trace x1 ---
p1 <- ggplot(nuts_df, aes(x = iter, y = x1)) +
  geom_line() +
  labs(title = "NUTS trace x1", x = "iteration", y = expression(x[1])) +
  theme_minimal()

# --- NUTS trace x2 ---
p2 <- ggplot(nuts_df, aes(x = iter, y = x2)) +
  geom_line() +
  labs(title = "NUTS trace x2", x = "iteration", y = expression(x[2])) +
  theme_minimal()

# --- NUTS scatter + contour ---
p3 <- ggplot() +
  geom_point(data = nuts_df, aes(x = x1, y = x2), alpha = 0.3, size = 0.5) +
  geom_contour(data = grid_df, aes(x = x, y = y, z = z),
               color = "black", bins = 10) +
  labs(title = "NUTS samples", x = expression(x[1]), y = expression(x[2])) +
  coord_fixed() +
  theme_minimal()

# --- MH trace x1 ---
p4 <- ggplot(mh_df, aes(x = iter, y = x1)) +
  geom_line() +
  labs(title = "MH trace x1", x = "iteration", y = expression(x[1])) +
  theme_minimal()

# --- MH trace x2 ---
p5 <- ggplot(mh_df, aes(x = iter, y = x2)) +
  geom_line() +
  labs(title = "MH trace x2", x = "iteration", y = expression(x[2])) +
  theme_minimal()

# --- MH scatter + contour ---
p6 <- ggplot() +
  geom_point(data = mh_df, aes(x = x1, y = x2), alpha = 0.3, size = 0.5) +
  geom_contour(data = grid_df, aes(x = x, y = y, z = z),
               color = "black", bins = 10) +
  labs(title = "MH samples", x = expression(x[1]), y = expression(x[2])) +
  coord_fixed() +
  theme_minimal()

# --- Arrange in 2×3 grid ---
(p1 | p2 | p3) / (p4 | p5 | p6) +
  plot_layout(
    widths  = c(1, 1, 3),  # make the 3rd column twice as wide
    heights = c(1, 1)      # keep both rows the same height
  )

lag_max <- 50
acf_nuts  <- acf(nuts_samps[,1],  plot = FALSE, lag.max = lag_max)$acf[-1]
acf_mh    <- acf(mh_samps[,1],     plot = FALSE, lag.max = lag_max)$acf[-1]


df <- data.frame(
  lag = rep(1:lag_max, 2),
  acf = c(acf_nuts, acf_mh),
  algorithm = factor(rep(c("MH", "NUTS"),
                         each = lag_max),
                     levels = c("NUTS","MH"))
)



# --- Plot ---
acf_1 <- ggplot(df, aes(x = lag, y = acf, color = algorithm)) +
  geom_line(size = 1) +
  labs(
    title = "Autocorrelation of x1 for Different Samplers",
    x = "Lag",
    y = "ACF",
    color = "Sampler"
  ) +
  theme_minimal()





acf_nuts  <- acf(nuts_samps[,2],  plot = FALSE, lag.max = lag_max)$acf[-1]
acf_mh    <- acf(mh_samps[,2],     plot = FALSE, lag.max = lag_max)$acf[-1]

df <- data.frame(
  lag = rep(1:lag_max, 2),
  acf = c(acf_nuts, acf_mh),
  algorithm = factor(rep(c("NUTS", "MH"),
                         each = lag_max),
                     levels = c("NUTS","MH"))
)
# --- Plot ---
acf_2 <- ggplot(df, aes(x = lag, y = acf, color = algorithm)) +
  geom_line(size = 1) +
  labs(
    title = "Autocorrelation of x2 for Different Samplers",
    x = "Lag",
    y = "ACF",
    color = "Sampler"
  ) +
  theme_minimal()

grid.arrange(acf_1, acf_2,  nrow = 2)



# --- 2D three-peak mixture definitions ---
weights <- c(0.3,0.4,0.3)
means   <- list(c(-3,-3), c(0,3), c(3,-1))
Sigma   <- diag(2); Sigma_inv <- solve(Sigma)

log_prob_2d <- function(x) {
  lp <- sapply(1:3, function(i)
    log(weights[i]) + dmvnorm(x, means[[i]], Sigma, log = TRUE))
  m <- max(lp); m + log(sum(exp(lp-m)))
}
grad_log_prob_2d <- function(x) {
  lp   <- sapply(1:3, function(i)
    log(weights[i]) + dmvnorm(x, means[[i]], Sigma, log = TRUE))
  m    <- max(lp)
  w    <- exp(lp - m); w <- w/sum(w)
  grads <- sapply(1:3, function(i) -Sigma_inv %*% (x - means[[i]]))
  as.numeric(grads %*% w)
}

# --- your NUTS & MH definitions (assume `nuts()` and `mh_sampler()` already in scope) ---

set.seed(42)
n_iter     <- 2000
nuts_samps <- nuts(log_prob_2d, grad_log_prob_2d, theta0=c(0,0), n_iter=n_iter)
mh_samps   <- mh_sampler(log_prob_2d, initial_theta=c(0,0), n_samples=n_iter, proposal_sd=1)



# Mixture parameters
weights <- c(0.3, 0.4, 0.3)
means   <- list(c(-3,  3), c(0,  3), c(3, -1))
Sigma   <- diag(2)

# Build grid for heatmap
grid_pts <- 100
x <- seq(-6, 6, length.out = grid_pts)
y <- seq(-6, 6, length.out = grid_pts)
heat_df <- expand.grid(x = x, y = y)
heat_df$z <- apply(heat_df, 1, function(r) {
  xi <- r["x"]; yj <- r["y"]
  sum(sapply(1:3, function(i)
    weights[i] * dmvnorm(c(xi, yj), mean = means[[i]], sigma = Sigma)
  ))
})

# Run samplers (assumes nuts() and mh_sampler() are defined)
set.seed(123)
n_iter     <- 100
nuts_samps <- nuts(log_prob_2d, grad_log_prob_2d, theta0 = c(0,0), n_iter = n_iter)
mh_samps   <- mh_sampler(log_prob_2d, initial_theta = c(0,0), n_samples = n_iter, proposal_sd = 1)

# Prepare sample data frames
nuts_df <- data.frame(x = nuts_samps[,1], y = nuts_samps[,2], sampler = "NUTS")
mh_df   <- data.frame(x = mh_samps[,1],   y = mh_samps[,2],   sampler = "MH")
samps_df <- rbind(nuts_df, mh_df)

# Plot
plot_1 <- ggplot() +
  geom_raster(data = heat_df, aes(x = x, y = y, fill = z), interpolate = TRUE) +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  geom_point(data = samps_df, aes(x = x, y = y, shape = sampler, color = sampler),
             size = 1.2, alpha = 0.6) +
  coord_fixed(xlim = c(-6,6), ylim = c(-6,6)) +
  labs(
    title = "NUTS vs MH Samples over 3-Peak Mixture Density [(-3,3),(0,3),(3,-1)]",
    subtitle = paste("Total iterations:", n_iter),
    x = expression(x[1]), y = expression(x[2])
  ) +
  theme_minimal()


# Run samplers (assumes nuts() and mh_sampler() are defined)
set.seed(123)
n_iter     <- 500
nuts_samps <- nuts(log_prob_2d, grad_log_prob_2d, theta0 = c(0,0), n_iter = n_iter)
mh_samps   <- mh_sampler(log_prob_2d, initial_theta = c(0,0), n_samples = n_iter, proposal_sd = 1)

# Prepare sample data frames
nuts_df <- data.frame(x = nuts_samps[,1], y = nuts_samps[,2], sampler = "NUTS")
mh_df   <- data.frame(x = mh_samps[,1],   y = mh_samps[,2],   sampler = "MH")
samps_df <- rbind(nuts_df, mh_df)

# Plot
plot_2 <- ggplot() +
  geom_raster(data = heat_df, aes(x = x, y = y, fill = z), interpolate = TRUE) +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  geom_point(data = samps_df, aes(x = x, y = y, shape = sampler, color = sampler),
             size = 1.2, alpha = 0.6) +
  coord_fixed(xlim = c(-6,6), ylim = c(-6,6)) +
  labs(
    title = "NUTS vs MH Samples over 3-Peak Mixture Density [(-3,3),(0,3),(3,-1)]",
    subtitle = paste("Total iterations:", n_iter),
    x = expression(x[1]), y = expression(x[2])
  ) +
  theme_minimal()

grid.arrange(plot_1,plot_2, nrow = 1)



# Banana‐shaped 2D density
b  <- 0.1
c0 <- 20
log_prob_banana <- function(x) {
  y1 <- x[1]
  y2 <- x[2] + b*(x[1]^2 - c0)
  -0.5*(y1^2 + y2^2)
}
grad_log_prob_banana <- function(x) {
  y1 <- x[1]
  y2 <- x[2] + b*(x[1]^2 - c0)
  d1 <- -y1 - 2*b*x[1]*y2
  d2 <- -y2
  c(d1, d2)
}

# assume nuts() and mh_sampler() are defined as before
set.seed(42)
n_iter     <- 100
nuts_samps <- nuts(log_prob_banana, grad_log_prob_banana, theta0 = c(0,0), n_iter = n_iter)
mh_samps   <- mh_sampler(log_prob_banana, initial_theta = c(0,0), n_samples = n_iter, proposal_sd = 1)

# build heatmap grid
grid_pts <- 100
x <- seq(-6, 6, length.out = grid_pts)
y <- seq(-6, 6, length.out = grid_pts)
heat_df <- expand.grid(x = x, y = y)
heat_df$z <- exp(apply(heat_df, 1, function(r) log_prob_banana(c(r["x"], r["y"]))))

# prepare samples with sampler label
nuts_df <- data.frame(x = nuts_samps[,1], y = nuts_samps[,2], sampler = "NUTS")
mh_df   <- data.frame(x = mh_samps[,1],   y = mh_samps[,2],   sampler = "MH")
samps_df <- rbind(nuts_df, mh_df)

# plot heatmap + samples colored by sampler only
ggplot() +
  geom_raster(data = heat_df, aes(x = x, y = y, fill = z), interpolate = TRUE) +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  geom_point(data = samps_df,
             aes(x = x, y = y, color = sampler, shape = sampler),
             size = 1.2, alpha = 0.7) +
  coord_fixed(xlim = c(-6,6), ylim = c(-6,6)) +
  labs(
    title = "NUTS vs MH Samples on Banana-shaped Density",
    subtitle = paste("Total iterations:", n_iter),
    x     = expression(x[1]),
    y     = expression(x[2])
  ) +
  theme_minimal()

