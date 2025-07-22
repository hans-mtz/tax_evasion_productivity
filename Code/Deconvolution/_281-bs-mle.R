## %% --- Load libraries and setup ---
library(splines)
library(statmod)
library(parallel)

set.seed(557788)

## --- Simulate data ---
n_obs <- 2000
eps <- rnorm(n_obs, mean = 0, sd = 0.35)
e <- rlnorm(n_obs, meanlog = -1.8, sdlog = 0.95)
V <- -eps + e # eps = e - V

png("Code/Products/bs_mle_hist.png", width = 800, height = 600)
plot(c(-1,2), c(0, max(max(density(V)$y),max(density(e)$y))), type = "n", xlab = "", ylab = "Probability")
hist(V, breaks = 200, probability = TRUE, col = "#FFA500", border = "white", add = TRUE)
lines(density(e), col = "blue", lwd = 2, lty = 1)
lines(density(eps), col = "black", lwd = 2, lty =2)
legend("topright", legend = c("V", "e", "eps"), col = c("#FFA500", "blue", "black"), lty = c(1, 1, 2), lwd = 2)
dev.off()
## --- Set spline parameters ---
n_knots <- 10
x_domain <- seq(0, range(V)[2], length.out = 500)
x <- seq(0, range(V)[2], length.out = 500)
## --- Build quantile-based knots on support of e: [0, ∞) ---
knot_candidates <- quantile(V[V >= 0], probs = seq(0.01, 0.99, length.out = n_knots))
boundary_knots <- c(0, max(V[V >= 0])+0.1)

## --- Construct B-spline basis on x_domain ---
bs_mat <- bs(
  x_domain,
  knots = knot_candidates[-c(1, length(knot_candidates))],
  degree = 3,
  intercept = FALSE,
  Boundary.knots = boundary_knots
)

## --- Second difference matrix for roughness penalty ---
build_D <- function(k) {
  D <- matrix(0, nrow = k - 2, ncol = k)
  for (i in 1:(k - 2)) {
    D[i, i:(i + 2)] <- c(1, -2, 1)
  }
  return(D)
}

build_D_order <- function(k,order=2){
  D <- diag(k)
  D <- diff(D, differences = order)
}

## --- Define spline function: s(e; θ) ---
s <- function(e, theta, bspline_spec) {
  theta0 <- c(theta, 0) #Drop last B-Spline for identifiability
  basis <- bs(
    e,
    knots = bspline_spec$knots,
    intercept = bspline_spec$intercept,
    Boundary.knots = bspline_spec$Boundary.knots,
    degree = 3,
    warn.outside = FALSE
  )
  if (length(theta0) != ncol(basis)) stop(
    "Length of theta0 does not match number of basis functions. \n
    You have this many basis functions: ",
    ncol(basis), "\n",
    "You have this many coefficients: ", length(theta0), "\n",
  )
  return(as.vector(basis %*% theta0))
}

## --- Gauss-Legendre adaptive integration ---
lcv <- function(x, a, b) (x + 1) * (b - a) / 2 + a

adaptive_integrate <- function(f, a, b, gl, depth = 0) {
  m <- (a + b) / 2
  if (depth >= 3) {
    nodes <- lcv(gl$nodes, a, b)
    weights <- gl$weights * (b - a) / 2
    return(sum(f(nodes) * weights))
  } else {
    return(
      adaptive_integrate(f, a, m, gl, depth + 1) +
      adaptive_integrate(f, m, b, gl, 3)
    )
  }
}

## --- Normalization constant for density ---
C_recursive <- function(theta, params) {
  f <- function(e) exp(s(e, theta, params$bspline))
  adaptive_integrate(f, params$a, params$b, params$gl)
}

## --- Density function f_e(e; θ) ---
f_e <- function(e, theta, params) {
  numerator <- exp(s(e, theta, params$bspline))
  denom <- C_recursive(theta, params)
  return(numerator / denom)
}

## --- Penalized log-likelihood ---
log_likelihood_pen <- function(theta, V, params, lambda = 0.1, parallel = TRUE) {
  gl <- params$gl
  bspline <- params$bspline
  # ll_vec <- sapply(V, function(v_i) {
  #   integrand <- function(e) {
  #     # x <- v_i - e
  #     x <- e - v_i 
  #     dnorm(x, mean = 0, sd = params$epsilon_sd) * exp(s(e, theta, bspline))
  #   }
  #   val <- adaptive_integrate(integrand, params$a, params$b, gl)
  #   log(val)
  # })

  mc_cores <- ifelse(parallel, detectCores() - 2, 1)

  ll_vec <- mclapply(V, function(v_i) {
    integrand <- function(e) {
      # x <- v_i - e
      x <- e - v_i 
      dnorm(x, mean = 0, sd = params$epsilon_sd) * exp(s(e, theta, bspline))
    }
    val <- adaptive_integrate(integrand, params$a, params$b, gl)
    log(val)
    },
    mc.cores = mc_cores
  )
  ll_vec <- unlist(ll_vec)

  D <- build_D(length(theta))
  penalty <- lambda * sum((D %*% theta)^2)

  return(sum(ll_vec) - length(V) * log(C_recursive(theta, params)) - penalty)
}


## --- Initialize θ via regression on log-density of V ---
initialize_theta <- function(V, bspline_spec) {
  d <- density(V[V >= 0], n = 512)
  d$y <- d$y + 1e-5  # avoid log(0)
  basis <- bs(
    d$x,
    knots = bspline_spec$knots,
    intercept = bspline_spec$intercept,
    Boundary.knots = bspline_spec$Boundary.knots,
    degree = 3
  )
  fit <- lm(log(d$y) ~ basis - 1)
  return(coef(fit)[-ncol(basis)]) # Drop last B-Spline for identifiability
}


## --- Optimization wrapper ---
estimate_theta <- function(V, bspline, gl, lambda = 0.1, parallel = TRUE) {
  bspline_spec <- attributes(bspline)
  theta0 <- initialize_theta(V, bspline_spec)
  params <- list(
    a = boundary_knots[1],
    b = boundary_knots[2],
    bspline = bspline_spec,
    gl = gl,
    epsilon_sd = 0.35
  )

  opt <- optim(
    par = theta0,
    fn = log_likelihood_pen,
    V = V,
    params = params,
    lambda = lambda,
    parallel = parallel,
    method = "BFGS",
    control = list(fnscale = -1, maxit = 200, REPORT = 20, trace=3)
  )
 
  llh <- log_likelihood_pen(
    theta = opt$par,
    V = V,
    params = params,
    lambda = 0
  )

  BIC <- -2 * llh + log(length(V)) * length(opt$par-1)
  
  return(
    list(
      theta = opt$par,
      params = params,
      opt = opt,
      llh = llh,
      BIC = BIC
    )
  )
}

## --- Run estimation ---
gl <- gauss.quad(15, "legendre")
result <- estimate_theta(V, bs_mat, gl, lambda = 0.1)
result_l100 <- estimate_theta(V, bs_mat, gl, lambda = 100)

## %% saving results ---------------------------
save(result, file = "Code/Products/bs_mle.RData")
load("Code/Products/bs_mle.RData")
## --- Plot estimated density f_e ---
png("Code/Products/bs_mle.png", width = 800, height = 600)
plot(c(0, 2), c(0, max(max(f_e(x_domain, result$theta, result$params)),max(density(e)$y))), type = "n", xlab = "", ylab = "Probability")
# hist(e, breaks = 100, probability = TRUE, col = "gray", border = "white")
lines(x_domain, f_e(x_domain, result$theta, result$params),
     type = "l", lwd = 2, col = "darkgreen", ylab = "Density", xlab = "e",
     main = "Estimated f_e vs True e")
# lines(x_domain, f_e(x_domain, result_l100$theta, result_l100$params),
#      col = "red", lwd = 2)
lines(density(e), col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Estimated f_e", "True e"), col = c("darkgreen", "blue"), lty = c(1, 2), lwd = 2)
dev.off()

## %% Choose lambda ---------------------------
# lambda_values <- seq(0.0, 0.01, length.out = 5)
lambda_values <- c(0.001, 0.01, 0.05, 0.10, 0.5, 1, 10)

mclapply(
  lambda_values,
  function(lambda) {
    result <- estimate_theta(V, bs_mat, gl, lambda, parallel = FALSE)
    return(
      list(
        lambda = lambda,
        result = result
      )
    )
  },
  mc.cores = detectCores() - 2
) -> lambda_list

bic_values <- sapply(lambda_list, function(x) x$result$BIC)
aic_values <- sapply(lambda_list, function(x) -2*x$result$llh+2*length(x$result$theta))
## %% Save results ---------------------------
save(lambda_list, bic_values, lambda_values, file = "Code/Products/lambda_list.RData")
load("Code/Products/lambda_list.RData")
## %% Plot BIC vs lambda ---------------------------

png("Code/Products/bic_plot.png", width = 800, height = 600)
plot(lambda_values, bic_values, type = "b", pch = 19,
     xlab = "Lambda", ylab = "BIC",
     main = "BIC vs Lambda", log = "x")
dev.off()

plot(lambda_values, aic_values, type = "b", pch = 19,
     xlab = "Lambda", ylab = "BIC",
     main = "BIC vs Lambda", log = "x")

hist(e, breaks = 200, probability = TRUE, col = "lightgray", border = "white")
lines(x_domain, f_e(x_domain, lambda_list[[1]]$result$theta, lambda_list[[1]]$result$params),
     type = "l", lwd = 2, col = "blue", ylab = "Density", xlab = "e",
     main = "Estimated f_e vs True e")
lines(x_domain, f_e(x_domain, lambda_list[[3]]$result$theta, lambda_list[[4]]$result$params),
     col = "purple", lwd = 2)
lines(x_domain, f_e(x_domain, lambda_list[[5]]$result$theta, lambda_list[[2]]$result$params),
     col = "red", lwd = 2)
lines(x_domain, f_e(x_domain, lambda_list[[7]]$result$theta, lambda_list[[3]]$result$params),
     col = "green", lwd = 2)
legend("topright", legend = c("lambda 0.001","lambda 0.05", "lambda 0.5","lambda 10","True e"), 
  col = c("blue", "purple","red", "green","lightgray"), lty = 1, lwd = 2)

