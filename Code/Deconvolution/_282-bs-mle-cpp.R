## --- Load libraries and setup ---
library(splines)
library(statmod)
library(parallel)
library(Rcpp)
library(RcppArmadillo)

set.seed(557788)

## --- Simulate data ---
n_obs <- 2000
eps <- rnorm(n_obs, mean = 0, sd = 0.35)
e <- rlnorm(n_obs, meanlog = -1.8, sdlog = 0.95)
V <- -eps + e # eps = e - V

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

## --- Rcpp implementation for adaptive integration ---
Rcpp::cppFunction(
  depends = "RcppArmadillo",
  code = '
  double adaptive_integrate_cpp(const Rcpp::Function& f, double a, double b, 
                                 const arma::vec& nodes, const arma::vec& weights, int depth = 0) {
    double m = (a + b) / 2.0;
    if (depth >= 3) {
      arma::vec transformed_nodes = (nodes + 1) * (b - a) / 2.0 + a;
      arma::vec f_values = Rcpp::as<arma::vec>(f(transformed_nodes));
      return arma::dot(f_values, weights) * (b - a) / 2.0;
    } else {
      return adaptive_integrate_cpp(f, a, m, nodes, weights, depth + 1) +
             adaptive_integrate_cpp(f, m, b, nodes, weights, 3);
    }
  }
  '
)

## --- Normalization constant for density ---
C_recursive <- function(theta, params) {
  f <- function(e) exp(s(e, theta, params$bspline))
  adaptive_integrate_cpp(f, params$a, params$b, params$gl$nodes, params$gl$weights)
}

## --- Density function f_e(e; θ) ---
f_e <- function(e, theta, params) {
  numerator <- exp(s(e, theta, params$bspline))
  denom <- C_recursive(theta, params)
  return(numerator / denom)
}

## --- Penalized log-likelihood ---
log_likelihood_pen <- function(theta, V, params, lambda = 0.1) {
  gl <- params$gl
  bspline <- params$bspline

  ll_vec <- mclapply(V, function(v_i) {
    integrand <- function(e) {
      x <- e - v_i 
      dnorm(x, mean = 0, sd = params$epsilon_sd) * exp(s(e, theta, bspline))
    }
    val <- adaptive_integrate_cpp(integrand, params$a, params$b, gl$nodes, gl$weights)
    log(val)
  },
  mc.cores = detectCores() - 2)
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
estimate_theta <- function(V, bspline, gl, lambda = 0.1) {
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

## --- Plot estimated density f_e ---
plot(x_domain, f_e(x_domain, result$theta, result$params),
     type = "l", lwd = 2, col = "blue", ylab = "Density", xlab = "e",
     main = "Estimated f_e vs True e")
lines(density(e), col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c("Estimated f_e", "True e"), col = c("blue", "darkgreen"), lty = c(1, 2), lwd = 2)


## %% Choose lambda ---------------------------
lambda_values <- seq(0.0, 0.01, length.out = 5)
lambda_values <- c(0.0, 0.01, 0.10, 1)
mclapply(
  lambda_values,
  function(lambda) {
    result <- estimate_theta(V, bs_mat, gl, lambda)
    return(
      list(
        lambda = lambda,
        result = result)
  },
  mc.cores = detectCores() - 2
) -> lambda_list

bic_values <- sapply(lambda_list, function(x) x$result$BIC)

png("Code/Products/bic_plot.png", width = 800, height = 600)
plot(lambda_values, bic_values, type = "b", pch = 19,
     xlab = "Lambda", ylab = "BIC",
     main = "BIC vs Lambda")
dev.off()