## %% --- Load libraries and setup ---
library(splines)
library(statmod)
library(parallel)

# load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/fs.RData")
# load("Code/Products/np-deconv-funs.RData")

set.seed(557788)
pspline_degree <- 3
n_knots <- 10
gl <- gauss.quad(15, "legendre")
lambda <- 10
# Choose industries to deconvolve ---------------------

# select_fs_l <- paste0(top_5_ev_inds, " log_mats_share")
select_fs_l <- grep("log_mats",names(fs_list), value = TRUE) # Get all industries with log_mats_share


## %% --- Second difference matrix for roughness penalty ---
build_D <- function(k) {
  D <- matrix(0, nrow = k - 2, ncol = k)
  for (i in 1:(k - 2)) {
    D[i, i:(i + 2)] <- c(1, -2, 1)
  }
  return(D)
}

build_D_order <- function(k,order=pspline_degree){
  D <- diag(k)
  D <- diff(D, differences = order)
}

get_bspline_spec <- function(V, n_knots = n_knots, spline_degree = pspline_degree) {
  knot_candidates <- quantile(V[V >= 0], probs = seq(0.01, 0.99, length.out = n_knots))
  boundary_knots <- c(0, max(V[V >= 0]) + 0.1)
  bspline_spec <- list(
    knots = knot_candidates[-c(1, length(knot_candidates))],
    intercept = FALSE,
    degree = pspline_degree,
    Boundary.knots = boundary_knots
  )
  return(bspline_spec)
}


## %% --- Define spline function: s(e; θ) ---
s <- function(e, theta, bspline_spec) {
  theta0 <- c(theta, 0) #Drop last B-Spline for identifiability
  basis <- bs(
    e,
    knots = bspline_spec$knots,
    intercept = bspline_spec$intercept,
    Boundary.knots = bspline_spec$Boundary.knots,
    degree = bspline_spec$degree,
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

## %% --- Normalization constant for density ---
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

## %% --- Penalized log-likelihood ---
log_likelihood_pen <- function(theta, V, params, lambda = lambda, parallel = TRUE) {
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
      dnorm(x, mean = params$epsilon_mean, sd = params$epsilon_sd) * exp(s(e, theta, bspline))
    }
    val <- adaptive_integrate(integrand, params$a, params$b, gl)
    log(val)
    },
    mc.cores = mc_cores
  )
  ll_vec <- unlist(ll_vec)

  D <- build_D_order(length(theta), order = pspline_degree)
  penalty <- lambda * sum((D %*% theta)^2)

  return(sum(ll_vec) - length(V) * log(C_recursive(theta, params)) - penalty)
}


## %% --- Initialize θ via regression on log-density of V ---
initialize_theta <- function(V, bspline_spec) {
  d <- density(V[V >= 0], n = 512)
  d$y <- d$y + 1e-5  # avoid log(0)
  basis <- bs(
    d$x,
    knots = bspline_spec$knots,
    intercept = bspline_spec$intercept,
    Boundary.knots = bspline_spec$Boundary.knots,
    degree = bspline_spec$degree,
    warn.outside = FALSE
  )
  fit <- lm(log(d$y) ~ basis - 1)
  return(coef(fit)[-ncol(basis)]) # Drop last B-Spline for identifiability
}


## --- Optimization wrapper ---
estimate_theta <- function(fs_list, gl, lambda = lambda, parallel = TRUE) {
  V <- fs_list$data$cal_V

  bspline_spec <- get_bspline_spec(V, n_knots = n_knots, spline_degree = pspline_degree)
  
  theta0 <- initialize_theta(V, bspline_spec)
  params <- list(
    a = bspline_spec$Boundary.knots[1],
    b = bspline_spec$Boundary.knots[2],
    bspline = bspline_spec,
    gl = gl,
    epsilon_mean = fs_list$epsilon_mu,
    epsilon_sd = fs_list$epsilon_sigma
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
 
  if (opt$convergence != 0) {
    cat("Optimization did not converge. Check the results.\n", 
    "Industry: ",fs_list$sic_3,"Intermediate: ", fs_list$inter,"\n")
  }

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

## %% --- Run estimation ---

# result <- estimate_theta(fs_list[[5]], gl, lambda = lambda, parallel = FALSE)


## %% run all inds ---------------------

# sp_deconv_list <- mclapply(
#   select_fs_l,
#   function(i) {
#     cat("Estimating industry ", i, "\n")
#     estimate_theta(fs_list[[i]], gl, lambda = lambda, parallel = FALSE)
#   },
#   mc.cores = detectCores() - 2
# )
# names(sp_deconv_list) <- select_fs_l

## %% Save results ---------------------

# save(
#   sp_deconv_list, #f_e, adaptive_integrate,
#   file = "Code/Products/bs_mle_data.RData"
# )
load("Code/Products/bs_mle_data.RData")
## %% Plot results ---------------------

# x <- seq(result$params$a, result$params$b, length.out = 1000)
# plot(x, f_e(x, result$theta, result$params), type = "l", col = "blue", lwd = 2,
#      xlab = "e", ylab = "Density", main = "Density of e")

### %% Plot results for all industries ---------------------
# load("Code/Products/bs_mle_data.RData")
par(mfrow = c(3, 2))
lapply(
  seq_along(sp_deconv_list),
  function(i) {
    x <- seq(sp_deconv_list[[i]]$params$a, sp_deconv_list[[i]]$params$b, length.out = 1000)
    plot(x, f_e(x, sp_deconv_list[[i]]$theta, sp_deconv_list[[i]]$params), type = "l", col = "blue", lwd = 2,
         xlab = "e", ylab = "Density", main = paste("Density of e for industry", select_fs_l[i]))
  }
)


## %% Get statistics from distributions --------------------- 

get_stats <- function(theta, params) {
  f <- function(e) f_e(e, theta, params)
  mean_e <- adaptive_integrate(function(e) e * f(e), params$a, params$b, params$gl)
  var_e <- adaptive_integrate(function(e) (e - mean_e)^2 * f(e), params$a, params$b, params$gl)
  skweness_e <- adaptive_integrate(function(e) ((e - mean_e)^3) * f(e), params$a, params$b, params$gl) / (var_e^(3/2))
  return(c(mean = mean_e, sd = sqrt(var_e), skewness = skweness_e))
}

get_stats(sp_deconv_list[[1]]$theta, sp_deconv_list[[1]]$params)

stats_list <- lapply(
  seq_along(sp_deconv_list),
  function(i) {
    get_stats(sp_deconv_list[[i]]$theta, sp_deconv_list[[i]]$params)
  }
)
stats_df <- do.call(rbind, stats_list)
stats_df

get_stats.list <- function(results_list) {
  temp_list <- lapply(
    seq_along(results_list),
    function(i) {
      get_stats(results_list[[i]]$theta, results_list[[i]]$params)
    }
  )
  temp_df <- do.call(rbind, temp_list)
  row.names(temp_df) <- names(results_list)
  temp_df <- as.data.frame(temp_df)
  return(temp_df)
}
stats_df <- get_stats.list(sp_deconv_list)
## %% Save Results ---------------------

save(
  sp_deconv_list, stats_df,
  file = "Code/Products/bs_mle_data.RData"
)

## %% --- Non-parametric deconvolution ----------------------

# load("Code/Products/bs_mle_data.RData")

## %% NP error distribution ----------------------

np_pdf <- function(list) {
  # Get the density of epsilon from the first industry in the list
  eps_density <- list$data$epsilon |> na.omit() |> 
    density(bw="SJ-dpi")
  
  # Create a function for the PDF
  eps_pdf <- approxfun(eps_density$x, eps_density$y, rule =2)
  
  return(eps_pdf)
}

np_pdf(fs_list[[1]])(seq(-3, 3, length.out = 100))

## %% Get all epsilon PDFs for selected industries ---------------------

eps_pdf_list <-lapply(
  select_fs_l,
  \(x) return(eps_pdf = np_pdf(fs_list[[x]]))
)
names(eps_pdf_list) <- select_fs_l

## %% Full NP Deconvolution Function ---------------------

llh_np <- function(theta, V, params, lambda = lambda, parallel = TRUE) {
  gl <- params$gl
  bspline <- params$bspline
  eps_pdf <- params$eps_pdf
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
      eps_pdf(x) * exp(s(e, theta, bspline))
    }
    val <- adaptive_integrate(integrand, params$a, params$b, gl)
    val <- ifelse(val <=0,-5e7,log(val)) # Avoid log(0)
    val
    },
    mc.cores = mc_cores
  )
  ll_vec <- unlist(ll_vec)
  # cat("ll_vec :", ll_vec, "\n")

  D <- build_D_order(length(theta), order = pspline_degree)
  penalty <- lambda * sum((D %*% theta)^2)

  return(sum(ll_vec) - length(V) * log(C_recursive(theta, params)) - penalty)
}

estimate_np_theta <- function(fs_list, eps_pdf_list, gl, lambda = lambda, parallel = TRUE) {
  V <- fs_list$data$cal_V

  bspline_spec <- get_bspline_spec(V, n_knots = n_knots, spline_degree = pspline_degree)
  
  theta0 <- initialize_theta(V, bspline_spec)
  cat("Initial theta0: ", theta0, "\n")
  params <- list(
    a = bspline_spec$Boundary.knots[1],
    b = bspline_spec$Boundary.knots[2],
    bspline = bspline_spec,
    gl = gl,
    epsilon_mean = fs_list$epsilon_mu,
    epsilon_sd = fs_list$epsilon_sigma,
    eps_pdf = eps_pdf_list

  )

  opt <- optim(
    par = theta0,
    fn = llh_np,
    V = V,
    params = params,
    lambda = lambda,
    parallel = parallel,
    method = "BFGS",
    control = list(fnscale = -1, maxit = 200, REPORT = 20, trace=3)
  )
 
  if (opt$convergence != 0) {
    cat("Optimization did not converge. Check the results.\n", 
    "Industry: ",fs_list$sic_3,"Intermediate: ", fs_list$inter,"\n")
  }

  llh <- llh_np(
    theta = opt$par,
    V = V,
    params = params,
    lambda = 0,
    parallel = parallel
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

## %% Run estimation for all industries ---------------------

estimate_np_theta(
  fs_list[[1]], eps_pdf_list[[1]], gl,
  lambda = lambda, parallel = TRUE
)

# V <- fs_list[[1]]$data$cal_V

# bspline_spec <- get_bspline_spec(V, n_knots = n_knots, spline_degree = pspline_degree)

# llh_np(
#   theta = c(0.6701048, 0.251192, -0.07530958, 0.3961341, 0.2114183, 0.2602096, 0.2424873, 0.03029904, -0.03350309, -1.483456),
#   V = fs_list[[1]]$data$cal_V,
#   params = list(
#     a = bspline_spec$Boundary.knots[1],
#     b = bspline_spec$Boundary.knots[2],
#     bspline = bspline_spec,
#     gl = gl,
#     epsilon_mean = fs_list[[1]]$epsilon_mu,
#     epsilon_sd = fs_list[[1]]$epsilon_sigma,
#     eps_pdf = eps_pdf_list[[1]]

#   ),
#   lambda = lambda,
#   parallel = TRUE
# )
## %% run all inds ---------------------

full_np_deconv_list <- mclapply(
  select_fs_l,
  function(i) {
    cat("Estimating industry ", i, "\n")
    estimate_np_theta(fs_list[[i]], eps_pdf_list[[i]], gl, lambda = lambda, parallel = FALSE)
  },
  mc.cores = detectCores() - 2
)
names(full_np_deconv_list) <- select_fs_l

## %% Save results ---------------------


save(
  sp_deconv_list, full_np_deconv_list, 
  stats_df,eps_pdf_list, #np_stats_df,
  file = "Code/Products/bs_mle_data.RData"
)

## %% Get statistics from distributions ---------------------

np_stats_df <- get_stats.list(full_np_deconv_list)
np_stats_df
## %% Save results ---------------------

save(
  sp_deconv_list, full_np_deconv_list, 
  stats_df,eps_pdf_list, np_stats_df,
  file = "Code/Products/bs_mle_data.RData"
)

load("Code/Products/bs_mle_data.RData")
