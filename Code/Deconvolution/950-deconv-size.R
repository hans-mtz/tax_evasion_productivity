## %% Set up packages and data ---------------------

library(tidyverse)
library(parallel)

# load("Code/Products/np-deconv-funs.RData")
load("Code/Products/930-boot-se-het.RData") #Load data
source("Code/Deconvolution/030-np-deconv-funs.R")

## %% 

eps_pdf_list <- np_pdf.list(list(data=df))


llh_sp_dec <- function(theta, model_matrix, params, lambda = lambda, parallel = TRUE) {
  gl <- params$gl
  bspline <- params$bspline
  eps_pdf <- params$eps_pdf

  mc_cores <- ifelse(parallel, detectCores() - 2, 1)

#   tmp_data <- data %>% filter(
#     !is.na(rev_ntile),
#     !is.na(cal_V),
#     corp == "Other")



  eta <- model_matrix %*% c(theta[1:10], 1)

  ll_vec <- mclapply(eta, function(v_i) {
    integrand <- function(e) {
      x <- e - v_i 
      eps_pdf(x) * exp(s(e, theta[11:length(theta)], bspline))
    }
    val <- adaptive_integrate(integrand, params$a, params$b, gl)
    val <- ifelse(val <=0,-5e7,log(val)) # Avoid log(0)
    val
    },
    mc.cores = mc_cores
  )
  ll_vec <- unlist(ll_vec)
  # cat("ll_vec :", ll_vec, "\n")

  D <- build_D_order(length(theta[11:length(theta)]), order = bspline$degree)
  penalty <- lambda * sum((D %*% theta[11:length(theta)])^2)

  return(sum(ll_vec) - length(eta_0) * log(C_recursive(theta[11:length(theta)], params)) - penalty)
}

llh_sp_dec()

estimate_sp_theta <- function(data, eps_pdf_list, gl=gl, lambda = lambda, parallel = TRUE) {
  tmp_data <- data %>% filter(
    !is.na(rev_ntile),
    !is.na(cal_V),
    corp == "Other"
  )
  # Not going to work with Revenue bc E[xi-eps|x]=E[xi]-E[eps|x], 
  # Even though I am not interested in f(xi|x)=f(xi), 
  # i need f(eps|x), but I have f(eps). When x is not revenue,
  # then, f(eps|x)=f(eps), and E[eps|x]=E[eps], 
  model_matrix <- model.matrix(~ -1 + rev_ntile + I(-cal_V), data = tmp_data)

  end_reg <- lm(cal_V ~ -1 + rev_ntile, data = tmp_data)
  eta_0 <- resid(end_reg)
  theta_reg <- coef(end_reg)

  bspline_spec <- get_bspline_spec_W(eta_0, n_knots = n_knots, spline_degree = pspline_degree)
  theta_bs <- initialize_theta_W(eta_0, bspline_spec)
  theta0 <- c(theta_reg, theta_bs)

  cat("Initial theta0: ", theta0, "\n")
  params <- list(
    a = bspline_spec$Boundary.knots[1],
    b = bspline_spec$Boundary.knots[2],
    gl = gl,
    bspline = bspline_spec,
    eps_pdf = eps_pdf_list$epdf
  )

  opt <- optim(
    par = theta0,
    fn = llh_sp_dec,
    model_matrix = model_matrix,
    params = params,
    lambda = lambda,
    parallel = parallel,
    method = "BFGS",
    control = list(fnscale = -1, maxit = 200, REPORT = 20, trace=3)
  )
 
  if (opt$convergence != 0) {
    cat("Optimization did not converge. Check the results.\n")
  }

  llh <- llh_sp_dec(
    theta = opt$par,
    model_matrix = model_matrix,
    params = params,
    lambda = 0,
    parallel = parallel
  )

  BIC <- -2 * llh + log(length(eta_0)) * (length(opt$par)-1)
  
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

## %% Testing functions

tmp_df <- df %>% filter(
!is.na(rev_ntile),
!is.na(cal_V),
corp == "Other"
)

end_reg <- lm(cal_V ~ -1 + rev_ntile, data = tmp_df)
eta_0 <- resid(end_reg)
theta_reg <- coef(end_reg)

bspline_spec <- get_bspline_spec_W(eta_0, n_knots = n_knots, spline_degree = pspline_degree)

theta_bs <- initialize_theta_W(eta_0, bspline_spec)
theta0 <- c(theta_reg, theta_bs)
cat("Initial theta0: ", theta0, "\n")
params <- list(
a = bspline_spec$Boundary.knots[1],
b = bspline_spec$Boundary.knots[2],
gl = gl,
bspline = bspline_spec,
eps_pdf = eps_pdf_list$epdf
)
model_matrix <- model.matrix(~ -1 + rev_ntile + I(-cal_V), data = tmp_df)

system.time(
    llh_sp_dec(theta0, model_matrix, params, lambda = lambda, parallel = F)
)

# Faster with parallelization
system.time(
    llh_sp_dec(theta0, model_matrix, params, lambda = lambda, parallel = T)
)

system.time(
    res_opt <- estimate_sp_theta(
        df,
        eps_pdf_list = eps_pdf_list,
        gl = gl,
        lambda = lambda,
        parallel = TRUE
    ) 
)
