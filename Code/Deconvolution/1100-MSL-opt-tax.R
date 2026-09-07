## %% MSM to recover the probability of detection and kappa %%

# 1) Estimate PF by industry
# 2) Compute tilde_cal_W = cal_W - alpha_K k - alpha_L l
# 3) Compute likelihood for each observation and each sampled eps
#   A) Form functions from data and estimated parameters that take simulated epsilon
#       e(eps)=M^*(1-exp{-V-eps}) omega(eps)=W-(1-beta)eps
# 4) Compute the log likelihood for each observation, integrating over the epsilons using Gaussian quadrature
# 5) Maximize the sum of the log likelihood over the f_psi and theta


## I want to compare productivities (sizes) accross industries. Why?
# FOC wrt e assumes omega is across industry-productivity. The larger a firm across industries, then the 
# lower the cost of evasion, up to the point where either cost of evasion or the probability of detection spikes.
# Two approaches:
# A) Epsilon is iid across industries
# B) Epsilon is industry specific
## Load packages and data --------------------------

library(tidyverse)
library(fixest)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/931.1-fs-se-het.RData")

sys.source("Code/Deconvolution/021-deconv-funs.R", attach(NULL, name = "env-deconv"))
sys.source("Code/Deconvolution/030-np-deconv-funs.R", attach(NULL, name = "env-np-deconv"))
sys.source("Code/Deconvolution/050-render-tbls.R", attach(NULL, name = "env-render"))

## %% Define variables --------------------------

instruments <- c(
    "lag_k", "lag_l", "lag_m", "lag_2_cal_W"
)

run_vars <- expand.grid(
    instruments=instruments,
    inds=names(fs_all_ls),
    stringsAsFactors = FALSE
)

## %% Testing functions and packages work 

estimate_prod_fn_bounds(names(fs_all_ls)[1], fs_all_ls, obj_fun_ivar1_bounds, "lag_m")
# Worked. Estimate for all inds


sapply(
    names(fs_all_ls),
    \(x) fs_all_ls[[x]]$data %>% summarize(N=n()) %>% pull(N)
)

## %% Estimate production function parameters by industry --------------------------

pf_all_ls <- mcmapply(
    estimate_prod_fn_bounds,
    x=run_vars$inds,
    ins=run_vars$instruments,
    MoreArgs = list(
    fs_list=fs_all_ls,
    f=obj_fun_ivar1_bounds
    ),
    mc.cores = detectCores()-2
)

names(pf_all_ls) <- paste(run_vars$inds, run_vars$instruments, sep="_")

pf_list <- mclapply(
    names(fs_all_ls),
    estimate_prod_fn_bounds,
    fs_list=fs_all_ls,
    f=obj_fun_ivar1_bounds,
    ins = "lag_m",
    mc.cores = detectCores()-2
)

names(pf_list) <- names(fs_all_ls)


pf_list_l2w <- mclapply(
    names(fs_all_ls),
    estimate_prod_fn_bounds,
    fs_list=fs_all_ls,
    f=obj_fun_ivar1_bounds,
    ins = "lag_2_cal_W",
    mc.cores = detectCores()-2
)

names(pf_list_l2w) <- names(fs_all_ls)

## %% Save results --------------------------

save(
    pf_list,
    pf_list_l2w,
    pf_all_ls,
    file = "Code/Products/1100-MSL-opttax.RData"
)

## %% PF Estimates Tables ----------------------

load("Code/Products/1100-MSL-opttax.RData")

PF_tbl <- rbind(
    sapply(
        names(pf_list),
        \(x) data.frame(
            "SIC"=x,
            "Ins"="$m^*_{it-1}$",
            "m"=pf_list[[x]]$coeffs["m"],
            "k"=pf_list[[x]]$coeffs["k"],
            "l"=pf_list[[x]]$coeffs["l"],
            "N"=fs_all_ls[[x]]$data %>% summarize(N=n()) %>% pull(N)
            ),
        USE.NAMES = FALSE
    ) |> t(),
    sapply(
        names(pf_list_l2w),
        \(x) data.frame(
            "SIC"=x,
            "Ins"="$\\mathcal{W}_{it-2}$",
            "m"=pf_list_l2w[[x]]$coeffs["m"],
            "k"=pf_list_l2w[[x]]$coeffs["k"],
            "l"=pf_list_l2w[[x]]$coeffs["l"],
            "N"=fs_all_ls[[x]]$data %>% summarize(N=n()) %>% pull(N)
        ),
        USE.NAMES = FALSE
    ) |> t()
)|> data.frame()

PF_tbl %>%
    mutate(
        across(
            c(m,k,l), ~round(as.numeric(.x),3)
            )
    ) %>%
    arrange(N,SIC, Ins) |> 
    tt()

PF_all_tbl <- sapply(
    names(pf_all_ls) |> seq_along(),
    \(x) data.frame(
        "SIC"=run_vars$inds[x],
        "Ins"=run_vars$instruments[x],
        "m"=pf_all_ls[[x]]$coeffs["m"],
        "k"=pf_all_ls[[x]]$coeffs["k"],
        "l"=pf_all_ls[[x]]$coeffs["l"],
        "N"=fs_all_ls[[run_vars$inds[x]]]$data %>% summarize(N=n()) %>% pull(N)
        ),
    USE.NAMES = FALSE
)

PF_all_tbl


# %% Using Lag_m as instrument

## %% Compute tilde W

mclapply(
    names(pf_list),
    \(x){
        alpha_K <- pf_list[[x]]$coeffs["k"]
        alpha_L <- pf_list[[x]]$coeffs["l"]
        tmp_df <- fs_all_ls[[x]]$data %>%
            mutate(
                tilde_cal_W = cal_W - alpha_K*k - alpha_L*l,
                beta = pf_list[[x]]$coeffs["m"],
                Ins = "lag_m"
            )
        return(tmp_df)
    }
) |> bind_rows() -> wdf
  
## %% aside: Check log_mats_share densities by industry

pdf("Code/Products/1100-log_mats_share_densities.pdf", width = 14, height = 18)
op <- par(mfrow = c(10, 3), mar = c(2, 2, 2, 1), oma = c(0, 0, 2, 0))
on.exit({
  par(op)
  dev.off()
}, add = TRUE)

test_data %>%
  filter(
    juridical_organization != 3,
    is.finite(log_mats_share),
    is.finite(k),
    is.finite(l),
    is.finite(m)
  ) |>
  split(~sic_3) |>
  lapply(
    \(x){
      if (sum(is.finite(x$log_mats_share)) < 2) return(NULL)
      plot(density(x$log_mats_share, na.rm = TRUE),
           main = paste("Industry", unique(x$sic_3)),
           col = "blue", lwd = 2)
      abline(v = c(log(0.05), log(0.75)), col = c("red","gray"), lty = 2)
    }
  )
par(op)
dev.off()

# Note (Aug 5, 2026): it looks like all industries have long left tails before triming from below,
# I'm not trimming from above except for industry 369. Maybe it will be good to
# trim from above at log(1), above that, firms wil be spending more on intermediates
# than their total revenue, which is not realistic. But if they are overreporting,  maybe I won't 
# capture the super-overreporters. 

## %% Estimate epsilon density accross industries



eps_dens_acrss <- np_pdf(list(data=wdf))

plot(
#   density(tilde_W_df$epsilon, na.rm=TRUE, bw="SJ-ste"),
  y=eps_dens_acrss(seq(-5, 5, length.out = 1000)),
  x=seq(-5, 5, length.out = 1000),
  type = "l", col = "blue", lwd = 2,
  xlab = "epsilon", ylab = "Density", main = "Densities accross industries"
)
plot(
    density(wdf$tilde_cal_W, na.rm=TRUE, bw="SJ-ste"),
    col = "darkgray", lwd = 2
)
legend(
    "topright",
    legend = c("Eps", "W"),
    col = c("blue", "darkgray"), lty = 1, lwd = 2,
    horiz = TRUE, bty = "n", cex = 1.1
)

quantile(wdf$tilde_cal_W[wdf$tilde_cal_W>=0], probs = seq(0.05, 0.995, length.out = 10), na.rm = TRUE)
quantile(wdf$epsilon, probs = seq(0, 1, 0.01), na.rm = TRUE)

## %% Deconvolve omega density accross industries -----------------------

get_bspline_spec_t_W <- function(W, n_knots = n_knots, spline_degree = pspline_degree) {
  knot_candidates <- quantile(W, probs = seq(0.01, 0.99, length.out = n_knots))
#   boundary_knots <- c(min(W) - 0.1, max(W) + 0.1)
  boundary_knots <- knot_candidates[c(1, length(knot_candidates))]
  bspline_spec <- list(
    knots = knot_candidates[-c(1, length(knot_candidates))],
    intercept = FALSE,
    degree = pspline_degree,
    Boundary.knots = boundary_knots
  )
  return(bspline_spec)
}

theta0 <- initialize_theta_W(wdf$tilde_cal_W, get_bspline_spec_t_W(wdf$tilde_cal_W, n_knots = n_knots, spline_degree = pspline_degree))

integrate(
  \(x) exp(s(x, theta0, get_bspline_spec_t_W(wdf$tilde_cal_W, n_knots = n_knots, spline_degree = pspline_degree))),
  -5, 5
) |> str()

integrate(
  \(x) exp(s(x, theta0, get_bspline_spec_t_W(wdf$tilde_cal_W, n_knots = n_knots, spline_degree = pspline_degree))),
  -5, 5
)$value

llh_np_omega_acrs <- function(theta, df, params, lambda = lambda, parallel = TRUE) {
  gl <- params$gl
  bspline <- params$bspline
  eps_pdf <- params$eps_pdf

  mc_cores <- ifelse(parallel, detectCores() - 2, 1)

  ll_vec <- mclapply(seq_along(nrow(df)), function(i) {
    integrand <- function(omg) {
        x <- (df$tilde_cal_W[i]-omg)/(1-df$beta[i])
        eps_pdf(x) * exp(s(omg, theta, bspline))
    }
    int <- try(integrate(integrand, params$a, params$b, stop.on.error = FALSE), silent = TRUE)
    if(inherits(int, "try-error")) cat("Integration failed for observation", i, "with error:", int, "\n")
    val <- ifelse(!inherits(int, "try-error"), int$value, -9e200)
    val <- ifelse(val <=0,-9e200,log(val)) # Avoid log(0)
    val
    },
    mc.cores = mc_cores
  )
  ll_vec <- unlist(ll_vec)

  D <- build_D_order(length(theta), order = pspline_degree)
  penalty <- lambda * sum((D %*% theta)^2)

  return(sum(ll_vec) - length(df$tilde_cal_W) * log(C_recursive.adp(theta, params)) - penalty)
}

estimate_np_theta_omega_acrs <- function(df, eps_pdf, gl, lambda = lambda, parallel = TRUE) {
#   alpha <- pf_list$coeffs
#   cat("Using production function coefficients: ", alpha, "\n")
  W_squig <- df$tilde_cal_W
#   W_squig <- fs_list$data %>% 
#     filter(
#         is.finite(k),
#         is.finite(l),
#         is.finite(cal_W)
#     ) %>%
#     mutate(
#         W_squig = cal_W - alpha[["k"]]*k - alpha[["l"]]*l
#     ) %>% 
#     pull(W_squig)
# cat("Summary of W_squig: ", summary(W_squig), "\n")
  bspline_spec <- get_bspline_spec_t_W(W_squig, n_knots = n_knots, spline_degree = pspline_degree)
  
  theta0 <- initialize_theta_W(W_squig, bspline_spec)
  cat("Initial theta0: ", theta0, "\n")
  params <- list(
    a = bspline_spec$Boundary.knots[1],
    b = bspline_spec$Boundary.knots[2],
    bspline = bspline_spec,
    gl = gl,
    # epsilon_mean = fs_list$epsilon_mu,
    # epsilon_sd = fs_list$epsilon_sigma,
    eps_pdf = eps_pdf #,
    # beta = alpha[["m"]]
  )

  opt <- optim(
    par = theta0,
    fn = llh_np_omega_acrs,
    df = df,
    params = params,
    lambda = lambda,
    parallel = parallel,
    method = "BFGS",
    control = list(fnscale = -1, maxit = 200, REPORT = 20, trace=3)
  )
 
  if (opt$convergence != 0) {
    cat("Optimization did not converge. Check the results.\n")
  }

  llh <- llh_np_omega_acrs(
    theta = opt$par,
    df = df,
    params = params,
    lambda = 0,
    parallel = parallel
  )

  BIC <- -2 * llh + log(length(W_squig)) * (length(opt$par)-1)
  
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

## Testing the function

llh_np_omega_acrs(
  theta = c(0.6701048, 0.251192, -0.07530958, 0.3961341, 0.2114183, 0.2602096, 0.2424873, 0.03029904, -0.03350309, -1.483456),
  df = tilde_W_df,
  params = list(
    a = -2,
    b = 2,
    bspline = get_bspline_spec_t_W(tilde_W_df$tilde_cal_W, n_knots = n_knots, spline_degree = pspline_degree),
    gl = gl,
    eps_pdf = eps_dens_acrss
  ),
  lambda = lambda,
  parallel = TRUE
)

res <- estimate_np_theta_omega_acrs(
  df = tilde_W_df[tilde_W_df$tilde_cal_W >= 0,],
  eps_pdf = eps_dens_acrss,
  gl = gl,
  lambda = lambda,
  parallel = TRUE
)

res



get_stats(res$theta, res$params)

adaptive_integrate.both_ways(
    function(x) f_e.np.adp(x, res$theta, res$params),
    res$params$a, res$params$b, gl
)
curve(
  Vectorize(function(x) f_e.np(x, res$theta, res$params)),
  res$params$a, res$params$b
)

integrate(
    function(x) exp(s(x, res$theta, res$params$bspline)),
    res$params$a, res$params$b
)

exp(s(-5:5, res$theta, res$params$bspline))
integrate_eps_np_pdf(
    res$params$a,
    function(x) f_e.np(x, res$theta, res$params),
    res$params$a, res$params$b
)

f_e.np(res$params$a, res$theta, res$params)
exp(s(res$params$b, res$theta, res$params$bspline))
C_recursive.adp(res$theta, res$params)
C_recursive(res$theta, res$params)
1/1.e-316
plot(
  y = Vectorize(function(x) f_e.np(x, res$theta, res$params))(seq(res$params$a, res$params$b, length.out = 100)),
  x = seq(res$params$a, res$params$b, length.out = 100),
  type = "l", col = "blue", lwd = 2,
  xlab = "omega", ylab = "Density", main = "Deconvolved omega density accross industries"
)
