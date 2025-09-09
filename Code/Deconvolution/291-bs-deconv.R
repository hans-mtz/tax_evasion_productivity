## %% --- Load libraries and setup ---
library(splines)
library(statmod)
library(parallel)

# load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/fs.RData")
load("Code/Products/np-deconv-funs.RData")

# Choose industries to deconvolve ---------------------

# select_fs_l <- paste0(top_5_ev_inds, " log_mats_share")
select_fs_l <- grep("log_mats",names(fs_list), value = TRUE) # Get all industries with log_mats_share


## %% --- Run estimation ---

result <- estimate_theta(fs_list[[5]], gl, lambda = lambda, parallel = FALSE)


## %% run all inds ---------------------

sp_deconv_list <- mclapply(
  select_fs_l,
  function(i) {
    cat("Estimating industry ", i, "\n")
    estimate_theta(fs_list[[i]], gl, lambda = lambda, parallel = FALSE)
  },
  mc.cores = detectCores() - 2
)
names(sp_deconv_list) <- select_fs_l

## %% Save results ---------------------

save(
  sp_deconv_list, #f_e, adaptive_integrate,
  file = "Code/Products/bs_mle_data.RData"
)
# load("Code/Products/bs_mle_data.RData")
## %% Plot results ---------------------

x <- seq(result$params$a, result$params$b, length.out = 1000)
plot(x, f_e(x, result$theta, result$params), type = "l", col = "blue", lwd = 2,
     xlab = "e", ylab = "Density", main = "Density of e")

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


get_stats(sp_deconv_list[[1]]$theta, sp_deconv_list[[1]]$params)

stats_list <- lapply(
  seq_along(sp_deconv_list),
  function(i) {
    get_stats(sp_deconv_list[[i]]$theta, sp_deconv_list[[i]]$params)
  }
)
stats_df <- do.call(rbind, stats_list)
stats_df

stats_df <- get_stats.list(sp_deconv_list)
## %% Save Results ---------------------

save(
  sp_deconv_list, stats_df,
  file = "Code/Products/bs_mle_data.RData"
)

## %% --- Non-parametric deconvolution ----------------------

# load("Code/Products/bs_mle_data.RData")

## %% NP error distribution ----------------------

np_pdf(fs_list[[1]])(seq(-3, 3, length.out = 100))

## %% Get all epsilon PDFs for selected industries ---------------------

eps_pdf_list <-lapply(
  select_fs_l,
  \(x) return(eps_pdf = np_pdf(fs_list[[x]]))
)
names(eps_pdf_list) <- select_fs_l

## %% Full NP Deconvolution Function ---------------------

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
  select_fs_l, 
  file = "Code/Products/bs_mle_data.RData"
)
