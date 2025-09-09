## %% load packages and data ---------------
library(tidyverse)
library(parallel)
library(splines)
load("Code/Products/np-deconv-funs.Rdata")
load("Code/Products/run-vars.RData")
load("Code/Products/fs.RData")
load("Code/Products/omega_ar1_deconv_mle.RData")

## %% SET UP VARIABLES ---------------------

set.seed(987654)
mc_cores <- detectCores()-2
select_inds <- str_extract(names(fs_list),"\\d+")
folder_results <- "/Volumes/SSD Hans 1/Github/gnr/Data/"

## %% Functions ---------------------

integrate_np_pdf <- function(x, theta, params ){
    # x: points to evaluate
    # theta: spline coefficients
    # params: list with spline parameters

    f <- function(y) f_e(y, theta, params)

    # Integrate f up to x
    if (x < params$a) {
        cdf_x <- 0
    } else if (x > params$b) {
        cdf_x <- 1
    } else {
        cdf_x <- adaptive_integrate(f, params$a, x, params$gl)
    }
    return(cdf_x)
}

invert_np_dens <- function(p, theta, params, verbose=FALSE){
    #This function takes a probability and return the 
    # corresponding point in the penalized b-spline density

    # p: probability to invert
    # theta: spline coefficients
    # params: list with spline parameters
    # control: list with control parameters for the root finding algorithm
    f <- function(x) (integrate_np_pdf(x, theta, params) - p)^2
    # x0 <- params$a + (params$b - params$a) * p # initial guess
    res <- optimize(f, interval = c(params$a, params$b))
    # if (res$convergence != 0) {
    #     warning("Root finding did not converge")
    # }
    if (verbose) cat(" Inverted p:", p, " to x:", res$minimum, ", value:", res$objective, "\n")
    return(res$minimum)
}   

## %% Testing ---------------------

# integrate_np_pdf(4.144168, omega_semi_np_deconv_ls[[1]][["theta"]], omega_semi_np_deconv_ls[[1]][["params"]])
# invert_np_dens(0.5, omega_semi_np_deconv_ls[[1]][["theta"]], omega_semi_np_deconv_ls[[1]][["params"]])

# u <- runif(1000)
# samp <- mclapply(
#     u, 
#     invert_np_dens, 
#     theta=omega_semi_np_deconv_ls[[1]][["theta"]], 
#     params=omega_semi_np_deconv_ls[[1]][["params"]],
#     mc.cores = mc_cores
# )
# do.call(rbind,samp)[,1]
# samp <- mclapply(
#     u, 
#     invert_np_dens, 
#     theta=omega_full_np_deconv_ls[[1]][["theta"]], 
#     params=omega_full_np_deconv_ls[[1]][["params"]],
#     mc.cores = mc_cores
# )

# plot(do.call(rbind,samp)[,1],u)
# plot(density(do.call(rbind,samp)[,1]|>as.numeric()))

## %% Sample from np epsilon density ---------------------

### %% functions ---------------------

np_pdf.list <- function(list) {
  # Create a function for the PDF
  # Get the density of epsilon
  eps_density <- list$data$epsilon |> na.omit() |> density(bw="SJ-dpi")
  
  # Get min and max points from density estimation
  x_min <- min(eps_density$x)
  x_max <- max(eps_density$x)
  
  # Create the standard approximation function
  standard_pdf <- approxfun(eps_density$x, eps_density$y, rule=2)
  
  # Create a function that applies decay outside the range
  eps_pdf <- function(x) {
    # For points within range, use standard density
    within_range <- (x >= x_min) & (x <= x_max)
    
    # Get standard values (will modify those outside range)
    result <- standard_pdf(x)
    
    # For points below minimum, apply exponential decay
    below_min <- x < x_min
    if (any(below_min)) {
      # Get boundary value and apply decay based on distance
      boundary_val <- standard_pdf(x_min)
      distance <- x_min - x[below_min]
      decay_factor <- exp(-distance)  # Exponential decay
      result[below_min] <- boundary_val * decay_factor
    }
    
    # For points above maximum, apply exponential decay
    above_max <- x > x_max
    if (any(above_max)) {
      # Get boundary value and apply decay based on distance
      boundary_val <- standard_pdf(x_max)
      distance <- x[above_max] - x_max
      decay_factor <- exp(-distance)  # Exponential decay
      result[above_max] <- boundary_val * decay_factor
    }
    
    return(result)
  }
  
  return(list(
    epdf = eps_pdf,
    x_min = x_min,
    x_max = x_max
    )
  )
}

integrate_eps_np_pdf <- function(x, eps_pdf, x_min, x_max){
    # This function integrates the non-parametric epsilon PDF from
    # x_min to x
    # x: points to evaluate
    # eps_pdf: function for the epsilon PDF
    # x_min, x_max: range of the epsilon PDF

    # Constant of normalization
    const <- adaptive_integrate.both_ways(eps_pdf, x_min, x_max, gl)
    # Integrate f up to x
    if (x < x_min) {
        cdf_x <- 0
    } else if (x > x_max) {
        cdf_x <- 1
    } else {
        cdf_x <- adaptive_integrate.both_ways(eps_pdf, x_min, x, gl)/const
    }
    return(cdf_x)
}

invert_eps_np_dens <- function(p, eps_pdf, x_min, x_max, verbose=FALSE){
    #This function takes a probability and return the 
    # corresponding point in the non-parametric epsilon density

    # p: probability to invert
    # eps_pdf: function for the epsilon PDF
    # x_min, x_max: range of the epsilon PDF
    # control: list with control parameters for the root finding algorithm
    f <- function(x) (integrate_eps_np_pdf(x, eps_pdf, x_min, x_max) - p)^2
    # x0 <- x_min + (x_max - x_min) * p # initial guess
    res <- optimize(f, interval = c(x_min, x_max))
    # if (res$convergence != 0) {
    #     warning("Root finding did not converge")
    # }
    if (verbose) cat(" Inverted p:", p, " to x:", res$minimum, ", value:", res$objective, "\n")
    return(res$minimum)
}

## %% Testing ---------------------

# temp <- np_pdf.list(fs_list[[1]])

# curve(
#     np_pdf.list(fs_list[[1]])$epdf(x),
#     from = -2, to = 2,
#     xlab = "Epsilon",
#     ylab = "Density",
#     main = "Non-parametric Epsilon Density",
#     col = "blue", lwd=2
# )

# adaptive_integrate.both_ways(temp$epdf, temp$x_min, temp$x_max)
# integrate_eps_np_pdf(0.0, temp$epdf, temp$x_min, temp$x_max)
# invert_eps_np_dens(0.5, temp$epdf, temp$x_min, temp$x_max)

## %% Simulated Productivity ---------------------
u <- matrix(runif(1000*2), ncol=2)
sim_productivity <- mapply(
    function(x,y){
        cat("Processing industry and instrument:",x,y,"\n")
        omega_ls <- mclapply(
            u[,1],
            invert_np_dens,
            theta=omega_semi_np_deconv_ls[[paste(x,y)]][["theta"]],
            params=omega_semi_np_deconv_ls[[paste(x,y)]][["params"]],
            mc.cores = mc_cores
        )
        omega_v <- do.call(rbind,omega_ls)
        temp_ls <- np_pdf.list(fs_list[[x]])
        epsilon_ls <- mclapply(
            u[,2],
            invert_eps_np_dens,
            eps_pdf=temp_ls$epdf,
            x_min=temp_ls$x_min,
            x_max=temp_ls$x_max,
            mc.cores = mc_cores
        )
        epsilon_v <- do.call(rbind,epsilon_ls)
        phi <- exp(omega_v + epsilon_v)

        return(
            data.frame(
                phi = phi,
                inds = x,
                sic_3 = str_extract(x,"\\d+") |> as.numeric(),
                intermediate = str_extract(x,"\\w+$"),
                ins = y
            )
        )
    },
    x=run_vars_pick_iv[,"inds"],
    y=run_vars_pick_iv[,"ins"],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
)

names(sim_productivity) <- paste(run_vars_pick_iv[,"inds"],run_vars_pick_iv[,"ins"])
## %% Save results ---------------------

save(
    sim_productivity,
    file = "Code/Products/np_productivity.RData"
)

## %% Sum Stats by Industry and Instrument ---------------------

sim_productivity_tbl <- do.call(rbind, sim_productivity) %>%
    group_by(sic_3, ins) %>%
    summarise(
        Mean = mean(phi),
        SD = sd(phi),
        Q1 = quantile(phi, probs=0.25)[[1]],
        Median = median(phi),
        Q3 = quantile(phi, probs=0.75)[[1]]
    ) %>%
    mutate(
        Method = case_when(
            ins == "lag_m" ~ "TE: $m^*_{t-1}$",
            ins == "lag_2_w_eps" ~ "TE: $\\mathcal{W}_{t-2}$",
            TRUE ~ ""
        )
    ) %>%
    select(
        sic_3, Method, Mean, SD, Q1, Median, Q3
    )

## %% Save results ---------------------

save(
    sim_productivity, sim_productivity_tbl,
    file = "Code/Products/np_productivity.RData"
)

## %% Review table ---------------------

load("Code/Products/np_productivity.RData")
sim_productivity_tbl
