## %% Set up --------------------------
library(fixest)
library(data.table)
library(tidyverse)
library(ivreg)
library(parallel)
library(Rcpp)

load("Code/Products/deconv_funs.Rdata")


## %% Defining parameters --------------------------
set.seed(1234)
mc_cores <- detectCores()-2
# looking for 24 % acceptance rate
sim_sd <- 2
# Proposal distribution sd; epsilon ~ N(0,1) ; 
# sd= 1 70%; sd=5 8%; sd=3 18%; 
n_sims <- 2000L#5000
burn <- 1000L#3000
n_individuals <- 500#1000
n_time <- 50#200
delta <- c(delta_0 = 0.25, delta_1 = 0.9, delta_2 = 0.6)
bbeta <- 0.5



# init <- coef(lm(W~lag_W,ar1_data))
# init <- 1.4^init/(1+1.4^init)

## %% R Functions ----------------------------------


g_ind <- function(theta, data, eps_sim, bbeta){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # Simulate the AR(1) process
    # Note: In DT, .() is like summarise in dplyr
    # and `:=` is like mutate

    data[, W_squig := W - (1 - bbeta) * eps_sim]
    data[, lag_W_squig := shift(W_squig), by = id]
    data[, resid := W_squig - delta_0 - delta_1 * lag_W_squig]
    data[, `:=`(Z1 = 1, Z2 = lag_W_squig)]
    
    moments_n <- data[, .(
        Z1_resid = mean(Z1 * resid, na.rm = TRUE),
        Z2_resid = mean(Z2 * resid, na.rm = TRUE)
    ), by = id]
    
    moments_n[, mom := Z1_resid^2 + Z2_resid^2]
    moments_n <- moments_n[, mom]
    
    return(moments_n)    
}

g_ind_gamma <- function(theta, ggamma, data, eps_sim, bbeta){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # Simulate the AR(1) process
    # Note: In DT, .() is like summarise in dplyr
    # and `:=` is like mutate

    data[, W_squig := W - (1 - bbeta) * eps_sim]
    data[, lag_W_squig := shift(W_squig), by = id]
    data[, resid := W_squig - delta_0 - delta_1 * lag_W_squig]
    data[, `:=`(Z1 = 1, Z2 = lag_W_squig)]
    
    moments_n <- data[, .(
        Z1_resid = mean(Z1 * resid, na.rm = TRUE),
        Z2_resid = mean(Z2 * resid, na.rm = TRUE)
    ), by = id]
    
    moments_n[, mom := ggamma[1]*Z1_resid^2 + ggamma[2]*Z2_resid^2]
    moments_n <- moments_n[, mom]
    
    return(moments_n)    
}

# g_ind(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)

g_chain_ind <- function(theta, data, eps_sim, bbeta){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])
    
    data[
        , W_squig := W - (1 - bbeta) * eps_sim][
            , lag_W_squig := shift(W_squig), by = id][
                , `:=`(
                    resid = W_squig - delta_0 - delta_1 * lag_W_squig,
                    Z1 = 1, 
                    Z2 = lag_W_squig
                )
    ]
    moments_n <- data[, .(
        Z1_resid = mean(Z1 * resid, na.rm = TRUE),
        Z2_resid = mean(Z2 * resid, na.rm = TRUE)
        ), by = id]

    return(moments_n)    
}

# g_chain_ind(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)


# system.time(
#     g_chain_ind(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# )

get_mcmc_chain <- function(delta, n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta){
    eps_current <- eps_sim_dt[["sim_1"]]
    # cat("Initial eps_current: ", length(eps_current), "\n")
    g_chain <- c()
    eps_chain <- c()
    acceptance_rate <- c()
    j <- 1
    for (i in 2:n_sims){
        eps_propose <- eps_current + eps_sim_dt[[paste0("sim_", i)]]
        # if (i %% 100 ) cat("Proposed eps_propose: ", length(eps_propose), "\n")
        logtrydensity <- g_ind(delta, ar1_data, eps_current, bbeta) - g_ind(delta, ar1_data, eps_propose, bbeta)
        choose <- log(runif(n_individuals)) < logtrydensity
        acceptance_rate <- c(acceptance_rate, mean(choose))
        chosen_ind <- c(1:n_individuals)[choose]
        eps_current[eps_sim_dt$id %in% chosen_ind] <- eps_propose[eps_sim_dt$id %in% chosen_ind]
        # if (i %% 100 ) cat("Chosen eps_current: ", length(eps_current), "\n")
        eps_chain[[j]] <- eps_current
        g_chain[[j]] <- g_chain_ind(delta, ar1_data, eps_current, bbeta)
        j <- j + 1
    }
    return(list(
        eps_chain = eps_chain,
        g_chain = g_chain,
        acceptance_rate = acceptance_rate,
        acceptance_rate_mean = mean(acceptance_rate)
    ))
}

get_mcmc_chain_gamma <- function(delta, ggamma, n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta){
    eps_current <- eps_sim_dt[["sim_1"]]
    # cat("Initial eps_current: ", length(eps_current), "\n")
    g_chain <- c()
    eps_chain <- c()
    acceptance_rate <- c()
    j <- 1
    for (i in 2:n_sims){
        eps_propose <- eps_current + eps_sim_dt[[paste0("sim_", i)]]
        # if (i %% 100 ) cat("Proposed eps_propose: ", length(eps_propose), "\n")
        logtrydensity <- g_ind_gamma(delta, ggamma, ar1_data, eps_current, bbeta) - g_ind_gamma(delta, ggamma, ar1_data, eps_propose, bbeta)
        choose <- log(runif(n_individuals)) < logtrydensity
        acceptance_rate <- c(acceptance_rate, mean(choose))
        chosen_ind <- c(1:n_individuals)[choose]
        eps_current[eps_sim_dt$id %in% chosen_ind] <- eps_propose[eps_sim_dt$id %in% chosen_ind]
        # if (i %% 100 ) cat("Chosen eps_current: ", length(eps_current), "\n")
        eps_chain[[j]] <- eps_current
        g_chain[[j]] <- ggamma*g_chain_ind(delta, ar1_data, eps_current, bbeta)
        j <- j + 1
    }
    return(list(
        eps_chain = eps_chain,
        g_chain = g_chain,
        acceptance_rate = acceptance_rate,
        acceptance_rate_mean = mean(acceptance_rate)
    ))
}

obj_mcmc_ar1 <- function(
    theta, n_individuals, n_sims, burn, ar1_data, eps_sim_dt, bbeta
    ){
    delta <- theta[1:2] #AR(1) parameters
    ggamma <- theta[3:4] #Gamma parameters No of moments
    mcmc_chain_list <- get_mcmc_chain_gamma(delta, ggamma, n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta)
    mcmc_chain_dt <- rbindlist(mcmc_chain_list$g_chain[burn:n_sims-1])
    g <- mcmc_chain_dt[, lapply(.SD, mean), by = id]
    g <- g[, lapply(.SD, mean), .SDcols = !"id"]
    g[, mom := Z1_resid^2 + Z2_resid^2]
    return(g[, mom])
}

# system.time(
#     val<-obj_mcmc_ar1(c(init,0.5,0.5), n_individuals, n_sims, burn, ar1_data, eps_sim_dt, bbeta)
# )
# val

get_mcmc_chain_ind <- function(delta, ggamma, n_sims, burn, ar1_data, eps_sim_dt, bbeta){
    eps_current <- eps_sim_dt[["sim_1"]]
    # cat("Initial eps_current: ", length(eps_current), "\n")
    g_chain <- 0
    acceptance_rate <- 0
    # eps_chain <- c()
    # j <- 1
    for (i in 2:n_sims){
        eps_propose <- eps_current + eps_sim_dt[[paste0("sim_", i)]]
        # if (i %% 100 ) cat("Proposed eps_propose: ", length(eps_propose), "\n")
        logtrydensity <- g_ind_gamma(delta, ggamma, ar1_data, eps_current, bbeta) - g_ind_gamma(delta, ggamma, ar1_data, eps_propose, bbeta)
        choose <- log(runif(1)) < logtrydensity
        # acceptance_rate <- c(acceptance_rate, mean(choose))
        # chosen_ind <- c(1:n_individuals)[choose]
        if (choose) {
            eps_current <- eps_propose
            acceptance_rate <- acceptance_rate + 1
        }
        # if (i %% 100 ) cat("Chosen eps_current: ", length(eps_current), "\n")
        # eps_chain[[j]] <- eps_current
        if (i > burn) {
            g_chain <- g_chain + ggamma*g_chain_ind(delta, ar1_data, eps_current, bbeta)[, .(Z1_resid, Z2_resid)]
        }
        # j <- j + 1
    }
    return(c(
        # eps_chain = eps_chain,
        id=unique(ar1_data[,id]),
        g_chain / n_sims,
        acceptance_rate = acceptance_rate / n_sims#,
        # acceptance_rate_mean = mean(acceptance_rate)
    ))
}

# system.time(
#     ind_1 <-get_mcmc_chain_ind(init, c(0.5,0.5), 100, 10, ar1_data[id==2], eps_sim_dt[id==2], bbeta)
# )
# ind_1

obj_mcmc_ar1_par <- function(theta, n_individuals, mc_cores, burn, n_sims, ar1_data, eps_sim_dt, bbeta){
    delta <- theta[1:2] #AR(1) parameters
    ggamma <- theta[3:4] #Gamma parameters No of moments
    mcmc_chain_list <- mclapply(
        1:n_individuals,
        function(i) {
            get_mcmc_chain_ind(delta, ggamma, n_sims, burn, ar1_data[id==i], eps_sim_dt[id==i], bbeta)
        },
        mc.cores = mc_cores
    )
    mcmc_chain_dt <- rbindlist(mcmc_chain_list)
    # mcmc_chain_dt <- do.call(rbind, mcmc_chain_list)
    # mcmc_chain_dt <- data.table(mcmc_chain_dt)
    cat("MCMC dt after rbindlist: ", dim(mcmc_chain_dt), "\n")
    g<-mcmc_chain_dt[, lapply(.SD, mean), .SDcols = !"id"]
    cat("MCMC dt: ", dim(g), "\n")
    # cat("MCMC dt: ", print(g), "\n")
    g[, mom := Z1_resid^2 + Z2_resid^2]
    return(g[, mom])
}


# Slower than the serial version pfff!
# system.time(
#     val<-obj_mcmc_ar1_par(c(init,0.5,0.5), 100, mc_cores, 1000, 1000, ar1_data, eps_sim_dt, bbeta)
# )

## %% C++ functions --------------------------

# #include <Rcpp.h>
# using namespace Rcpp;

# // [[Rcpp::export]]
# DataFrame g_chain_ind_cpp(NumericVector theta, DataFrame data, NumericVector eps_sim, double bbeta) {
#     double delta_0 = pow(1.4, theta[0]) / (1 + pow(1.4, theta[0]));
#     double delta_1 = pow(1.4, theta[1]) / (1 + pow(1.4, theta[1]));

#     NumericVector W = data["W"];
#     IntegerVector id = data["id"];
#     NumericVector W_squig = W - (1 - bbeta) * eps_sim;

#     NumericVector lag_W_squig(W_squig.size(), NA_REAL);
#     for (int i = 1; i < W_squig.size(); i++) {
#         if (id[i] == id[i - 1]) {
#             lag_W_squig[i] = W_squig[i - 1];
#         }
#     }

#     NumericVector resid = W_squig - delta_0 - delta_1 * lag_W_squig;
#     NumericVector Z1(W.size(), 1.0);
#     NumericVector Z2 = lag_W_squig;

#     std::map<int, std::pair<double, double>> moments_map;
#     for (int i = 0; i < id.size(); i++) {
#         if (!NumericVector::is_na(resid[i])) {
#             moments_map[id[i]].first += Z1[i] * resid[i];
#             moments_map[id[i]].second += Z2[i] * resid[i];
#         }
#     }

#     IntegerVector unique_ids = unique(id);
#     NumericVector Z1_resid(unique_ids.size());
#     NumericVector Z2_resid(unique_ids.size());

#     for (int i = 0; i < unique_ids.size(); i++) {
#         int current_id = unique_ids[i];
#         Z1_resid[i] = moments_map[current_id].first;
#         Z2_resid[i] = moments_map[current_id].second;
#     }

#     return DataFrame::create(
#         Named("id") = unique_ids,
#         Named("Z1_resid") = Z1_resid,
#         Named("Z2_resid") = Z2_resid
#     );
# }

# library(Rcpp)
# sourceCpp("Code/Deconvolution/600-g_chain_ind.cpp")
sourceCpp("Code/Rcpp/100-mcmc.cpp")

# system.time(
#     g_chain_ind_cpp(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# )
# system.time(
#     g_chain_ind(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# )

## %% R wrappers of C++ functions --------------------------------------------


get_mcmc_chain_gamma_cpp_wrapper <- function(delta, ggamma, n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta){
    eps_current <- eps_sim_dt[["sim_1"]]
    ind <- unique(ar1_data[,id])
    # cat("Initial eps_current: ", length(eps_current), "\n")
    g_chain <- c()
    eps_chain <- c()
    acceptance_rate <- c()
    j <- 1
    for (i in 2:n_sims){
        eps_propose <- eps_current + eps_sim_dt[[paste0("sim_", i)]]
        # if (i %% 100 ) cat("Proposed eps_propose: ", length(eps_propose), "\n")
        logtrydensity <- g_ind_gamma_cpp(delta, ggamma, ar1_data, eps_current, bbeta) - g_ind_gamma_cpp(delta, ggamma, ar1_data, eps_propose, bbeta)
        choose <- log(runif(n_individuals)) < logtrydensity
        acceptance_rate <- c(acceptance_rate, mean(choose))
        chosen_ind <- c(1:n_individuals)[choose]
        eps_current[eps_sim_dt$id %in% chosen_ind] <- eps_propose[eps_sim_dt$id %in% chosen_ind]
        # if (i %% 100 ) cat("Chosen eps_current: ", length(eps_current), "\n")
        eps_chain[[j]] <- eps_current
        g_chain[[j]] <- ggamma*g_chain_ind_cpp(delta, ar1_data, eps_current, bbeta)[, c("Z1_resid", "Z2_resid")]
        j <- j + 1
    }
    return(list(
        eps_chain = eps_chain,
        g_chain = c(ind, g_chain),
        acceptance_rate = acceptance_rate,
        acceptance_rate_mean = mean(acceptance_rate)
    ))
}


obj_mcmc_ar1_cpp_wrp <- function(
    theta, n_individuals, n_sims, burn, ar1_data, eps_sim_dt, bbeta
    ){
    delta <- theta[1:2] #AR(1) parameters
    ggamma <- theta[3:4] #Gamma parameters No of moments
    mcmc_chain_list <- get_mcmc_chain_gamma_cpp_wrapper(delta, ggamma, n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta)
    # mcmc_chain_dt <- rbindlist(mcmc_chain_list$g_chain[burn:n_sims-1])
    mcmc_chain_dt <- do.call(rbind, mcmc_chain_list$g_chain[burn:n_sims-1])
    mcmc_chain_dt <- data.table(mcmc_chain_dt)
    g <- mcmc_chain_dt[, lapply(.SD, mean), by = id]
    g <- g[, lapply(.SD, mean), .SDcols = !"id"]
    g[, mom := Z1_resid^2 + Z2_resid^2]
    return(g[, mom])
}

obj_mcmc_ar1_cpp <- function(
    theta, n_individuals, n_sims, burn, ar1_data, eps_sim_dt, bbeta
    ){
    delta <- theta[1:2] #AR(1) parameters
    ggamma <- theta[3:4] #Gamma parameters No of moments
    mcmc_chain_list <- get_mcmc_chain_cpp(delta, ggamma, n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta)
    # mcmc_chain_dt <- rbindlist(mcmc_chain_list$g_chain[burn:n_sims-1])
    mcmc_chain_dt <- do.call(rbind, mcmc_chain_list$g_chain[burn:n_sims-1])
    mcmc_chain_dt <- data.table(mcmc_chain_dt)
    g <- mcmc_chain_dt[, lapply(.SD, mean), by = id]
    g <- g[, lapply(.SD, mean), .SDcols = !"id"]
    g[, mom := Z1_resid^2 + Z2_resid^2]
    return(g[, mom])
}

obj_mcmc_ar1_par_cpp <- function(theta, n_individuals, mc_cores, burn, n_sims, ar1_data, eps_sim_dt, bbeta){
    delta <- theta[1:2] #AR(1) parameters
    ggamma <- theta[3:4] #Gamma parameters No of moments
    mcmc_chain_list <- mclapply(
        1:n_individuals,
        function(i) {
            get_mcmc_chain_ind_cpp(delta, ggamma, n_sims, burn, ar1_data[id==i], eps_sim_dt[id==i], bbeta)[["g_chain"]]
        },
        mc.cores = mc_cores
    )
    # mcmc_chain_dt <- rbindlist(mcmc_chain_list)
    mcmc_chain_dt <- do.call(rbind, mcmc_chain_list)
    mcmc_chain_dt <- data.table(mcmc_chain_dt)
    # cat("MCMC dt after rbindlist: ", dim(mcmc_chain_dt), "\n")
    # cat("mcmc names: ", names(mcmc_chain_dt), "\n")
    g<-mcmc_chain_dt[, lapply(.SD, mean)]
    # cat("MCMC dt: ", dim(g), "\n")
    # cat("MCMC dt: ", print(g), "\n")
    g[, mom := g[,1]^2 + g[,2]^2]
    return(g[, mom])
}




## %% Make Cluster for parallelization --------------------------

# cl <- makeCluster(mc_cores)

## %% Host function --------------------------
# this parallel function is slower than serial version because of the 
# overhead of sending data to each of the cores in the cluster
# Other solutions involve using packages like bigmemory or filebacked matrices
# or using a database to store the data and then read it in each core
# or using a package like Rcpp to write the function in C++ and then call it from R


# obj_mcmc_ar1_par_cl <- function(theta, n_individuals, mc_cores, burn, n_sims, ar1_data, eps_sim_dt, bbeta, cl){
#     delta <- theta[1:2] #AR(1) parameters
#     ggamma <- theta[3:4] #Gamma parameters No of moments
#     # clusterExport(cl, c("delta", "ggamma", "n_sims", "burn", "ar1_data", "eps_sim_dt", "bbeta"))
#     mcmc_chain_list <- mclapply(
#         1:n_individuals,
#         function(i) {
#             get_mcmc_chain_ind(delta, ggamma, n_sims, burn, ar1_data[id==i], eps_sim_dt[id==i], bbeta)
#         },
#         mc.cores = mc_cores
#     )
#     mcmc_chain_dt <- rbindlist(mcmc_chain_list)
#     cat("MCMC dt after rbindlist: ", dim(mcmc_chain_dt), "\n")
#     g<-mcmc_chain_dt[, lapply(.SD, mean), .SDcols = !"id"]
#     cat("MCMC dt: ", dim(g), "\n")
#     # cat("MCMC dt: ", print(g), "\n")
#     g[, mom := Z1_resid^2 + Z2_resid^2]
#     return(g[, mom])
# }

## %% Simulate the AR(1) process --------------------------

w <- c()
for (i in 1:n_individuals){
    w_temp<-arima.sim(list(order=c(1,0,0), ar=delta[["delta_1"]]), n = n_time)
    w <- c(w,w_temp)
}
epsilon <- rnorm(n_individuals * n_time)

# ar1_data <- data.frame(
#     id = rep(1:n_individuals, each = n_time),
#     time = rep(1:n_time, n_individuals),
#     omega = w + delta[["delta_0"]]/(1-delta[["delta_1"]]) 
#     ) %>%
#     group_by(id) %>%
#     mutate(
#         lag_omega = lag(omega),
#         Z = 1.5+ omega + rnorm(1),
#         lag_Z = lag(Z),
#         lag_2_Z = lag(Z, 2)
#     ) %>%
#     ungroup() %>%
#     mutate(
#         W = omega + (1-bbeta)*epsilon,
#         lag_W = lag(W),
#         lag_2_W = lag(W, 2)
#     )

ar1_data <- data.table(
    id = rep(1:n_individuals, each = n_time),
    time = rep(1:n_time, n_individuals),
    omega = w + delta[["delta_0"]]/(1-delta[["delta_1"]])
)

ar1_data[, lag_omega := shift(omega), by = id]
ar1_data[, Z := 1.5 + omega + rnorm(.N), by = id]
ar1_data[, lag_Z := shift(Z), by = id]
ar1_data[, lag_2_Z := shift(Z, 2), by = id]

ar1_data[, `:=`(
    W = omega + (1 - bbeta) * epsilon#,
    # lag_W = shift(W),
    # lag_2_W = shift(W, 2)
)][,`:=`(
    # W = omega + (1 - bbeta) * epsilon,
    lag_W = shift(W),
    lag_2_W = shift(W, 2)
    )
]

init <- coef(lm(W~lag_W,ar1_data))
init <- 1.4^init/(1+1.4^init)

# ar1_data
## %% Simulate draws from the distribution of epsilon --------------------------
# load("Code/Products/mcmc_h.Rdata")

if (!exists("eps_sim_dt") || ncol(eps_sim_dt) != n_sims+2){

    eps_sim_r <- rnorm(n_individuals * n_time * n_sims, sd=sim_sd)
    eps_sim_dt <- data.table(
        id = rep(1:n_individuals, each = n_time),
        time = rep(1:n_time, n_individuals)
    )

    eps_sim_dt[, paste0("sim_", 1:n_sims) := as.data.table(matrix(eps_sim_r, ncol = n_sims))]

}

# system.time(
#     eps_sim_r <- rnorm(n_individuals * n_time * n_sims)
# )

# Parallelized version
# system.time(
#     eps_sim_r <- mclapply(1:n_sims, function(x) rnorm(n_individuals * n_time), mc.cores = mc_cores) |> unlist()
# )

## %% Testing functions --------------------------
# attach("Code/Products/mcmc_h.Rdata")

# system.time(
#     g_chain_ind_cpp(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# )
# system.time(
#     g_chain_ind(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# )

# system.time(
#     val_par_cpp<-obj_mcmc_ar1_par_cpp(c(init,0.5,0.5), n_individuals, mc_cores, burn, n_sims, ar1_data, eps_sim_dt, bbeta)
# )
# val_par_cpp

# system.time(
#     val <- obj_mcmc_ar1_cpp(c(init,0.5,0.5), n_individuals, n_sims, burn, ar1_data, eps_sim_dt, bbeta)
# )
# val_cpp

# system.time(
#     val_par<-obj_mcmc_ar1_par(c(init,0.5,0.5), n_individuals, mc_cores, burn, n_sims, ar1_data, eps_sim_dt, bbeta)
# )

# val_par

# system.time(
#     val <- obj_mcmc_ar1(c(init,0.5,0.5), n_individuals, n_sims, burn, ar1_data, eps_sim_dt, bbeta)
# )
# val
# g_chain |> str()

# g_chain <- get_mcmc_chain(init, n_sims, ar1_data, eps_sim_dt, bbeta)

# g_chain |> str()

# system.time(
#     g_chain <- get_mcmc_chain_gamma(init, c(0.5,0.5), n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta)
# )

# g_chain |> str()

# ### Serial version with C++ wrapper function 
# system.time(
#     mcmc_chain_list_serial <- get_mcmc_chain_gamma_cpp_wrapper(init, c(0.5,0.5), n_individuals, n_sims, ar1_data, eps_sim_dt, bbeta)
# )
# # Elapsed time: 2426.3 seconds (or 40.44 minutes)
# ### Parallelized version with C++ function 
# system.time(
#     mcmc_chain_list_parallel <- mclapply(
#         1:n_individuals,
#         function(i) {
#             get_mcmc_chain_ind_cpp(init, c(0.5,0.5), n_sims, burn, ar1_data[id==i], eps_sim_dt[id==i], bbeta)[["g_chain"]]
#         },
#         mc.cores = mc_cores
#     )
# )
# # Elpased time: 166.0 seconds (or 2.77 minutes)
# ### All C++ function
# system.time(
#     mcmc_chaing_list_cpp <- get_mcmc_chain_cpp(init, c(1,1), n_individuals, burn, n_sims, ar1_data, eps_sim_dt, bbeta)
# )
# # Elapsed time: 1911.4 seconds (or 31.85 minutes)
### %% test cpp function --------------------------
# attach("Code/Products/mcmc_h.Rdata")
# library(Rcpp)
# library(data.table)
# sourceCpp("Code/Rcpp/100-mcmc.cpp")
# g_chain_ind_cpp(init, ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# g_ind_gamma_cpp(init, c(0.5,0.5), ar1_data, eps_sim_dt[["sim_1"]], bbeta)
# get_mcmc_chain_ind_cpp(init, c(1,1), n_sims, burn, ar1_data[id==2], eps_sim_dt[id==2], bbeta)


## %% Estimation --------------------------



res <- optim(
    par = c(init, 1, 1),
    fn = obj_mcmc_ar1_par_cpp,
    n_individuals = n_individuals,
    mc_cores = mc_cores,
    burn = burn,
    n_sims = n_sims,
    ar1_data = ar1_data,
    eps_sim_dt = eps_sim_dt,
    bbeta = bbeta,
    method = "BFGS",
    control = list(
        maxit = 100,
        reltol = 1e-6,
        trace = 2,
        REPORT = 1
    )
)
res

1.4^res$par/(1+1.4^res$par)
## %% Save the results --------------------------

save(
    # g_chain, 
    eps_sim_dt, epsilon, 
    ar1_data, delta, bbeta, res,
    file = "Code/Products/mcmc_h.Rdata"
)

# save(
#     list = ls(),
#     file = "Code/Products/mcmc_h.Rdata"
# )
# load("Code/Products/mcmc_h.Rdata")

## %% Reviewing the results --------------------------

load("Code/Products/mcmc_h.Rdata")

 par<-c(5.861500,   -7.949178)
1.4^par/(1+1.4^par)
# g_chain$acceptance_rate[3999:4999] |> plot(x=1:length(g_chain$acceptance_rate[3999:4999]),y=_, type = "l", main = "Acceptance rate", ylab = "Acceptance rate", xlab = "Iteration")

# plot(density(epsilon))
# lines(density(g_chain$eps_chain[[4999]]/80), col = "red")
