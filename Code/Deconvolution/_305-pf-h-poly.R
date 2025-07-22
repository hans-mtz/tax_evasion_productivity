# %% Load packages
library(fixest )
library(tidyverse)
library(ivreg)
library(parallel)

load("Code/Products/deconv_funs.Rdata")
## Simulating to test theoretic identification result for
## a second order polynomial for the Markov process of productivity

#%% Simulate the data -- This did not work

# set.seed(4598)

# delta <- c(delta_0 = 0.25, delta_1 = 0.9, delta_2 = 0.6)
# beta <- 0.5
# df<-data.frame(
#     id = rep(letters[1:25], each = 10),
#     time = rep(1:10, 25),
#     epsilon = rnorm(250),
#     eta = rnorm(250)
# ) %>% #arrange(time, id) %>%
# group_by(id) %>%
# mutate(
#     omega = ifelse(time==1,delta[["delta_0"]] +eta,0),
#     omega_ar1 = ifelse(time==1,delta[["delta_0"]] +eta,0),
#     omega = delta[["delta_0"]]/(1-delta[["delta_1"]]) + delta[["delta_1"]] * lag(omega, default = (delta[["delta_0"]] +eta[1])) + delta[["delta_2"]] * (lag(omega, default = (delta[["delta_0"]] +eta[1])))^2 + eta,
#     lag_omega = lag(omega),
#     omega_ar1 = delta[["delta_0"]]/(1-delta[["delta_1"]]) + delta[["delta_1"]] * lag(omega_ar1, default = (delta[["delta_0"]] +eta[1]))  + eta,
#     lag_omega_ar1 = lag(omega_ar1),
#     W = omega + (1-beta)*epsilon,
#     lag_W = lag(W),
#     Z = 1.5+ omega + rnorm(1),
#     lag_Z = lag(Z),
#     lag_2_Z = lag(Z, 2),
#     W_ar1 = omega_ar1 + (1-beta)*epsilon,
#     lag_W_ar1 = lag(W_ar1),
#     Z_ar1 = omega_ar1 + 0.6*rnorm(1),
#     lag_Z_ar1 = lag(Z_ar1),
#     lag_2_Z_ar1 = lag(Z_ar1, 2)
# )
# df |> View()


# lm(omega_ar1~lag_omega_ar1,df) |> summary()
# fixest::feols(omega_ar1~lag_omega_ar1,df) |> summary()


# # omega <- c()
# # i<-0
# # T<-0
# # omega[1] <- delta[["delta_0"]] + df$eta[[1]]
# # for( ind %in% unique(df$id)){
# #     i<-i+1
# #     print(i)
# #     print(T)
# #     print(i+T)
# #     for( t %in% 1:10){
# #         omega[i+T] <- delta[["delta_0"]] + df[df$id==ind & df$time==t, "eta"]+ df[df$id==ind & df$time==t, "eta"]
# #         omega[i] <- delta[["delta_0"]] + delta[["delta_1"]] * omega[(ind-1)*10+t] + delta[["delta_2"]] * omega[(ind-1)*10+t]^2 + eta[(ind-1)*10+t])
# #         T <- T+t
# #     }
# # }

# cor(df$W,df$Z)
# cor(df$omega,df$Z)
# cor(df$omega,df$lag_omega,use="pairwise.complete.obs")
# cor(df$omega_ar1,df$lag_omega_ar1,use="pairwise.complete.obs")
# cor(df$epsilon,df$omega)
# lm(W~poly(lag_W,2,raw=TRUE), df) |> summary()

# lm(W_ar1~lag_W_ar1, df) |> summary()

# lm(omega_ar1~lag_omega_ar1,df) |> summary()

# fixest::feols(omega_ar1~lag_omega_ar1,df) |> summary()
# ivreg( W ~ 1| poly(lag_W,2,raw=TRUE) | poly(lag_Z,2,raw=TRUE), data=df) |> summary()

# ivreg( W_ar1 ~ 1| lag_W_ar1 | lag_Z_ar1, data=df) |> summary()

# %% Testing OLS ------------------


# x <- 1.5+rnorm(250)
# y <- 1.5+rnorm(250)
# lm(y~x) |> summary()
# y <- 1+0.8*x+rnorm(250)
# lm(y~x) |> summary()

# %% Simulate AR(1) process for panel data -----------------

set.seed(1234)

n_individuals <- 500# 1000
n_time <- 50 #200
delta <- c(delta_0 = 0.25, delta_1 = 0.9, delta_2 = 0.6)
beta <- 0.5
w <- c()
for (i in 1:n_individuals){
    w_temp<-arima.sim(list(order=c(1,0,0), ar=delta[["delta_1"]]), n = n_time)
    w <- c(w,w_temp)
}
epsilon <- rnorm(n_individuals * n_time)
ar1_data <- data.frame(
    id = rep(1:n_individuals, each = n_time),
    time = rep(1:n_time, n_individuals),
    omega = w + delta[["delta_0"]]/(1-delta[["delta_1"]]) 
    ) %>%
    group_by(id) %>%
    mutate(
        lag_omega = lag(omega),
        Z = 1.5+ omega + rnorm(1),
        lag_Z = lag(Z),
        lag_2_Z = lag(Z, 2)
    ) %>%
    ungroup() %>%
    mutate(
        W = omega + (1-beta)*epsilon,
        lag_W = lag(W),
        lag_2_W = lag(W, 2)
    )

## %% Testing Data
### OLS works with omega
lm(omega~lag_omega,ar1_data) |> summary()
### OLS does not work with W
lm(W~lag_W,ar1_data) |> summary()
### IV works with W
ivreg(W~1|lag_W|lag_Z, data=ar1_data) |> summary()

# %% Testing estimating by simulation -----------------

n_sims <- 10000
eps_sim_r <- rnorm(n_individuals * n_time * n_sims)
eps_sim_mat <- data.frame(matrix(eps_sim_r,ncol=n_sims))
mc_cores <- detectCores()-2
# W_t = \delta_0 + |delta_1 W_t-1 + eta_t + (1-\beta)\epsilon_t - delta_1(1-\beta) epsilon_t-1
# phi = eta-t + (1-\berta)epsilon_t - delta_1(1-\beta) epsilon_t-1
# phi is the problem, it is correlated with W_t-1

params<-list(
    gauss_int=gauss_hermite,
    epsilon_mu= 0,
    epsilon_sigma= 1,
    beta = beta
)

## %% Defining the functions -----------------

E_h_ar1 <- function(W,params=params){

    w_1<-sapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x),
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        }
    )
    return(w_1)
}

E_h_ar1_par <- function(W,params=params){

    w_1<-mclapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x),
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        },
        mc.cores = mc_cores
    )
    return(do.call(rbind,w_1))
}

obj_s <- function(theta, data, eps_sim, beta){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # Simulate the AR(1) process
    resid <- data %>%
        mutate(
            W_squig = W - (1-beta) * eps_sim
        ) %>%
        group_by(id) %>%
        mutate(
            # W_squig = W - (1-beta) * eps_sim,
            lag_W_squig = lag(W_squig),
            resid = W_squig - delta_0 - delta_1 * lag_W_squig
        ) %>%
        ungroup() #%>%
        # pull(resid)
    
    moments <- apply(
        data.frame(Z1=1,Z2=resid$lag_W_squig),
        2,
        function(x) {
            mean(x * resid$resid, na.rm = TRUE)
        }
    )
   
    obj <- t(moments) %*% moments
    return(sqrt(obj[1]))
}


obj_Eh <- function(theta, data, params){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # Simulate the AR(1) process
    resid <- data %>%
        mutate(
            E_h1_W = E_h_ar1(W, params),
            lag_E_h1_W = lag(E_h1_W),
            # resid = W - delta_0 - delta_1 * (lag_W - (1-beta) * eps_sim),
            resid = E_h1_W - delta_0 - delta_1 * lag_E_h1_W,
            # xresid = lag_W*resid
        ) %>%
        pull(resid)
    
    moments <- apply(
        data.frame(Z1=1),
        2,
        function(x) {
            mean(x * resid, na.rm = TRUE)
        }
    )
   
    obj <- t(moments) %*% moments
    return(sqrt(obj[1]))
}

obj_Eh_par <- function(theta, data, params){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # Simulate the AR(1) process
    resid <- data %>%
        mutate(
            E_h1_W = E_h_ar1_par(W, params),
            lag_E_h1_W = lag(E_h1_W),
            # resid = W - delta_0 - delta_1 * (lag_W - (1-beta) * eps_sim),
            resid = E_h1_W - delta_0 - delta_1 * lag_E_h1_W,
            # xresid = lag_W*resid
        ) #%>%
        # pull(resid)
    
    moments <- apply(
        data.frame(Z1=1, Z2=resid$lag_E_h1_W),
        2,
        function(x) {
            mean(x * resid$resid, na.rm = TRUE)
        }
    )
   
    obj <- t(moments) %*% moments
    return(sqrt(obj[1]))
}

obj_wrap_mc<-function(theta,data,eps_sim_mat,beta){
    obj_r<-mclapply(
        1:n_sims,
        function(i){
            obj_s(theta, data, eps_sim_mat[[i]], beta)
        },
        mc.cores = mc_cores
    )
    obj_v <- do.call(rbind, obj_r)
    obj <- mean(obj_v)
    return(obj)
}

obj_wrap<-function(theta,data,eps_sim_mat,beta){
    obj_r<-lapply(
        1:n_sims,
        function(i){
            obj_s(theta, data, eps_sim_mat[[i]], beta)
        }#,
        # mc.cores = mc_cores
    )
    obj_v <- do.call(rbind, obj_r)
    obj <- mean(obj_v)
    return(obj)
}

## %% Testing the functions -----------------
init <- c(0.25,0.9)
init <- 1.4^init/(1+1.4^init)
system.time(
obj_s(init, ar1_data, eps_sim_mat[[1]], beta)
)
system.time(
obj_wrap(init, ar1_data, eps_sim_mat, beta)
)
system.time(
obj_wrap_mc(init, ar1_data, eps_sim_mat, beta)
)
system.time(
    obj_Eh(init, ar1_data, params)
)
system.time(
    obj_Eh_par(init, ar1_data, params)
)
## %% estimate by simulation -----------------
init <- coef(lm(W~lag_W,ar1_data))
init <- 1.4^init/(1+1.4^init)
system.time(
    res<-optim(
        init,
        obj_wrap_mc,
        data=ar1_data,
        eps_sim_mat=eps_sim_mat,
        beta=beta,
        method="BFGS"
    )   
)
res
1.4^res$par/(1+1.4^res$par)
system.time(
    res<-optim(
        init,
        obj_wrap,
        data=ar1_data,
        eps_sim_mat=eps_sim_mat,
        beta=beta,
        method="BFGS"#,
        # lower=c(0,0),
        # upper=c(1,1)
    )
)

res
1.4^res$par/(1+1.4^res$par)


# Closest but not quite, Z1=1, Z2=W, with bounds LBFGS

init <- coef(lm(W~lag_W,ar1_data))
init <- 1.4^init/(1+1.4^init)

res_Eh<-optim(
    init,
    obj_Eh_par,
    data=ar1_data,
    params=params,
    method="BFGS"#,
    # lower=c(0,0),
    # upper=c(1,1)
)

res_Eh
1.4^res_Eh$par/(1+1.4^res_Eh$par)

## %% ELVIS ------------------

# g_mc_chain <- c(n_individuals, n_sims)
eps_sim_df <- data.frame(
    id = rep(1:n_individuals, each = n_time),
    time = rep(1:n_time, n_individuals),
    matrix(eps_sim_r,ncol=n_sims, dimnames = list(NULL, paste0("sim_",1:n_sims)))
)
g_ind <- function(theta, data, eps_sim, beta){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # Simulate the AR(1) process
    moments_n <- data %>%
        mutate(
            W_squig = W - (1-beta) * eps_sim
        ) %>%
        group_by(id) %>%
        mutate(
            # W_squig = W - (1-beta) * eps_sim,
            lag_W_squig = lag(W_squig),
            resid = W_squig - delta_0 - delta_1 * lag_W_squig,
            Z1 = 1,
            Z2 = lag_W_squig
        ) %>%
        summarise(
            across(
                c(Z1, Z2),
                ~ mean(.x*resid, na.rm = TRUE)
            )
        ) %>%
        mutate(
            mom = Z1*Z1 + Z2*Z2,
        ) %>% 
        pull(mom)
    
    return(moments_n)    
}

g_chain_ind <- function(theta, data, eps_sim, beta){
    delta_0 <- 1.4^theta[1]/(1+1.4^theta[1])
    delta_1 <- 1.4^theta[2]/(1+1.4^theta[2])

    # eps_sim <- data.frame(array(eps_sim_array,c(n_individuals*n_time, 1)))
    # Simulate the AR(1) process
    moments_n <- data %>%
        mutate(
            W_squig = W - (1-beta) * eps_sim
        ) %>%
        group_by(id) %>%
        mutate(
            # W_squig = W - (1-beta) * eps_sim,
            lag_W_squig = lag(W_squig),
            resid = W_squig - delta_0 - delta_1 * lag_W_squig,
            Z1 = 1,
            Z2 = lag_W_squig
        ) %>%
        summarise(
            across(
                c(Z1, Z2),
                ~ mean(.x*resid, na.rm = TRUE)
            )
        )# %>%
        # pull(Z1,Z2)
    
    return(moments_n[,c("Z1","Z2")])    
}
g_ind(init, ar1_data, eps_sim_df[["sim_1"]], beta)

g_chain_ind(init, ar1_data, eps_sim_df[["sim_2"]], beta)

log(runif(n_individuals))

get_mcmc_chain <- function(delta, n_sims, ar1_data, eps_sim_df, beta){
    eps_current <- eps_sim_df[["sim_1"]]
    g_chain <- c()
    eps_chain <- c()
    acceptance_rate <- c()
    j <- 1
    for (i in 2:n_sims){
        eps_propose <- eps_current + eps_sim_df[[paste0("sim_", i)]]
        logtrydensity <- g_ind(delta, ar1_data, eps_current, beta) - g_ind(delta, ar1_data, eps_propose, beta)
        choose <- log(runif(n_individuals)) < logtrydensity
        acceptance_rate <- c(acceptance_rate, mean(choose))
        chosen_ind <- c(1:n_individuals)[choose]
        eps_current[eps_sim_df$id %in% chosen_ind] <- eps_propose[eps_sim_df$id %in% chosen_ind]
        eps_chain[[j]] <- eps_current
        g_chain[[j]] <- g_chain_ind(delta, ar1_data, eps_current, beta)
        j <- j + 1
    }
    return(list(
        eps_chain = eps_chain,
        g_chain = g_chain,
        acceptance_rate = acceptance_rate,
        acceptance_rate_mean = mean(acceptance_rate)
    ))
}

system.time(
    g_chain <- get_mcmc_chain(init, n_sims, ar1_data, eps_sim_df, beta)
)
g_chain |> str()
