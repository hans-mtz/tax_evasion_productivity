# install.packages("statmod")
# Load data and packages ---------------
library(tidyverse)
library(fixest)
library(statmod)
library(parallel)
# load("Code/Products/colombia_data.RData")
# load("Code/Products/global_vars.RData")

# Use Gaus-Hermite -----------------------

gauss_hermite<-gauss.quad(10,"hermite")
gauss_laguerre<-gauss.quad(10,"laguerre")

# Expectation of a function over a normal random variable with
# mean mu and variance sigma

E_nrv<-function(f,mu,sigma,quad,...){
    integral<-sum(f(quad$nodes*sqrt(2)*sigma+mu,...)*quad$weights)
    return(integral/sqrt(pi)) #removed \pi^(-1/2) because it will be added in the LogLikelihood
}

# Expectation of a function over a lognormal r.v. with logmean logmu and logvariance logsigma
# integral using Gauss-Laguerre quadrature over (0,+Inf)

E_lnrv<-function(f,logmu,logsigma,quad,...){
    y<-exp(sqrt(2*quad$nodes)*logsigma+logmu)
    y_prime<-sqrt(quad$nodes)
    integral<-sapply(
        1:length(quad$nodes),
        function(i,...){f(y[i],...)*quad$weights[i]/y_prime[i]}
        ,...) |> sum()
    return(integral/sqrt(pi)) #removed \pi^(-1/2) because it will be added in the LogLikelihood
}

E_nrv_vch<-function(f,v,mu,sigma,quad,...){
    x<- exp(quad$nodes*sqrt(2)*sigma+mu)-v
    x_prime<-exp(quad$nodes*sqrt(2)*sigma+mu)
    integral<- sapply(1:length(quad$nodes),function(i,...){x_prime[i]*f(x[i],...)*quad$weights[i]},...) |> sum()
    return(integral/sqrt(pi)) #removed \pi^(-1/2) because it will be added in the LogLikelihood
}


## Normal distributed function with additive independent error (output shock)

f_e<-function(epsilon,v,mu,sigma){
    num<-sum(v,epsilon,-mu)
    num_2 <- num*num
    den <- sigma*sigma
    f_e<-exp(-0.5*num_2/den)
    return(f_e/(sigma*sqrt(2*pi)))
}

f_e_n<-function(epsilon,v,mu,sigma){
    x<-sum(v,epsilon)
    return(dnorm(x,mean = mu, sd=sigma))
}

f_e_ln<-function(epsilon,v,mu,sigma){
    x<-sum(v,epsilon)
    if (x<=0) {
        f_e<-1e-300
    } else {
        num<-log(x)-mu
        num_2 <- num*num
        den <- sigma*sigma
        f_e<-exp(-0.5*num_2/den)/(x*sigma*sqrt(2*pi))
    }
    return(f_e)
}

eval_llh<-function(v,mu,sigma,params){
    log_lh<-log(E_nrv(f_e,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,v,mu,sigma))
    return(log_lh)
}

eval_llh_ln<-function(v,mu,sigma,params){
    log_lh<-log(E_nrv(f_e_ln,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,v,mu,sigma))
    return(log_lh)
}

eval_llh_ln2<-function(v,logmu,logsigma,params){
    log_lh<-log(E_lnrv(\(x,v,...)dnorm(x-v,...),logmu, logsigma, params$gauss_int, v=v,mean=params$epsilon_mu,sd=params$epsilon_sigma))
    return(log_lh)
}

eval_llh_ln_vch<-function(v,mu,sigma,params){
    log_lh<-log(E_nrv_vch(f_e_ln,v,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,v,mu,sigma))
    return(log_lh)
}

obj_fun<- function(theta,V,params){
    mu<-theta[1]
    sigma<-1.3^(theta[2])
    # n<-length(V)
    sum_llh<-sapply(V,eval_llh,mu,sigma,params) |> sum()
    # cons <- -(n/2)*log(2*pi^2)
    # log_lik<- -n*log(sigma)+sum_llh
    # return(log_lik)
    return(sum_llh)
}

obj_fun_ln<- function(theta,V,params){
    mu<-theta[1]
    sigma<-1.3^(theta[2])
    sum_llh<-sapply(V,eval_llh_ln,mu,sigma,params) |> sum()
    return(sum_llh)
}

obj_fun_ln2<- function(theta,V,params){
    logmu<-theta[1]
    logsigma<-theta[2]
    sum_llh<-sapply(V,eval_llh_ln2,logmu,logsigma,params) |> sum()
    return(sum_llh)
}

obj_fun_ln_vch<- function(theta,V,params){
    mu<-theta[1]
    sigma<-1.3^(theta[2])
    sum_llh<-sapply(V,eval_llh_ln_vch,mu,sigma,params) |> sum()
    return(sum_llh)
}

first_stage <- function(sic, var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
        ) %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            !is.na(.data[[var]]),
            .data[[var]] < Inf,
            .data[[var]] > -Inf
        ) %>%
        fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        filter(
            sic_3 == sic,
            !is.na(.data[[var]]),
            .data[[var]] < Inf,
            .data[[var]] > -Inf
        ) %>%
        mutate(
            cal_V = .data[[var]] - log(beta) - log(big_E)
        ) 

    result_list <- list(
        cal_V = tbl$cal_V,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E
    )
    return(result_list)
}

first_stage_panel <- function(sic, var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
        ) %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            !is.na(.data[[var]]),
            .data[[var]] < Inf,
            .data[[var]] > -Inf
        ) %>%
        fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        filter(
            sic_3 == sic,
            !is.na(.data[[var]]),
            .data[[var]] < Inf,
            .data[[var]] > -Inf
        ) %>%
        mutate(
            y = log(gross_output),
            cal_V = .data[[var]] - log(beta) - log(big_E),
            cal_W = log(gross_output)-beta*(m-cal_V)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E
    )
    return(result_list)
}

E_h <- function(W,params=params){

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

    w_2<-sapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x)^2,
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        }
    )

    w_3<-sapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x)^3,
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        }
    )

    return(cbind(w_1,w_2,w_3))
    
}

obj_fun_markov<-function(alpha,data,params){

    eta <-data %>% 
        ungroup() %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - alpha[1]*k-alpha[2]*l,
            lag_w_eps = lag(w_eps, order_by = year)
        ) %>%
        lm(w_eps~E_h(lag_w_eps,params), data=., na.action = na.exclude) |>
        residuals()


    moments<-apply(
        data[c("k","l")],
        2, 
        function(i){
        mean(i*eta, na.rm = TRUE)
        }
    )

    obj <- t(moments) %*% moments
    return(sqrt(obj[1]))

}



## Saving functions --------------------
print("Saving functions")
save(list=ls(), file="Code/Products/deconv_funs.Rdata")