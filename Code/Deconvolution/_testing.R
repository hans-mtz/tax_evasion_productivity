# install.packages("statmod")
# Load data and packages ---------------
library(tidyverse)
library(fixest)
library(statmod)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")


# Use Gaus-Hermite -----------------------

out<-gauss.quad(10,"hermite")
gauss_laguerre<-gauss.quad(10,"laguerre")

# out$nodes*out$weights |> sum()

# Expectation of a function over a normal random variable with
# mean mu and variance sigma, integral using Guass-Hermite Quadrature
# over (-Inf,+Inf)

E_nrv<-function(f,mu,sigma,out,...){
    integral<-sum(f(out$nodes*sqrt(2)*sigma+mu,...)*out$weights)
    return(integral) #removed \pi^(-1/2) because it will be added in the LogLikelihood
}

E_nrv(\(x,mu_e)exp(-(x-mu_e)^2),0,1,out,0)

E_nrv(\(x)exp(-(x)^2),0,1,out)

E_nrv(\(x)dnorm(x+v,mean=mu,sd=sigma),0,1,out,0)

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

E_lnrv(\(x)x^2,0,1,gauss_laguerre)
E_lnrv(dnorm,-1.5,1,gauss_laguerre,0.5,1.2)
E_lnrv(\(x,y,...)dnorm(x-y,...),-1.5,1,gauss_laguerre,y=0.3,mean=0.5,sd=1.2)

E_nrv_vch<-function(f,v,mu,sigma,out,...){
    x<- exp(out$nodes*sqrt(2)*sigma+mu)-v
    x_prime<-exp(out$nodes*sqrt(2)*sigma+mu)
    integral<- sapply(1:length(out$nodes),function(i,...){x_prime[i]*f(x[i],...)*out$weights[i]},...) |> sum()
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

f_e_ln(0.1,0,0,1)

E_nrv(f_e_ln,0,1,out,-0.01,1,1.5)

E_nrv(f_e,0,1,out,-0.01,1,1.5)

E_nrv_vch(f_e_ln,-5,0,1,out,-0.01,1,1.5)

params<-list(gauss_int=out, epsilon_mu=0, epsilon_sigma=1,)
params<-list(gauss_int=gauss_laguerre, epsilon_mu=0, epsilon_sigma=1)

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

eval_llh_ln3<-function(v,logmu,logsigma,params){
    log_lh<-log(E_lnrv(f_e,logmu, logsigma, params$gauss_int, v=-v,mu=params$epsilon_mu,sigma=params$epsilon_sigma))
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
    mu<-1.3^(theta[1])
    sigma<-1.3^(theta[2])
    # n<-length(V)
    sum_llh<-sapply(V,eval_llh_ln_vch,mu,sigma,params) |> sum()
    # cons <- -(n/2)*log(2*pi^2) #-n/sum(x_i)
    # log_lik<- -n*log(sigma)+sum_llh
    # return(log_lik)
    return(sum_llh)
}

obj_fun_ln2<- function(theta,V,params){
    logmu<-theta[1]
    logsigma<-1.3^(theta[2])
    sum_llh<-sapply(V,eval_llh_ln2,logmu,logsigma,params) |> sum()
    return(sum_llh)
}

obj_fun_ln3<- function(theta,V,params){
    logmu<-theta[1]
    logsigma<-1.3^(theta[2])
    sum_llh<-sapply(V,eval_llh_ln3,logmu,logsigma,params) |> sum()
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

## 

fs<-first_stage(322, "log_share", colombia_data_frame)

params<-list(
    gauss_int=out,
    epsilon_mu=fs$epsilon_mu,
    epsilon_sigma=fs$epsilon_sigma
)

params<-list(
    gauss_int=gauss_laguerre,
    epsilon_mu=fs$epsilon_mu,
    epsilon_sigma=fs$epsilon_sigma
)

eval_llh(fs$cal_V[5],0,1,params)
eval_llh_ln(fs$cal_V[5],0,1,params)
eval_llh_ln(fs$cal_V[5],0,1,params)
eval_llh_ln_vch(fs$cal_V[5],0,1,params)
system.time(
    sapply(fs$cal_V,eval_llh,0,1,params) |> sum()
)

# No gain in parallelization in this stage
system.time(
    mclapply(fs$cal_V,eval_llh,0,1,params, mc.cores = detectCores()) |> Reduce(c,x=_) |> sum()
)

eval_llh_ln2(10,1,1,params)
eval_llh_ln3(10,1,1,params)
obj_fun(c(0,1),fs$cal_V,params)
obj_fun_ln(c(0,1),fs$cal_V,params)
obj_fun_ln2(c(1,1),fs$cal_V,params)
obj_fun_ln3(c(1,1),fs$cal_V,params)

opt_res<-optim(c(0,1),obj_fun,NULL,fs$cal_V,params,method = "BFGS", control=list(fnscale=-1))
optim(c(0,1),obj_fun,NULL,fs$cal_V,params,method = "SANN", control=list(fnscale=-1))
optim(c(13.6,-7.9),obj_fun,NULL,fs$cal_V,params,method = "Nelder-Mead", control=list(fnscale=-1))
optim(c(13.6,-7.9),obj_fun,NULL,fs$cal_V,params,method = "SANN", control=list(fnscale=-1))

mean_evasion <- log((18.77^2)/sqrt(18.77^2+1.3^(-7.51*2)))


opt_res_ln<-optim(c(0,1),obj_fun_ln,NULL,fs$cal_V,params,method = "BFGS", control=list(fnscale=-1))
opt_res_ln_vch<-optim(c(0,1),obj_fun_ln_vch,NULL,fs$cal_V,params,method = "BFGS", control=list(fnscale=-1))


opt_res_ln<-optim(c(0,1),obj_fun_ln,NULL,fs$cal_V,params,method = "SANN", control=list(fnscale=-1))
optim(opt_res_ln$par,obj_fun_ln,NULL,fs$cal_V,params,method = "Nelder-Mead", control=list(fnscale=-1))
optim(opt_res_ln$par,obj_fun_ln,NULL,fs$cal_V,params,method = "SANN", control=list(fnscale=-1))
optim(c(13.6,-7.9),obj_fun_ln,NULL,fs$cal_V,params,method = "Nelder-Mead", control=list(fnscale=-1))
optim(c(13.6,-7.9),obj_fun_ln,NULL,fs$cal_V,params,method = "SANN", control=list(fnscale=-1))


optim(c(0.1,0.1),obj_fun_ln2,NULL,fs$cal_V,params,method = "BFGS", control=list(fnscale=-1))
1.3
mu<-1.3^opt_res_ln$par[1]
sigma<-1.3^opt_res_ln$par[2]
opt_res_ln$ev_params<-c(
    mu_hat=exp(mu+0.5*sigma^2),
    sigma_hat=exp(mu+0.5*sigma^2)*sqrt(exp(sigma^2)-1),
    mode=exp(mu-sigma^2)
)

## Lognorm --------------------------------------
load("Code/Products/deconv.RData")
load("Code/Products/deconv_mle.RData")
lognorm_res_list_2<-lapply(
    names(lognorm_res_list),
    function(x){
        mu<-1.3^lognorm_res_list[[x]]$par[1]
        sigma<-1.3^lognorm_res_list[[x]]$par[2]
        mu_hat<-exp(mu+0.5*sigma^2)
        sigma_hat<-mu_hat*sqrt(exp(sigma^2)-1)
        mode<-exp(mu-sigma^2)
        # n<-length(fs_list[[x]]$cal_V)
        # me<- 1.96*sqrt(sigma_hat^2/n+sigma_hat^4/(2*(n-1)))
        ev_params<-c(
            log_mu = mu,
            log_sigma = sigma,
            mu_hat=mu_hat,
            sigma_hat=sigma_hat,
            mode=mode,
            median=exp(mu)
            # mean_LCI=mu_hat*exp(-me),
            # mean_UCI=mu_hat*exp(me)
        )
        return(lognorm_res_list)
    }
    # ,
    # mc.cores = mc_cores
)

names(lognorm_res_list_2)<-names(fs_list)

##

sapply(names(norm_res_list),\(x)norm_res_list[[x]]$ev_params) |> t() |> round(4)
sapply(names(lognorm_res_list),\(x)lognorm_res_list[[x]]$ev_params) |> t() |> round(4)


plot(
    seq(0,2,by=0.01),
    dlnorm(
        seq(0,2,by=0.01),
        lognorm_res_list[[1]]$ev_params[1],
        lognorm_res_list[[1]]$ev_params[2]
    ),
    type="l",
    col="blue",
    xlab = "", ylab = ""
)
lines(
    seq(0,2,by=0.01),
    dnorm(
        seq(0,2,by=0.01),
        norm_res_list[[1]]$ev_params[1],
        norm_res_list[[1]]$ev_params[2]
    ),
    col = "red",
    xlab = "", ylab = ""
)
