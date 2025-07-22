# %% install.packages("statmod")
# Load data and packages ---------------
library(tidyverse)
library(fixest)
library(statmod)
library(ivreg)
library(parallel)
library(MCMCprecision)
# load("Code/Products/colombia_data.RData")
# load("Code/Products/global_vars.RData")



# %% Use Gaus-Hermite -----------------------

gauss_hermite<-gauss.quad(10,"hermite")
gauss_laguerre<-gauss.quad(10,"laguerre")
threshold_cut <- 0.05

# Deconvolution using Moments ---------------
## Deconvolution functions ----------------------------


mmt_deconv_year <- function(sic, var, data) {
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
                cal_V = .data[[var]] - log_D
            ) %>%
            group_by(year) %>%
            summarise(
                mean_evasion = mean(cal_V, na.rm = TRUE) + mean_epsilon,
                variance_evasion = var(cal_V, na.rm = TRUE) - variance_epsilon,
                n = n()
            ) %>%
            mutate(
                sic_3 = sic
            )
        return(tbl)
    }

mmt_deconv <- function(sic, var, data) {
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                juridical_organization == 3,
                # !is.na(.data[[var]]),
                # .data[[var]] < Inf,
                # .data[[var]] > -Inf
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                .data[[var]] > log(threshold_cut)
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
                is.finite(.data[[var]]),
                # is.finite(k),
                # is.finite(l),
                # is.finite(m),
                .data[[var]] > log(threshold_cut)
                # !is.na(.data[[var]]),
                # .data[[var]] < Inf,
                # .data[[var]] > -Inf
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_evasion = mean(cal_V, na.rm = TRUE) + mean_epsilon,
                variance_evasion = var(cal_V, na.rm = TRUE) - variance_epsilon,
                n = n()
            ) %>%
            mutate(
                sic_3 = sic,
                inter = var
            )
        return(tbl)
    }

# Testing the presence of Evasion by Overreporting -----------------

test_ev_cd <- function(sic, var, data) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                # .data[[var]] > log(threshold_cut),
                juridical_organization == 3#,
                # ...
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
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut)
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_V = mean(cal_V, na.rm = TRUE),
                n = n(),
                # se = sqrt(abs(variance_epsilon)/n)
                se = sd(cal_V)/sqrt(n)
            ) %>%
            mutate(
                t_stat = (mean_V)/se,
                rej_rule = t_stat > 1.645,
                test_result = ifelse(
                    rej_rule>=1,
                    "Reject H_0: cal_V==0, H_a: cal_V>0", 
                    "Fail to reject H_0: cav_V==0"
                ),
                sic_3 = sic,
                intermediates = var
            )
        return(tbl)
    }

test_ev_trim <- function(sic, var, data) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut),
                juridical_organization == 3#,
                # ...
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
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut)
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_V = mean(cal_V, na.rm = TRUE),
                n = n(),
                # se = sqrt(abs(variance_epsilon)/n)
                se = sd(cal_V)/sqrt(n)
            ) %>%
            mutate(
                t_stat = (mean_V)/se,
                rej_rule = t_stat > 1.645,
                test_result = ifelse(
                    rej_rule>=1,
                    "Reject H_0: cal_V==0, H_a: cal_V>0", 
                    "Fail to reject H_0: cav_V==0"
                ),
                sic_3 = sic,
                intermediates = var
            )
        return(tbl)
    }

test_ev_cond <- function(sic, var, data, cond) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                # juridical_organization == 3#,
                {{cond}}
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
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m)
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_V = mean(cal_V, na.rm = TRUE),
                n = n(),
                # se = sqrt(abs(variance_epsilon)/n)
                se = sd(cal_V)/sqrt(n)
            ) %>%
            mutate(
                t_stat = (mean_V)/se,
                rej_rule = t_stat > 1.645,
                test_result = ifelse(
                    rej_rule>=1,
                    "Reject H_0: cal_V==0, H_a: cal_V>0", 
                    "Fail to reject H_0: cav_V==0"
                ),
                sic_3 = sic,
                intermediates = var
            )
        return(tbl)
    }

# Double sided test for the presence of evasion ------------------
# Get beta on Corps, test on All firms
test_ev_2t <- function(sic, var, data) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut),
                juridical_organization == 3 #,
                # ...
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
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut)
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_V = mean(cal_V, na.rm = TRUE),
                n = n(),
                # se = sqrt(abs(variance_epsilon)/n)
                se = sd(cal_V)/sqrt(n)
            ) %>%
            mutate(
                t_stat = (mean_V)/se,
                rej_rule = abs(t_stat) > 1.96,
                test_result = ifelse(
                    rej_rule >= 1,
                    "Reject H_0: cal_V==0; H_a: cal_V!=0", 
                    "Fail to reject H_0: cav_V==0"
                ),
                sic_3 = sic,
                intermediates = var
            )
        return(tbl)
    }

# Double sided test for the presence of evasion 2 Samples ------------------
# Get beta on Corps, test on Non-Corps
test_ev_2t_2smpl <- function(sic, var, data) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut),
                juridical_organization == 3 #,
                # ...
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
                juridical_organization != 3,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut)
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_V = mean(cal_V, na.rm = TRUE),
                n = n(),
                # se = sqrt(abs(variance_epsilon)/n)
                se = sd(cal_V)/sqrt(n)
            ) %>%
            mutate(
                t_stat = (mean_V)/se,
                rej_rule = abs(t_stat) > 1.96,
                test_result = ifelse(
                    rej_rule >= 1,
                    "Reject H_0: cal_V==0; H_a: cal_V!=0", 
                    "Fail to reject H_0: cav_V==0"
                ),
                sic_3 = sic,
                intermediates = var
            )
        return(tbl)
    }


# Double sided test for the presence of evasion 2 Samples ------------------
# Get beta on Corps, test on user-defined group

test_ev_2t_2smpl_cond <- function(sic, var, cond, data) {
    # This function perfomrs a one tail t-test for the presence of tax evasion 
    # through input overreporting. 
    # - Under the null hypothesis, there is no tax evasion H_0: cal_V == 0;
    # - Under the alternative hypothesis, there is tax evasion H_a: cal_V > 0;
    # There is no need to trim the data if the probability the firm reported a zero
    # is the same for corporations and non-corporations. 
        fml <- paste0(var,"~1") |> as.formula()
        fs_reg <- data %>%
            mutate(
                treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
            ) %>%
            filter(
                sic_3 == sic,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut),
                juridical_organization == 3 #,
                # ...
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
                # juridical_organization != 3,
                is.finite(.data[[var]]),
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y),
                .data[[var]] > log(threshold_cut),
                eval(cond, envir = .)
            ) %>%
            mutate(
                cal_V = .data[[var]] - log_D
            ) %>%
            summarise(
                mean_V = mean(cal_V, na.rm = TRUE),
                n = n(),
                # se = sqrt(abs(variance_epsilon)/n)
                se = sd(cal_V)/sqrt(n)
            ) %>%
            mutate(
                t_stat = (mean_V)/se,
                rej_rule = abs(t_stat) > 1.96,
                test_result = ifelse(
                    rej_rule >= 1,
                    "Reject H_0: cal_V==0; H_a: cal_V!=0", 
                    "Fail to reject H_0: cav_V==0"
                ),
                sic_3 = sic,
                intermediates = var,
                cond = deparse(cond)
            )
        return(tbl)
    }
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

eval_llh_fun<-function(v,f,mu,sigma,params){
    log_lh<-log(E_nrv(f,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,v,mu,sigma))
    return(log_lh)
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
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
            # !is.na(.data[[var]]),
            # .data[[var]] < Inf,
            # .data[[var]] > -Inf
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
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        mutate(
            cal_V = .data[[var]] - log_D
        ) #%>%  
        # filter(
        #     is.finite(cal_V),
        #     is.finite(cal_W),

        # )

    result_list <- list(
        cal_V = tbl$cal_V,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E
    )
    return(result_list)
}

first_stage_panel <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    corp_data <- data %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
        ) %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) #%>%
    fs_reg <- lm(fml, data = corp_data) # %>%
        # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    corp_data$epsilon <- -epsilon

    ## Deconvolution ------------------------

    tbl <- data %>%
        left_join(
            corp_data %>% select(plant, year, epsilon),
            by = c("plant", "year")
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        select(!m) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y, epsilon
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

first_stage_panel_me <- function(sic, var, r_var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    corp_data <- data %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        )
    fs_reg <- lm(fml, data = corp_data) # %>%
    # fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- 1
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    corp_data$epsilon <- -epsilon

    ## Deconvolution ------------------------

    tbl <- data %>%
        left_join(
            corp_data %>% select(plant, year, epsilon),
            by = c("plant", "year")
        ) %>%
        filter(
            sic_3 == sic,
            is.finite(.data[[var]]),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y),
            .data[[var]] > log(threshold_cut)
        ) %>%
        select(!m) %>%
        mutate(
            # y = log(gross_output),
            cal_V = .data[[var]] - log_D,
            m  = log(.data[[r_var]]), #log(materials/sales)+log(sales)=log(materials)
            cal_W = y-beta*(m-cal_V)
        ) %>%
        filter(
            is.finite(cal_V),
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, m, k, l, y, epsilon
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E,
        sic_3 = sic,
        inter = r_var
    )
    return(result_list)
}

## LogNormal and Truncated

## Production Function & Productivity -----------------

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

obj_fun_ar1<-function(alpha,data,params){

    eta <-data %>% 
        ungroup() %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - alpha[1]*k-alpha[2]*l,
            lag_w_eps = lag(w_eps, order_by = year)
        ) %>%
        lm(w_eps~lag_w_eps|lag_k+lag_m, data=., na.action = na.exclude) |>
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

obj_fun_ivar1<-function(alpha,data,params,ins){
    a_k <- 1.3^alpha[1]
    a_l <- 1.3^alpha[2]
    fml <- as.formula(paste0("w_eps ~ lag_w_eps |",ins))

    df <-data %>% 
        ungroup() %>%
        filter(
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m)
        ) %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - a_k*k-a_l*l,
            lag_w_eps = lag(w_eps, order_by = year),
            lag_2_w_eps = lag(w_eps, 2,order_by = year),
            lag_k = lag(k, order_by = year),
            lag_l = lag(l, order_by = year),
            lag_m = lag(m, order_by = year)
            
        )

    eta <- ivreg::ivreg(fml, data=df, na.action = "na.exclude") |>
        residuals()


    moments<-apply(
        df[c("k","l")],
        2, 
        function(i){
        mean(i*eta, na.rm = TRUE)
        }
    )

    obj <- t(moments) %*% moments
    return(sqrt(obj[1]))

}

## Truncated Normal Distribution funs -----------------------------------

f_trc_norm <- function(epsilon,v,mu,sigma){
    x<-sum(epsilon,v)
    alpha <- -mu/sigma
    if (x>=0){
        num<-f_e(epsilon,v,mu,sigma)
        den<- (1-pnorm(alpha,mean=0, sd=1))
        return((1/sigma)*(num/den))
    } else {
       return(1e-300)
    }
}

f_w_n<-function(epsilon,v,mu,sigma,beta){
    x<-sum(v,-(1-beta)*epsilon)
    return(dnorm(x,mean = mu, sd=sigma))
}

eval_llh_fun<-function(v,f,mu,sigma,params,...){
    log_lh<-log(E_nrv(f,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,v,mu,sigma,...))
    return(log_lh)
}

obj_fun_f<- function(theta,f,V,params,...){
    mu<-theta[1]
    sigma<-1.3^(theta[2])
    sum_llh<-sapply(V,eval_llh_fun,f,mu,sigma,params,...) |> sum()
    return(sum_llh)
}

## Bootstrapping -----------------------

resample_by_group<-function(data,...){

    sampled_plants_by_corp <- data %>%
        ungroup() %>%
        mutate(
            Corp = ifelse(juridical_organization==3,"Corp","Other")
        ) %>%
        group_by(...,Corp) %>%
        reframe(
            plant = sample(unique(plant), replace = TRUE)
        ) 

    resampled_data <-sampled_plants_by_corp %>% 
        left_join(
            data,
            by = c("plant","sic_3"),
            relationship = "many-to-many"
        )
    return(resampled_data)
}

bayesian_sampling <- function(data, ..., alpha=4){

    tmp <- data %>%
        ungroup() %>%
        group_by(...) %>%
        reframe(
            unique_plants = unique(plant),
            sampled_plants = sample(
                unique_plants, 
                replace = TRUE, 
                prob = MCMCprecision::rdirichlet(1, rep(alpha, length(unique_plants)))
            )
        ) %>%
        select(plant = sampled_plants, ...)

    resampled_data <- tmp %>%
        left_join(
            data,
            by = c("plant","sic_3"),
            relationship = "many-to-many"
        )

    return(resampled_data)
}


## MLE Deconvulution Functions -------------------

deconvolute_norm<-function(x,prod_fun_list,fs_list){
    alpha <- prod_fun_list[[x]]$coeffs

    params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma
    )

    temp_df <- fs_list[[x]]$data %>%
        filter(
            is.finite(cal_W),
            is.finite(k),
            is.finite(l)
        ) %>%
        mutate(
            W_squiggle = cal_W - alpha[["k"]]*k-alpha[["l"]]*l
        )

    init<-c(
        mu=mean(temp_df$W_squiggle, na.rm = TRUE), 
        sigma=1.3^(sd(temp_df$W_squiggle, na.rm = TRUE))
    )

    res<-optim(
        init,
        obj_fun_f,
        NULL,
        f_w_n,
        temp_df$W_squiggle,
        params,
        alpha[["m"]],
        method = "BFGS",
        control=list(fnscale=-1) #Maximizing instead of minimizing
    )

    mu<-res$par[1]
    sigma<-1.3^(res$par[2])#|> round(6)
    # n<-length(fs_list[[x]]$data$cal_V)
    ev_params<-c(
        mu = mu,
        sigma = sigma,
        mean=mu,
        sd=sigma,
        mode=mu,
        median=mu,
        convergence = res$convergence,
        id = x,
        dist = "normal"
    )
    return(ev_params)
}

deconvolute_norm_iv<-function(x,ins,prod_fun_list,fs_list){
    select <- paste(x,ins)
    alpha <- prod_fun_list[[select]]$coeffs

    params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma
    )

    temp_df <- fs_list[[x]]$data %>%
        filter(
            is.finite(cal_W),
            is.finite(k),
            is.finite(l)
        ) %>%
        mutate(
            W_squiggle = cal_W - alpha[["k"]]*k-alpha[["l"]]*l
        )

    init<-c(
        mu=mean(temp_df$W_squiggle, na.rm = TRUE), 
        sigma=1.3^(sd(temp_df$W_squiggle, na.rm = TRUE))
    )

    res<-optim(
        init,
        obj_fun_f,
        NULL,
        f_w_n,
        temp_df$W_squiggle,
        params,
        alpha[["m"]],
        method = "BFGS",
        control=list(fnscale=-1) #Maximizing instead of minimizing
    )

    mu<-res$par[[1]]
    sigma<-1.3^(res$par[[2]])#|> round(6)
    # n<-length(fs_list[[x]]$data$cal_V)
    ev_params<-c(
        mu = mu,
        sigma = sigma,
        mean=mu,
        sd=sigma,
        mode=mu,
        median=mu,
        convergence = res$convergence,
        sic_3 = stringr::str_extract(x,"\\d{3}"),
        ins = ins,
        dist = "normal"
    )
    return(ev_params)
}

deconvolute_lognorm<-function(x,fs_list){
    params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma
    )

    init<-c(
        mu=mean(fs_list[[x]]$data$cal_V, na.rm = TRUE), 
        sigma=1.3^(sd(fs_list[[x]]$data$cal_V, na.rm = TRUE))
    )

    res<-optim(
        init,
        obj_fun_ln,
        NULL,
        fs_list[[x]]$data$cal_V,
        params,
        method = "BFGS",
        control=list(fnscale=-1) #Maximizing instead of minimizing
    )

    mu<-res$par[[1]]
    sigma<-1.3^(res$par[[2]])#|> round(6)
    mean_lognorm<-exp(mu+0.5*sigma^2)# |> round(6)
    sd_lognorm<-mean_lognorm*sqrt(exp(sigma^2)-1)# |> round(6)
    mode<-exp(mu-sigma^2)# |> round(6)
    # n<-length(fs_list[[x]]$data$cal_V)
    # me<- 1.96*sqrt((sd_lognorm^2)/n+(sd_lognorm^4)/(2*(n-1)))
    ev_params<-c(
        mu = mu,
        sigma = sigma,
        mean=mean_lognorm,
        sd=sd_lognorm,
        mode=mode,
        median=exp(mu),# |> round(6),
        convergence = res$convergence,
        dist = "lognormal",
        sic_3 = fs_list[[x]]$sic_3,
        inter = fs_list[[x]]$inter
        # n = n,
        # mu_LCI = mu_hat*exp(-me),
        # mu_UCI = mu_hat*exp(me)

    )
    return(ev_params)
}

get_trcnorm_stats <- function(mu,sigma){
    alpha <- -mu/sigma
    Z = 1 - pnorm(alpha,mean=0, sd=1)
    phi_a = dnorm(alpha,mean=0,sd=1)
    lambda = phi_a/Z
    mean_trcnorm = mu+sigma*lambda
    num_median = pnorm(alpha,mean=0,sd=1)+1
    median_trcnorm = mu+sigma*qnorm(num_median/2, mean=0, sd=1)
    variance_trcnorm = sigma*sigma*(1-lambda*(lambda-alpha))
    return(list(
        mean = mean_trcnorm,
        sd= sqrt(variance_trcnorm),
        mode = mu,
        median = median_trcnorm,
        variance = variance_trcnorm
    ))
}

deconvolute_trcnorm<-function(x,fs_list){
    params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma
    )
    init<-c(
        mu=mean(fs_list[[x]]$data$cal_V, na.rm = TRUE), 
        sigma=1.3^(sd(fs_list[[x]]$data$cal_V, na.rm = TRUE))
    )
    res<-optim(
        init,
        obj_fun_f,
        NULL,
        f_trc_norm,
        fs_list[[x]]$data$cal_V,
        params,
        method = "BFGS",
        control=list(fnscale=-1)
    )
    mu<-res$par[1]
    sigma<-1.3^res$par[2]

    trc_norm_stas <- get_trcnorm_stats(mu,sigma)
    mean_trcnorm <- trc_norm_stas$mean
    variance_trcnorm <- trc_norm_stas$variance
    median_trcnorm <- trc_norm_stas$median

    ev_params<-c(
        mu = mu,
        sigma = sigma,
        mean=mean_trcnorm, 
        sd= sqrt(variance_trcnorm),
        mode = mu,
        median = median_trcnorm,
        convergence = res$convergence,
        dist = "truncated normal",
        sic_3 = fs_list[[x]]$sic_3,
        inter = fs_list[[x]]$inter
        # n = n#,
        # mu_LCI = mu-1.96*sigma/sqrt(n),
        # mu_UCI = mu+1.96*sigma/sqrt(n)
    )
    return(ev_params)
}

## Estimation of Production Function -------------------

# estimate_prod_fn<-function(x,fs_list,f,...){
#     params<-list(
#         gauss_int=gauss_hermite,
#         epsilon_mu=fs_list[[x]]$epsilon_mu,
#         epsilon_sigma=fs_list[[x]]$epsilon_sigma,
#         beta = fs_list[[x]]$beta
#     )

#     alpha0<-coef(
#         lm(
#             cal_W ~ k+l,
#             fs_list[[x]]$data
#         )
#     )

#     res<-optim(
#         alpha0[-1],
#         f,
#         NULL,
#         fs_list[[x]]$data,
#         params,
#         ...,
#         method = "BFGS",
#         control = list(
#             maxit = 300
#         )
#     )
#     return(
#         list(
#             coeffs=c(
#                 m=fs_list[[x]]$beta,
#                 res$par
#                 ),
#             convergence = res$convergence
#             )
#     )
# }

estimate_prod_fn<-function(x,fs_list,f,ins){
    params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma,
        beta = fs_list[[x]]$beta
    )

    alpha0<-coef(
        lm(
            cal_W ~ k+l,
            fs_list[[x]]$data
        )
    )

    res<-optim(
        alpha0[-1],
        f,
        NULL,
        fs_list[[x]]$data,
        params,
        ins,
        method = "BFGS",
        control = list(
            maxit = 300
        )
    )

    fml <- as.formula(paste0("w_eps ~ lag_w_eps |",ins))

    ivreg_sum<-fs_list[[x]]$data %>% 
        ungroup() %>%
        filter(
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            is.finite(y)
        ) %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - res$par[["k"]]*k-res$par[["l"]]*l,
            lag_w_eps = lag(w_eps, order_by = year),
            lag_2_w_eps = lag(w_eps, 2,order_by = year),
            lag_k = lag(k, order_by = year),
            lag_l = lag(l, order_by = year),
            lag_m = lag(m, order_by = year)          
        ) %>%
        ivreg::ivreg(fml, data=., na.action = "na.omit") |>
        summary(diagnostics =TRUE)

    return(
        list(
            coeffs=c(
                m=fs_list[[x]]$beta,
                k = 1.3^res$par[["k"]],
                l = 1.3^res$par[["l"]]
                ),
            convergence = res$convergence,
            instrument = ins,
            diagnostics = ivreg_sum$diagnostics |>
                as.data.frame() %>%
                mutate(
                    stars = case_when(
                        `p-value` < 0.01 ~ "***",
                        `p-value` < 0.05 ~ "**",
                        `p-value` < 0.1 ~ "*",
                        .default =  ""
                    )
                )
            )
    )
}

obj_fun_ivar1_bounds<-function(alpha,data,params,ins){
    a_k <- alpha[1]
    a_l <- alpha[2]
    fml <- as.formula(paste0("w_eps ~ lag_w_eps |",ins))

    df <-data %>% 
        ungroup() %>%
        filter(
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m)
        ) %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - a_k*k-a_l*l,
            lag_w_eps = lag(w_eps, order_by = year),
            lag_2_w_eps = lag(w_eps, 2,order_by = year),
            lag_k = lag(k, order_by = year),
            lag_l = lag(l, order_by = year),
            lag_m = lag(m, order_by = year)
            
        )

    eta <- ivreg::ivreg(fml, data=df, na.action = "na.exclude") |>
        residuals()


    moments<-apply(
        df[c("k","l")],
        2, 
        function(i){
        mean(i*eta, na.rm = TRUE)
        }
    )

    obj <- t(moments) %*% moments
    return(sqrt(obj[1]))

}

estimate_prod_fn_bounds<-function(x,fs_list,f,ins){
    params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma,
        beta = fs_list[[x]]$beta
    )

    alpha0<-coef(
        lm(
            cal_W ~ k+l,
            fs_list[[x]]$data
        )
    )

    res<-optim(
        alpha0[-1],
        f,
        NULL,
        fs_list[[x]]$data,
        params,
        ins,
        method = "L-BFGS-B",
        lower = c(0,0),
        upper = c(1,1),
        control = list(
            maxit = 300
        )
    )

    fml <- as.formula(paste0("w_eps ~ lag_w_eps |",ins))

    ivreg_sum<-fs_list[[x]]$data %>% 
        ungroup() %>%
        filter(
            is.finite(cal_W),
            is.finite(k),
            is.finite(l),
            is.finite(m)
        ) %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - res$par[["k"]]*k-res$par[["l"]]*l,
            lag_w_eps = lag(w_eps, order_by = year),
            lag_2_w_eps = lag(w_eps, 2,order_by = year),
            lag_k = lag(k, order_by = year),
            lag_l = lag(l, order_by = year),
            lag_m = lag(m, order_by = year)
            
        ) %>%
        ivreg::ivreg(fml, data=.) |>
        summary(diagnostics =TRUE)

    return(
        list(
            coeffs=c(
                m=fs_list[[x]]$beta,
                k = res$par[["k"]],
                l = res$par[["l"]]
                ),
            convergence = res$convergence,
            instrument = ins,
            diagnostics = ivreg_sum$diagnostics |>
                as.data.frame() %>%
                mutate(
                    stars = case_when(
                        `p-value` < 0.01 ~ "***",
                        `p-value` < 0.05 ~ "**",
                        `p-value` < 0.1 ~ "*",
                        .default =  ""
                    )
                )
            )
    )
}



## Saving functions --------------------

print("Saving functions")
save(list=ls(), file="Code/Products/deconv_funs.Rdata")