## Load packages -----
### optimizer
library(nloptr)
library(fixest)
### Load data
load("Code/Products/colombia_data.RData")
## Deconvolution
### First learn g

logl_g <- function(theta, data, select) {
    X <- cbind(1, data[, select])
    k <- ncol(X)
    x_beta <- cbind(1, data[, select]) %*% theta[1:k]
    e_u <- data[, c("log_share")] - x_beta
    return(-sum(dnorm(e_u, theta[k + 1], theta[k + 2], log = TRUE)))
}

logl_g_u <- function(theta, u) {
    return(-sum(dnorm(u, theta[1], theta[2], log = TRUE)))
}


summary(
    feols(
        log_share ~ share_sales_tax + age + lag_log_sales + total_owners + factor(sic_3) + factor(metro_area_code) + factor(year),
        data = colombia_data_frame
    )
)

u_hat <- residuals(
    feols(
        log_share ~ share_sales_tax + age + lag_log_sales + total_owners + factor(sic_3) + factor(metro_area_code) + factor(year),
        data = colombia_data_frame
    )
)
logl_g_u(c(0, 1), u_hat)

g_res <- optim(c(0, 1), logl_g_u, u = u_hat)


### Using g to learn f
mu <- g_res$par[1]
sigma <- g_res$par[2]

frechet <- function(x, alpha, s, m = 0) {
    # x>m
    # alpha \in (0,Inf)
    # s \in (0,Inf)
    #  m\in (-Inf,Inf)
    if (x <= m) {
        return(0)
    } else {
        interior <- (x - m) / s
        A <- alpha / s
        B <- interior^(-1 - alpha)
        C <- exp(-(interior^(-alpha)))
        return(A * B * C)
    }
}

frech_v <- Vectorize(frechet, vectorize.args = "x")

integrand <- function(e, u, mu, sigma, alpha, s) {
    dnorm(-u + e, mu, sigma) * frech_v(e, alpha, s)
}

logl <- function(u, mu, sigma, alpha, s) {
    l <- integrate(
        integrand,
        lower = 0,
        upper = Inf,
        u = u,
        mu = mu,
        sigma = sigma,
        alpha = alpha,
        s = s
    )
    if(l$value<=0){
        -9e100
    } else {
        return(log(l$value))
    }
}

##
obj_f <- function(theta, mu, sigma, formula, data) {
    alpha <- exp(theta[1])
    s <- exp(theta[2])
    df <- model.frame(formula, data)
    y <- df[, 1]
    X <- model.matrix(formula, df)
    k <- length(X[1, ])
    beta <- theta[3:(2 + k)]
    u <- y - X %*% beta

    # return(-sum(sapply(u, logl, mu = mu, sigma = sigma, alpha = alpha, s = s)))
    return(u)
}

## testing functions -----

integrand(c(0,1,-1),0,mu,sigma,1,1)
sapply(c(0,1,-41), logl, mu = mu, sigma = sigma, alpha = 1, s = 1)
fml <- log_share ~ share_sales_tax+age+lag_log_sales+total_owners+factor(sic_3)+factor(metro_area_code)+factor(year)
k<-length(model.matrix(fml, colombia_data_frame)[1,])
obj_f(rep(1,k+3),mu=mu,sigma=sigma,formula=fml,data=colombia_data_frame)
optim(rep(0,k+3), obj_f, mu=mu, sigma=sigma, formula=fml, data=colombia_data_frame)
