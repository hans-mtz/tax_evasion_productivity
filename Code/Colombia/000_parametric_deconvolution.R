## Load packages -----
### optimizer
library(nloptr)
library(fixest)
### Load data
load("Code/Products/colombia_data.RData")
## Deconvolution
### First learn g

logl_g <- function(theta,data,select){
    X <- cbind(1,data[,select])
    k <- ncol(X)
    x_beta <- cbind(1,data[,select])%*%theta[1:k]
    e_u <- data[,c("log_share")]-x_beta
    return(-sum(dnorm(e_u, theta[k+1], theta[k+2], log = TRUE)))
}

logl_g_u <- function(theta,u){
    return(-sum(dnorm(u, theta[1], theta[2], log = TRUE)))
}


summary(
    feols(
        log_share~share_sales_tax+age+lag_log_sales+total_owners+factor(sic_3)+factor(metro_area_code)+factor(year),
        data=colombia_data_frame
    )
)

u_hat<-residuals(
    feols(
        log_share~share_sales_tax+age+lag_log_sales+total_owners+factor(sic_3)+factor(metro_area_code)+factor(year),
        data=colombia_data_frame
    )
)
logl_g_u(c(0,1),u_hat)

g_res<-optim(c(0,1), logl_g_u, u = u_hat)


### Using g to learn f

frechet <- function(x, alpha, s, m=0){
    # x>m
    # alpha \in (0,Inf)
    # s \in (0,Inf)
    #  m\in (-Inf,Inf)
    interior <- (x-m)/s
    A <- alpha/s
    B <- interior^(-1-alpha)
    C <- exp(-(interior^(-alpha)))
    return(A*B*C)
}

integrand <- function(u,e,mu,sigma,alpha,s){
    x<-log(e+u)
    frechet(x,alpha,s)*dnorm(u,mu,sigma)/(x)
}

logl <- function(e,mu,sigma,alpha,s){
    l <- integrate(integrand,0,Inf,e=e,mu=mu,sigma=sigma,alpha=alpha,s=s)
    return(log(l))
}

obj_f<-function(theta,log_share,mu,sigma){
    # mu <- theta[1]
    # sigma <- theta[2]
    alpha <- theta[1]
    s <- theta[2]
    m <- theta[3]
    beta <- theta[4]
    e_u <- log_share - beta
    return(-sum(sapply(u,logl,e=e_u,mu=mu,alpha=alpha,s=s,m=m)))
}