## %% Set up libraries --------------------------

library(splines)
library(statmod)
library(parallel)
set.seed(557788)
## %% Testing bs function --------------------------

a <- 0
b <- 2
# n_splines <- 10
n_nodes <- 10
n_obs <- 2000

n_knots <- round(3*(log(n_obs))^(1/2))
n_knots
round(1.3*n_obs^(1/5))
n_knots<-round(1.3*n_obs^(1/5))
n_knots
# n_knots <- 7

## %% Simulate Data --------------------------
# n_sims <- 3000
eps <- rnorm(n_obs,  mean = 0, sd = 0.35)
ev <- rlnorm(n_obs, meanlog = -1.8, sdlog = 0.95)
V <- -eps + ev
x <- seq(a, range(V)[2], length.out = 512)

y_max <- max(hist(ev)$density, hist(V)$density, hist(eps)$density)
plot(range(V), c(0, y_max), type = "n", xlab = "x", ylab = "")
hist(ev, col = rgb(0, 0.5, 1, 0.5), freq = FALSE, breaks = 30, add = TRUE)  # Light blue
hist(V, col = rgb(1, 0.6, 0, 0.5), freq = FALSE, breaks = 60, add = TRUE)  # Orange
hist(eps, col = rgb(0.6, 0, 0.6, 0.5), freq = FALSE, breaks = 30, add = TRUE)  # Purple

y_max <- max(density(ev)$y, density(V)$y, density(eps)$y)
plot(range(V), c(0, y_max), type = "n", xlab = "x", ylab = "")
lines(density(ev), col = "blue", lwd = 2)
lines(density(V), col = "red", lwd = 2)
lines(density(eps), col = "purple", lwd = 2)
legend(
    "topright",
    c("e", "V", "eps"),
    col = c("blue", "red", "purple"),
    lwd = 2,
    bty = "n",
    inset = c(0.05, 0.05)
)

## %% 9 Spline basis --------------------------
# knots <- seq(a,b, length.out = n_knots+2)
# knots <- c(seq(a,1, length.out = 6),seq(1.01,b, length.out = 3))
# bs_mat <- ns(
#     x, 
#     # df = n_splines,
#     knots = knots[-c(1,length(knots))],
#     # degree = 3, 
#     intercept = TRUE, 
#     Boundary.knots = c(min(knots),max(knots))
# )


# plot(range(x), c(0,1), type = "n", xlab = "x", ylab = "")
# matlines(x, bs_mat, type="l",lty = 1, ylim = c(0, 1))
# lines(x, rowSums(bs_mat), col = "lightgray", lwd = 2, lty =3)
# abline(v=attributes(bs_mat)$knots, col = "red", lty = 2)
# abline(v=attributes(bs_mat)$Boundary.knots, col = "darkred", lty = 2)
## %% B Spline basis --------------------------

# knots <- c(seq(a,1, length.out = 7),seq(1.01,b, length.out = 3))
# knots
# Higher density of knots at the lower end of the range
get_my_knots_recursive <- function(a,b,n_knots, breakpoint){
    m <- (a+b)/2
    if (breakpoint > 2) {
        return(seq(a,b, length.out = n_knots))
    } else {
        return(c(
            get_my_knots_recursive(a, m, n_knots, breakpoint + 1),
            get_my_knots_recursive(m, b, n_knots, 3)
        ))
    }
}

## %% B Spline basis --------------------------
# knots <- get_my_knots_recursive(a,b,2, 0)
# knots
# plot(range(knots), c(0,1), type = "n", xlab = "x", ylab = "")
# abline(v=knots, col = "red", lty = 2)

## Equally spaced knots
# knots <- seq(a,b, length.out = n_knots+2)
# knots

# bs_mat <- bs(
#     x, 
#     # df = n_splines+1,
#     knots = knots[-c(1,length(knots))],
#     degree = 3, 
#     intercept = TRUE, 
#     Boundary.knots = c(a,b)
# )

# plot(range(x), c(0,1), type = "n", xlab = "x", ylab = "")
# matlines(x, bs_mat[,-ncol(bs_mat)], lty = 1, ylim = c(0, 1))
# lines(x, rowSums(bs_mat[,-ncol(bs_mat)]), col = "lightgray", lwd = 2, lty =3)
# abline(v=c(boundary_knots,knot_candidates), col = "red", lty = 2)
# bs_mat |> dim()


## Equally spaced percentiles of observed data

knots <- quantile(V[V>=0], seq(0, 1, length.out = n_knots+2))

bs_mat <- bs(
    sort(V[V>=0]), 
    # df = n_knots+4,
    knots = knots[-c(1,length(knots))],
    degree = 3, 
    intercept = FALSE, 
    Boundary.knots = knots[c(1,length(knots))]
)

plot(range(V[V>=0]), c(0,1), type = "n", xlab = "x", ylab = "")
matpoints(sort(V[V>=0]), bs_mat[,-ncol(bs_mat)], lty = 1, ylim = c(0, 1), type = "l")
lines(sort(V[V>=0]), rowSums(bs_mat[,-ncol(bs_mat)]), col = "lightgray", lwd = 2, lty =3)
abline(v=attributes(bs_mat)$knots, col = "red", lty = 2)
abline(v=attributes(bs_mat)$Boundary.knots, col = "blue", lty = 2)
bs_mat |> dim()

# lines(x, rowSums(bs_mat[,-ncol(bs_mat)]), col = "lightgray", lwd = 2, lty =3)

## %% Gauss Quad --------------------------
# n_nodes <- 15
# gauss_hermite<-gauss.quad(n_nodes,"hermite") # w(x) = exp(-x^2); x in [-Inf, Inf)
gauss_laguerre<-gauss.quad(n_nodes,"laguerre") # w(x) = exp(-x); x in [0,Inf)
gauss_legendre<-gauss.quad(n_nodes,"legendre") # w(x) = 1; x in [-1,1]

lcv<-function(x,a,b) {
    new_nodes <- (x+1)*(b-a)/2 + a
    return(new_nodes)
}

lcv(gauss_legendre$nodes, a, b) # x in [a,b]
cv<-function(x,b) {
    new_nodes <- b-b*exp(-x) # x in [0,Inf)
    return(new_nodes)
}
cv(gauss_laguerre$nodes, b) # x in [0,b]

get_integral_nodes_rec <- function(a, b, breakpoint, gauss_legendre){
    m <- (a+b)/2
    if (breakpoint > 2) {
        return(
            c(lcv(gauss_legendre$nodes, a, b))
        )
    } else {
        return(
            c(get_integral_nodes_rec( a, m, breakpoint + 1, gauss_legendre),
            get_integral_nodes_rec(m, b, 3, gauss_legendre) )
        )
    }
}
range(V[V>=0])[2]
quad_nodes <- get_integral_nodes_rec(a, range(V[V>=0])[2], -1, gauss_legendre)

plot(range(V[V>=0]), c(0,1), type = "n", xlab = "x", ylab = "")
matlines(sort(V[V>=0]), bs_mat[,-ncol(bs_mat)], lty = 1, ylim = c(0, 1))
lines(sort(V[V>=0]), rowSums(bs_mat[,-ncol(bs_mat)]), col = "lightgray", lwd = 2, lty =3)
abline(v = quad_nodes, col = "red", lty = 2)
length(quad_nodes)

# quad_nodes <- get_integral_nodes_rec(a, b, 0, gauss_legendre)
# plot(range(x), c(0,1), type = "n", xlab = "x", ylab = "")
# matlines(x, bs_mat[,-ncol(bs_mat)], lty = 1, ylim = c(0, 1))
# lines(x, rowSums(bs_mat[,-ncol(bs_mat)]), col = "lightgray", lwd = 2, lty =3)
# abline(v = quad_nodes, col = "red", lty = 2)
# length(quad_nodes)
## %% Define Parameters --------------------------

params <- list(
    a = a,
    b = range(V[V>=0])[2],#b,
    gauss_legendre = gauss_legendre,
    epsilon_mu = 0,
    epsilon_sd = 0.35,
    bspline = attributes(bs_mat)
)


## %% Define Non-Parametric MLE Functions --------------------------

s <- function(e,theta, params){
    theta_0 <- c(theta,0)
    bsplines <- bs(
        e, 
        # degree = params$bspline$degree,
        knots = params$bspline$knots,
        intercept = params$bspline$intercept,
        Boundary.knots = params$bspline$Boundary.knots,
        warn.outside = FALSE
    )
    # cat("bsplines:",dim(bsplines),"\n")
    s <- bsplines %*% theta_0
    return(s[,1])
}


s(lcv(gauss_legendre$nodes, a, b)[1], rep(1, ncol(bs_mat)-1),params)


# s(lcv(gauss_legendre$nodes, a, b), 1:(n_knots+1),params)

# s(lcv(gauss_legendre$nodes, a, b), 0.5*(1:(n_knots+1)),params)

get_integral_recursive <- function(f, a, b, breakpoint, gauss_legendre,...){
    m <- (a+b)/2
    if (breakpoint > 2) {
        return(
            sum(
                f(lcv(gauss_legendre$nodes, a, b), ...) * gauss_legendre$weights
            )*((b-a)/2))
    } else {
        return(
            get_integral_recursive(f, a, m, breakpoint + 1, gauss_legendre, ...) +
            get_integral_recursive(f, m, b, 3, gauss_legendre, ...)
        )
    }
}

C_recursive <- function(theta,params){
    # inner_int <- exp(s(lcv(params$gauss_legendre$nodes, params$a, params$b), theta,params))
    # int <- sum(inner_int*params$gauss_legendre$weights)*((params$b-params$a)/2)
    int <- get_integral_recursive(
        \(x, theta, params){
            exp(s(x, theta,params))
        },
        params$a,
        params$b,
        0,
        params$gauss_legendre,
        theta = theta,
        params = params
    )
    return(log(int))
}

C_recursive(rep(1, ncol(bs_mat)-1),params)

C <- function(theta,params){
    inner_int <- exp(s(lcv(params$gauss_legendre$nodes, params$a, params$b), theta,params))
    int <- sum(inner_int*params$gauss_legendre$weights)*((params$b-params$a)/2)
    return(log(int))
}

C(rep(1, ncol(bs_mat)-1),params)
# C(0.5*(1:(n_knots+1)),params)

f_e<-function(e,theta,params){
    f_e <- exp(s(e, theta,params)-C_recursive(theta,params))
    return(f_e)
}
f_e(1.5, rep(1, ncol(bs_mat)-1),params)

plot(x, f_e(x, rep(1, ncol(bs_mat)-1),params), type = "l", col = "blue", lwd = 2)

plot(sort(V[V>=0]), f_e(sort(V[V>=0]), rep(1, ncol(bs_mat)-1),params), type = "l", col = "blue", lwd = 2)
## %% Log-Likelihood Function --------------------------

inner_int <- function(e,v_i,theta, params){
    x_i <- e-v_i
    return(
        dnorm(x_i,mean=params$epsilon_mu, sd=params$epsilon_sd)*exp(s(e,theta,params))
    )
}

integral <- function(v_i, theta, params){
    A<-sum(inner_int(lcv(params$gauss_legendre$nodes, params$a, params$b),v_i,theta,params)*params$gauss_legendre$weights)
    B<- A*((params$b-params$a)/2)
    return(B)
}

log_likelihood <- function(theta, V, params){
    eval_ll_i <- sapply(
        V,
        function(v_i){
            A <- integral(v_i, theta, params)
            B <- log(A)
        }
    )
    llh <- sum(eval_ll_i)-length(V)*C_recursive(theta,params)
    return(llh)
}

log_likelihood_rec <- function(theta, V, params){
    eval_ll_i <- sapply(
        V,
        function(v_i){
            A <- get_integral_recursive(
                inner_int,
                params$a,
                params$b,
                0,
                params$gauss_legendre,
                v_i=v_i, theta=theta, params=params)
            B <- log(A)
        }
    )
    llh <- sum(eval_ll_i)-length(V)*C_recursive(theta,params)
    return(llh)
}

log_likelihood_rec_par <- function(theta, V, params){
    eval_ll_i <- mclapply(
        V,
        function(v_i){
            A <- get_integral_recursive(
                inner_int,
                params$a,
                params$b,
                0,
                params$gauss_legendre,
                v_i=v_i, theta=theta, params=params)
            B <- log(A)
        },
        mc.cores = detectCores()-2
    )
    eval_ll_i <- do.call(rbind,eval_ll_i)
    llh <- sum(eval_ll_i)-length(V)*C_recursive(theta,params)
    return(llh)
}


# system.time(
#     log_likelihood_rec(
#         rep(1,ncol(bs_mat)-1),
#         V,
#         params
#     )
# )
# system.time(
#     log_likelihood(
#         rep(1,ncol(bs_mat)-1),
#         V,
#         params
#     )
# )
# system.time(
#     log_likelihood_rec_par(
#         rep(1,ncol(bs_mat)-1),
#         V,
#         params
#     )
# )




## %% Saving Data --------------------------

save(
    list = ls(),
    file = "Code/Products/bs-mle-data.RData"
)

## %% Estimate Log-Likelihood Function --------------------------

get_init <- function(V,bspline_spec){
    ## This function is used to initialize the optimization
    dens_v <- density(V[V>=0], n = 512)
    v_linear_fit <- lm(log(dens_v$y+1e-5) ~ bs(dens_v$x,
        knots = bspline_spec$knots,
        intercept = bspline_spec$intercept,
        Boundary.knots = bspline_spec$Boundary.knots,
        warn.outside = FALSE
    )-1)
    init <- coef(v_linear_fit)[-ncol(v_linear_fit$qr$qr)]
    return(init)
}

init <- get_init(V, params$bspline)
# init <- coef(lm(V~ ns(
#         V, 
#         # degree = params$bspline$degree,
#         knots = params$bspline$knots,
#         intercept = params$bspline$intercept,
#         Boundary.knots = params$bspline$Boundary.knots
#     )-1))

# init
# names(init) <- paste0("bs",1:length(init))
# init

log_likelihood_rec_par(
    init,
    V,
    params
)

log_likelihood_rec_par(
    init,
    V,
    params
)

## %% Optimization --------------------------

res<-optim(
    par = init,
    fn = log_likelihood_rec_par,
    V = V,
    params = params,
    method = "BFGS",
    control = list(
        maxit = 200,
        reltol = 1e-6,
        fnscale = -1,
        trace = 3,
        REPORT = 20
    )
    # lower = rep(-Inf, n_splines),
    # upper = rep(Inf, n_splines)
)

res
# res<-optim(
#     par = res$par,
#     fn = log_likelihood,
#     V = V,
#     params = params,
#     method = "SANN"
#     # lower = rep(-Inf, n_splines),
#     # upper = rep(Inf, n_splines)
# )

res
png(
    filename = "Code/Products/bs-mle.png",
    width = 800,
    height = 600,
    res = 100
)
plot(density(ev), col = "#1b7e1b", lwd = 2)
lines(density(V), col = "red", lwd = 2)
lines(x, f_e(x, res$par, params), type = "l", col = "blue", lwd = 2)
dev.off()

# mean_bs<-sum(
#  (lcv(gauss_legendre$nodes,a,b)*f_e(lcv(gauss_legendre$nodes,a,b), res$par, params))*gauss_legendre$weights
# )*((b-a)/2)
# mean_bs

compare_tbl<-data.frame(
    type = c("true", "estimated"),
    mean = c(
        mean(ev),
        get_integral_recursive(\(x,...)x*f_e(x,...), a, params$b, 0, gauss_legendre, theta = res$par, params = params)
    ),
    sd = c(
        sd(ev),
        sqrt(
            get_integral_recursive(\(x,...)x^2*f_e(x,...), a, params$b, 0, gauss_legendre, theta = res$par, params = params) -
            (get_integral_recursive(\(x,...)x*f_e(x,...), a, params$b, 0, gauss_legendre, theta = res$par, params = params))^2
        )
    )
)
compare_tbl

mean(ev)
get_integral_recursive(\(x,...)x*f_e(x,...), params$a, params$b, 0, gauss_legendre, theta = res$par, params = params)

# C(res$par,params)
# exp(s(lcv(gauss_legendre$nodes,a,b), res$par,params))
# f_e(lcv(gauss_legendre$nodes,a,b), res$par, params)

png(
    filename = "Code/Products/bs-mle-quad-points.png",
    width = 800,
    height = 600,
    res = 100
)

plot(range(quad_nodes), c(0, max(density(ev)$y, f_e(quad_nodes, res$par, params))), type = "n", xlab = "x", ylab = "")
lines(quad_nodes, f_e(quad_nodes, res$par, params), type = "l", col = "blue", lwd = 2)
lines(quad_nodes, dlnorm(quad_nodes, meanlog = -1.8, sdlog = 0.95), col = "darkgreen", lwd = 2)
abline(v = quad_nodes, col = "red", lty = 2)
# title()
dev.off()

## BIC knot deletion  ---------------------------
load("Code/Products/bs-mle-data.RData")

BIC <- function(theta,V, llh){
    n <- length(V)
    k <- length(theta)
    # llh <- log_likelihood_rec_par(theta, V, params)
    return(-2*llh + log(n)*(k-1))
}
BIC(res$par, V, res$value)
select_irrelevant_knot <- function(theta, V, llh, params){
    bs_knots <- params$bspline$knots
    BIC_0 <- BIC(theta, V, llh)
    df<-mclapply(
        seq_along(bs_knots),
        function(i){
            bs_knots_i <- bs_knots[-i]
            params_i <- params
            params_i$bspline$knots <- bs_knots_i

            init <- get_init(V, params_i$bspline)
            res_i <- optim(
                par = init,
                fn = log_likelihood_rec_par,
                V = V,
                params = params_i,
                method = "BFGS",
                control = list(
                    maxit = 200,
                    reltol = 1e-6,
                    fnscale = -1,
                    trace = 3,
                    REPORT = 20
                )
            )
            theta_i <- res_i$par
            llh_i <- res_i$value
            return(list(
                knot_index = i,
                theta_i = theta_i,
                params_i = params_i,
                BIC = BIC(theta_i, V, llh_i),
                delta_BIC = BIC_0 - BIC(theta_i, V, llh_i)
            )
            )
        },
        mc.cores = detectCores()-2
    )
    delta_BIC_df <- sapply(
        seq_along(bs_knots),
        \(x)c(knot_index = df[[x]]$knot_index, delta_BIC=df[[x]]$delta_BIC)
    )
    delta_BIC_df <- data.frame(t(delta_BIC_df))

    cat("Biggest drop in BIC",max(delta_BIC_df$delta_BIC),"by removing knot ",which.max(delta_BIC_df$delta_BIC),"\n")
    return(df)
}

(select_irrelevant_knot(res$par, V, res$value, params) -> select_df)
sapply(
        seq_along(select_df),
        \(x)c(knot_index = select_df[[x]]$knot_index, delta_BIC=select_df[[x]]$delta_BIC)
    ) |> t() |> data.frame() 

plot_logspline_d <- function(theta,params,ev=ev){

    my_plot <-plot(range(x), c(0, max(density(ev)$y, f_e(x, theta, params))), type = "n", xlab = "x", ylab = "")
    lines(x, f_e(x, theta, params), type = "l", col = "darkgreen", lwd = 2)
    hist(ev, col = rgb(0, 0.5, 1, 0.5), freq = FALSE, breaks = 30, add = TRUE)  # Light blue
    lines(x, dlnorm(x, meanlog = -1.8, sdlog = 0.95), col = "blue", lwd = 2)
    # abline(v = quad_nodes, col = "lightgray", lty = 2)
    abline(v = params$bspline$knots, col = "red", lty = 2)
    legend(
        "topright",
        c("Estimated f_e", "True e", "V"),
        col = c("darkgreen", "blue", "lightblue"),
        lwd = 2,
        bty = "n",
        inset = c(0.05, 0.05)
    )
    return(my_plot)
}


plot_logspline_d(res$par, params, ev=ev)
plot_logspline_d(select_df[[3]]$theta_i, select_df[[3]]$params_i, ev=ev)
plot_logspline_d(select_df[[4]]$theta_i, select_df[[4]]$params_i, ev=ev)


## Remove first knot ----------------------------

# Selected inner knot = 1

params_i <- params
params_i$bspline$knots <- params$bspline$knots[-3]

res_i<-optim(
    # par = rep(1,length(params_i$bspline$knots)+3),
    par = res$par[-1],
    fn = log_likelihood_rec_par,
    V = V,
    params = params_i,
    method = "BFGS",
    control = list(
        maxit = 200,
        reltol = 1e-6,
        fnscale = -1,
        trace = 3,
        REPORT = 20
    )
    # lower = rep(-Inf, n_splines),
    # upper = rep(Inf, n_splines)
)
res_i
ftr<- 1#max(f_e(quad_nodes, res_i$par, params_i))/max(density(ev)$y)
plot(range(quad_nodes), c(0, max(density(ev)$y, f_e(quad_nodes, res_i$par, params_i))/ftr), type = "n", xlab = "x", ylab = "")
lines(quad_nodes, f_e(quad_nodes, res_i$par, params_i)/ftr, type = "l", col = "blue", lwd = 2)
lines(quad_nodes, dlnorm(quad_nodes, meanlog = -1.8, sdlog = 0.95), col = "darkgreen", lwd = 2)
abline(v = quad_nodes, col = "gray", lty = 2)
abline(v = params_i$bspline$knots, col = "purple", lty = 2)

BIC(res$par, V, params)
BIC(res_i$par, V, params_i)

(select_irrelevant_knot(res_i$par, V, params_i) -> bic_select)
sapply(
        seq_along(bic_select),
        \(x)c(
            knot_index = bic_select[[x]]$knot_index,
            BIC=bic_select[[x]]$BIC,
            delta_BIC=bic_select[[x]]$delta_BIC)
    ) |> t() |> data.frame()

## Potential knots 1,2, and 5
bic_select[[5]]$theta_i
bic_select[[5]]$params_i$bspline$knots



## Delete knot 5 ----------------------------

select_irrelevant_knot(bic_select[[5]]$theta_i, V, bic_select[[5]]$params_i) -> bic_select_2
sapply(
        seq_along(bic_select_2),
        \(x)c(
            knot_index = bic_select_2[[x]]$knot_index,
            BIC=bic_select_2[[x]]$BIC,
            delta_BIC=bic_select_2[[x]]$delta_BIC)
    ) |> t() |> data.frame()

ftr<- max(f_e(quad_nodes, bic_select_2[[1]]$theta_i, bic_select_2[[1]]$params_i))/max(density(ev)$y)
plot(range(quad_nodes), c(0, max(density(ev)$y, f_e(quad_nodes, bic_select_2[[1]]$theta_i, bic_select_2[[1]]$params_i))/ftr), type = "n", xlab = "x", ylab = "")
lines(quad_nodes, f_e(quad_nodes, bic_select_2[[1]]$theta_i, bic_select_2[[1]]$params_i)/ftr, type = "l", col = "blue", lwd = 2)
lines(quad_nodes, dlnorm(quad_nodes, meanlog = -1.8, sdlog = 0.95), col = "darkgreen", lwd = 2)
abline(v = quad_nodes, col = "gray", lty = 2)
abline(v = bic_select_2[[1]]$params_i$bspline$knots, col = "purple", lty = 2)


select_irrelevant_knot(bic_select_2[[1]]$theta_i, V, bic_select_2[[1]]$params_i) -> bic_select_3

sapply(
        seq_along(bic_select_3),
        \(x)c(
            knot_index = bic_select_3[[x]]$knot_index,
            BIC=bic_select_3[[x]]$BIC,
            delta_BIC=bic_select_3[[x]]$delta_BIC)
    ) |> t() |> data.frame()

ftr<- max(f_e(quad_nodes, bic_select_3[[1]]$theta_i, bic_select_3[[1]]$params_i))/max(density(ev)$y)
plot(range(quad_nodes), c(0, max(density(ev)$y, f_e(quad_nodes, bic_select_3[[1]]$theta_i, bic_select_3[[1]]$params_i))/ftr), type = "n", xlab = "x", ylab = "")
lines(quad_nodes, f_e(quad_nodes, bic_select_3[[1]]$theta_i, bic_select_3[[1]]$params_i)/ftr, type = "l", col = "blue", lwd = 2)
lines(quad_nodes, dlnorm(quad_nodes, meanlog = -1.8, sdlog = 0.95), col = "darkgreen", lwd = 2)
abline(v = quad_nodes, col = "gray", lty = 2)
abline(v = bic_select_3[[1]]$params_i$bspline$knots, col = "purple", lty = 2)

compare_tbl<-data.frame(
    type = c("true", "estimated"),
    mean = c(
        mean(ev),
        get_integral_recursive(\(x,...)x*f_e(x,...), bic_select_3[[1]]$params_i$a, bic_select_3[[1]]$params_i$b, 0, gauss_legendre, theta = bic_select_3[[1]]$theta_i, params = bic_select_3[[1]]$params_i)
    ),
    sd = c(
        sd(ev),
        sqrt(
            get_integral_recursive(\(x,...)x^2*f_e(x,...), bic_select_3[[1]]$params_i$a, bic_select_3[[1]]$params_i$b, 0, gauss_legendre, theta = bic_select_3[[1]]$theta_i, params = bic_select_3[[1]]$params_i) -
            (get_integral_recursive(\(x,...)x*f_e(x,...), bic_select_3[[1]]$params_i$a, bic_select_3[[1]]$params_i$b, 0, gauss_legendre, theta = bic_select_3[[1]]$theta_i, params = bic_select_3[[1]]$params_i))^2
        )
    )
)
compare_tbl

## Knots Example --------------------------

knots <- c(1,1.8,3:5,6.5,7,8.1,9.2,10)  # 10 => 10-4 = 6 Basis splines
x <- seq(min(knots)-1, max(knots)+1, length.out = 501)
bb <- splineDesign(knots, x = x[x>=4&x<=7], outer.ok = FALSE)

plot(range(x), c(0,1), type = "n", xlab = "x", ylab = "",
     main =  "B-splines - sum to 1 inside inner knots")
mtext(expression(B[j](x) *"  and "* sum(B[j](x), j == 1, 6)), adj = 0)
abline(v = knots, lty = 3, col = "light gray")
abline(v = knots[c(4,length(knots)-3)], lty = 3, col = "gray10")
lines(x[x>=4&x<=7], rowSums(bb), col = "gray", lwd = 2)
matlines(x[x>=4&x<=7], bb, ylim = c(0,1), lty = 1)
