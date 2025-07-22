# %% Simular datos
set.seed(123)
n_v <- 1000
n_eps <- 500
eps <- rnorm(n_v, mean = 0, sd = 0.2)  # Variable epsilon
e <- rnorm(n_v, mean = 0.5, sd = 0.5)   # Variable e
v <-  eps + e  # Observaciones de v
gauss_hermite <- statmod::gauss.quad(20, "hermite")  # Cuadratura de Gauss-Hermite
gauss_legendre <- statmod::gauss.quad(20, "legendre")  # Cuadratura de Gauss-Legendre
eps_obs <- sample(eps, n_eps, replace = TRUE)


# %% Empirical characteristic function -----------------

varphi <- function(t, x) {
    # Función característica empírica
    vec <- sapply(
        t,
        function(t_i) {
            mean(
                exp(1i * t_i * x),
                na.rm = TRUE
            )   
        }
    )
    return(vec)
}

# varphi(100, fs_list[[1]]$data$epsilon)
varphi(1:100, eps)
curve(varphi(x, eps), from = -30, to = 30, n = 2048, col = "blue", lwd = 2,
      xlab = "t", ylab = "Characteristic Function", main = "Empirical Characteristic Function of v")
varphi_K <- function(t, kernel = "normal", kappa = 2) {

    k <- switch(
        kernel,
        "normal" = exp(-t^2/2),
        "sinc" = ifelse(abs(t) <= 1, 1, 0),
        "k2" = ifelse(abs(t) <= 1, (1 - t^2)^3, 0),
        "lutkenoner" = ifelse(abs(t) <= 1, (cos(pi*t/2))^kappa, 0)
    )

    return(k)
}

varphi_K(1, kernel = "normal")
curve(varphi_K(x, kernel = "k2"), from = -3, to = 3, n = 2048, col = "blue", lwd = 2,
      xlab = "t", ylab = "Kernel Function", main = "Kernel Function for Deconvolution")


K_U <- function(x, v, u, h, kernel = "normal", kappa = 2, quad = "hermite") {
    out <- switch(
        quad,
        "hermite" = gauss_hermite,
        "legendre" = gauss_legendre
    )
    m <- length(u)
    vec <- sapply(
        x,
        function(x_i) {
            aux <- function(t) {
                exp(-1i * x_i * t) * varphi_K(t, kernel, kappa) * ifelse(abs(varphi(t/h, u))>=1/sqrt(m) ,1 ,0 )/ varphi(t/h, u)
            }
            # cat("aux dim:", length(aux(gauss_hermite$nodes)), "\n")
            int <- sum(aux(out$nodes) * out$weights * ifelse(quad=="hermite", exp(gauss_hermite$nodes^2), 1))
            return(int / 2 * pi)
        }
    )
    return(vec)
}
K_U(0:3, v, eps, h = bw.nrd0(v), kernel = "k2", quad = "legendre")
curve(K_U(x, v, eps, h = bw.nrd0(v), kernel = "k2", quad = "legendre"), from = -1, to = 1, n = 512, col = "blue", lwd = 2,
      xlab = "x", ylab = "Kernel Function", main = "Kernel Function K_U for Deconvolution")
# K_U(1,fs_list[[1]]$data$epsilon |> na.omit(), h = bw.nrd0(fs_list[[1]]$data$epsilon |> na.omit()), kernel = "lutkenoner")

f_x <- function(x, x_star, u, bw = "nrd0", kernel = "normal", kappa = 2) {
    
    h <- switch(
        bw,
        "nrd0" = bw.nrd0(x_star),
        "nrd" = bw.nrd(x_star),
        "SJ-ste" = bw.SJ(x_star, method = "ste"),
        "SJ-dpi" = bw.SJ(x_star, method = "dpi"),
        "ucv" = bw.ucv(x_star),
        "bcv" = bw.bcv(x_star)
    )
    f_list <- parallel::mclapply(
        x,
        function(x_i) {
            p <- (x_i - x_star) / h
            # Estimación de la densidad de x
            aux <- sum(Re(K_U(p, x_star, u, h, kernel, kappa, "legendre"))) / (length(x_star) * h)
            return(pmax.int(0,aux))
        },
        mc.cores = parallel::detectCores() - 2
    )
    f_vec <- do.call(rbind, f_list) |> as.vector()
    return(f_vec)

    # p <- (x - x_star) / h
    # # Estimación de la densidad de x
    # aux <- sum(Re(K_U(p, u, h, kernel, kappa))) / (length(x_star) * h)
    # return(aux)
}

f_x(1:10, v, eps, bw = "nrd0", kernel = "k2")

curve(f_x(x, v, eps, bw = "nrd0", kernel = "k2"), from = -3, to = 3, n = 512, col = "blue", lwd = 2,
      xlab = "x", ylab = "Density", main = "Estimated Density of x")
lines(density(e, n = 512), col = "red", lty = 2)  # Original density of e
# TODO
# - Looks like it needs a normalization factor. It looks like it matches the mean and maybe the variance
# - Change bw functions. Use the ones from `deconvlution` package

f_x(-1, fs_list[[1]]$data$cal_V |> na.omit(), fs_list[[1]]$data$epsilon |> na.omit(), bw = "nrd0", kernel = "lutkenoner")



# %% Deconvolution using fft() in R AI ------------------

# %% stimar las densidades
dens_v <- density(v, n = 2048)  # PDF de v
dens_eps <- density(eps_obs, n = 2048)  # PDF de epsilon


# %% Transformar al dominio de Fourier
F_v <- fft(dens_v$y, inverse = TRUE)
F_eps <- fft(dens_eps$y, inverse = TRUE)
 # Did not work , getting negative values
# Deconvolución en el dominio de Fourier
F_e <- F_v / F_eps

# Transformar de vuelta al dominio original
f_e <- Re(fft(F_e, inverse = FALSE)) / length(F_e)

# %% Graficar la densidad estimada de e
plot(density(f_e, n= 2048), type = "l", col = "blue", lwd = 2,
     xlab = "e", ylab = "Density", main = "Deconvolved Density of e")
plot(density(e, n = 2048), col = "red", lty = 2)  # Original density of e
lines(dens_eps$x, dens_eps$y, col = "red", lwd = 2)
lines(dens_v$x, dens_v$y, col = "green", lwd = 2)
legend("topright", legend = c("Deconvolved Density of e", "Density of epsilon", "Density of v"),
       col = c("blue", "red", "green"), lwd = 2)