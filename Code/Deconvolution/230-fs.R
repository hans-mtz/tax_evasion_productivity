# %% Load data and packages ---------------
library(tidyverse)
library(parallel)
# load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/run-vars.RData") 
# load("Code/Products/intermediates.RData") #Top evading industries
# load("Code/Products/boot_tax_ev_mmt.RData") # run_vars

## %% define variables ---------------------

mc_cores <- detectCores()-2
# top_evading_inds <- tax_ev_test_tbl %>% arrange(desc(log_mats_share)) %>% pull(sic_3)
# run_vars<-expand.grid(
#     inds=top_5_ev_inds_mag[1:5],
#     input=c("log_mats_share","log_deductible_intermediates_share"),
#     stringsAsFactors = FALSE
# )

inter_named <- c(
    "log_mats_share" = "materials",
    "log_deductible_intermediates_share" = "deductible_intermediates"   
)
# inter_named[rep("log_mats_share", 10)]
run_vars$r_input <- inter_named[run_vars$input]

## %% Saving variables ---------------------

save(
    run_vars,
    file = "Code/Products/run-vars.RData"
)

## Deconvoluting ------------------------

### %% First Stage (Trimming) -------------------------

fs_list<-mcmapply(
    first_stage_panel, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars[,"inds"],
    run_vars[,"input"],
    run_vars[,"r_input"],
    MoreArgs = list(data=test_data),#colombia_data_frame),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

names(fs_list)<-paste(run_vars[,"inds"],run_vars[,"input"])

## %% Save results ---------------------

save(
    fs_list,
    file = "Code/Products/fs.RData"
)

### %% First Stage (ME) -------------------------

fs_list_me<-mcmapply(
    first_stage_panel_me, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
    run_vars[,"inds"],
    run_vars[,"input"],
    run_vars[,"r_input"],
    MoreArgs = list(data=test_data),#colombia_data_frame),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

names(fs_list_me)<-paste(run_vars[,"inds"],run_vars[,"input"])

## %% Save results ---------------------

save(
    fs_list, fs_list_me, #run_vars,
    file = "Code/Products/fs.RData"
)

## %% Plot -----------------------------------------

load("Code/Products/fs.RData")

plot(density(fs_list[[2]]$data$cal_V, na.rm=TRUE), col="red")
lines(density(-fs_list[[1]]$data$epsilon, na.rm=TRUE), main="Density of epsilon")


for(i in seq_along(fs_list)[1]) {
    plot(
        # fs_list[[i]]$data$cal_V,
        density(fs_list[[i]]$data$epsilon, na.rm=TRUE, bw="SJ-ste"),
        main=sub("^(\\d{3}).*$", "Industry \\1", names(fs_list)[i]),
        xlab=" ",
        ylab=" ",
        col="blue", lwd=2
    )
    curve(dnorm(x, mean=mean(fs_list[[i]]$data$epsilon, na.rm=TRUE), sd=sd(fs_list[[i]]$data$epsilon, na.rm=TRUE)), 
          add=TRUE, col="darkgray", lwd=2)
    legend("topright", 
           legend=c("Epsilon density", "Normal distribution fit"),
           col=c("blue", "darkgray"), lty=1, lwd=2)
    # abline(h=0, col="red")
}

## %% Recovering epsilon density non-parametrically -----------------

fs_list[[1]]$data$epsilon |> na.omit() |> 
    density(bw="nrd0") -> eps_density

eps_pdf <- approxfun(eps_density$x, eps_density$y, yleft = 0, yright = 0)

curve(
    eps_pdf,
    from = min(eps_density$x), to = max(eps_density$x),
    xlab = "Epsilon",
    ylab = "Density",
    main = "Non-parametric Epsilon Density",
    col = "blue", lwd=2
)
lines(
    density(fs_list[[1]]$data$epsilon, na.rm=TRUE, bw="nrd0"),
    col="red", lwd=2
)

## %% My own function -----------------------------

epdf <- function(x, x_data, bw = "nrd0") {
    # h <- bw.SJ(x_data, method = "dpi")
    h <- switch(
        bw,
        "nrd0" = bw.nrd0(x_data),
        "nrd" = bw.nrd(x_data),
        "SJ-ste" = bw.SJ(x_data, method = "ste"),
        "SJ-dpi" = bw.SJ(x_data, method = "dpi"),
        "ucv" = bw.ucv(x_data),
        "bcv" = bw.bcv(x_data)
    ) # Using a different bandwidth method
    p <- (x-x_data)/h
    x_data_sd <- sd(x_data, na.rm = TRUE)
    K <- dnorm(p, mean = 0, sd = x_data_sd)
    density_estimate <- sum(K, na.rm = TRUE) / (length(x_data) * h)
    return(density_estimate)
}
vepdf<-Vectorize(epdf, "x")
vepdf(1:10, fs_list[[1]]$data$epsilon |> na.omit())

bws <- c("nrd0", "nrd", "SJ-ste", "SJ-dpi", "ucv", "bcv")
curve(
    eps_pdf,
    from = min(fs_list[[1]]$data$epsilon, na.rm = TRUE), 
    to = max(fs_list[[1]]$data$epsilon, na.rm = TRUE),
    xlab = "Epsilon",
    ylab = "Density",
    main = "Custom Epsilon Density Function",
    col = "black", lwd=2
)
my_colors <- rainbow(length(bws))
names(my_colors) <- bws  # Generate random colors for each bandwidth
for (bw in bws) {
    curve(
        vepdf(x, fs_list[[1]]$data$epsilon |> na.omit(), bw=bw),
        add = TRUE,
        lwd=2, lty = 2,
        col =   my_colors[bw]# Random color for each curve
    )
}
legend(
    "topright",
    legend = paste("Bandwidth:", bws),
    col = my_colors,
    lty = 2, lwd = 2
)

for (bw in bws) {
    lines(
        density(fs_list[[1]]$data$epsilon |> na.omit(), bw=bw),
        add = TRUE,
        lwd=2, lty = 2,
        col =   my_colors[bw]# Random color for each curve
    )
}
## %% stuff ------------------------------

mi_funcion <- function(x) x^2
mi_lista <- list(
  cuadrado = mi_funcion,
  suma = function(a, b) a + b
)

# Usar la función desde la lista
mi_lista$cuadrado(4)   # Devuelve 16
mi_lista$suma(2, 3)    # Devuelve 5
