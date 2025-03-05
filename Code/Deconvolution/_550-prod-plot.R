

## Plotting ---------------------
phi_GNR<-CD_GNR_prod[CD_GNR_prod$inds==inds,"CD.productivity"]
xmax<-max(
    quantile(phi_GNR, probs = 0.95)[[1]],
    quantile(phi, probs = 0.95)[[1]]
)
ymax<-max(max(density(phi_GNR)$y),max(density(phi)$y))
plot(NULL,xlim=c(0,xmax),ylim=c(0,ymax))
density(phi[phi<=xmax]) |> lines(col="red")
density(phi_GNR) |> 
    lines(col="blue", main=paste0(inds,"— Productivity"))
density(phi_GNR) |> plot()
density(phi) |> plot()
# density(df$exp_W_sqg) |> lines(col="magenta")

data.frame(phi=phi_GNR,method="GNR") |> 
    rbind(data.frame(phi=phi,method="Tax Ev + GNR")) %>%
    ggplot(aes(x=phi, group=method))+
    geom_density(aes(colour=method, fill=method))

par(mfrow=c(1,1))
plot(density(phi))

# hist(omega+epsilon)

# par(mfrow=c(2,1))

# CD_GNR_prod %>%
#     filter(inds==322) %>%
#     ggplot(aes(x=CD.productivity)) +
#     geom_density()

# par(mfrow=c(2,1))
# density(CD_GNR_prod[CD_GNR_prod$inds==322,"CD.productivity"]) |> plot(col="blue")
# density(phi[phi<=max_W_sqg]) |> lines(col="red")

# density(CD_GNR_prod[CD_GNR_prod$inds==322,"CD.productivity"]) |> plot(col="blue")
# density(phi[phi<=max_W_sqg]) |> lines(col="red")
# par(mfrow=c(1,1))
# density(CD_GNR_prod[CD_GNR_prod$inds==322,"CD.productivity"]) |> plot(col="blue")
# density(phi[phi<=max_W_sqg]) |> lines(col="red")
# hist(CD_GNR_prod[CD_GNR_prod$inds==322,"CD.productivity"])# |> plot(col="blue")
# hist(phi[phi<=max_W_sqg], )# |> lines(col="red")

# F_phi<-ecdf(phi)
# F_phi(max_W_sqg)
# plot(F_phi)

## Plotting All ----------------------

plot_dens_comparison<-function(
    x, xmax=NULL, sim_dens_lim=NULL,
    omega_norm_res_list,
    fs_list,
    CD_GNR_prod
    ){
    # Generate random draws 1000 draw --------
    omega_mu<-omega_norm_res_list[[x]][["mu.mu"]] |> as.numeric()
    omega_sigma<-omega_norm_res_list[[x]][["sigma.sigma"]] |> as.numeric()

    omega<-rnorm(1000,mean=omega_mu,sd=omega_sigma)

    epsilon_mu<-fs_list[[x]]$epsilon_mu
    epsilon_sigma<-fs_list[[x]]$epsilon_sigma

    epsilon<-rnorm(1000,mean=epsilon_mu,sd=epsilon_sigma)

    # Generate productivity --------------

    phi <- exp(omega+epsilon)

    # alpha <- prod_fun_list[[x]]$coeffs
    # df<-fs_list[[x]]$data %>%
    #     filter(
    #         is.finite(cal_W),
    #         is.finite(k),
    #         is.finite(l)
    #     ) %>%
    #     mutate(
    #         W_squiggle = cal_W - alpha[["k"]]*k-alpha[["l"]]*l,
    #         exp_W_sqg = exp(W_squiggle)
    #     )

    inds <- str_extract(x,"\\d+")

    # max_exp_W_sqg<-CD_GNR_prod[CD_GNR_prod$inds==inds,"CD.productivity"]|> max() 

    # density(CD_GNR_prod[CD_GNR_prod$inds==inds,"CD.productivity"]) |> 
    #     plot(col="blue", main=paste0(inds,"— Productivity"))
    # density(phi[phi<=max_exp_W_sqg]) |> lines(col="red")
    phi_GNR<-CD_GNR_prod[CD_GNR_prod$inds==inds,"CD.productivity"]
    if (is.null(xmax)){
        # xmax<-max(
        #     quantile(phi_GNR, probs = 0.8)[[1]],
        #     quantile(phi, probs = 0.8)[[1]]
        # )
        # sim_dens_lim <- xmax

        xmax<-max(
            mean(phi_GNR)+1*sd(phi_GNR),
            mean(phi)+1*sd(phi)
        )
        sim_dens_lim <- xmax
    }
    cat(xmax,inds,"\n")
    ymax<-max(max(density(phi_GNR)$y),max(density(phi[phi<=sim_dens_lim])$y))
    plot(NULL,xlim=c(0,xmax),ylim=c(0,ymax),
        main=paste0(inds,"-- Productivity"),xlab=NULL, ylab=NULL)
    density(phi_GNR[phi_GNR<=sim_dens_lim]) |> 
        lines(col="blue")
    density(phi[phi<=sim_dens_lim]) |> lines(col="red")
}

plot_dens_comparison(
    names(fs_list)[4],
    30000, 30000, omega_norm_res_list, fs_list,CD_GNR_prod)

plot_dens_comparison(
    names(fs_list)[5],
    400, 400, omega_norm_res_list, fs_list,CD_GNR_prod)

par(mfrow=c(5,1))
density_comp_plots<-lapply(
    names(fs_list),
    plot_dens_comparison,
    NULL, NULL,
    omega_norm_res_list,
    fs_list,
    CD_GNR_prod
)
par(mfrow=c(1,1))

## Percentile Ratios -------------------
phi<-phi[phi<=max_exp_W_sqg]
quants<-quantile(phi, probs=c(0.05, 0.10,0.25,0.75,0.90,0.95))

rq(phi/quants[["5%"]]~1,tau=0.95) |> summary(se="boot")
rq(phi/quants[["10%"]]~1,tau=0.9) |> summary(se="boot")
rq(phi/quants[["25%"]]~1,tau=0.75) |> summary(se="boot")


GNR_prod_quants<-CD_GNR_prod %>%
    filter(inds==322) %>%
    reframe(
        quants = quantile(CD.productivity,probs=c(0.05,0.1,0.25,0.75,0.9,0.95)),
        probs = c(0.05,0.1,0.25,0.75,0.9,0.95)
    )

rq(phi/quants[["5%"]]~1,tau=0.95,) |> summary(se="boot")
rq(phi/quants[["10%"]]~1,tau=0.9) |> summary(se="boot")
rq(phi/quants[["25%"]]~1,tau=0.75) |> summary(se="boot")



