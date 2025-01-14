# Load data and packages ---------------
library(tidyverse)
library(fixest)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")

# Data Wrangling ------------------------------------
# colombia_data_frame<-colombia_data_frame %>%
#     mutate(
#         # mats_deduct_share = rowSums(cbind(materials,deductible_expenses), na.rm = TRUE)/sales,
#         log_ded_share = log(mats_deduct_share),
#         log_mats_share = log((materials/sales))
#     )


# Generate deconvolution moments-----

## CD estimates from GNR

# CD_estimates<-tribble(
#     ~industry,~m,~l,~k,
#     'all', .53875558,.29393800,.21727172,
#     '311', .64110727,.20207484,.18848132,
#     '321', .53290702,.25227628,.21228284,
#     '322', .46880366,.30505987,.17425505,
#     '331', .50108806,.46521823,.60734950E-01,
#     '381', .56862492,.35114693,.14644627
# )


## GNR- Intermediates (Original idea) ----------------

# colombia_data_frame %>%
#     filter(sic_3==322,log_share<Inf) %>%
#     mutate(
#         treat = ifelse(juridical_organization==3,"Corp","Non-Corp")
#     ) %>%
#     mutate(
#         epsilon = feols(log_share~1,data=.) %>% residuals(),
#         log_D = coefficients(feols(log_share~treat,data=.))[[1]],
#         big_E = mean(exp(epsilon)),
#         beta = exp(log_D-log(big_E)),

#     ) %>%
#     group_by(treat) %>%
#     summarise(
#         mean_eps = mean(epsilon, na.rm=TRUE),
#         variance_eps= var(epsilon, na.rm = TRUE)
#     )

# colombia_data_frame %>%
#     ungroup() %>%
#     filter(sic_3==322,log_share<Inf,juridical_organization==3) %>%
#     mutate(
#         epsilon = feols(log_share~1,data=.) %>% residuals(),
#         # log_D = coefficients(feols(log_share~treat,data=.))[[1]],
#         # big_E = mean(exp(epsilon)),
#         # beta = exp(log_D-log(big_E))
#     ) %>%
#     summarise(
#         mean_eps = mean(epsilon, na.rm=TRUE),
#         variance_eps= var(epsilon, na.rm = TRUE)
#     )

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
            feols(fml, data = .)

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
                !is.na(.data[[var]]),
                .data[[var]] < Inf,
                .data[[var]] > -Inf
            ) %>%
            feols(fml, data = .)

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
            ) %>%
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

# Testing the presence of Evasion by Overreporting -----------------

test_ev_cd <- function(sic, var, data) {
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
            feols(fml, data = .)

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
                sic_3 = sic
            )
        return(tbl)
    }

## Testing functions --------------------------------------

# mmt_deconv_year(322,"log_share",colombia_data_frame)
# mmt_deconv(322,"log_share",colombia_data_frame)
# test_ev_cd(322,"log_share",colombia_data_frame)
# mmt_deconv_year(322,"log_ded_share",colombia_data_frame)
# mmt_deconv(322,"log_ded_share",colombia_data_frame)
# test_ev_cd(322,"log_ded_share",colombia_data_frame)
# mmt_deconv_year(322,"log_mats_share",colombia_data_frame)
# mmt_deconv(322,"log_mats_share",colombia_data_frame)
# test_ev_cd(322,"log_mats_share",colombia_data_frame)

# test_ev_cd(384,"log_share",colombia_data_frame)
# test_ev_cd(384,"log_ded_share",colombia_data_frame)
# test_ev_cd(384,"log_mats_share",colombia_data_frame)

# test_ev_cd(312,"log_share",colombia_data_frame)
# test_ev_cd(312,"log_ded_share",colombia_data_frame)
# test_ev_cd(312,"log_mats_share",colombia_data_frame)

# test_ev_cd(356,"log_share",colombia_data_frame)
# test_ev_cd(356,"log_ded_share",colombia_data_frame)
# test_ev_cd(356,"log_mats_share",colombia_data_frame)

# test_ev_cd(332,"log_share",colombia_data_frame)
# test_ev_cd(332,"log_ded_share",colombia_data_frame)
# test_ev_cd(332,"log_mats_share",colombia_data_frame)

# Intermediates ------------------------

ev_log_s<-lapply(top_20_inds$sic_3, mmt_deconv, var="log_share", data=colombia_data_frame)
ev_log_s_df<-do.call(rbind,ev_log_s)

ev_log_s_year<-lapply(top_20_inds$sic_3, mmt_deconv_year, var="log_share", data=colombia_data_frame)
ev_log_s_df_year<-do.call(rbind,ev_log_s_year)

test_log_s<-lapply(top_20_inds$sic_3, test_ev_cd, var="log_share", data=colombia_data_frame)
test_log_s_df<-do.call(rbind,test_log_s)

## Deductibles --------------------------

ev_log_ded_s<-lapply(top_20_inds$sic_3, mmt_deconv, var="log_ded_share", data=colombia_data_frame)
ev_log_ded_s_df<-do.call(rbind,ev_log_ded_s)

ev_log_ded_s_year<-lapply(top_20_inds$sic_3, mmt_deconv_year, var="log_ded_share", data=colombia_data_frame)
ev_log_ded_s_df_year<-do.call(rbind,ev_log_ded_s_year)

test_log_ded_s<-lapply(top_20_inds$sic_3, test_ev_cd, var="log_ded_share", data=colombia_data_frame)
test_log_ded_s_df<-do.call(rbind,test_log_ded_s)

## Materials --------------------------

ev_log_mats_s<-lapply(top_20_inds$sic_3, mmt_deconv, var="log_mats_share", data=colombia_data_frame)
ev_log_mats_s_df<-do.call(rbind,ev_log_mats_s)

ev_log_mats_s_year<-lapply(top_20_inds$sic_3, mmt_deconv_year, var="log_mats_share", data=colombia_data_frame)
ev_log_mats_s_df_year<-do.call(rbind,ev_log_mats_s_year)

test_log_mats_s<-lapply(top_20_inds$sic_3, test_ev_cd, var="log_mats_share", data=colombia_data_frame)
test_log_mats_s_df<-do.call(rbind,test_log_mats_s)

## Combining Results to Compare -----------------

ev_df <- cbind(Intermediates=ev_log_s_df,Materials=ev_log_mats_s_df,Deductibles=ev_log_ded_s_df)
ev_df |> View()

ev_df_year <- cbind(Intermediates=ev_log_s_df_year,Materials=ev_log_mats_s_df_year,Deductibles=ev_log_ded_s_df_year)#[,c(1,4,7,2,5,8,3)]
ev_df_year |> View()

test_df <- cbind(Intermediates=test_log_s_df,Materials=test_log_mats_s_df,Deductibles=test_log_ded_s_df)
test_df |> View()


## Testing CD ---------------------------------

test_tbl_CD<-test_df %>%
    select(
        Industry =Intermediates.sic_3,
        ends_with(c("mean_V","rej_rule","test_result","t_stat","se"))
    ) %>%
    group_by(Industry) %>%
    mutate(
        across(
            ends_with("t_stat"),
            ~1-pnorm(.x),
            .names = "{.col}_pval"
        ),
        across(
            ends_with("_pval"),
            ~case_when(
                .x <= 0.001 ~ "***",
                .x <= 0.05 ~ "**",
                .x <= 0.1 ~ "*",
                .default = ""
            ),
            .names = "{.col}_stars"
        ),
        evasion = rowSums(cbind(Intermediates.rej_rule, Materials.rej_rule, Deductibles.rej_rule)) >0,
        Intermediates  = glue::glue(
            "{round(Intermediates.mean_V,3)} ({paste0(round(Intermediates.se,3),Intermediates.t_stat_pval_stars)})"
        ),
        Deductibles  = glue::glue(
            "{round(Deductibles.mean_V,3)} ({paste0(round(Deductibles.se,3),Deductibles.t_stat_pval_stars)})"
        ),
        Materials  = glue::glue(
            "{round(Materials.mean_V,3)} ({paste0(round(Materials.se,3),Materials.t_stat_pval_stars)})"
        ),
        .after = 1
    ) %>%
    select(
        # ends_with("stars"),
        Intermediates, Deductibles, Materials, evasion
    )

evasion_inds <- test_tbl_CD %>% filter(evasion ==TRUE) %>% pull(Industry)

evasion_inds_description <- ciiu_3 %>% 
    filter(Código %in% evasion_inds) %>% 
    pull(Descripción)

names(evasion_inds_description) <- evasion_inds

## Plotting - Data Wrangling -----------------------------------

means_long <- ev_df %>%
    pivot_longer(
        ends_with(".mean_evasion"),
        names_to = "share",
        values_to ="mean"
    ) %>%
    select(
        sic_3=Intermediates.sic_3,
        share, mean
    ) %>%
    mutate(
        share = sub("\\.mean_evasion","", share)
    )
View(means_long)
var_long <- ev_df %>%
    pivot_longer(
        ends_with(".variance_evasion"),
        names_to = "share",
        values_to = "variance"
    ) %>%
    select(
        sic_3=Intermediates.sic_3,
        share, variance
    ) %>%
    mutate(
        share = sub("\\.variance_evasion","", share)
    )
View(var_long)

n_long <- ev_df %>%
    pivot_longer(
        ends_with(".n"),
        names_to = "share",
        values_to = "n"
    ) %>%
    select(
        sic_3=Intermediates.sic_3,
        share, n
    ) %>%
    mutate(
        share = sub("\\.n","", share)
    )
View(n_long)

## Plotting -----------------------------------
# load("Code/Products/deconv.RData")

deconv_mmt_long <- means_long %>%
    left_join( var_long) %>%
    left_join( n_long) %>%
    filter( sic_3 %in% evasion_inds) %>%
    mutate(
        LCI = mean-1.96*(sqrt(abs(variance)/n)),
        UCI = mean+1.96*(sqrt(abs(variance)/n))
    )

order_sic<-deconv_mmt_long%>%
    group_by(sic_3) %>%
    summarise(highest_mean=max(mean)) %>%
    arrange(desc(highest_mean)) %>%
    pull(sic_3)
deconv_mmt_long%>%
    filter( sic_3 %in% order_sic[1:10]) %>% #Only top 10 Evasion Industries
    ggplot(aes(x=factor(sic_3, levels = order_sic), y=mean, group=share,color=share, fill=share))+
    geom_point()+
    geom_errorbar(aes(ymin=LCI, ymax=UCI)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth=0.5) +
    coord_flip() +
    labs(x="Industry", y="Mean Evasion", title = "Mean Evasion by Industry") +
    theme_classic()+
    theme(legend.position = c(0.95, 0.95), legend.justification = c(1, 1), legend.title = element_blank())

### Plotting by year --------------------------

means_long_y <- ev_df_year %>%
    pivot_longer(
        ends_with(".mean_evasion"),
        names_to = "share",
        values_to ="mean"
    ) %>%
    select(
        sic_3=Intermediates.sic_3,
        year=Intermediates.year,
        share, mean
    ) %>%
    mutate(
        share = sub("\\.mean_evasion","", share)
    )
View(means_long_y)
var_long_y <- ev_df_year %>%
    pivot_longer(
        ends_with(".variance_evasion"),
        names_to = "share",
        values_to = "variance"
    ) %>%
    select(
        sic_3=Intermediates.sic_3,
        year=Intermediates.year,
        share, variance
    ) %>%
    mutate(
        share = sub("\\.variance_evasion","", share)
    )
View(var_long_y)

n_long_y <- ev_df_year %>%
    pivot_longer(
        ends_with(".n"),
        names_to = "share",
        values_to = "n"
    ) %>%
    select(
        sic_3=Intermediates.sic_3,
        year=Intermediates.year,
        share, n
    ) %>%
    mutate(
        share = sub("\\.n","", share)
    )
View(n_long_y)

# top_6_ev_inds <- c(322,324,342,313,321,351)

deconv_mmt_long_y <- means_long_y %>%
    left_join( var_long_y) %>%
    left_join( n_long_y) %>%
    filter( sic_3 %in% order_sic[1:6]) %>%
    mutate(
        LCI = mean-1.96*(sqrt(abs(variance)/n)),
        UCI = mean+1.96*(sqrt(abs(variance)/n))
    )
View(deconv_mmt_long_y)

deconv_mmt_long_y%>%
    filter(sic_3==322) %>%
    ggplot(aes(x=factor(year), y=mean, group=share,color=share, fill=share))+
    geom_line()+
    geom_errorbar(aes(ymin=LCI, ymax=UCI)) +
    geom_vline(xintercept = "83", linetype = "dashed", linewidth=0.5) +
    # coord_flip() +
    labs(x="Year", y="Mean Evasion", title = paste0("Mean Evasion by Year ",322)) +
    theme_classic()+
    theme(legend.position = "top", legend.justification = c(1, 1), legend.title = element_blank())

year_inds_plots<-lapply(order_sic[1:6],\(x)
    deconv_mmt_long_y%>%
        filter(sic_3==x) %>%
        ggplot(aes(x=factor(year), y=mean, group=share,color=share, fill=share))+
        geom_line()+
        geom_errorbar(aes(ymin=LCI, ymax=UCI)) +
        geom_vline(xintercept = "83", linetype = "dashed", linewidth=0.5) +
        # coord_flip() +
        labs(x="Year", y="Mean Evasion", title = "Mean Evasion by Year", subtitle = evasion_inds_description[[paste0(x)]]) +
        theme_classic()+
        theme(legend.position = "top", legend.justification = c(1, 1), legend.title = element_blank())
)


## Saving the data frames ---------------------


save(deconv_mmt_long, test_tbl_CD, order_sic, file = "Code/Products/deconv.RData")

save(deconv_mmt_long_y, evasion_inds_description, deconv_mmt_long_y, file = "Code/Products/deconv_year.RData")
