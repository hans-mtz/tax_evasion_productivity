## %% load libraries and data ------------------------------
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/intermediates.RData")
library(tidyverse)
library(fixest)

## %% Regressions ------------------------------------------

inter <- c(
    "intermediates", # materials + services + energy
    "mats_serv", # materials + services,
    "materials", # materials,
    "deductible_intermediates", # deductibles (materials+electricity+fuels+R&M services),
    "services"# non_deductibles (other services),
)
inter_labs <- c(
    "Materials + Services + Energy",
    "Materials + Services",
    "Materials",
    "Deductible Intermediates",
    "Non-deductible Intermediates"
)

inter_log_share <- c(
    "log_share", # log of intermediates share,
    "log_mats_serv_share", # log of materials + services share,
    "log_mats_share", # log of materials share,
    "log_deductible_intermediates_share", # log of deductible intermediates share,
    "log_services_share" # log of non-deductible intermediates share
)
complements <- c(
    " ",
    "energy",
    "serv_energy",
    "services",
    "deductible_intermediates"
)

fml <- paste0(inter_log_share," ~ poly(",intermediates, ",k,l, degree = 2, raw = TRUE)")
fml

## %% Run Loop ---------------------------------

vars <- expand.grid(fml, top_5_ev_inds, stringsAsFactors = FALSE)

bigE_tbl<-mapply(
    function(x,z){
        colombia_data_frame %>%
            filter(
                sic_3 == z,
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y)
            ) %>%
            mutate(
                mats_service_share = nom_mats_serv/nom_gross_output,
                log_mats_serv_share = log(mats_service_share),
            ) %>%
            fixest::feols(
                as.formula(x), 
                data = .
            ) |> residuals() -> resids
        
        return(
            c(
                sic_3 = z,
                intermediates = str_extract(x,paste0("(",paste(intermediates,collapse="|"),")")),
                mean=mean(-resids) |> round(4), 
                sd=sd(-resids) |> round(4),
                bigE = mean(exp(-resids)) |> round(4)
            ) 
        )
    },
    x=vars$Var1,
    z=vars$Var2,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
)

# bigE_tbl <- do.call(rbind,bigE_list)

# rownames(bigE_tbl) <- inter_labs

bigE_tbl |> t() |> as.data.frame() -> bigE_tbl
bigE_tbl %>%
    mutate(
        intermediates = factor(intermediates, levels=inter,labels = inter_labs),
    ) -> bigE_tbl

## %% Adding Complements ----------------------------

fml_2 <- paste0(inter_log_share," ~ poly(",intermediates,",",complements,",k,l, degree = 2, raw = TRUE)")
fml_2[1] <- fml[1]

fml_2

vars_2 <- expand.grid(fml_2, top_5_ev_inds, stringsAsFactors = FALSE)

bigE_comp_tbl<-mapply(
    function(x,z){
        colombia_data_frame %>%
            filter(
                sic_3 == z,
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y)
            ) %>%
            mutate(
                mats_service_share = nom_mats_serv/nom_gross_output,
                log_mats_serv_share = log(mats_service_share),
                serv_energy = rowSums(cbind(services, energy))
            ) %>%
            fixest::feols(
                as.formula(x), 
                data = .
            ) |> residuals() -> resids
        
        return(
            c(
                sic_3 = z,
                intermediates = str_extract(x,paste0("(",paste(intermediates,collapse="|"),")")),
                mean=mean(-resids) |> round(4), 
                sd=sd(-resids) |> round(4),
                bigE = mean(exp(-resids)) |> round(4)
            ) 
        )
    },
    x=vars_2$Var1,
    z=vars_2$Var2,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
)

bigE_comp_tbl |> t() |> as.data.frame() -> bigE_comp_tbl
bigE_comp_tbl %>%
    mutate(
        intermediates = factor(intermediates, levels=inter,labels = inter_labs),
    ) -> bigE_comp_tbl
bigE_comp_tbl

## %% CD - GRN - eps --------------------------------------

fml_CD <- paste0(inter_log_share," ~ 1")
fml_CD
bigE_CD_list<-lapply(
    fml_CD,
    function(x){
        colombia_data_frame %>%
            filter(
                sic_3 == 311,
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(y)
                # is.finite(log_share)
            ) %>%
            mutate(
                mats_service_share = nom_mats_serv/nom_gross_output,
                log_mats_serv_share = log(mats_service_share),
                serv_energy = rowSums(cbind(services, energy))
            ) %>%
            fixest::feols(
                as.formula(x), 
                data = .
            ) |> residuals() -> resids
        
        return(
            c(
                mean=mean(-resids), 
                sd=sd(-resids),
                bigE = mean(exp(-resids))
            ) |> round(4)
        )
    }
)

bigE_CD_tbl <- do.call(rbind,bigE_CD_list)
bigE_CD_tbl

 colombia_data_frame %>%
            filter(
                is.finite(k),
                is.finite(l),
                is.finite(m),
                is.finite(log_share)
            ) %>%
            mutate(
                mats_service_share = nom_mats_serv/nom_gross_output,
                log_mats_serv_share = log(mats_service_share),
            ) %>%
            fixest::feols(
                log_mats_share ~ 1, 
                data = .
            )

## %% Saving tables ----------------------------

save(
    bigE_tbl, bigE_comp_tbl, bigE_CD_tbl,
    file="Code/Products/bigE.RData"
)
