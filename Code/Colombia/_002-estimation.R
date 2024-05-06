library("stringr")
# library("fixest")
source("Code/Colombia/00_data_reading.r")


# Colombian big industries ----
inds_col <- cdf %>%
    group_by(sic_3) %>% 
    mutate(n_sic=n()) %>% 
    filter(n_sic>=2500) %>% 
    arrange(desc(n_sic)) %>% 
    select(sic_3) %>%
    unique() %>% 
    pull()

## Relevant variables ----

size_col <- cdf %>% 
    select(
        !c(plant,year, sic_3, p_gdp)
    ) %>% names()

# size_col <- c(
#     "labor_employees", #employment
#     "w7", #wages
#     "e7", #energy
#     "c7", #industrial expenditures
#     "c12", #water, mail, telephone
#     "i39", #capital
#     "s5" #sales
# )

output_col <- c(
    "gross_output", #real value of gross production
    "sales" #real sales
    # "va" #value added
)
input_col <- c(
    "materials",
    "intermediate_inputs",
    "mats_serv"
)

## Loop ----

if (!exists("probs")) {
    probs <- c(0.00,seq(0.80,0.99,by=0.01),0.99)
}
res_col <- list()
i <- 1
for (o in output_col) {
    for (j in inds_col) {
        for (s in size_col) {
            for (p in probs) {
                quant <- quantile(
                    cdf[,s],
                    probs = p,
                    na.rm = TRUE
                )
                temp1 <- data.frame(Output=o,Industry=j,Size=s,Quantile=p)
                temp2 <- cdf %>%
                    filter(sic_3==j, .data[[o]]!=0) %>% 
                    filter(.data[[s]] >= quant[[1]]) %>% 
                    summarise(
                        n=n(),
                        beta=exp(
                            mean(
                                log(
                                    .data[[input_col[2]]]/.data[[o]]
                                ),
                                na.rm=TRUE
                            )
                        )
                    )
                res_col[[i]] <- cbind(temp1, temp2)
                i <- i+1
            }
        }
    }
}


## Collecting res_col in data.frame ----

res_col_df <- do.call(rbind, res_col)

res_col_df <- data.frame(
    Size = size_col,
    Independence = c(
        rep(
            c("Not independent", "Independent"),
            c(7, 4)
        )
    )
) %>% right_join(res_col_df)

## Cleaning space ----

keep <- c(
    grep("df|col",ls(),value=TRUE),
    "stata_labels",
    "vars",
    "change_base_81"
)
rem <- setdiff(ls(), keep)
rm(list = rem)

## Exporting sum stats COL ----

sum_stat_CO<-cdf %>% 
    filter(sic_3 %in% inds_col) %>% 
    group_by(sic_3) %>% 
    summarise(
        `Obs.` = n(),
        across(
            c(sales, intermediate_inputs),
            list(
                Avg = ~ mean(.x, na.rm = TRUE)/1000, 
                SD = ~sd(.x, na.rm = TRUE)/1000,
                Missing = ~sum(is.na(.x))
            )
        )
    ) %>% 
    left_join(
        data.frame(
            sic_3 = inds_col,
            description = inds_desc_COL
        )
    ) %>% select(1,9,2:8)

save(sum_stat_CO, file = paste0(data_dir_col,"CO.RData"))
