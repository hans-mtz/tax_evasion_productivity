## Exploring results ----

# Formula to get df's to export ----

res_by_inds <- function(inds="C10", data=results_df){
    temp <- data %>%
        filter(
            Output=="total_valor_prod", 
            Industry==inds
        ) %>%
        select(-n,-Output,-Industry) %>%
        pivot_wider(
            names_from = Size,
            values_from = beta
        )
    temp <- temp[,c(1:3,6:7,4:5,8:11)]
    temp
}

## Exporting to list ----

res_ls <- lapply(industries, res_by_inds)

## Exporting to csv ----

mapply(
    function(x,y){
        write.csv(
            x,
            file = paste0("Results/",y,".csv")
        )
    },
    res_ls,
    industries
)

## Sum stats ----

sum_stat_EC <- df %>% 
    filter(cod_ciiu2d %in% industries[1:5]) %>%
    group_by(cod_ciiu2d) %>%
    summarise(
        `Obs.` = n(),
        across(
            c(total_valor_ventas, total_input),
            list(
                Avg = ~ mean(.x, na.rm = TRUE)/1000000, 
                SD = ~sd(.x, na.rm = TRUE)/1000000,
                Missing = ~sum(is.na(.x))
            )
        )
        # `Avg. Sales` = mean(total_valor_ventas, na.rm = TRUE),
        # `Sd. Sales` = sd(total_valor_ventas, na.rm = TRUE)
    ) %>% 
    left_join(
        data.frame(
            cod_ciiu2d = industries,
            description = inds_des
        )
    ) %>% select(1,9,2:8)

save(sum_stat_EC, file = "../data/Results/EC.RData")
