library(tidyverse)
library(fixest)

## Load data ----

source("../data/Estimation/data.R")

## Estimate beta ----

size_vars <- names(df[,11:20])
output_vars <- names(df[,c("total_valor_prod","total_valor_ventas")])
# industries <- inds %>% 
#                 filter(cod_letra=="C", ciiu2d_n>=10) %>% 
#                 select(cod_ciiu2d) %>% 
#                 unique(.) %>% pull()
# big_inds <- big_inds[[1]]

industries <- inds %>% 
                filter(cod_letra=="C", ciiu2d_n>=19) %>% 
                arrange(desc(ciiu2d_n)) %>% 
                select(cod_ciiu2d) %>% 
                unique(.) %>% 
                pull()

inds_des <- inds %>%
    filter(cod_letra=="C", ciiu2d_n>=19) %>% 
    arrange(desc(ciiu2d_n)) %>% 
    select(des_ciiu2d) %>% 
    unique(.) %>% 
    pull()

names(inds_des) <- industries
inds_des
## Loop ----

# probs <- c(0,seq(0.8,0.99,by=0.025),0.99)
if (!exists("probs")) {
    probs <- c(0.00,seq(0.80,0.99,by=0.01),0.99)
}
results <- list()
i <- 1
for (o in output_vars) {
    for (j in industries) {
        for (s in size_vars) {
            for (p in probs) {
                quant <- quantile(
                    df[,s],
                    probs = p,
                    na.rm = TRUE
                )
                temp1 <- data.frame(Output=o,Industry=j,Size=s,Quantile=p)
                temp2 <- df %>%
                    filter(cod_ciiu2d==j, .data[[o]]!=0) %>%
                    filter(.data[[s]] >= quant[[1]]) %>% 
                    summarise(
                        n=n(),
                        beta=exp(
                            mean(
                                log(
                                    total_input/.data[[o]]
                                ),
                                na.rm=TRUE
                            )
                        )
                    )
                results[[i]] <- cbind(temp1, temp2)
                i <- i+1
            }
        }

    }
}

## Collecting results in data.frame ----

results_df <- do.call(rbind,results)

results_df <- data.frame(
    Size = size_vars,
    Independence = c(
        rep(
            c("Not independent", "Independent"),
            c(2,2)
        ),
        rep(
            c("Not independent", "Independent"),
            c(2,4)
        )
    )
) %>% right_join(results_df)

## Cleaning space ----

keep <- c(
    grep("df|vars|ind|res",ls(),value=TRUE)
)
rem <- setdiff(ls(), keep)
rm(list = rem)
