library(foreign)
# library(dplyr)
## Reading Colombian data ----
data_dir_col <- "Data/Colombia/"
col_df <- read.dta(paste0(data_dir_col,"mergecap.dta"))

# Stata variable labels ----
stata_labels <- attributes(col_df)$var.labels
vars <- names(col_df)

names(stata_labels) <- vars

# Save data --------------

save(col_df, file = "Code/Products/col_df.RData")

## Exploring ---- 

# # years 77-91
# col_df %>% 
#     select(datayear) %>% 
#     unique()

# # 7,000 firms
# col_df %>% 
#     group_by(datayear) %>% 
#     summarise(n=n())

# col_df %>% 
#     group_by(sic) %>% 
#     summarise(n=n()) %>%
#     ungroup() %>% 
#     summarise(avg_firms=mean(n))

# col_df %>%
#     ggplot(aes(sic))+
#     geom_bar()

# col_df %>% 
#     filter(sic==3116) %>% 
#     group_by(sic) %>%
#     select(pe) %>% 
#     summarise(
#         mean = mean(pe, na.rm = TRUE),
#         missing = sum(is.na(pe)),
#         min = min(pe, na.rm = TRUE),
#         max = max(pe, na.rm = TRUE)
#     )

# col_df %>% 
#     filter(sic==3116) %>% 
#     group_by(sic) %>%
#     select(s10) %>% 
#     summarise(
#         mean = mean(s10, na.rm = TRUE),
#         missing = sum(is.na(s10)),
#         min = min(s10, na.rm = TRUE),
#         max = max(s10, na.rm = TRUE),
#         zeros = sum(s10==0, na.rm = TRUE)
#     )

# col_df %>% 
#     filter(sic==3116) %>% 
#     summarise(
#         n=n(),
#         beta = exp(
#             mean(
#                 log(
#                     s10/pe
#                 ),
#                 na.rm = TRUE
#             )
#         )
#     )

# ## exploring results ----

# res_col_df %>% 
#     filter(Industry==3116)
