# library(foreign)
library(haven)
# library(dplyr)
## Reading Colombian data ----
data_dir_col <- "Data/Colombia/"
# stata_col_df <- read.dta(paste0(data_dir_col,"gnr-colombia-stata-data.dta"))
stata_col_df <- read_dta(paste0(data_dir_col,"gnr-colombia-stata-data.dta"))

# Stata variable labels ----

stata_col_labels<-lapply(
    names(stata_col_df),
    \(x) attr(stata_col_df[[x]], "label")
)
names(stata_col_labels) <- names(stata_col_df)

stata_col_df_labels <- do.call(rbind,stata_col_labels)

# Save data --------------

save(stata_col_df, stata_col_df_labels, file = "Code/Products/stata_colombia_df.RData")

