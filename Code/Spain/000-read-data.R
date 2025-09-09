## %% Load libraries %% 

library(tidyverse)
library(ggplot2)
# library(foreign)
library(haven)

## %% Read data from Stata Format ----------------------

data_folder <- "Data/Spain/"
df <- read_dta(paste0(data_folder, "ESEE_2006_2018_1.dta"))


df_labels <- lapply(
    names(df),
    \(x) {
        c(
            var = x,
            label=attributes(df[[x]])$label[1]
        )
    }
) |> do.call(rbind, args=_)


is_labelled <- lapply(
    names(df),
    \(x) {
        is.labelled(
            df[[x]]
        )
    }
)

names(is_labelled) <- names(df)

var_labels <- lapply(
    names(df),
    \(x) {
        if(is_labelled[x]) {
            print_labels(
                df[[x]],
                x)
        } else {
            NA
        }
    }
) #|> do.call(rbind, args=_)

## %% save data %%

save(
    df,
    df_labels,
    # is_labelled,
    var_labels,
    file = paste0(data_folder, "spain-data.RData")
)
