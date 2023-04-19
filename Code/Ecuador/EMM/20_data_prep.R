library(tidyverse)

## Reading data ## 
source("Code/Ecuador/EMM/10_reading_data.R")

## Getting variable labels ----

variable_labs <- list()
for (i in spss_years) {
    variable_labs[[i]] <-
        lapply(
            data_list[[i]],
            function(x) attributes(x)$variable.labels
        )
}

## Mergin datasets ##

# Merging split data sets into 1 --------
no_split_data <- list()
for (i in rev(names(data_list))) {
    if (length(names(data_list[[i]])) > 1) {
        for (j in names(data_list[[i]])) {
            no_split_data[[i]] <- merge(
                no_split_data[[i]], data_list[[i]][[j]],
                all.y = TRUE
            )
        }
    } else {
    no_split_data[[i]] <- data_list[[i]]
    }
}

# Merging 2015 
merge(
    data_list[["2015"]][[3]],
    data_list[["2015"]][[2]]
) |> head()
intersect(
    colnames(data_list[["2015"]][[1]]),
    colnames(data_list[["2015"]][[2]])
)

do.call(merge, data_list[["2015"]])
