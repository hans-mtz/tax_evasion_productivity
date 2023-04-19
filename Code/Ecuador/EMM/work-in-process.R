library(tidyverse)

## Inspecting data ----
names(data_list[["2015"]][['2015_EMM_Base_de_datos.sav']])
dim(data_list[["2015"]][["2015_"]]) #Dimension
attributes(data_list[[1]][[1]]) #Variable names, row names, variable names
names(data_list[["2002"]][[1]])
names(
   attributes(data_list[[1]][[1]])$variable.labels
)
attributes(data_list[[1]][[1]])$variable.labels["COD_ENCU"]

head(data_list[[13]][[2]])

## 2015 ----
lapply(data_list[["2015"]],function(x)attributes(x)$variable.labels)

### Output ----

data_list[['2015']][[3]] %>%
    group_by(id_diie) %>%
    summarise(
        across(
            contains("ventas"),
            list(
                missing=~sum(is.na(.x)),
                total=sum
            )
        )
    )

###

    

