library(foreign)
# library(tidyverse)

#### Extracting data from zip files ####----
# here::here()
## Directories
data_dir <- "Data/Ecuador/Encuesta de Manufactura y Mineria/SPSS"
out_dir <- paste(data_dir, "extracted", sep = "/")

## Reading available years, zip files and paths

spss_years <- list.files(path = data_dir)

zip_files <- list()
zip_dir <- list()
for (z in spss_years) {
   zip_dir[[z]] <- paste(data_dir, "/", z, sep = "")
   zip_files[[z]] <- list.files(
      path = zip_dir[[z]],
      pattern = "spss(_[[:alpha:]]*)?.zip$" # REGEX to get 2015 __spss_TOMOX.zip
   )
}

## Extracting files
for (d in spss_years) {
   for (f in zip_files[[d]]) {
      temp <- paste(zip_dir[[d]], f, sep = "/")
      temp_out <- zip_dir[[d]]
      unzip(
         temp,
         exdir = temp_out,
         overwrite = FALSE # Ignores the file if it's already extracted
      )
   }
}

## Reading from SPSS files ----

spss_files <- list()
for (z in spss_years) {
   spss_files[[z]] <- list.files(
      path = zip_dir[[z]],
      pattern = ".(sav|SAV)$"
   )
}

## My code to read the data
#### my version preserves the year as name of the
#### in the list

data_list <- list()
for (d in spss_years) {
   for (f in spss_files[[d]]) {
      temp_sav <- paste(zip_dir[[d]], f, sep = "/")
      data_list[[d]][[f]] <- read.spss(
         temp_sav,
         to.data.frame = TRUE,
         use.value.labels = FALSE
      )
   }
}

## Chat GPT version
# data_list <- lapply(spss_years, function(d) {
#     Map(read.spss, paste(zip_dir[[d]], spss_files[[d]], sep = "/"), to.data.frame = TRUE)
# })
