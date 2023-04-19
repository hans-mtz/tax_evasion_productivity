library(foreign)
library(tidyverse)
library(todor)

## Load data ----

source("../data/Estimation/output.R")
source("../data/Estimation/input.R")
source("../data/Estimation/firms.R")

## Removing auxiliary files ----
rm(list=ls(pattern = "_"))
