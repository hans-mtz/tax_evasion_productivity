# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/deconv_mle.RData")
load("Code/Products/deconv_prod_fun.RData")

## Obtainig productivity estimates --------------
# Productivity = exp(omega+varepsilon)
# 1 Get W_squiggle = big_W - alpha*[k,l] = omega+(1-\beta)varepsilon
# Deconvolute
