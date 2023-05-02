# Borah output for model fit

# This script reviews the loo values after leaving out problematic observations

# Author: Bridget Bittmann
# Date created: 01/17/2023

# Import packages
library(brms)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(flexmix)
library(modelr)
library(loo)

# Import models ####

# saveRDS(lt.auto_noyear, file = '~/Desktop/CASCWork/model_output/lt_auto_noyear.RDS')
# saveRDS(lt.div.auto, file = '~/Desktop/CASCWork/model_output/lt_div_auto.RDS')
# saveRDS(lt.mix.auto, file = '~/Desktop/CASCWork/model_output/lt_mix_auto.RDS')
# saveRDS(lt.urb.auto, file = '~/Desktop/CASCWork/model_output/lt_urb_auto.RDS')
# saveRDS(lt.climate, file = '~/Desktop/CASCWork/model_output/lt_clim_auto.RDS')

# This model includes fixed effects for urban, precip, temp, ET, and canals with ARMA
lt.auto.noyear <- readRDS('~/Desktop/CASCWork/model_output/lt_auto_noyear.RDS')

# This model includes varying effect for urban and fixed effects for precip, temp, ET, and canals 
# with ARMA
lt.div.auto <- readRDS('~/Desktop/CASCWork/model_output/lt_div_auto.RDS')

# This model includes varying effect for urban and fixed effects for temp, precip, and ET with ARMA.
lt.mix.auto <- readRDS('~/Desktop/CASCWork/model_output/lt_mix_auto.RDS')

# This model includes fixed effects for temp, precip, and ET with ARMA
lt.clim.auto <- readRDS('~/Desktop/CASCWork/model_output/lt_clim_auto.RDS')

# This model includes a varying effect for urban plus ARMA
lt.urb.auto <- readRDS('~/Desktop/CASCWork/model_output/lt_urb_auto.RDS')


# Import the loo objects and analyze ####
# These have all gone through the re-loo process to leave out problematic observations. All pareto K
# values are less than 0.7 (the accepted threshold).

arma_full <- readRDS('~/Desktop/CASCWork/borah-out/reloo-ARMA.RDS')
arma_full

climate <- readRDS('~/Desktop/CASCWork/borah-out/reloo-climate-ARMA.RDS')
climate

var_urb_full <- readRDS('~/Desktop/CASCWork/borah-out/reloo-div-ARMA.RDS')
var_urb_full

arma_mix <- readRDS('~/Desktop/CASCWork/borah-out/reloo-mix-ARMA.RDS')
arma_mix

urb <- readRDS('~/Desktop/CASCWork/borah-out/reloo-urb-ARMA.RDS')
urb

try<- readRDS('~/Desktop/CASCWork/borah-out/loo-ARMA-try.RDS')
try

loo_compare(arma_full, arma_mix, urb, climate, var_urb_full)
# We can see that the varying effect of urban and non-varying effect aren't differnt from one 
# another given their elpd difference +/- 2 SE. This is also the case for the model that does not include
# canal flows. Finally, we see that just urban and just climate models are different (elpd >4 and +/- 2 SE
# doesn't cross zero). 

# However, this analysis is based on the understanding of our knowledge of the system. We want this for 
# causal inference, not prediction necessarily. 

## AFTER FIXING DATASET FOR MASON DRAIN AND CREEK ####

# This model includes varying effect for urban and fixed effects for precip, temp, ET, and canals 
# with ARMA
arma_gr <- readRDS('~/Desktop/CASCWork/borah-out/arma_012523.RDS')
summary(arma_gr)

# This model includes fixed effects for urban, precip, temp, ET, and canals with ARMA
arma_ng <- readRDS('~/Desktop/CASCWork/borah-out/arma_nogroup_012523.RDS')
summary(arma_ng)

# This model has no ARMA but all variables
mix <- readRDS('~/Desktop/CASCWork/borah-out/rf_mix_012423.RDS')
summary(mix)

## IMPORT LOO AFTER FIXING DATAFRAME ####

loo.full <- readRDS('~/Desktop/CASCWork/borah-out/reloo-arma-012523.RDS')
loo.full

loo.ng <- readRDS('~/Desktop/CASCWork/borah-out/reloo-arma-ng-012523.RDS')
loo.ng

loo_compare(loo.full, loo.ng)
