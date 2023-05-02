print('Re-LOO for problematic obs in ARMA model')

print('Author: Bridget Bittmann')
print('Date created: 01/16/2023')

# Import packages
library(brms)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(flexmix)
library(modelr)
library(loo)
library(here)

# Import the data
rf <- read.csv('~/scratch/CASC/mixed_model_input.csv')
rf$lt <- log(rf$Sum_AF)

# print('ARMA Model')
# load('~/scratch/reloo-CASC/data/lt_auto_noyear.Rdata')
# summary(lt.auto_noyear)
# 
# loo1 <- loo(lt.auto_noyear, reloo = TRUE)
# loo1
# saveRDS(loo1, file = '~/scratch/reloo-CASC/model-output/loo-ARMA-try.RDS')

# re.loo <- reloo(lt.auto_noyear, loo1)
# saveRDS(re.loo, file = '~/scratch/reloo-CASC/model-output/reloo-ARMA.RDS')

# Model with ARMA terms and climate+urban variables 
#print('Model with ARMA terms and climate+urban variables ')

# load('~/scratch/reloo-CASC/data/lt_mix_auto.Rdata')
# summary(lt.mix.auto)
# 
# loo2 <- loo(lt.mix.auto, save_PSIS = TRUE)
# loo2
# re.loo2 <- reloo(lt.mix.auto, loo2)
# re.loo2
# saveRDS(re.loo2, file = '~/scratch/reloo-CASC/model-output/reloo-mix-ARMA.RDS')
# 
# # Model with ARMA terms and climate
# print('Model with ARMA terms and climate')
# 
# load('~/scratch/reloo-CASC/data/lt_climate_auto.Rdata')
# summary(lt.climate)
# 
# loo3 <- loo(lt.climate, save_PSIS = TRUE)
# loo3
# re.loo3 <- reloo(lt.climate, loo3)
# re.loo3
# saveRDS(re.loo3, file = '~/scratch/reloo-CASC/model-output/reloo-climate-ARMA.RDS')
# 
# # Model with ARMA terms and urban variables 
# print('Model with ARMA terms and urban variables ')
# 
# load('~/scratch/reloo-CASC/data/lt_urb_auto.Rdata')
# summary(lt.urb.auto)
# 
# loo4 <- loo(lt.urb.auto, save_PSIS = TRUE)
# loo4
# re.loo4 <- reloo(lt.urb.auto, loo4)
# re.loo4
# saveRDS(re.loo4, file = '~/scratch/reloo-CASC/model-output/reloo-urb-ARMA.RDS')
# 
# Model with ARMA terms and urban(varying) + climate + storage variables
# print('Model with ARMA terms and urban (varying)+climate+storage variables ')
# 
# load('~/scratch/reloo-CASC/data/lt_div_auto.Rdata')
# summary(lt.div.auto)
# 
# loo5 <- loo(lt.div.auto, save_PSIS = TRUE)
# loo5
# re.loo5 <- reloo(lt.div.auto, loo5)
# re.loo5
# saveRDS(re.loo5, file = '~/scratch/reloo-CASC/model-output/reloo-div-ARMA.RDS')

# Model with correct dataset, ARMA full with group ##
print('Model with correct dataset, ARMA full with group')

arma_gr <- readRDS('~/scratch/reloo-CASC/data/arma_012523.RDS')
summary(arma_gr)

loo6 <- loo(arma_gr, save_PSIS = TRUE)
loo6
re.loo6 <- reloo(arma_gr, loo6)
re.loo6
saveRDS(re.loo6, file = '~/scratch/reloo-CASC/model-output/reloo-arma-012523.RDS')


# Model with correct dataset, ARMA full with no group ##
print('Model with correct dataset, ARMA full with no group')

arma_ng <- readRDS('~/scratch/reloo-CASC/data/arma_nogroup_012523.RDS')
summary(arma_ng)

loo7 <- loo(arma_ng, save_PSIS = TRUE)
loo7
re.loo7 <- reloo(arma_ng, loo7)
re.loo7
saveRDS(re.loo7, file = '~/scratch/reloo-CASC/model-output/reloo-arma-ng-012523.RDS')


