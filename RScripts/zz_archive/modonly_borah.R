# Mixed Effects Model for Drains #
# only models, no loo

# By Bridget Bittmann
# Date created: 01/11/23
# Date modified: 

# Import packages:
library(brms)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)
library(here)


# Set directory to current location
here::here()

# Import the data 

rf <- read.csv('~/scratch/CASC/model_input_correct.csv')
rf$lt <- log(rf$Sum_AF)

## MODEL FIT ####
mae <- function(model, data_compare){
  yhat <- posterior_predict(model)
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae_lt <- function(model, data_compare){
  yhat <- exp(posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

# Create a model to understand changes in drainage rates through time

# Create priors for model with all variables ####
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
#   set_prior('normal(0,5)', class = 'b', coef = 'et'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow')
# )
# 
# # MODEL: ALL WITH GROUP LEVEL EFFECT FOR URBAN AREA ####
# rf.mix.new <- brm(Sum_AF ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow,
#                   data = rf,
#                   iter = 2000,
#                   family = 'lognormal',
#                   prior = priors,
#                   control = list(max_treedepth = 20,
#                                  adapt_delta = 0.999),
#                   cores = getOption('mc.cores', parallel::detectCores()))
# loo1 <- loo(rf.mix.new, reloo = TRUE)
# print("Model: No arma, all variables")
# summary(rf.mix.new)
# 
# print('LOO')
# loo1
# 
# print('MAE')
# mae(rf.mix.new, rf$Sum_AF)
# 
# saveRDS(rf.mix.new, file = '~/scratch/CASC/model_output/rf_mix_012423.RDS')
# saveRDS(loo1, file = '~/scratch/CASC/model_output/loo_noarma_012423.RDS')

# Create priors for mix + div_flows model ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow')
)

# ## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS ####
# 
# lt.div.auto.011123 <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma(gr = Name),
#                           data = rf,
#                           iter = 4000,
#                           family = 'normal',
#                           prior = priors,
#                           control = list(max_treedepth = 20,
#                                          adapt_delta = 0.999),
#                           cores = getOption('mc.cores', parallel::detectCores()))
# #Model convergence
# print('Model: ARMA and varying effects')
# summary(lt.div.auto.011123)
# 
# ## Model Fit
# print('MAE')
# mae_lt(lt.div.auto.011123, rf$Sum_AF)
# 
# # print('LOO')
# # loo2 <- loo(lt.div.auto.011123, reloo = TRUE)
# # loo2
# 
# saveRDS(lt.div.auto.011123, file = '~/scratch/CASC/model_output/arma_012523.RDS')
# saveRDS(loo2, file = '~/scratch/CASC/model_output/loo_arma_012423.RDS')

## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS NO GROUP, NO YEAR, order assumed ####

lt.auto_ng_011123 <- brm(lt ~ (1 | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                         data = rf,
                         iter = 4000,
                         family = 'normal',
                         prior = priors,
                         control = list(max_treedepth = 20,
                                        adapt_delta = 0.999),
                         cores = getOption('mc.cores', parallel::detectCores()),
                         save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(lt.auto_ng_011123)

# print('LOO')
# loo3 <- loo(lt.auto_ng_011123, reloo = TRUE)
# loo3

print('MAE')
mae_lt(lt.auto_ng_011123, rf$Sum_AF)

saveRDS(lt.auto_ng_011123, file = '~/scratch/CASC/model_output/arma_nogroup_012523.RDS')
# saveRDS(loo3, file = '~/scratch/CASC/model_ouptput/loo_arma_nogroup_012423.RDS')

