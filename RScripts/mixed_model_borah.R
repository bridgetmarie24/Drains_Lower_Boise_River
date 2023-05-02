## Borah Model runs ##

# Mixed Effects Model for Drains #

# By Bridget Bittmann
# Date created: 01/11/23
# Date modified: 

# Import packages:
library(brms)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)
library(loo)
library(here)


# Set directory to current location
here::here()

# Import the data 

rf <- read.csv('~/scratch/CASC/mixed_model_input_041423.csv')
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

# # Create priors for model with all variables ####
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
# 
# Create priors for mix + div_flows model ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow')
)
# 
# ## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS ####
# 
# lt.div.auto.011123 <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma(gr = Name),
#                    data = rf,
#                    iter = 4000,
#                    family = 'normal',
#                    prior = priors,
#                    control = list(max_treedepth = 20,
#                                   adapt_delta = 0.999),
#                    cores = getOption('mc.cores', parallel::detectCores()))
# #Model convergence
# print('Model: ARMA and varying effects')
# summary(lt.div.auto.011123)
# 
# ## Model Fit
# print('MAE')
# mae_lt(lt.div.auto.011123, rf$Sum_AF)
# 
# print('LOO')
# loo2 <- loo(lt.div.auto.011123, reloo = TRUE)
# loo2
# 
# saveRDS(lt.div.auto.011123, file = '~/scratch/CASC/model_output/arma_012423.RDS')
# saveRDS(loo2, file = '~/scratch/CASC/model_output/loo_arma_012423.RDS')
# 
## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS NO GROUP, NO YEAR, order assumed ####

rf_arma_full <- brm(lt ~ (1 | Name) + scale_class1_urban + scale_et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                           data = rf,
                           iter = 4000,
                           family = 'normal',
                           prior = priors,
                           control = list(max_treedepth = 20,
                                          adapt_delta = 0.999),
                           cores = getOption('mc.cores', parallel::detectCores()),
                           save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(rf_arma_full)

# print('LOO')
# loo3 <- loo(rf_arma_full, reloo = TRUE)
# loo3

print('MAE')
mae_lt(rf_arma_full, rf$Sum_AF)

saveRDS(rf_arma_full, file = '~/scratch/CASC/model_output/arma_nogroup_041423.Rdata')
# saveRDS(loo3, file = '~/scratch/CASC/model_ouptput/loo_arma_nogroup_012423.RDS')

# # ARMA model for urban and climate, no canals
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'et'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban')
# )
# 
# lt.nocanal <- brm(lt ~ (1 | Name + scale_class1_urban) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + arma( gr = Name),
#                          data = rf,
#                          iter = 4000,
#                          family = 'normal',
#                          prior = priors,
#                          control = list(max_treedepth = 20,
#                                         adapt_delta = 0.999),
#                          cores = getOption('mc.cores', parallel::detectCores()),
#                          save_pars = save_pars(all = TRUE))
# 
# print('Model: Autoregressive, no canals')
# summary(lt.nocanal)
# 
# loo4 <- loo(lt.nocanal, reloo = TRUE)
# print(loo4)
# 
# print('MAE')
# mae_lt(lt.nocanal, rf$Sum_AF)
# 
# saveRDS(lt.nocanal, file = '~/scratch/CASC/model_output/arma_nocanal_0213.Rdata')
# saveRDS(loo4, file = '~/scratch/CASC/model_ouptput/loo_nocanal_0213.RDS')
# 
# # Model for ARMA + climate
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'et'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp')
# )
# 
# lt.clim <- brm(lt ~ (1 | Name) + et + scale_irrig_prcp + scale_irrig_temp + arma( gr = Name),
#                   data = rf,
#                   iter = 4000,
#                   family = 'normal',
#                   prior = priors,
#                   control = list(max_treedepth = 20,
#                                  adapt_delta = 0.999),
#                   cores = getOption('mc.cores', parallel::detectCores()),
#                   save_pars = save_pars(all = TRUE))
# print('Climate model')
# summary(lt.clim)
# 
# print('LOO')
# loo5 <- loo(lt.clim, reloo = TRUE)
# loo5
# 
# print('MAE')
# mae_lt(lt.clim, rf$Sum_AF)
# 
# saveRDS(lt.clim, file = '~/scratch/CASC/model_output/arma_clim_0213.Rdata')
# saveRDS(loo5, file = '~/scratch/CASC/model_ouptput/loo_clim_0213.RDS')
# 
# #ARMA with urban 
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban')
# )
# 
# lt.urb <- brm(lt ~ (1 | Name + scale_class1_urban) + scale_class1_urban + arma( gr = Name),
#                   data = rf,
#                   iter = 4000,
#                   family = 'normal',
#                   prior = priors,
#                   control = list(max_treedepth = 20,
#                                  adapt_delta = 0.999),
#                   cores = getOption('mc.cores', parallel::detectCores()),
#                   save_pars = save_pars(all = TRUE))
# 
# print('Urban model')
# summary(lt.urb)
# 
# print('Loo')
# loo6 <- loo(lt.urb, reloo = TRUE)
# loo6
# 
# print('MAE')
# mae_lt(lt.urb, rf$Sum_AF)
# 
# saveRDS(lt.urb, file = '~/scratch/CASC/model_output/arma_urb_0213.Rdata')
# saveRDS(loo6, file = '~/scratch/CASC/model_ouptput/loo_urb_0213.RDS')





