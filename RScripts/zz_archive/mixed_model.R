# Mixed Effects Model for Drains #

# By Bridget Bittmann
# Date created: 10/03/2022
# Date modified: 

# Import packages:
library(brms)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)
library(loo)
library(here)

# Set directory to current location
here::here()

# Import the data 

rf <- read.csv('mixed_model_input.csv')
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
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,5)', class = 'b', coef = 'et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow')
)

prior_mod <- brm(Sum_AF ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_temp + scale_irrig_prcp,
                 family = "lognormal",
                 data = rf,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                 prior = priors, 
                 sample_prior = "only")

preds_from_prior <-  add_predicted_draws(rf, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0, 200000)

# MODEL: ALL WITH GROUP LEVEL EFFECT FOR URBAN AREA ####
rf.mix.new <- brm(Sum_AF ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow,
              data = rf,
              iter = 2000,
              family = 'lognormal',
              prior = priors,
              control = list(max_treedepth = 20,
                            adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))
loo2 <- loo(rf.mix.new, save_psis = TRUE, reloo = TRUE)
print("Model: No arma, all variables")
print(loo2)
summary(rf.mix.new)
saveRDS(rf.mix.new, file = 'model_output/rf_mix_011122.RDS')

lt.mix <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp 
              + scale_irrig_temp,
              data = rf,
              iter = 4000,
              family = 'normal',
              prior = priors,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))
mae_lt(lt.mix, rf$Sum_AF)
lt.mix <- add_criterion(lt.mix, criterion = 'loo')
save(lt.mix, file = '~/Desktop/CASCWork/model_output/lt_mix.Rdata')

# Evaluate model convergence
summary(rf.mix)
plot(rf.mix)
conditional_effects(rf.mix)
pp_check(rf.mix)

# Add criterion for model fit and save
rf.mix <- add_criterion(rf.mix, criterion = 'loo')
save(rf.mix, file = '~/Desktop/CASCWork/model_output/rf_mix.Rdata')
mae(rf.mix, rf$Sum_AF)

# Visualize effects
mcmc_intervals(rf.mix, pars = c('b_scale_class1_urban',
                                'b_scale_irrig_prcp',
                                'b_scale_irrig_temp',
                                'b_et'))

# MODEL: ALL NO GROUP LEVEL EFFECT FOR URBAN ####
rf.mix.ng <- brm(Sum_AF ~ (1 | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp,
              data = rf,
              iter = 2000,
              family = 'lognormal',
              prior = priors,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))
# Evaluate model convergence
plot(rf.mix.ng)
conditional_effects(rf.mix.ng)
summary(rf.mix.ng)
pp_check(rf.mix.ng)

# Add criterion for model fit and save
rf.mix.ng <- add_criterion(rf.mix.ng, criterion = 'loo')
save(rf.mix.ng, file = '~/Desktop/CASCWork/model_output/rf_mix_ng.Rdata')
mae(rf.mix.ng, rf$Sum_AF)

summ# Visualize effects
mcmc_intervals(rf.mix.ng, pars = c('b_scale_class1_urban',
                            'b_scale_irrig_prcp',
                            'b_scale_irrig_temp',
                            'b_et'), prob = 0.5, prob_outer = 0.95)

## MODEL: AUTOREGRESSIVE MIX MODEL NO GROUP ####

rf.mixng.auto <- brm(lt ~ (1 | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + arma(time=Year, gr = Name),
                 data = rf,
                 iter = 4000,
                 family = 'normal',
                 prior = priors,
                 control = list(max_treedepth = 20,
                                adapt_delta = 0.999),
                 cores = getOption('mc.cores', parallel::detectCores()))
summary(rf.mixng.auto)
plot(rf.mixng.auto)
rf.mixng.auto <- add_criterion(rf.mixng.auto, criterion = 'loo')
save(rf.mixng.auto, file = '~/Desktop/CASCWork/model_output/rf_mix_ng_auto.Rdata')

## MODEL: AUTOREGRESSIVE MIX WITH GROUP ####

lt.mix.auto <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + arma(time=Year, gr = Name),
                   data = rf,
                   iter = 4000,
                   family = 'normal',
                   prior = priors,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.999),
                   cores = getOption('mc.cores', parallel::detectCores()))
summary(lt.mix.auto)
plot(lt.mix.auto)
conditional_effects(lt.mix.auto)
mae_lt(lt.mix.auto, rf$Sum_AF)
lt.mix.auto <- add_criterion(lt.mix.auto, criterion = 'loo')
save(lt.mix.auto, file = '~/Desktop/CASCWork/model_output/lt_mix_auto.Rdata')

# Create priors for model with just urban ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban')
)

prior_mod <- brm(Sum_AF ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
                 family = "lognormal",
                 data = rf,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                 prior = priors, 
                 sample_prior = "only")

preds_from_prior <-  add_predicted_draws(rf, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0, 200000)

# MODEL: URBAN ####
rf.urb <- brm(Sum_AF ~ (1 | Name) + scale_class1_urban,
                 data = rf,
                 iter = 2000,
                 family = 'lognormal',
                 prior = priors,
                 control = list(max_treedepth = 20,
                                adapt_delta = 0.999),
                 cores = getOption('mc.cores', parallel::detectCores()))

# Evaluate model convergence
plot(rf.urb)
conditional_effects(rf.urb)
summary(rf.urb)
pp_check(rf.urb)

# Add criterion for model fit and save
rf.urb <- add_criterion(rf.urb, criterion = 'loo')
save(rf.urb, file = '~/Desktop/CASCWork/model_output/rf_urb.Rdata')
mae(rf.urb, rf$Sum_AF)

# Visualize effects
mcmc_intervals(rf.urb, pars = 'b_scale_class1_urban', prob = 0.5, prob_outer = 0.95)

## MODEL: AUTOREGRESSIVE URBAN ####
lt.urb.auto <- brm(lt ~ (1 | Name) + scale_class1_urban + arma(gr = Name, time = Year),
              data = rf,
              iter = 4000,
              family = 'normal',
              prior = priors,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))

summary(lt.urb.auto)
plot(lt.urb.auto)
conditional_effects(lt.urb.auto)
lt.urb.auto <- add_criterion(lt.urb.auto, criterion = 'loo')
save(lt.urb.auto, file = '~/Desktop/CASCWork/model_output/lt_urb_auto.Rdata')

# Create priors for climate model ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp')
)

prior_mod <- brm(Sum_AF ~ (1 | Name) + et + scale_irrig_temp + scale_irrig_prcp,
                 family = "lognormal",
                 data = rf,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                 prior = priors, 
                 sample_prior = "only")

preds_from_prior <-  add_predicted_draws(rf, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = et, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0, 200000)

# MODEL: CLIMATE ####

rf.climate <- brm(Sum_AF ~ (1 | Name) + et + scale_irrig_prcp + scale_irrig_temp,
              data = rf,
              iter = 2000,
              family = 'lognormal',
              prior = priors,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))

# Evaluate model convergence
plot(rf.climate)
conditional_effects(rf.climate)
summary(rf.climate)
pp_check(rf.climate)

# Add criterion for model fit and save
rf.climate <- add_criterion(rf.climate, criterion = 'loo')
save(rf.climate, file = '~/Desktop/CASCWork/model_output/rf_climate.Rdata')
mae(rf.climate, rf$Sum_AF)

# Visualize effects
mcmc_intervals(rf.climate, pars = 'b_scale_class1_urban', prob = 0.5, prob_outer = 0.95)

## MODEL: AUTOREGRESSIVE CLIMATE ####

lt.climate <- brm(lt ~ (1 | Name) + et + scale_irrig_prcp + scale_irrig_temp + arma(time = Year, gr = Name),
                  data = rf,
                  iter = 4000,
                  family = 'normal',
                  prior = priors,
                  control = list(max_treedepth = 20,
                                 adapt_delta = 0.999),
                  cores = getOption('mc.cores', parallel::detectCores()))

summary(lt.climate)
lt.climate <- add_criterion(lt.climate, criterion = 'loo')
mae_lt(lt.climate, rf$Sum_AF)
save(lt.climate, file = '~/Desktop/CASCWork/model_output/lt_climate_auto.Rdata')

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

## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS ####

lt.div.auto <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma(time=Year, gr = Name),
                   data = rf,
                   iter = 4000,
                   family = 'normal',
                   prior = priors,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.999),
                   cores = getOption('mc.cores', parallel::detectCores()))
#Model convergence
summary(lt.div.auto)
plot(lt.div.auto)

## Model Fit
mae_lt(lt.div.auto, rf$Sum_AF)
lt.div.auto <- add_criterion(lt.div.auto, criterion = 'loo')

## Export and save
save(lt.div.auto, file = '~/Desktop/CASCWork/model_output/lt_div_auto.Rdata')

## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS NO GROUP ####

lt.div.auto.ng <- brm(lt ~ (1 | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma(time=Year, gr = Name),
                   data = rf,
                   iter = 4000,
                   family = 'normal',
                   prior = priors,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.999),
                   cores = getOption('mc.cores', parallel::detectCores()))
summary(lt.div.auto.ng)
lt.div.auto.ng <- add_criterion(lt.div.auto.ng, criterion = 'loo')
save(lt.div.auto.ng, file = '~/Desktop/CASCWork/model_output/lt_div_auto_ng.Rdata')

post_samples <- add_epred_draws(lt.div.auto.ng)

## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS NO GROUP, NO YEAR, order assumed ####

lt.auto_noyear_0109 <- brm(lt ~ (1 | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                      data = rf,
                      iter = 4000,
                      family = 'normal',
                      prior = priors,
                      control = list(max_treedepth = 20,
                                     adapt_delta = 0.999),
                      cores = getOption('mc.cores', parallel::detectCores()),
                      save_pars = save_pars(all = TRUE))
summary(lt.auto_noyear)
lt.div.auto.ng <- add_criterion(lt.div.auto.ng, criterion = 'loo')
loo_auto <- loo(lt.auto_noyear_0109, save_psis = TRUE)
save(lt.auto_noyear, file = '~/Desktop/CASCWork/model_output/lt_auto_noyear.Rdata')

lt.auto_nogr <- brm(lt ~ (1 | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma(time = Year),
                      data = rf,
                      iter = 4000,
                      family = 'normal',
                      prior = priors,
                      control = list(max_treedepth = 20,
                                     adapt_delta = 0.999),
                      cores = getOption('mc.cores', parallel::detectCores()))

summary(lt.auto_noygr)

## Model fit comparison ####

load('~/Desktop/CASCWork/model_output/rf_climate.Rdata')
load('~/Desktop/CASCWork/model_output/rf_urb.Rdata')
load('~/Desktop/CASCWork/model_output/rf_mix_ng.Rdata')
load('~/Desktop/CASCWork/model_output/rf_mix.Rdata')

loo(rf.climate, rf.urb, rf.mix.ng, rf.mix)
loo(lt.mix.auto, lt.mix, lt.climate, lt.div.auto, lt.urb.auto, rf.mixng.auto, lt.div.auto.ng)

mae_lt(lt.div.auto.ng, rf$Sum_AF)
mae_lt(lt.div.auto, rf$Sum_AF)
mae_lt(rf.mixng.auto, rf$Sum_AF)
mae_lt(lt.mix.auto, rf$Sum_AF)
mae_lt(lt.climate, rf$Sum_AF)
mae_lt(lt.urb.auto, rf$Sum_AF)
mae_lt(lt.mix, rf$Sum_AF)

trial <- add_criterion(lt.auto_noyear, criterion = 'loo')

trial <- loo(lt.auto_noyear)
trial2 <- loo(AF.arma)
trial3 <- loo(AF.mix)
yrep <- posterior_predict(AF.mix)
ppc_loo_pit_overlay(y = diversions$Acre_feet, yrep = yrep, lw = weights(trial3$psis_object))

new <- add_criterion(AF.mix, criterion = 'loo', save_psis = TRUE)
loo3 <- loo(lt.auto_noyear, save_psis = TRUE)
reloo3 <- reloo(loo3, lt.auto_noyear, chains = 1)

y <- rf$lt
yrep <- posterior_predict(lt.auto_noyear)
psis2 <- loo3$psis_object
lw <- weights(psis2)
ppc_loo_pit_qq(y = y, 
            yrep = yrep, 
            lw = lw)
summary(lt.climate)
summary(lt.mix)
summary(lt.mix.auto)
loo(lt.climate)
loo(lt.div.auto)
loo(lt.mix.auto)
loo(lt.mix)
loo(lt.urb.auto)
