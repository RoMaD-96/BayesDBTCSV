#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(lubridate)
library(readr)
library(cmdstanr)


#   ____________________________________________________________________________
#   Functions                                                               ####

source("Scripts/functions.R")

#   ____________________________________________________________________________
#   Data                                                                    ####

load("Data/pred_scenarios.RData")

nba_data <- pred_scenarios$predict_first_round$train_data


stan_data <- prepare_stan_data(nba_data,
  mean_logStrength = 0,
  ind_home = 1,
  mean_home = 0,
  sd_home = 10,
  p_spike = 0.05,
  mu_slab = 0,
  mu_spike = 100,
  sd_slab = 5,
  sd_spike = 0.1,
  s_prior_shape = 1,
  s_prior_rate = 1
)
str(stan_data)



#   ____________________________________________________________________________
#   Model fit                                                               ####

model_glick <- cmdstan_model("Stan/glickman_2001.stan")
model_wbt_spike_slab <- cmdstan_model("Stan/wbt_spike_slab.stan")
model_const_var <- cmdstan_model("Stan/glickman_1999.stan")


fit_glick <- model_glick$sample(
  data = stan_data,
  seed = 433,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

fit_const_var <- model_const_var$sample(
  data = stan_data,
  seed = 433,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)


fit_wbt_spike_slab <- model_wbt_spike_slab$sample(
  data = stan_data,
  seed = 433,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

loo_wbt <- fit_wbt_spike_slab$loo()
loo_wbt$p_loo

loo_glick <- fit_glick$loo()
loo_glick$p_loo


loo_const_var <- fit_const_var$loo()
loo_const_var$p_loo

loo::loo_compare(loo_const_var,loo_glick,loo_wbt)

# loo::loo_compare(loo_glick, loo_wbt_spike_slab)
#
# fit_wbt_spike_slab$summary(variables="sd_logStrength")
# fit_glick$summary(variables="sigma")
