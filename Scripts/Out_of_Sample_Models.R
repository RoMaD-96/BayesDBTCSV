#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(lubridate)
library(readr)
library(cmdstanr)

#   ____________________________________________________________________________
#   Data                                                                    ####

load("Data/pred_scenarios.RData")

#   ____________________________________________________________________________
#   Functions                                                               ####

source("Scripts/Functions.R")

#   ____________________________________________________________________________
#   Running models                                                          ####

model_glick <- cmdstan_model("Stan/glickman_2001.stan")
model_const_var <- cmdstan_model("Stan/glickman_1999.stan")
model_wbt_spike_slab <- cmdstan_model("Stan/wbt_spike_slab.stan")
model_exp_bt <- cmdstan_model("Stan/weighted_bt.stan")

# Run all scenarios
results_glick <- list()
results_const_var <- list()
results_spike_slab <- list()
results_exp_bt <- list()


# Exp BT model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting exp BT model for:", scenario_name)
    results_exp_bt[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "exp_bt", seed = 433)
  }
}

# Glickman (2001) model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Glickman (2001) model for:", scenario_name)
    results_glick[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "glick_2001", seed = 433)
  }
}

# Glickman (1999) model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Glickman (1999) model for:", scenario_name)
    results_const_var[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "const_var", seed = 433)
  }
}


# Spike-Slab model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Spike-Slab model for:", scenario_name)
    results_spike_slab[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "wbt_spike_slab", seed = 433)
  }
}
