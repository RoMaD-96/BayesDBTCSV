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

source("Scripts/functions.R")

#   ____________________________________________________________________________
#   Running models                                                          ####

model_glick <- cmdstan_model("~/Desktop/Work/Projects/BayesDBTCSV/glickman_2001.stan")
model_const_var <- cmdstan_model("~/Desktop/Work/Projects/BayesDBTCSV/glickman_1999.stan")
model_wbt_spike_slab <- cmdstan_model("~/Desktop/Work/Projects/BayesDBTCSV/wbt_spike_slab.stan")


# Run all scenarios
results_glick <- list()
results_const_var <- list()
results_spike_slab <- list()

# Fit Glickman (2001) model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Glickman (2001) model for:", scenario_name)
    results_glick[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "glick_2001", seed = 433)
  }
}

# Fit Glickman (1999) model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Glickman (1999) model for:", scenario_name)
    results_const_var[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "const_var", seed = 433)
  }
}


# Fit Spike-Slab model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Spike-Slab model for:", scenario_name)
    results_spike_slab[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "wbt_spike_slab", seed = 433)
  }
}


# save(results_glick, file = "Results/results_glick.RData")
# save(results_spike_slab, file = "Results/results_spike_slab.RData")
