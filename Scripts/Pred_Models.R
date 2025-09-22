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
#   Fit Functions                                                           ####

# Build Stan data
prepare_stan_data <- function(data,
                              newdata = NULL,
                              mean_logStrength = 0,
                              ind_home = 1,
                              mean_home = 0,
                              sd_home = 10) {

  team_ids <- sort(unique(c(data$hometeamId, data$awayteamId)))
  team_lookup <- setNames(seq_along(team_ids), team_ids)

  data2 <- data %>%
    mutate(
      team1 = team_lookup[hometeamId],
      team2 = team_lookup[awayteamId],
      y = as.integer(homeScore > awayScore)
    )

  stan_list <- list(
    N = nrow(data2),
    nteams = length(team_ids),
    ntimes_rank = length(unique(data2$instants_rank)),
    instants_rank = data2$instants_rank,
    team1 = as.integer(data2$team1),
    team2 = as.integer(data2$team2),
    mean_logStrength = mean_logStrength,
    y = data2$y,
    ind_home = as.integer(ind_home),
    mean_home = mean_home,
    sd_home = sd_home,
    s_prior_shape = 0.1,
    s_prior_rate = 0.1,
    #spike and slab commensurate
    p_spike = 0.05,
    mu_slab = 0,
    mu_spike = 100,
    sd_slab = 5,
    sd_spike = 0.1
  )

  # ---- if playoff data provided, append ----
  if (!is.null(newdata)) {
    newdata2 <- newdata %>%
      mutate(
        team1_prev = team_lookup[hometeamId],
        team2_prev = team_lookup[awayteamId],
        instants_rank = as.integer(instants_rank)
      )
    stan_list$N_prev <- nrow(newdata2)
    stan_list$instants_rank_prev <- as.integer(newdata2$instants_rank)
    stan_list$team1_prev <- as.integer(newdata2$team1_prev)
    stan_list$team2_prev <- as.integer(newdata2$team2_prev)
  } else {
    stan_list$N_prev <- 0
    stan_list$instants_rank_prev <- integer(0)
    stan_list$team1_prev <- integer(0)
    stan_list$team2_prev <- integer(0)
  }

  stan_list
}

# Function to fit model and make predictions for a scenario

fit_scenario <- function(scenario, model_type = "glick") {

  cat("\\n=== ", scenario$name, " ===\\n")
  cat(scenario$description, "\\n")
  cat("Training games:", nrow(scenario$train_data), "\\n")
  cat("Prediction games:", nrow(scenario$predict_data), "\\n")

  # Prepare Stan data
  stan_data <- prepare_stan_data(
    data = scenario$train_data,
    newdata = scenario$predict_data
  )

  # Choose model
  if (model_type == "glick") {
    model <- model_glick
  } else if (model_type == "wbt_spike_slab") {
    model <- model_wbt_spike_slab
  }
    else {
    stop("Unknown model type")
  }

  # Fit model
  fit <- model$sample(
    data = stan_data,
    seed = 433,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 200
  )

  # LOO for model comparison
  loo_result <- fit$loo()
  return(list(
    scenario = scenario,
    fit = fit,
    stan_data = stan_data,
    loo = loo_result
  ))
}

#   ____________________________________________________________________________
#   Running models                                                          ####

model_glick <- cmdstan_model("~/Desktop/Work/Projects/BayesDBTCSV/glickman_2001.stan")
model_wbt_spike_slab <- cmdstan_model("~/Desktop/Work/Projects/BayesDBTCSV/wbt_spike_slab.stan")





# Run all scenarios
results_glick <- list()
results_spike_slab <- list()

# Fit Glickman model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Glickman model for:", scenario_name)
    results_glick[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "glick")
  }
}

# Fit Spike-Slab model for each scenario
for (scenario_name in names(pred_scenarios)) {
  if (nrow(pred_scenarios[[scenario_name]]$predict_data) > 0) {
    cat("\\n\\nFitting Spike-Slab model for:", scenario_name)
    results_spike_slab[[scenario_name]] <- fit_scenario(pred_scenarios[[scenario_name]], "wbt_spike_slab")
  }
}


save(results_glick, file = "Results/results_glick.RData")
save(results_spike_slab, file = "Results/results_spike_slab.RData")
