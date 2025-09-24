#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(tidyr)
library(purrr)
library(posterior)

#   ____________________________________________________________________________
#   Data                                                                    ####

load("~/Desktop/Weighted_BTD/Data/pred_scenarios.RData")

#   ____________________________________________________________________________
#   Model                                                                   ####

source("Scripts/pred_models.R")

#   ____________________________________________________________________________
#   Outcome Probability Table                                               ####


probs_glick <- extract_match_probs(results_glick, "Glickman")
probs_spike <- extract_match_probs(results_spike_slab, "Spike–Slab")
probs_const_var <- extract_match_probs(results_const_var, "Const_Var")
df_probs <- bind_rows(probs_glick, probs_spike, probs_const_var)

pretty_names <- c(
  "predict_first_round" = "First Round", "first_round" = "First Round",
  "predict_semifinals" = "Semifinals", "semifinals" = "Semifinals",
  "predict_conf_finals" = "Conference Finals", "conference_finals" = "Conference Finals",
  "predict_nba_finals" = "NBA Finals", "nba_finals" = "NBA Finals"
)
desired_order <- c("First Round", "Semifinals", "Conference Finals", "NBA Finals")

per_game_table <- df_probs %>%
  mutate(
    round = dplyr::recode(as.character(scenario), !!!pretty_names, .default = as.character(scenario)),
    round = factor(round, levels = desired_order)
  ) %>%
  select(round, game_id, home_team, away_team, y_true, true_outcome, method, p_home, p_away) %>%
  tidyr::pivot_wider(
    id_cols = c(round, game_id, home_team, away_team, y_true, true_outcome),
    names_from = method,
    values_from = c(p_home, p_away),
    names_glue = "{.value}_{method}"
  ) %>%
  mutate(across(starts_with("p_"), ~ round(.x, 3))) %>%
  arrange(round, game_id)



per_game_table <- per_game_table %>%
  dplyr::select(
    round, game_id, home_team, away_team, true_outcome,
    `p_home_Spike–Slab`, `p_away_Spike–Slab`,
    p_home_Glickman, p_away_Glickman, p_home_Const_Var, p_away_Const_Var
  )


print(per_game_table[44:84, ], n = Inf)
