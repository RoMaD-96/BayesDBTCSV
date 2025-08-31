#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(tidyr)
library(purrr)
library(posterior)

#   ____________________________________________________________________________
#   Data                                                                    ####

load("~/Desktop/Weighted_BTD/Data/pred_scenarios.RData")
load("~/Desktop/Weighted_BTD/Results/results_glick.RData")
load("~/Desktop/Weighted_BTD/Results/results_spike_slab.RData")


#   ____________________________________________________________________________
#   Outcome Probability Table                                               ####

# Build labelers from a scenario's TRAIN DATA (matches prepare_stan_data logic)
build_labeler_from_train <- function(train_df) {
  team_ids <- sort(unique(c(train_df$hometeamId, train_df$awayteamId)))

  has_names <- all(c("hometeamCity","hometeamName","awayteamCity","awayteamName") %in% names(train_df))
  if (has_names) {
    team_meta <- bind_rows(
      train_df %>% transmute(team_id = hometeamId,
                             team_label = paste0(hometeamCity, " ", hometeamName)),
      train_df %>% transmute(team_id = awayteamId,
                             team_label = paste0(awayteamCity, " ", awayteamName))
    ) %>%
      distinct(team_id, .keep_all = TRUE)

    id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
  } else {
    # fallback: label is just the ID
    id_to_label <- setNames(team_ids, team_ids)
  }

  # index -> label (Stan uses indices 1..length(team_ids))
  idx_to_label <- setNames(unname(id_to_label[team_ids]), seq_along(team_ids))
  list(team_ids = team_ids, id_to_label = id_to_label, idx_to_label = idx_to_label)
}

extract_match_probs <- function(results_list, method_label) {
  map2_dfr(results_list, names(results_list), function(res, scen_name) {
    fit <- res$fit; scenario <- res$scenario; sd <- res$stan_data
    if (is.null(sd$N_prev) || sd$N_prev == 0) return(NULL)

    # posterior home-win probs in y_prev order
    dm <- posterior::as_draws_matrix(fit$draws(variables = "y_prev"))
    p_hat_named <- colMeans(dm)
    idx <- as.integer(gsub("^y_prev\\[|\\]$", "", names(p_hat_named)))
    ord <- order(idx)
    p_home <- as.numeric(p_hat_named)[ord]
    n_prev <- length(p_home)

    # labelers (from TRAIN data)
    build_labeler_from_train <- function(train_df) {
      team_ids <- sort(unique(c(train_df$hometeamId, train_df$awayteamId)))
      has_names <- all(c("hometeamCity","hometeamName","awayteamCity","awayteamName") %in% names(train_df))
      if (has_names) {
        team_meta <- bind_rows(
          train_df %>% transmute(team_id = hometeamId,
                                 team_label = paste0(hometeamCity, " ", hometeamName)),
          train_df %>% transmute(team_id = awayteamId,
                                 team_label = paste0(awayteamCity, " ", awayteamName))
        ) %>% distinct(team_id, .keep_all = TRUE)
        id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
      } else {
        id_to_label <- setNames(team_ids, team_ids)
      }
      idx_to_label <- setNames(unname(id_to_label[team_ids]), seq_along(team_ids))
      list(team_ids = team_ids, id_to_label = id_to_label, idx_to_label = idx_to_label)
    }
    lab <- build_labeler_from_train(scenario$train_data)

    # team labels from predict_data IDs (ordered to y_prev)
    pd <- scenario$predict_data
    ht <- at <- rep(NA_character_, n_prev)
    if (all(c("hometeamId","awayteamId") %in% names(pd))) {
      ht <- unname(lab$id_to_label[as.character(pd$hometeamId)])[ord]
      at <- unname(lab$id_to_label[as.character(pd$awayteamId)])[ord]
    }
    # fallback via Stan indices
    if ((all(is.na(ht)) || all(is.na(at))) && !is.null(sd$team1_prev) && !is.null(sd$team2_prev)) {
      h_idx <- as.integer(sd$team1_prev)[ord]
      a_idx <- as.integer(sd$team2_prev)[ord]
      if (min(c(h_idx, a_idx), na.rm = TRUE) == 0) { h_idx <- h_idx + 1; a_idx <- a_idx + 1 }
      ht <- unname(lab$idx_to_label[as.character(h_idx)])
      at <- unname(lab$idx_to_label[as.character(a_idx)])
    }
    # final fallback to raw IDs
    if (all(c("hometeamId","awayteamId") %in% names(pd))) {
      ht <- ifelse(is.na(ht), as.character(pd$hometeamId)[ord], ht)
      at <- ifelse(is.na(at), as.character(pd$awayteamId)[ord], at)
    }

    # === NEW: true outcome (aligned to the same order) ===
    y_true <- if (all(c("homeScore","awayScore") %in% names(pd))) {
      as.integer(pd$homeScore > pd$awayScore)[ord]
    } else {
      rep(NA_integer_, n_prev)
    }
    true_outcome <- ifelse(is.na(y_true), NA_character_,
                           ifelse(y_true == 1, "Home Win", "Away Win"))

    tibble(
      scenario = scen_name,
      game_id  = seq_len(n_prev),
      home_team = ht,
      away_team = at,
      y_true = y_true,
      true_outcome = true_outcome,
      p_home  = p_home,
      p_away  = 1 - p_home,
      method  = method_label
    )
  })
}

# Rebuild + pivot, keeping y_true / true_outcome as shared (method-independent) columns
probs_glick <- extract_match_probs(results_glick,      "Glickman")
probs_spike <- extract_match_probs(results_spike_slab, "Spike–Slab")
df_probs <- bind_rows(probs_glick, probs_spike)

pretty_names <- c(
  "predict_first_round" = "First Round", "first_round" = "First Round",
  "predict_semifinals"  = "Semifinals",  "semifinals"  = "Semifinals",
  "predict_conf_finals" = "Conference Finals", "conference_finals" = "Conference Finals",
  "predict_nba_finals"  = "NBA Finals",  "nba_finals"  = "NBA Finals"
)
desired_order <- c("First Round","Semifinals","Conference Finals","NBA Finals")

per_game_table <- df_probs %>%
  mutate(round = dplyr::recode(as.character(scenario), !!!pretty_names, .default = as.character(scenario)),
         round = factor(round, levels = desired_order)) %>%
  select(round, game_id, home_team, away_team, y_true, true_outcome, method, p_home, p_away) %>%
  tidyr::pivot_wider(
    id_cols = c(round, game_id, home_team, away_team, y_true, true_outcome),
    names_from = method,
    values_from = c(p_home, p_away),
    names_glue = "{.value}_{method}"
  ) %>%
  mutate(across(starts_with("p_"), ~round(.x, 3))) %>%
  arrange(round, game_id)



per_game_table <- per_game_table %>%
  dplyr::select(
    round, game_id, home_team, away_team, y_true, true_outcome,
    `p_home_Spike–Slab`, `p_away_Spike–Slab`,
    p_home_Glickman, p_away_Glickman
  )


print(per_game_table, n = Inf)
