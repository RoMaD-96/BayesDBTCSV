#   ____________________________________________________________________________
#   Build Stan data                                                         ####

prepare_stan_data <- function(data,
                              newdata = NULL,
                              mean_logStrength = 0,
                              ind_home = 1,
                              mean_home = 0,
                              sd_home = 10,
                              p_spike = 0.05,
                              mu_slab = 0,
                              mu_spike = 30,
                              sd_slab = 5,
                              sd_spike = 0.1,
                              s_prior_shape = 1,
                              s_prior_rate = 1,
                              phi = 1.5,
                              rho = 5) {
  team_ids <- sort(unique(c(data$hometeamId, data$awayteamId)))
  team_lookup <- setNames(seq_along(team_ids), team_ids)

  data2 <- data %>%
    mutate(
      team1 = team_lookup[hometeamId],
      team2 = team_lookup[awayteamId],
      winner_team = team_lookup[as.factor(winner)],
      y = as.integer(homeScore > awayScore)
    )

  stan_list <- list(
    N = nrow(data2),
    nteams = length(team_ids),
    ntimes_rank = length(unique(data2$instants_rank)),
    instants_rank = data2$instants_rank,
    reference_season = max(data2$instants_rank),
    team1 = as.integer(data2$team1),
    team2 = as.integer(data2$team2),
    winner_team = as.integer(data2$winner_team),
    mean_logStrength = mean_logStrength,
    y = data2$y,
    ind_home = as.integer(ind_home),
    mean_home = mean_home,
    sd_home = sd_home,
    s_prior_shape = s_prior_shape,
    s_prior_rate = s_prior_rate,
    # Spike-and-slab prior
    p_spike = p_spike,
    mu_slab = mu_slab,
    mu_spike = mu_spike,
    sd_slab = sd_slab,
    sd_spike = sd_spike,
    phi = phi,
    rho = rho
  )

  # Append playoff data if given
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


#   ____________________________________________________________________________
#   Download & resize logos                                                 ####

download_and_resize_logo <- function(team, url, size = 50) {
  # Safe filename
  file_name <- gsub(" ", "_", team)
  original_path <- file.path("Logos", paste0(file_name, ".png"))
  small_path <- file.path("Logos_small", paste0(file_name, "_small.png"))

  # Skip if the small version exists
  if (!file.exists(small_path)) {
    # Download the original if missing
    if (!file.exists(original_path)) {
      download.file(url, original_path, mode = "wb", quiet = TRUE)
    }

    # Resize the image
    img <- image_read(original_path)
    img_small <- image_resize(img, paste0(size, "x", size))
    image_write(img_small, small_path)

    message("Processed: ", team)
  }

  return(small_path)
}



#   ____________________________________________________________________________
#   Fit scenario                                                            ####

fit_scenario <- function(scenario, model_type = "glick_2001", seed = 433) {
  cat("\\n=== ", scenario$name, " ===\\n")
  cat(scenario$description, "\\n")
  cat("Training games:", nrow(scenario$train_data), "\\n")
  cat("Prediction games:", nrow(scenario$predict_data), "\\n")

  # Prepare Stan data
  stan_data <- prepare_stan_data(
    data = scenario$train_data,
    newdata = scenario$predict_data
  )

  # Pick the model
  if (model_type == "glick_2001") {
    model <- model_glick
  } else if (model_type == "const_var") {
    model <- model_const_var
  } else if (model_type == "wbt_spike_slab") {
    model <- model_wbt_spike_slab
  } else if (model_type == "exp_bt") {
    model <- model_exp_bt
  } else {
    stop("Unknown model type")
  }

  # Fit model
  fit <- model$sample(
    data = stan_data,
    seed = seed,
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
#   LOOIC                                                                   ####

get_looic <- function(loo_obj) {
  if (is.null(loo_obj)) {
    return(NA_real_)
  }
  if (!is.null(loo_obj$looic)) {
    return(as.numeric(loo_obj$looic))
  }
  if (!is.null(loo_obj$estimates)) {
    est <- loo_obj$estimates
    rn <- rownames(est)
    if (!is.null(rn) && "looic" %in% rn) {
      return(as.numeric(est["looic", "Estimate"]))
    }
    if (!is.null(rn) && "elpd_loo" %in% rn) {
      return(as.numeric(-2 * est["elpd_loo", "Estimate"]))
    }
  }
  if (!is.null(loo_obj$elpd_loo)) {
    return(as.numeric(-2 * loo_obj$elpd_loo))
  }
  NA_real_
}




#   ____________________________________________________________________________
#   Brier score                                                             ####

compute_brier_score <- function(result) {
  fit <- result$fit
  scenario <- result$scenario
  stan_data <- result$stan_data
  if (stan_data$N_prev == 0) {
    return(NA_real_)
  }
  dm <- posterior::as_draws_matrix(fit$draws(variables = "y_prev"))
  p_hat_named <- colMeans(dm)
  idx <- as.integer(gsub("^y_prev\\[|\\]$", "", names(p_hat_named)))
  p_hat <- as.numeric(p_hat_named[order(idx)])
  y_oos <- as.integer(scenario$predict_data$homeScore > scenario$predict_data$awayScore)
  if (length(p_hat) != length(y_oos)) {
    return(NA_real_)
  }
  brier_per_obs <- (p_hat - y_oos)^2 + ((1 - p_hat) - (1 - y_oos))^2
  brier_score <- mean(brier_per_obs)
  return(brier_score)
}





#   ____________________________________________________________________________
#   Tidy LOOIC table                                                        ####

res_to_df <- function(res_list, method_name) {
  tibble(
    scenario = names(res_list),
    value    = map_dbl(res_list, ~ get_looic(.x$loo)),
    method   = method_name,
    metric   = "LOOIC"
  )
}



#   ____________________________________________________________________________
#   Contour plot joined                                                     ####

extract_team_draws <- function(fit, strength_var = "logStrength", sd_var, team_index, n_seasons = 10) {
  draws_strength <- fit$draws(variables = strength_var, format = "draws_df")
  draws_sd <- fit$draws(variables = sd_var, format = "draws_df")

  out <- vector("list", n_seasons)
  for (t in seq_len(n_seasons)) {
    strength_col <- sprintf("%s[%d,%d]", strength_var, t, team_index)
    sd_col <- sprintf("%s[%d,%d]", sd_var, t, team_index)

    out[[t]] <- tibble(
      logStrength = draws_strength[[strength_col]],
      sd          = draws_sd[[sd_col]], # standardize y name
      Season      = t,
      .draw       = seq_len(nrow(draws_strength))
    )
  }
  bind_rows(out) %>%
    mutate(Season_Label = factor(paste("Season", Season),
      levels = paste("Season", seq_len(n_seasons))
    ))
}



#   ____________________________________________________________________________
#   Team-season wins                                                        ####

team_season_wins <- function(y_vec) {
  # home team gets y, away team gets 1 - y
  df <- tibble(season, home_team, away_team, y = y_vec)
  bind_rows(
    df %>% transmute(season, team = home_team, win = y),
    df %>% transmute(season, team = away_team, win = 1 - y)
  ) %>%
    group_by(season, team) %>%
    summarise(wins = sum(win), .groups = "drop") %>%
    complete(season = 1:S, team = 1:nteams, fill = list(wins = 0)) %>%
    arrange(season, team)
}



#   ____________________________________________________________________________
#   Build labeler from train                                                ####

build_labeler_from_train <- function(train_df) {
  team_ids <- sort(unique(c(train_df$hometeamId, train_df$awayteamId)))

  has_names <- all(c("hometeamCity", "hometeamName", "awayteamCity", "awayteamName") %in% names(train_df))
  if (has_names) {
    team_meta <- bind_rows(
      train_df %>% transmute(
        team_id = hometeamId,
        team_label = paste0(hometeamCity, " ", hometeamName)
      ),
      train_df %>% transmute(
        team_id = awayteamId,
        team_label = paste0(awayteamCity, " ", awayteamName)
      )
    ) %>%
      distinct(team_id, .keep_all = TRUE)

    id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
  } else {
    # No names, label by ID
    id_to_label <- setNames(team_ids, team_ids)
  }

  # index to label
  idx_to_label <- setNames(unname(id_to_label[team_ids]), seq_along(team_ids))
  list(team_ids = team_ids, id_to_label = id_to_label, idx_to_label = idx_to_label)
}

#   ____________________________________________________________________________
#   Extract Match Probabilities                                             ####

extract_match_probs <- function(results_list, method_label) {
  map2_dfr(results_list, names(results_list), function(res, scen_name) {
    fit <- res$fit
    scenario <- res$scenario
    sd <- res$stan_data
    if (is.null(sd$N_prev) || sd$N_prev == 0) {
      return(NULL)
    }

    # Posterior home-win probs in y_prev order
    dm <- posterior::as_draws_matrix(fit$draws(variables = "y_prev"))
    p_hat_named <- colMeans(dm)
    idx <- as.integer(gsub("^y_prev\\[|\\]$", "", names(p_hat_named)))
    ord <- order(idx)
    p_home <- as.numeric(p_hat_named)[ord]
    n_prev <- length(p_home)

    # Labelers from train data
    build_labeler_from_train <- function(train_df) {
      team_ids <- sort(unique(c(train_df$hometeamId, train_df$awayteamId)))
      has_names <- all(c("hometeamCity", "hometeamName", "awayteamCity", "awayteamName") %in% names(train_df))
      if (has_names) {
        team_meta <- bind_rows(
          train_df %>% transmute(
            team_id = hometeamId,
            team_label = paste0(hometeamCity, " ", hometeamName)
          ),
          train_df %>% transmute(
            team_id = awayteamId,
            team_label = paste0(awayteamCity, " ", awayteamName)
          )
        ) %>% distinct(team_id, .keep_all = TRUE)
        id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
      } else {
        id_to_label <- setNames(team_ids, team_ids)
      }
      idx_to_label <- setNames(unname(id_to_label[team_ids]), seq_along(team_ids))
      list(team_ids = team_ids, id_to_label = id_to_label, idx_to_label = idx_to_label)
    }
    lab <- build_labeler_from_train(scenario$train_data)

    # Team labels from predict_data IDs, in y_prev order
    pd <- scenario$predict_data
    ht <- at <- rep(NA_character_, n_prev)
    if (all(c("hometeamId", "awayteamId") %in% names(pd))) {
      ht <- unname(lab$id_to_label[as.character(pd$hometeamId)])[ord]
      at <- unname(lab$id_to_label[as.character(pd$awayteamId)])[ord]
    }
    # Fall back to Stan indices
    if ((all(is.na(ht)) || all(is.na(at))) && !is.null(sd$team1_prev) && !is.null(sd$team2_prev)) {
      h_idx <- as.integer(sd$team1_prev)[ord]
      a_idx <- as.integer(sd$team2_prev)[ord]
      if (min(c(h_idx, a_idx), na.rm = TRUE) == 0) {
        h_idx <- h_idx + 1
        a_idx <- a_idx + 1
      }
      ht <- unname(lab$idx_to_label[as.character(h_idx)])
      at <- unname(lab$idx_to_label[as.character(a_idx)])
    }

    if (all(c("hometeamId", "awayteamId") %in% names(pd))) {
      ht <- ifelse(is.na(ht), as.character(pd$hometeamId)[ord], ht)
      at <- ifelse(is.na(at), as.character(pd$awayteamId)[ord], at)
    }

    # True outcome
    y_true <- if (all(c("homeScore", "awayScore") %in% names(pd))) {
      as.integer(pd$homeScore > pd$awayScore)[ord]
    } else {
      rep(NA_integer_, n_prev)
    }
    true_outcome <- ifelse(is.na(y_true), NA_character_,
      ifelse(y_true == 1, "Home Win", "Away Win")
    )

    tibble(
      scenario = scen_name,
      game_id = seq_len(n_prev),
      home_team = ht,
      away_team = at,
      y_true = y_true,
      true_outcome = true_outcome,
      p_home = p_home,
      p_away = 1 - p_home,
      method = method_label
    )
  })
}



#   ____________________________________________________________________________
#   Assign Season                                                           ####

# Map game dates to season labels like "2024–25"
assign_season <- function(dates) {
  yrs <- year(dates)
  mos <- month(dates)
  season_end <- if_else(mos >= 10, yrs + 1L, yrs)
  season_start <- season_end - 1L
  sprintf("%d–%02d", season_start, season_end %% 100)
}



#   ____________________________________________________________________________
#   Prediction Scenarios                                                    ####

# Build playoff prediction scenarios with progressive instants_rank
create_prediction_scenarios <- function(train_data, playoff_data) {

  # Latest rank in the historical training data
  max_historical_rank <- max(train_data$instants_rank, na.rm = TRUE)

  # Tag playoff games by round
  playoff_by_round <- playoff_data %>%
    mutate(
      round_order = case_when(
        gameLabel %in% c("East First Round", "West First Round") ~ 1,
        gameLabel %in% c("East Conf. Semifinals", "West Conf. Semifinals") ~ 2,
        gameLabel %in% c("East Conf. Finals", "West Conf. Finals") ~ 3,
        gameLabel == "NBA Finals" ~ 4,
        TRUE ~ 5
      ),
      round_name = case_when(
        round_order == 1 ~ "First Round",
        round_order == 2 ~ "Semifinals",
        round_order == 3 ~ "Conference Finals",
        round_order == 4 ~ "NBA Finals",
        TRUE ~ "Other"
      )
    ) %>%
    arrange(gameDate)

  scenarios <- list()

  # Scenario 1: predict First Round
  first_round_predict <- playoff_by_round %>%
    filter(round_order == 1) %>%
    mutate(instants_rank = max_historical_rank)

  scenarios[["predict_first_round"]] <- list(
    name = "Predict First Round",
    train_data = train_data,
    predict_data = first_round_predict,
    description = paste0("Predict First Round games using only regular season historical data (instants_rank=", max_historical_rank, ")")
  )

  # Scenario 2: predict Semifinals (historical + First Round)
  first_round_games <- playoff_by_round %>%
    filter(round_order == 1) %>%
    mutate(instants_rank = max_historical_rank + 1)

  if (nrow(first_round_games) > 0) {
    semifinals_predict <- playoff_by_round %>%
      filter(round_order == 2) %>%
      mutate(instants_rank = max_historical_rank + 1)

    scenarios[["predict_semifinals"]] <- list(
      name = "Predict Semifinals",
      train_data = bind_rows(train_data, first_round_games),
      predict_data = semifinals_predict,
      description = paste0("Predict Semifinals using historical data + First Round results (instants_rank=", max_historical_rank + 1, ")")
    )
  }

  # Scenario 3: predict Conference Finals (historical + First Round + Semifinals)
  first_round_train <- playoff_by_round %>%
    filter(round_order == 1) %>%
    mutate(instants_rank = max_historical_rank + 1)

  semifinals_train <- playoff_by_round %>%
    filter(round_order == 2) %>%
    mutate(instants_rank = max_historical_rank + 2)

  if (nrow(first_round_train) > 0 && nrow(semifinals_train) > 0) {
    conf_finals_predict <- playoff_by_round %>%
      filter(round_order == 3) %>%
      mutate(instants_rank = max_historical_rank + 2)

    scenarios[["predict_conf_finals"]] <- list(
      name = "Predict Conference Finals",
      train_data = bind_rows(train_data, first_round_train, semifinals_train),
      predict_data = conf_finals_predict,
      description = paste0("Predict Conference Finals using historical data + First Round (rank ", max_historical_rank + 1, ") + Semifinals (rank ", max_historical_rank + 2, ")")
    )
  }

  # Scenario 4: predict NBA Finals (historical + all previous rounds)
  first_round_train4 <- playoff_by_round %>%
    filter(round_order == 1) %>%
    mutate(instants_rank = max_historical_rank + 1)

  semifinals_train4 <- playoff_by_round %>%
    filter(round_order == 2) %>%
    mutate(instants_rank = max_historical_rank + 2)

  conf_finals_train4 <- playoff_by_round %>%
    filter(round_order == 3) %>%
    mutate(instants_rank = max_historical_rank + 3)

  if (nrow(first_round_train4) > 0 && nrow(semifinals_train4) > 0 && nrow(conf_finals_train4) > 0) {
    nba_finals_predict <- playoff_by_round %>%
      filter(round_order == 4) %>%
      mutate(instants_rank = max_historical_rank + 3)

    scenarios[["predict_nba_finals"]] <- list(
      name = "Predict NBA Finals",
      train_data = bind_rows(train_data, first_round_train4, semifinals_train4, conf_finals_train4),
      predict_data = nba_finals_predict,
      description = paste0("Predict NBA Finals using historical data + First Round (rank ", max_historical_rank + 1, ") + Semifinals (rank ", max_historical_rank + 2, ") + Conference Finals (rank ", max_historical_rank + 3, ")")
    )
  }

  return(scenarios)
}



#   ____________________________________________________________________________
#   Second Half Scenario                                                    ####

# Split the regular season into first/second half and predict the second
create_second_half_scenario <- function(train_data) {

  # Most recent season's regular season games
  max_rank <- max(train_data$instants_rank, na.rm = TRUE)

  current_season_reg <- train_data %>%
    filter(instants_rank == max_rank, gameType == "Regular Season")

  # Everything except the current season
  historical_data <- train_data %>%
    filter(instants_rank < max_rank)

  # Split the current season by date
  if (nrow(current_season_reg) > 0) {
    # Midpoint date
    season_dates <- sort(unique(current_season_reg$gameDate))
    midpoint_date <- season_dates[ceiling(length(season_dates) / 2)]

    # First half: games before the midpoint
    first_half <- current_season_reg %>%
      filter(gameDate < midpoint_date) %>%
      mutate(instants_rank = max_rank)  # same rank as historical

    # Second half: games from the midpoint on (to predict)
    second_half <- current_season_reg %>%
      filter(gameDate >= midpoint_date) %>%
      mutate(instants_rank = max_rank)  # same rank for prediction

    scenario <- list(
      name = "Predict Second Half of Season",
      train_data = bind_rows(historical_data, first_half),
      predict_data = second_half,
      description = paste0(
        "Predict second half of regular season (",
        nrow(second_half), " games from ",
        format(min(second_half$gameDate), "%Y-%m-%d"), " to ",
        format(max(second_half$gameDate), "%Y-%m-%d"),
        ") using historical data + first half (",
        nrow(first_half), " games)"
      ),
      midpoint_date = midpoint_date,
      n_train_games = nrow(first_half),
      n_predict_games = nrow(second_half)
    )

    return(scenario)
  } else {
    return(NULL)
  }
}
