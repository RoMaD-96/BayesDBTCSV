#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(lubridate)
library(readr)
library(cmdstanr)


#   ____________________________________________________________________________
#   Data                                                                    ####

df <- read_csv("nba_data.csv", col_types = cols(
  gameDate = col_datetime(),
  hometeamId = col_character(),
  awayteamId = col_character(),
  homeScore = col_double(),
  awayScore = col_double()
))

assign_season <- function(dates) {
  yrs <- year(dates)
  mos <- month(dates)
  season_end <- if_else(mos >= 10, yrs + 1L, yrs)
  season_start <- season_end - 1L
  sprintf("%d–%02d", season_start, season_end %% 100)
}

df <- df %>% mutate(season = assign_season(gameDate))

last_season_label <- assign_season(max(df$gameDate, na.rm = TRUE))
last_start <- as.integer(substr(last_season_label, 1, 4))      # e.g. "2024–25" -> 2024
last10 <- sprintf("%d–%02d", seq(last_start - 9L, last_start), (seq(last_start - 9L, last_start) + 1L) %% 100)

df10 <- df %>%
  filter(season %in% last10) %>%
  mutate(
    season = factor(season, levels = last10),                 # oldest -> newest
    instants_rank = as.integer(season)                        # 1..10
  ) %>%
  arrange(gameDate)

df10_nopre <- df10 %>%
  filter(gameType != "Preseason")

# For the most recent season only, remove Playoffs
df10_final <- df10_nopre %>%
  filter(!(instants_rank == max(instants_rank, na.rm = TRUE) &
             gameType == "Playoffs"))

# Check
table(df10_final$season, df10_final$instants_rank)


##  ............................................................................
##  Playoffs                                                                ####

playoffs <- df10 %>%
  filter(instants_rank == max(instants_rank, na.rm = TRUE),
         gameType == "Playoffs")

# Build the main training set
train <- df10_final

# Re-use the same team mapping
team_ids <- sort(unique(c(train$hometeamId, train$awayteamId)))
team_lookup <- setNames(seq_along(team_ids), team_ids)

# Build playoff matchup indices
playoffs_data <- playoffs %>%
  mutate(
    team1_prev = team_lookup[hometeamId],
    team2_prev = team_lookup[awayteamId],
    instants_rank = as.integer(instants_rank)
  )


##  ............................................................................
##  Predictive scenario function                                            ####

# Define the playoff rounds in order
playoff_rounds <- c(
  "East First Round", "West First Round",
  "East Conf. Semifinals", "West Conf. Semifinals",
  "East Conf. Finals", "West Conf. Finals",
  "NBA Finals"
)

# Function to create prediction scenarios with progressive instants_rank
create_prediction_scenarios <- function(train_data, playoff_data) {

  # Get the maximum instants_rank from historical training data
  max_historical_rank <- max(train_data$instants_rank, na.rm = TRUE)

  # Organize playoff data by rounds
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

  # Scenario 1: Predict First Round
  first_round_predict <- playoff_by_round %>%
    filter(round_order == 1) %>%
    mutate(instants_rank = max_historical_rank)

  scenarios[["predict_first_round"]] <- list(
    name = "Predict First Round",
    train_data = train_data,
    predict_data = first_round_predict,
    description = paste0("Predict First Round games using only regular season historical data (instants_rank=", max_historical_rank, ")")
  )

  # Scenario 2: Predict Semifinals (using original data + First Round results)
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

  # Scenario 3: Predict Conference Finals (using original data + First Round + Semifinals)
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

  # Scenario 4: Predict NBA Finals (using original data + all previous playoff rounds)
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


pred_scenarios <- create_prediction_scenarios(train, playoffs_data)

save(pred_scenarios, file = "Data/pred_scenarios.RData")
