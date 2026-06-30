#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(lubridate)
library(readr)
library(cmdstanr)

#   ____________________________________________________________________________
#   Functions                                                               ####

source("Scripts/Functions.R")

#   ____________________________________________________________________________
#   Data                                                                    ####

df <- read_csv("nba_data.csv", col_types = cols(
  gameDate = col_datetime(),
  hometeamId = col_character(),
  awayteamId = col_character(),
  homeScore = col_double(),
  awayScore = col_double()
))

df <- df %>% mutate(season = assign_season(gameDate))

last_season_label <- assign_season(max(df$gameDate, na.rm = TRUE))
last_start <- as.integer(substr(last_season_label, 1, 4)) # e.g. "2024–25" -> 2024
last10 <- sprintf("%d–%02d", seq(last_start - 9L, last_start), (seq(last_start - 9L, last_start) + 1L) %% 100)

df10 <- df %>%
  filter(season %in% last10) %>%
  mutate(
    season = factor(season, levels = last10),
    instants_rank = as.integer(season)
  ) %>%
  arrange(gameDate)

df10_nopre <- df10 %>%
  filter(gameType != "Preseason")

# Drop Playoffs from the most recent season
df10_final <- df10_nopre %>%
  filter(!(instants_rank == max(instants_rank, na.rm = TRUE) &
    gameType == "Playoffs"))

# Sanity check
table(df10_final$season, df10_final$instants_rank)


##  ............................................................................
##  Playoffs                                                                ####

playoffs <- df10 %>%
  filter(
    instants_rank == max(instants_rank, na.rm = TRUE),
    gameType == "Playoffs"
  )

# Main training set
train <- df10_final

# Same team mapping
team_ids <- sort(unique(c(train$hometeamId, train$awayteamId)))
team_lookup <- setNames(seq_along(team_ids), team_ids)

# Playoff matchup indices
playoffs_data <- playoffs %>%
  mutate(
    team1_prev = team_lookup[hometeamId],
    team2_prev = team_lookup[awayteamId],
    instants_rank = as.integer(instants_rank)
  )


##  ............................................................................
##  Build Scenarios                                                         ####

# Playoff rounds in order
playoff_rounds <- c(
  "East First Round", "West First Round",
  "East Conf. Semifinals", "West Conf. Semifinals",
  "East Conf. Finals", "West Conf. Finals",
  "NBA Finals"
)

# Second half scenario
second_half_scenario <- create_second_half_scenario(train)

pred_scenarios <- create_prediction_scenarios(train, playoffs_data)


# Add it to the rest
pred_scenarios[["predict_second_half"]] <- second_half_scenario

save(pred_scenarios, file = "Data/pred_scenarios.RData")
