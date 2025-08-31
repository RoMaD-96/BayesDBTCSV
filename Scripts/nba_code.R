library(dplyr)
library(lubridate)
library(readr)
library(cmdstanr)

#   ____________________________________________________________________________
#   Data                                                                    ####

load("~/Desktop/Weighted_BTD/Data/pred_scenarios.RData")

nba_data <- pred_scenarios$predict_first_round$train_data

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
    s_prior_shape = 2,
    s_prior_rate = 1,
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



stan_data <- prepare_stan_data(nba_data)
str(stan_data)


model_glick <- cmdstan_model("~/Desktop/Weighted_BTD/glickman_2001.stan")
model_wbt_spike_slab <- cmdstan_model("~/Desktop/Weighted_BTD/wbt_spike_slab.stan")

## 3. Sample -----

fit_glick <- model_glick$sample(
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

loo_glick <- fit_glick$loo()
loo_wbt_spike_slab <- fit_wbt_spike_slab$loo()

loo::loo_compare(loo_glick, loo_wbt_spike_slab)

fit_wbt_spike_slab$summary(variables="sd_logStrength")
fit_glick$summary(variables="sigma")



#   ____________________________________________________________________________
#   Plots                                                                   ####

# Packages
library(dplyr)
library(ggplot2)
library(tidybayes)   # gather_draws(), median_qi()
library(posterior)   # fit$draws()
nba_data <- pred_scenarios$predict_first_round$train_data
# --- 0) Build a lookup: team ID -> "City Team"
team_meta <- bind_rows(
  nba_data %>% transmute(team_id = hometeamId,
                           team_label = paste0(hometeamCity, " ", hometeamName)),
  nba_data %>% transmute(team_id = awayteamId,
                           team_label = paste0(awayteamCity, " ", awayteamName))
) %>%
  distinct(team_id, .keep_all = TRUE)

id_to_label <- setNames(team_meta$team_label, team_meta$team_id)

# These must match how you built stan_data$team1/2 :
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])  # same order as team_ids

draws_df <- fit_glick$draws(variables = "logStrength", format = "draws_df")  # cmdstanr → posterior draws_df
td <- tidybayes::gather_draws(draws_df, logStrength[t, k])             # t = time (1..10), k = team index

# --- 2) Map indices to labels and summarize (median + 95% CI)
summ <- td %>%
  mutate(
    Team = factor(team_labels[k], levels = team_labels),
    Rank = as.integer(t)                # 1 (oldest) ... 10 (most recent)
  ) %>%
  group_by(Team, Rank) %>%
  median_qi(.value, .width = 0.95) %>%  # gives .value, .lower, .upper
  ungroup()

# --- 3) Plot: line + 95% ribbon, faceted by team; x-axis 1..10
ggplot(summ, aes(x = Rank, y = .value, group = Team)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.25) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ Team, scales = "fixed") +
  scale_x_continuous(breaks = 1:13) +
  labs(
    x = "Season rank (1 = oldest … 10 = most recent)",
    y = "LogStrength (posterior median ± 95% CI)",
    title = "Team LogStrength across last 10 seasons"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )


# These must match how you built stan_data$team1/2 :
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])  # same order as team_ids

draws_df <- fit_wbt_spike_slab$draws(variables = "logStrength", format = "draws_df")  # cmdstanr → posterior draws_df
td <- tidybayes::gather_draws(draws_df, logStrength[t, k])             # t = time (1..10), k = team index

# --- 2) Map indices to labels and summarize (median + 95% CI)
summ <- td %>%
  mutate(
    Team = factor(team_labels[k], levels = team_labels),
    Rank = as.integer(t)                # 1 (oldest) ... 10 (most recent)
  ) %>%
  group_by(Team, Rank) %>%
  median_qi(.value, .width = 0.95) %>%  # gives .value, .lower, .upper
  ungroup()

# --- 3) Plot: line + 95% ribbon, faceted by team; x-axis 1..10
ggplot(summ, aes(x = Rank, y = .value, group = Team)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.25) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ Team, scales = "free_x") +
  scale_x_continuous(breaks = 1:13) +
  labs(
    x = "Season rank (1 = oldest … 10 = most recent)",
    y = "LogStrength (posterior median ± 95% CI)",
    title = "Team LogStrength across last 10 seasons"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )


# These must match how you built stan_data$team1/2 :
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])  # same order as team_ids

draws_df <- fit_wbt_spike_slab$draws(variables = "logStrength", format = "draws_df")  # cmdstanr → posterior draws_df
td <- tidybayes::gather_draws(draws_df, logStrength[t, k])             # t = time (1..10), k = team index

# --- 2) Map indices to labels and summarize (median + 95% CI)
summ <- td %>%
  mutate(
    Team = factor(team_labels[k], levels = team_labels),
    Rank = as.integer(t)                # 1 (oldest) ... 10 (most recent)
  ) %>%
  group_by(Team, Rank) %>%
  median_qi(.value, .width = 0.95) %>%  # gives .value, .lower, .upper
  ungroup()

# --- 3) Plot: line + 95% ribbon, faceted by team; x-axis 1..10
ggplot(summ, aes(x = Rank, y = .value, group = Team)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.25) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ Team, scales = "fixed") +
  scale_x_continuous(breaks = 1:13) +
  labs(
    x = "Season rank (1 = oldest … 10 = most recent)",
    y = "LogStrength (posterior median ± 95% CI)",
    title = "Team LogStrength across last 10 seasons"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )




