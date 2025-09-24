#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(ggplot2)
library(posterior)
library(tidyr)
library(patchwork)

#   ____________________________________________________________________________
#   Models                                                                  ####

source("Scripts/nba_code.R")


#   ____________________________________________________________________________
#   Total Win Data                                                          ####

draws_spike_slab <- fit_wbt_spike_slab$draws(format = "draws_df")

# Extract y_rep
y_rep_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("y_rep", format = "matrix"))

y_obs <- stan_data$y
G <- length(y_obs)
S <- stan_data$ntimes_rank
season <- stan_data$instants_rank
home_team <- stan_data$team1
away_team <- stan_data$team2
nteams <- stan_data$nteams


# Posterior-predictive mean probability per game
p_hat <- colMeans(y_rep_spike_slab)


# Observed wins
obs_wins <- team_season_wins(y_obs)

# Replicated wins per draw
rep_wins_ci <- lapply(1:S, function(s) {
  rows <- season == s
  H <- matrix(0, nrow = nteams, ncol = sum(rows))
  A <- matrix(0, nrow = nteams, ncol = sum(rows))
  H[cbind(home_team[rows], seq_len(sum(rows)))] <- 1
  A[cbind(away_team[rows], seq_len(sum(rows)))] <- 1

  # y_rep subset for these games
  Y <- y_rep_spike_slab[, rows, drop = FALSE]
  # team wins per draw: (H %*% y) + (A %*% (1 - y))
  TW <- H %*% t(Y) + A %*% (1 - t(Y)) # matrix mult; careful with dims

  tibble(
    team = rep(1:nteams, each = 1),
    season = s,
    mean = rowMeans(TW),
    lo = apply(TW, 1, quantile, 0.025),
    hi = apply(TW, 1, quantile, 0.975)
  )
}) %>%
  bind_rows() %>%
  left_join(obs_wins, by = c("season", "team")) %>%
  rename(obs = wins)



team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))

team_meta <- dplyr::bind_rows(
  nba_data %>% transmute(
    team_id = hometeamId,
    team_label = paste0(hometeamCity, " ", hometeamName)
  ),
  nba_data %>% transmute(
    team_id = awayteamId,
    team_label = paste0(awayteamCity, " ", awayteamName)
  )
) %>% distinct(team_id, .keep_all = TRUE)

# Vector of labels
teams_labels <- team_meta$team_label[match(team_ids, team_meta$team_id)]
teams_abbr <- c(
  "ATL", # Atlanta Hawks
  "BOS", # Boston Celtics
  "CLE", # Cleveland Cavaliers
  "NOP", # New Orleans Pelicans
  "CHI", # Chicago Bulls
  "DAL", # Dallas Mavericks
  "DEN", # Denver Nuggets
  "GSW", # Golden State Warriors
  "HOU", # Houston Rockets
  "LAC", # Los Angeles Clippers
  "LAL", # Los Angeles Lakers
  "MIA", # Miami Heat
  "MIL", # Milwaukee Bucks
  "MIN", # Minnesota Timberwolves
  "BKN", # Brooklyn Nets
  "NYK", # New York Knicks
  "ORL", # Orlando Magic
  "IND", # Indiana Pacers
  "PHI", # Philadelphia 76ers
  "PHX", # Phoenix Suns
  "POR", # Portland Trail Blazers
  "SAC", # Sacramento Kings
  "SAS", # San Antonio Spurs
  "OKC", # Oklahoma City Thunder
  "TOR", # Toronto Raptors
  "UTA", # Utah Jazz
  "MEM", # Memphis Grizzlies
  "WAS", # Washington Wizards
  "DET", # Detroit Pistons
  "CHA" # Charlotte Hornets
)

rep_wins_ci_labeled <- rep_wins_ci %>%
  mutate(team_abbr = factor(team, levels = seq_len(nteams), labels = teams_abbr))

df_facets <- rep_wins_ci_labeled %>%
  arrange(season, team) %>%
  mutate(x = as.integer(team_abbr))

start_year <- 2015
season_labels <- sapply(0:(S - 1), function(i) {
  sprintf("%02d/%02d", (start_year + i) %% 100, (start_year + i + 1) %% 100)
})

# Labels
season_lab_map <- setNames(season_labels, as.character(1:S))
my_labeller <- labeller(season = as_labeller(function(x) season_lab_map[as.character(x)]))


#   ____________________________________________________________________________
#   Plots                                                                   ####


# Plot 1: Last season with teams sorted by wins
df_last_season <- df_facets %>%
  filter(season == S) %>%
  arrange(desc(obs)) %>% # Sort by observed wins
  mutate(x_sorted = row_number())

p_in_sample_24_25 <- df_last_season %>%
  ggplot(aes(x = x_sorted)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey30", alpha = 0.2) +
  geom_point(aes(y = mean, color = "Posterior predictive mean"), size = 2.4) +
  geom_point(aes(y = obs, color = "Observed"), shape = 18, size = 2.5) +
  scale_x_continuous(
    breaks = seq_len(nteams),
    labels = df_last_season$team_abbr[order(df_last_season$x_sorted)]
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Posterior predictive mean" = "#c41010", "Observed" = "black")
  ) +
  labs(
    y = "Wins",
    x = "Team"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 17),
    axis.title.x = element_text(size = 17),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.position = "top",
    axis.text.x = element_text(size = 13, angle = 45, hjust = 0.5, vjust = 0.7),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm")
  )

# Plot 2: Other 9 seasons
df_other_seasons <- df_facets %>%
  filter(season < S) %>%
  arrange(season, team)

p_in_sample_other_seasons <- df_other_seasons %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey30", alpha = 0.2) +
  geom_point(aes(y = mean, color = "Posterior predictive mean"), size = 1.8) +
  geom_point(aes(y = obs, color = "Observed"), shape = 18, size = 1.9) +
  scale_x_continuous(
    breaks = seq_len(nteams),
    labels = levels(df_facets$team_abbr)
  ) +
  facet_wrap(~season, ncol = 3, labeller = my_labeller) +
  scale_color_manual(
    name = NULL,
    values = c("Posterior predictive mean" = "#c41010", "Observed" = "black")
  ) +
  labs(y = "Wins", x = "Team") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.placement = "outside",
    strip.text.x = element_text(size = 17),
    strip.text.y = element_text(size = 17),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 19),
    axis.title.x = element_text(size = 19),
    legend.title = element_blank(),
    legend.text = element_text(size = 17),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 0.5, vjust = 0.7),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm")
  )

print(p_in_sample_24_25)
print(p_in_sample_other_seasons)

ggsave(
  filename = "p_in_sample_24_25.pdf", path = "Plots",
  plot = p_in_sample_24_25,
  width = 12, height = 7, device = "pdf", dpi = 500
)
ggsave(
  filename = "p_in_sample_other_seasons.pdf", path = "Plots",
  plot = p_in_sample_other_seasons,
  width = 17, height = 14, device = "pdf", dpi = 500
)
