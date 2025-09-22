library(dplyr)
library(ggplot2)
library(posterior)
library(tidyr)
library(patchwork)

# Inputs you already have:
# y_obs <- stan_data$y                       # 0/1, home win
# y_rep_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("y_rep", format = "matrix"))
draws_spike_slab <- fit_wbt_spike_slab$draws(format = "draws_df")

# Extract y_rep (posterior predictive samples)
y_rep_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("y_rep", format = "matrix"))

y_obs <- stan_data$y
G <- length(y_obs)
S <- stan_data$ntimes_rank
season <- stan_data$instants_rank          # 1..S for each game
home_team <- stan_data$team1
away_team <- stan_data$team2
nteams <- stan_data$nteams


# Posterior-predictive mean probability per game
p_hat <- colMeans(y_rep_spike_slab)        # length G



# Helper to convert a vector of outcomes into team-season win counts
team_season_wins <- function(y_vec){
  # each game contributes y to home team, (1 - y) to away team
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

# Observed wins
obs_wins <- team_season_wins(y_obs)

# Replicated wins per draw (this is memory heavy if many draws; we compute envelopes per season-team)
# For efficiency, do it season-by-season:
rep_wins_ci <- lapply(1:S, function(s){
  rows <- season == s
  # For these games, construct per-draw team wins efficiently
  # Build sparse contribution vectors
  H <- matrix(0, nrow = nteams, ncol = sum(rows))
  A <- matrix(0, nrow = nteams, ncol = sum(rows))
  H[cbind(home_team[rows], seq_len(sum(rows)))] <- 1
  A[cbind(away_team[rows], seq_len(sum(rows)))] <- 1

  # y_rep subset for these games: draws x games_s
  Y <- y_rep_spike_slab[, rows, drop = FALSE]
  # team wins per draw: (H %*% y) + (A %*% (1 - y))
  # result: nteams x draws
  TW <- H %*% t(Y) + A %*% (1 - t(Y))   # matrix mult; careful with dims

  tibble(
    team = rep(1:nteams, each = 1),
    season = s,
    mean = rowMeans(TW),
    lo = apply(TW, 1, quantile, 0.025),
    hi = apply(TW, 1, quantile, 0.975)
  )
}) %>% bind_rows() %>%
  left_join(obs_wins, by = c("season","team")) %>%
  rename(obs = wins)



team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))

team_meta <- dplyr::bind_rows(
  nba_data %>% transmute(team_id = hometeamId,
                         team_label = paste0(hometeamCity, " ", hometeamName)),
  nba_data %>% transmute(team_id = awayteamId,
                         team_label = paste0(awayteamCity, " ", awayteamName))
) %>% distinct(team_id, .keep_all = TRUE)

# Vector of labels aligned to 1:nteams (i.e., the indices in stan_data$team1)
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
  "CHA"  # Charlotte Hornets
)

rep_wins_ci_labeled <- rep_wins_ci %>%
      mutate(team_abbr = factor(team, levels = seq_len(nteams), labels = teams_abbr))

df_facets <- rep_wins_ci_labeled %>%
  arrange(season, team) %>%   # keep polygon from crossing itself
  mutate(x = as.integer(team_abbr))

start_year <- 2015
season_labels <- sapply(0:(S - 1), function(i) {
  sprintf("%02d/%02d", (start_year + i) %% 100, (start_year + i + 1) %% 100)
})

# Named vector mapping season index -> label
season_lab_map <- setNames(season_labels, as.character(1:S))
my_labeller <- labeller(season = as_labeller(function(x) season_lab_map[as.character(x)]))


p_team_wins_facets <- df_facets %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey30", alpha = 0.2) +
  geom_point(aes(y = mean, color = "Posterior predictive mean"), size = 1.8) +
  geom_point(aes(y = obs,  color = "Observed"), shape = 18, size = 1.9) +
  scale_x_continuous(
    breaks = seq_len(nteams),
    labels = levels(df_facets$team_abbr)
  ) +
  facet_wrap(~ season, ncol = 2, labeller = my_labeller) +
  scale_color_manual(
    name = NULL,
    values = c("Posterior predictive mean" = "#c41010", "Observed" = "black")
  ) +
  labs(y = "Wins", x = "Team") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 15, angle = 60, hjust = 0.5, vjust = 0.7),
    strip.background = element_rect(fill = "grey95")
  )

p_team_wins_facets

ggsave(
  filename = "p_team_wins_facets.pdf", path = "Plots",
  plot = p_team_wins_facets,
  width = 17, height = 14, device = "pdf", dpi = 500
)

