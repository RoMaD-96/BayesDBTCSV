#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(lubridate)
library(readr)
library(cmdstanr)
library(bayesplot)
library(ggplot2)
library(patchwork)
library(posterior)

# Set theme for plots
color_scheme_set("brightblue")

#   ____________________________________________________________________________
#   Models                                                                  ####

source("Scripts/Fit_Models.R")


#   ____________________________________________________________________________
#   Draws from the Posterior Predictive                                     ####

draws_spike_slab <- fit_wbt_spike_slab$draws(format = "draws_df")

y_rep_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("y_rep", format = "matrix"))

y_obs <- stan_data$y


#   ____________________________________________________________________________
#   Posterior Predictive Plots                                              ####

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

# Vector of labels aligned to 1:nteams
facet_labels <- team_meta$team_label[match(team_ids, team_meta$team_id)]

# Factor for grouping
group_fac <- factor(stan_data$winner_team,
  levels = seq_along(team_ids),
  labels = facet_labels
)


##  ............................................................................
##  Plot home/away win probability                                          ####


# Observed proportions per team
obs_df <- data.frame(
  outcome = y_obs,
  team = as.character(group_fac)
)

obs_props <- obs_df %>%
  group_by(team, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(team) %>%
  mutate(prop_obs = n / sum(n)) %>%
  ungroup()

# Posterior predictive proportions per team
team_char <- as.character(group_fac)
teams <- sort(unique(team_char))
n_draws <- nrow(y_rep_spike_slab)

team_indices <- lapply(teams, function(t) which(team_char == t))
names(team_indices) <- teams

# Home-win proportion per team per draw
home_win_props <- sapply(teams, function(t) {
  idx <- team_indices[[t]]
  rowMeans(y_rep_spike_slab[, idx])
})


# Median and 95% CI for home win proportion
yrep_home <- data.frame(
  team = teams,
  median_prop = apply(home_win_props, 2, median),
  lo = apply(home_win_props, 2, quantile, 0.025),
  hi = apply(home_win_props, 2, quantile, 0.975)
)
yrep_home$outcome <- 1

# Away win = 1 - home win
yrep_away <- data.frame(
  team = teams,
  median_prop = 1 - yrep_home$median_prop,
  lo = 1 - yrep_home$hi,
  hi = 1 - yrep_home$lo,
  outcome = 0
)

yrep_summary <- bind_rows(yrep_home, yrep_away)

# Merge with observed
plot_df <- yrep_summary %>%
  left_join(obs_props %>% select(team, outcome, prop_obs),
    by = c("team", "outcome")
  )

plot_df$outcome_label <- factor(plot_df$outcome,
  levels = c(0, 1),
  labels = c("Away Win", "Home Win")
)

# Reorder team factor to match original facet order
plot_df$team <- factor(plot_df$team, levels = facet_labels)



#   ____________________________________________________________________________
#   Plots                                                                   ####

##  ............................................................................
##  Plot home/away win probability                                          ####

p_stat_win <- ggplot(plot_df, aes(x = outcome_label)) +
  # 95% credible interval
  geom_errorbar(aes(ymin = lo, ymax = hi, colour = "yrep"),
    width = 0.2, linewidth = 0.9
  ) +
  # Posterior predictive median
  geom_point(aes(y = median_prop, colour = "yrep"),
    size = 2.5
  ) +
  # Observed proportion
  geom_segment(
    aes(
      x = as.numeric(outcome_label) - 0.25,
      xend = as.numeric(outcome_label) + 0.25,
      y = prop_obs, yend = prop_obs,
      colour = "y"
    ),
    linewidth = 1
  ) +
  scale_colour_manual(
    name = NULL,
    limits = c("yrep", "y"),
    values = c("y" = "#083e9a", "yrep" = "#88beff"),
    labels = c(
      "yrep" = expression(italic(y)[rep]),
      "y"    = expression(italic(y))
    )
  ) +
  facet_wrap(~team, ncol = 6, nrow = 5) +
  coord_cartesian(ylim = c(0.3, 0.7)) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 17),
    strip.text.y = element_text(size = 17),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 19),
    axis.title.x = element_text(size = 19),
    legend.title = element_blank(),
    legend.text = element_text(size = 17),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18, hjust = 0)
  ) +
  labs(y = "Proportion", x = "")

p_stat_win

ggsave(
  filename = "p_stat_win.pdf", path = "Plots",
  plot = p_stat_win,
  width = 17, height = 14, device = "pdf", dpi = 500
)



##  ............................................................................
##  Plot global home win probability                                        ####

p_stat_mean <- ppc_stat(
  y      = y_obs,
  yrep   = y_rep_spike_slab,
  stat   = "mean",
  freq   = FALSE
) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 17),
    axis.title.x = element_text(size = 17),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18, hjust = 0)
  ) +
  labs(
    y = "Frequency",
    x = "Home win probability"
  ) +
  annotate(
    "text",
    x = -Inf, y = 60, label = "PPP = 0.501",
    hjust = -0.1, vjust = 1.2, size = 5
  )

ggsave(
  filename = "p_stat_mean.pdf", path = "Plots",
  plot = p_stat_mean,
  width = 12, height = 7, device = "pdf", dpi = 500
)
