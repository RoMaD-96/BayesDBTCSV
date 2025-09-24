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

source("Scripts/nba_code.R")


#   ____________________________________________________________________________
#   Draws from the Posterior Predictive                                     ####

# Extract draws from both models
draws_spike_slab <- fit_wbt_spike_slab$draws(format = "draws_df")

# Extract y_rep (posterior predictive samples)
y_rep_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("y_rep", format = "matrix"))

# Observed outcomes
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

p_stat_win <- ppc_bars_grouped(
  y = y_obs,
  yrep = y_rep_spike_slab,
  group = group_fac,
  prob = 0.95,
  freq = FALSE,
  facet_args = list(ncol = 6, nrow = 5)
) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 17),
    strip.text.y = element_text(size = 17),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 19),
    axis.title.x = element_text(size = 19),
    legend.title = element_blank(), # Changed from element_blank() to show "Density"
    legend.text = element_text(size = 17),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18, hjust = 0)
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Away Win", "Home Win")) +
  labs(
    y = "Proportion",
    x = ""
  )

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
    legend.title = element_blank(), # Changed from element_blank() to show "Density"
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
