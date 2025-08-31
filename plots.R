# Novel Contour Plot: prob_mix vs logStrength for Detroit Pistons, OKC Thunder, and Chicago Bulls
library(dplyr)
library(ggplot2)
library(tidybayes)
library(posterior)
library(viridis)

# Extract posterior draws for both prob_mix and logStrength
draws_prob_mix <- fit$draws(variables = "prob_mix", format = "draws_df")
draws_logStrength <- fit$draws(variables = "logStrength", format = "draws_df")

# Convert to tidy format
td_prob_mix <- tidybayes::gather_draws(draws_prob_mix, prob_mix[t, k])
td_logStrength <- tidybayes::gather_draws(draws_logStrength, logStrength[t, k])

# Identify target teams
target_teams <- c("Detroit Pistons", "Oklahoma City Thunder", "Chicago Bulls")

# Find their indices in the team_labels vector
target_indices <- which(team_labels %in% target_teams)
names(target_indices) <- team_labels[target_indices]

print("Target team indices:")
print(target_indices)

# Filter and combine the draws for target teams
combined_draws <- td_prob_mix %>%
  filter(k %in% target_indices) %>%
  rename(prob_mix = .value) %>%
  select(.chain, .iteration, .draw, t, k, prob_mix) %>%
  inner_join(
    td_logStrength %>%
      filter(k %in% target_indices) %>%
      rename(logStrength = .value) %>%
      select(.chain, .iteration, .draw, t, k, logStrength),
    by = c(".chain", ".iteration", ".draw", "t", "k")
  ) %>%
  mutate(
    Team = factor(team_labels[k], levels = target_teams),
    Season = t  # 1 = oldest, 10 = most recent
  )

# Create contour plot - pooling across all seasons
contour_plot_all <- combined_draws %>%
  ggplot(aes(x = prob_mix, y = logStrength, color = Team)) +
  geom_density_2d(alpha = 0.7, size = 1) +
  geom_point(alpha = 0.1, size = 0.5) +
  scale_color_viridis_d(name = "Team", option = "plasma", end = 0.9) +
  labs(
    x = "Mixing Probability (prob_mix)",
    y = "Log Strength",
    title = "Posterior Contours: prob_mix vs logStrength",
    subtitle = "All seasons pooled (Detroit Pistons, OKC Thunder, Chicago Bulls)",
    caption = "Contour lines show posterior density; points show individual draws"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

print(contour_plot_all)

# Alternative: Contour plot for most recent season only (Season 10)
contour_plot_recent <- combined_draws %>%
  filter(Season == 10) %>%  # Most recent season
  ggplot(aes(x = prob_mix, y = logStrength)) +
  geom_density_2d(alpha = 0.8, size = 1.2, color = "darkblue") +
  geom_point(alpha = 0.3, size = 0.8, color = "darkred") +
  facet_wrap(~ Team, scales = "free_x") +
  labs(
    x = "Mixing Probability (prob_mix)",
    y = "Log Strength",
    title = "Posterior Contours: prob_mix vs logStrength",
    subtitle = "Most Recent Season Only - Faceted by Team",
    caption = "Contour lines show posterior density; points show individual draws"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )

print(contour_plot_recent)

# Alternative: Faceted contour plot by team with filled contours
contour_plot_faceted <- combined_draws %>%
  ggplot(aes(x = prob_mix, y = logStrength)) +
  geom_density_2d_filled(alpha = 0.7, bins = 8) +
  geom_point(alpha = 0.1, size = 0.3, color = "white") +
  facet_wrap(~ Team, scales = "free") +
  scale_fill_viridis_d(name = "Density", option = "plasma") +
  labs(
    x = "Mixing Probability (prob_mix)",
    y = "Log Strength",
    title = "Posterior Density: prob_mix vs logStrength by Team",
    subtitle = "All seasons pooled",
    caption = "Filled contours show posterior density levels"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold")
  )

print(contour_plot_faceted)

# Summary statistics for interpretation
summary_stats <- combined_draws %>%
  group_by(Team) %>%
  summarise(
    prob_mix_mean = mean(prob_mix),
    prob_mix_sd = sd(prob_mix),
    logStrength_mean = mean(logStrength),
    logStrength_sd = sd(logStrength),
    correlation = cor(prob_mix, logStrength),
    .groups = 'drop'
  )

print("Summary Statistics:")
print(summary_stats)

# Show correlation between the two variables for each team
correlation_plot <- combined_draws %>%
  group_by(Team) %>%
  summarise(correlation = cor(prob_mix, logStrength), .groups = 'drop') %>%
  ggplot(aes(x = Team, y = correlation, fill = Team)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(correlation, 3)), vjust = -0.5) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  labs(
    x = "Team",
    y = "Correlation (prob_mix, logStrength)",
    title = "Correlation between prob_mix and logStrength by Team"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

