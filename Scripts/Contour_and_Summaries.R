#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidybayes)
library(posterior)
library(ggpubr)

#   ____________________________________________________________________________
#   Data                                                                    ####

source("Scripts/Fit_Models.R")

# Prepare data
nba_data <- pred_scenarios$predict_first_round$train_data

team_meta <- bind_rows(
  nba_data %>% transmute(
    team_id = hometeamId,
    team_label = paste0(hometeamCity, " ", hometeamName)
  ),
  nba_data %>% transmute(
    team_id = awayteamId,
    team_label = paste0(awayteamCity, " ", awayteamName)
  )
) %>%
  distinct(team_id, .keep_all = TRUE)

id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])

# Build team lookup
team_meta <- bind_rows(
  nba_data %>% transmute(
    team_id = hometeamId,
    team_label = paste0(hometeamCity, " ", hometeamName)
  ),
  nba_data %>% transmute(
    team_id = awayteamId,
    team_label = paste0(awayteamCity, " ", awayteamName)
  )
) %>%
  distinct(team_id, .keep_all = TRUE)

id_to_label <- setNames(team_meta$team_label, team_meta$team_id)

# Ordered team IDs and labels
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])

# Extract draws for both logStrength and sd_logStrength
draws_strength <- fit_glick$draws(variables = "logStrength", format = "draws_df")
draws_sd <- fit_glick$draws(variables = "sigma", format = "draws_df")

# Team lookup
team_meta <- bind_rows(
  nba_data %>% transmute(team_id = hometeamId, team_label = paste0(hometeamCity, " ", hometeamName)),
  nba_data %>% transmute(team_id = awayteamId, team_label = paste0(awayteamCity, " ", awayteamName))
) %>%
  distinct(team_id, .keep_all = TRUE)

id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])


#   ____________________________________________________________________________
#   Contour plot                                                            ####

# Season labels
n_seasons <- 10
start_first_season <- 2015
season_years <- sprintf(
  "%02d/%02d",
  (start_first_season - 2000):(start_first_season - 2000 + n_seasons - 1),
  (start_first_season - 1999):(start_first_season - 1999 + n_seasons - 1)
)


# Oklahoma City Thunder (top)
team_index_cavs <- which(team_labels == "Oklahoma City Thunder")

dat_glick_cavs <- extract_team_draws(
  fit_glick,
  strength_var = "logStrength", sd_var = "sigma",
  team_index = team_index_cavs, n_seasons = n_seasons
) %>% dplyr::mutate(Model = "SIV")

dat_spike_cavs <- extract_team_draws(
  fit_wbt_spike_slab,
  strength_var = "logStrength", sd_var = "sd_logStrength",
  team_index = team_index_cavs, n_seasons = n_seasons
) %>% dplyr::mutate(Model = "Proposal")

plot_dat_cavs <- dplyr::bind_rows(dat_glick_cavs, dat_spike_cavs) %>%
  dplyr::mutate(
    Season_Year = factor(season_years[Season], levels = season_years),
    Model = factor(Model, levels = c("SIV", "Proposal"))
  )

p_top <- ggplot(plot_dat_cavs, aes(x = sd, y = logStrength)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
  stat_density_2d(color = "black", linewidth = 0.3, alpha = 0.5) +
  facet_grid(Model ~ Season_Year) +
  scale_fill_viridis_c(option = "plasma", name = "Density") +
  labs(
    title = "Oklahoma City Thunder",
    x = "SD values",
    y = "Log-strength values"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_text(size = 18, margin = margin(b = 15, r = 10)),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.3, "cm"),
    legend.key.height = unit(0.7, "cm"),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 24, hjust = 0)
  )


# Philadelphia 76ers (bottom)
team_index_76 <- which(team_labels == "Philadelphia 76ers")

dat_glick_76 <- extract_team_draws(
  fit_glick,
  strength_var = "logStrength", sd_var = "sigma",
  team_index = team_index_76, n_seasons = n_seasons
) %>% dplyr::mutate(Model = "SIV")

dat_spike_76 <- extract_team_draws(
  fit_wbt_spike_slab,
  strength_var = "logStrength", sd_var = "sd_logStrength",
  team_index = team_index_76, n_seasons = n_seasons
) %>% dplyr::mutate(Model = "Proposal")

plot_dat_76 <- dplyr::bind_rows(dat_glick_76, dat_spike_76) %>%
  dplyr::mutate(
    Season_Year = factor(season_years[Season], levels = season_years),
    Model = factor(Model, levels = c("SIV", "Proposal"))
  )

p_bottom <- ggplot(plot_dat_76, aes(x = sd, y = logStrength)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
  stat_density_2d(color = "black", linewidth = 0.3, alpha = 0.5) +
  facet_grid(Model ~ Season_Year) +
  scale_fill_viridis_c(option = "plasma", name = "Density") +
  labs(
    title = "Philadelphia 76ers",
    x = "SD values",
    y = "Log-strength values"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_text(size = 18, margin = margin(b = 15, r = 10)),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.3, "cm"),
    legend.key.height = unit(0.7, "cm"),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 24, hjust = 0)
  )


combined_ggarrange <- ggarrange(
  p_top, p_bottom,
  ncol = 1, nrow = 2,
  heights = c(1, 1),
  common.legend = TRUE, legend = "top"
)

combined_ggarrange


ggsave(
  filename = "Plots/joint_post_okc_and_76ers_arrange.pdf",
  plot = combined_ggarrange,
  width = 19, height = 15, device = "pdf", dpi = 500
)

#   ____________________________________________________________________________
#   Numerical summaries                                                     ####

# Combine all data
all_draws <- bind_rows(
  plot_dat_cavs %>% mutate(Team = "Oklahoma City Thunder"),
  plot_dat_76 %>% mutate(Team = "Philadelphia 76ers")
)

# Summary statistics by Team, Season, and Model
summary_table <- all_draws %>%
  group_by(Team, Season_Year, Model) %>%
  summarise(
    # Log-strength summaries
    mean_lambda = mean(logStrength),
    sd_lambda = sd(logStrength),
    q025_lambda = quantile(logStrength, 0.025),
    q975_lambda = quantile(logStrength, 0.975),
    ci_width_lambda = q975_lambda - q025_lambda,

    # Innovation SD summaries
    mean_sigma = mean(sd),
    sd_sigma = sd(sd),
    q025_sigma = quantile(sd, 0.025),
    q975_sigma = quantile(sd, 0.975),
    ci_width_sigma = q975_sigma - q025_sigma,
    .groups = "drop"
  ) %>%
  arrange(Team, Season_Year, Model)

print(summary_table, n = 50)

# Comparison table showing differences between models
comparison_table <- summary_table %>%
  select(
    Team, Season_Year, Model,
    mean_lambda, q025_lambda, q975_lambda, ci_width_lambda,
    mean_sigma, q025_sigma, q975_sigma, ci_width_sigma
  ) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(
      mean_lambda, q025_lambda, q975_lambda, ci_width_lambda,
      mean_sigma, q025_sigma, q975_sigma, ci_width_sigma
    ),
    names_glue = "{Model}_{.value}"
  ) %>%
  mutate(
    # Percentage change in mean sigma
    sigma_pct_change = ((Proposal_mean_sigma - SIV_mean_sigma) / SIV_mean_sigma) * 100
  )

print(comparison_table, n = 50)

# Paper table with CI bounds
paper_table_clean <- comparison_table %>%
  select(
    Team, Season_Year,
    # Lambda: mean and CI bounds
    SIV_mean_lambda, SIV_q025_lambda, SIV_q975_lambda,
    Proposal_mean_lambda, Proposal_q025_lambda, Proposal_q975_lambda,
    # Sigma: mean and CI bounds
    SIV_mean_sigma, SIV_q025_sigma, SIV_q975_sigma,
    Proposal_mean_sigma, Proposal_q025_sigma, Proposal_q975_sigma,
    # Percentage change
    sigma_pct_change
  ) %>%
  rename(
    `Mean λ (SIV)` = SIV_mean_lambda,
    `λ Lower (SIV)` = SIV_q025_lambda,
    `λ Upper (SIV)` = SIV_q975_lambda,
    `Mean λ (Prop.)` = Proposal_mean_lambda,
    `λ Lower (Prop.)` = Proposal_q025_lambda,
    `λ Upper (Prop.)` = Proposal_q975_lambda,
    `Mean σ (SIV)` = SIV_mean_sigma,
    `σ Lower (SIV)` = SIV_q025_sigma,
    `σ Upper (SIV)` = SIV_q975_sigma,
    `Mean σ (Prop.)` = Proposal_mean_sigma,
    `σ Lower (Prop.)` = Proposal_q025_sigma,
    `σ Upper (Prop.)` = Proposal_q975_sigma,
    `σ Change (%)` = sigma_pct_change
  ) %>%
  mutate(
    across(where(is.numeric) & !contains("Change"), ~ round(., 3)),
    `σ Change (%)` = sprintf("%+.1f", `σ Change (%)`)
  )

print(paper_table_clean, n = 50)

# Compact format with CI as interval notation [lower, upper]
paper_table_compact <- comparison_table %>%
  mutate(
    across(where(is.numeric), ~ round(., 2)),
    # Interval strings
    SIV_CI_lambda = sprintf("[%.2f, %.2f]", SIV_q025_lambda, SIV_q975_lambda),
    Proposal_CI_lambda = sprintf("[%.2f, %.2f]", Proposal_q025_lambda, Proposal_q975_lambda),
    SIV_CI_sigma = sprintf("[%.2f, %.2f]", SIV_q025_sigma, SIV_q975_sigma),
    Proposal_CI_sigma = sprintf("[%.2f, %.2f]", Proposal_q025_sigma, Proposal_q975_sigma),
    sigma_pct_change = sprintf("%+.1f", sigma_pct_change)
  ) %>%
  select(
    Team, Season_Year,
    SIV_mean_lambda, SIV_CI_lambda,
    Proposal_mean_lambda, Proposal_CI_lambda,
    SIV_mean_sigma, SIV_CI_sigma,
    Proposal_mean_sigma, Proposal_CI_sigma,
    sigma_pct_change
  ) %>%
  rename(
    `Mean λ (SIV)` = SIV_mean_lambda,
    `95% CI λ (SIV)` = SIV_CI_lambda,
    `Mean λ (Prop.)` = Proposal_mean_lambda,
    `95% CI λ (Prop.)` = Proposal_CI_lambda,
    `Mean σ (SIV)` = SIV_mean_sigma,
    `95% CI σ (SIV)` = SIV_CI_sigma,
    `Mean σ (Prop.)` = Proposal_mean_sigma,
    `95% CI σ (Prop.)` = Proposal_CI_sigma,
    `σ Change (%)` = sigma_pct_change
  )

print(paper_table_compact, n = 50)
