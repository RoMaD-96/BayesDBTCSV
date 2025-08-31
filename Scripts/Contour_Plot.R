# Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidybayes)
library(posterior)

# Build team lookup (same as before)
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

# Get ordered team IDs and labels
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])

# Find Golden State Warriors index
gsw_index <- which(team_labels == "Golden State Warriors")

# Extract draws for both logStrength and sd_logStrength
draws_strength <- fit_glick$draws(variables = "logStrength", format = "draws_df")
draws_sd <- fit_glick$draws(variables = "sigma", format = "draws_df")

# Combine draws for GSW only
gsw_data <- data.frame()

for (t in 1:10) { # Assuming 10 time periods based on your plots
  # Get column names for this time and team
  strength_col <- paste0("logStrength[", t, ",", gsw_index, "]")
  sd_col <- paste0("sigma[", t, ",", gsw_index, "]")

  # Extract and combine
  temp_df <- data.frame(
    logStrength = draws_strength[[strength_col]],
    sigma = draws_sd[[sd_col]],
    Season = t,
    .draw = 1:nrow(draws_strength)
  )

  gsw_data <- rbind(gsw_data, temp_df)
}

# Create season labels (adjust based on your actual seasons)
# Assuming season 1 is oldest and 10 is most recent
gsw_data <- gsw_data %>%
  mutate(
    Season_Label = factor(paste("Season", Season),
      levels = paste("Season", 1:10)
    )
  )

# Create contour plot faceted by season
p1 <- ggplot(gsw_data, aes(x = logStrength, y = sigma)) +
  stat_density_2d(aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.8
  ) +
  stat_density_2d(
    color = "black",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  facet_wrap(~Season_Label,
    ncol = 5,
    scales = "fixed"
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Density"
  ) +
  labs(
    subtitle = "Spike-Slab Model - Contour plots by season",
    x = "logStrength",
    y = "sd_logStrength"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40")
  )

print(p1)


# Extract draws for both logStrength and sd_logStrength
draws_strength <- fit_wbt_spike_slab$draws(variables = "logStrength", format = "draws_df")
draws_sd <- fit_wbt_spike_slab$draws(variables = "sd_logStrength", format = "draws_df")

# Combine draws for GSW only
gsw_data <- data.frame()

for (t in 1:10) { # Assuming 10 time periods based on your plots
  # Get column names for this time and team
  strength_col <- paste0("logStrength[", t, ",", gsw_index, "]")
  sd_col <- paste0("sd_logStrength[", t, ",", gsw_index, "]")

  # Extract and combine
  temp_df <- data.frame(
    logStrength = draws_strength[[strength_col]],
    sd_logStrength = draws_sd[[sd_col]],
    Season = t,
    .draw = 1:nrow(draws_strength)
  )

  gsw_data <- rbind(gsw_data, temp_df)
}

# Create season labels (adjust based on your actual seasons)
# Assuming season 1 is oldest and 10 is most recent
gsw_data <- gsw_data %>%
  mutate(
    Season_Label = factor(paste("Season", Season),
      levels = paste("Season", 1:10)
    )
  )

# Create contour plot faceted by season
p1 <- ggplot(gsw_data, aes(x = logStrength, y = sd_logStrength)) +
  stat_density_2d(aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.9
  ) +
  stat_density_2d(
    color = "black",
    linewidth = 0.2,
    alpha = 0.5
  ) +
  facet_wrap(~Season_Label,
    ncol = 5,
    scales = "fixed"
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Density"
  ) +
  labs(
    subtitle = "Spike-Slab Model - Contour plots by season",
    x = "logStrength",
    y = "sd_logStrength"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40")
  )

print(p1)



#   ____________________________________________________________________________
#   Contour Plot Joined                                                     ####

# --- Helper to extract one model's draws for a single team across seasons ---
extract_team_draws <- function(fit, strength_var = "logStrength", sd_var, team_index, n_seasons = 10) {
  draws_strength <- fit$draws(variables = strength_var, format = "draws_df")
  draws_sd <- fit$draws(variables = sd_var, format = "draws_df")

  out <- vector("list", n_seasons)
  for (t in seq_len(n_seasons)) {
    strength_col <- sprintf("%s[%d,%d]", strength_var, t, team_index)
    sd_col <- sprintf("%s[%d,%d]", sd_var, t, team_index)

    out[[t]] <- tibble(
      logStrength = draws_strength[[strength_col]],
      sd          = draws_sd[[sd_col]], # <- standardize y name
      Season      = t,
      .draw       = seq_len(nrow(draws_strength))
    )
  }
  bind_rows(out) %>%
    mutate(Season_Label = factor(paste("Season", Season),
      levels = paste("Season", seq_len(n_seasons))
    ))
}

# --- Build team lookup (unchanged) ---
team_meta <- bind_rows(
  nba_data %>% transmute(team_id = hometeamId, team_label = paste0(hometeamCity, " ", hometeamName)),
  nba_data %>% transmute(team_id = awayteamId, team_label = paste0(awayteamCity, " ", awayteamName))
) %>%
  distinct(team_id, .keep_all = TRUE)

id_to_label <- setNames(team_meta$team_label, team_meta$team_id)
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))
team_labels <- unname(id_to_label[team_ids])


##  ............................................................................
##  Cleveland Cavaliers                                                     ####

team_index <- which(team_labels == "Cleveland Cavaliers")

# --- Extract both models and stack ---
dat_glick <- extract_team_draws(
  fit_glick,
  strength_var = "logStrength",
  sd_var       = "sigma", # from your first block
  team_index   = team_index,
  n_seasons    = 10
) %>%
  mutate(Model = "Glick")

dat_spike <- extract_team_draws(
  fit_wbt_spike_slab,
  strength_var = "logStrength",
  sd_var       = "sd_logStrength", # from your second block
  team_index   = team_index,
  n_seasons    = 10
) %>%
  mutate(Model = "Spike–Slab")

plot_dat <- bind_rows(dat_glick, dat_spike)

# --- One plot, faceted by Season (rows) and Model (columns) ---
p_cc <- ggplot(plot_dat, aes(y = logStrength, x = sd)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
  stat_density_2d(color = "black", linewidth = 0.3, alpha = 0.5) +
  facet_grid(Model ~ Season_Label) +
  scale_fill_viridis_c(option = "plasma", name = "Density") +
  labs(
    title = "Cleveland Cavaliers",
    y = "logStrength",
    x = "sd (model-specific)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_cc)
ggsave(
  filename = "joint_post_cc_plot.png", path = "Plots",
  plot = p_cc,
  width = 14, height = 10, device = "png", dpi = 500
)


##  ............................................................................
##  Oklahoma City Thunder                                                   ####

team_index <- which(team_labels == "Oklahoma City Thunder")

# --- Extract both models and stack ---
dat_glick <- extract_team_draws(
  fit_glick,
  strength_var = "logStrength",
  sd_var       = "sigma", # from your first block
  team_index   = team_index,
  n_seasons    = 10
) %>%
  mutate(Model = "Glick")

dat_spike <- extract_team_draws(
  fit_wbt_spike_slab,
  strength_var = "logStrength",
  sd_var       = "sd_logStrength", # from your second block
  team_index   = team_index,
  n_seasons    = 10
) %>%
  mutate(Model = "Spike–Slab")

plot_dat <- bind_rows(dat_glick, dat_spike)

# --- One plot, faceted by Season (rows) and Model (columns) ---
p_okc <- ggplot(plot_dat, aes(y = logStrength, x = sd)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
  stat_density_2d(color = "black", linewidth = 0.3, alpha = 0.5) +
  facet_grid(Model ~ Season_Label) +
  scale_fill_viridis_c(option = "plasma", name = "Density") +
  labs(
    title = "Oklahoma City Thunder",
    y = "logStrength",
    x = "sd (model-specific)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_okc)

ggsave(
  filename = "joint_post_okc_plot.png", path = "Plots",
  plot = p_okc,
  width = 14, height = 10, device = "png", dpi = 500
)
