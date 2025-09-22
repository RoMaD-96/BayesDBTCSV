#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(ggplot2)
library(tidybayes)
library(posterior)
library(ggimage)
library(magick)

#   ____________________________________________________________________________
#   Sources                                                                ####

source("Scripts/nba_code.R")

#   ____________________________________________________________________________
#   Data                                                                    ####

# Build lookup and prepare data
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



#   ____________________________________________________________________________
#   Logos                                                                   ####

# Logo URLs
logo_urls <- c(
  "Atlanta Hawks" = "https://a.espncdn.com/i/teamlogos/nba/500/atl.png",
  "Boston Celtics" = "https://a.espncdn.com/i/teamlogos/nba/500/bos.png",
  "Brooklyn Nets" = "https://a.espncdn.com/i/teamlogos/nba/500/bkn.png",
  "Charlotte Hornets" = "https://a.espncdn.com/i/teamlogos/nba/500/cha.png",
  "Chicago Bulls" = "https://a.espncdn.com/i/teamlogos/nba/500/chi.png",
  "Cleveland Cavaliers" = "https://a.espncdn.com/i/teamlogos/nba/500/cle.png",
  "Dallas Mavericks" = "https://a.espncdn.com/i/teamlogos/nba/500/dal.png",
  "Denver Nuggets" = "https://a.espncdn.com/i/teamlogos/nba/500/den.png",
  "Detroit Pistons" = "https://a.espncdn.com/i/teamlogos/nba/500/det.png",
  "Golden State Warriors" = "https://a.espncdn.com/i/teamlogos/nba/500/gs.png",
  "Houston Rockets" = "https://a.espncdn.com/i/teamlogos/nba/500/hou.png",
  "Indiana Pacers" = "https://a.espncdn.com/i/teamlogos/nba/500/ind.png",
  "Los Angeles Clippers" = "https://a.espncdn.com/i/teamlogos/nba/500/lac.png",
  "Los Angeles Lakers" = "https://a.espncdn.com/i/teamlogos/nba/500/lal.png",
  "Memphis Grizzlies" = "https://a.espncdn.com/i/teamlogos/nba/500/mem.png",
  "Miami Heat" = "https://a.espncdn.com/i/teamlogos/nba/500/mia.png",
  "Milwaukee Bucks" = "https://a.espncdn.com/i/teamlogos/nba/500/mil.png",
  "Minnesota Timberwolves" = "https://a.espncdn.com/i/teamlogos/nba/500/min.png",
  "New Orleans Pelicans" = "https://a.espncdn.com/i/teamlogos/nba/500/no.png",
  "New York Knicks" = "https://a.espncdn.com/i/teamlogos/nba/500/ny.png",
  "Oklahoma City Thunder" = "https://a.espncdn.com/i/teamlogos/nba/500/okc.png",
  "Orlando Magic" = "https://a.espncdn.com/i/teamlogos/nba/500/orl.png",
  "Philadelphia 76ers" = "https://a.espncdn.com/i/teamlogos/nba/500/phi.png",
  "Phoenix Suns" = "https://a.espncdn.com/i/teamlogos/nba/500/phx.png",
  "Portland Trail Blazers" = "https://a.espncdn.com/i/teamlogos/nba/500/por.png",
  "Sacramento Kings" = "https://a.espncdn.com/i/teamlogos/nba/500/sac.png",
  "San Antonio Spurs" = "https://a.espncdn.com/i/teamlogos/nba/500/sa.png",
  "Toronto Raptors" = "https://a.espncdn.com/i/teamlogos/nba/500/tor.png",
  "Utah Jazz" = "https://a.espncdn.com/i/teamlogos/nba/500/utah.png",
  "Washington Wizards" = "https://a.espncdn.com/i/teamlogos/nba/500/wsh.png"
)


# Download and resize all logos
logo_paths <- sapply(names(logo_urls), function(team) {
  download_and_resize_logo(team, logo_urls[team], size = 40)
})

#   ____________________________________________________________________________
#   Models                                                                  ####

# Glickman model
draws_glick <- fit_glick$draws(variables = "logStrength", format = "draws_df")
td_glick <- tidybayes::gather_draws(draws_glick, logStrength[t, k]) %>%
  mutate(method = "Glickman")

# WBT Spike-Slab model
draws_spike <- fit_wbt_spike_slab$draws(variables = "logStrength", format = "draws_df")
td_spike <- tidybayes::gather_draws(draws_spike, logStrength[t, k]) %>%
  mutate(method = "Spike-Slab")

# Combine both
td_combined <- bind_rows(td_glick, td_spike)

summ_combined <- td_combined %>%
  mutate(
    Team = factor(team_labels[k], levels = team_labels),
    Rank = as.integer(t)
  ) %>%
  group_by(Team, Rank, method) %>%
  median_qi(.value, .width = 0.95) %>%
  ungroup()

# Add logo paths
summ_combined <- summ_combined %>%
  mutate(
    logo_path = logo_paths[as.character(Team)]
  )

# Season labels
season_years <- 2015:2024
season_labels <- paste0(substr(season_years, 3, 4), "/", substr(season_years + 1, 3, 4))



# Plot
p_all_logos <- ggplot(
  filter(summ_combined, method == "Spike-Slab"),
  aes(x = Rank, y = .value)
) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey20", alpha = 0.2) +
  geom_line(color = "black", size = 0.8, alpha = 0.9) +
  geom_image(aes(image = logo_path), size = 0.09, asp = 1.5) +
  facet_wrap(~Team, scales = "fixed", ncol = 5) +
  scale_x_continuous(
    breaks = 1:length(season_labels),
    labels = season_labels
  ) +
  labs(
    x = "Season",
    y = "LogStrength (posterior median ± 95% CI)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    legend.position = "top",
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 10, angle = 30, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95")
  )

print(p_all_logos)

ggsave(
  filename = "ability_plot.png", path = "Plots",
  plot = p_all_logos,
  width = 17, height = 14, device = "png", dpi = 500
)
