#   ____________________________________________________________________________
#   Libraries                                                               ####

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(posterior)

#   ____________________________________________________________________________
#   Models                                                                  ####

source("Scripts/pred_models.R")

brier_scores_glick      <- lapply(results_glick, compute_brier_score)
brier_scores_spike_slab <- lapply(results_spike_slab, compute_brier_score)
brier_scores_const_var <- lapply(results_const_var, compute_brier_score)

#   ____________________________________________________________________________
#   Plot Data                                                               ####

# Updated labels with abbreviated names
pretty_names <- c(
  "predict_first_round" = "First Round",
  "first_round"         = "First Round",
  "predict_semifinals"  = "Conf. Semifinals",
  "semifinals"          = "Conf. Semifinals",
  "predict_conf_finals" = "Conf. Finals",
  "conference_finals"   = "Conf. Finals",
  "predict_nba_finals"  = "NBA Finals",
  "nba_finals"          = "NBA Finals"
)
desired_order <- c("First Round", "Conf. Semifinals", "Conf. Finals", "NBA Finals")


# Build data for Brier
df_brier <-
  bind_rows(
    tibble(
      scenario = names(brier_scores_glick),
      value = as.numeric(unlist(brier_scores_glick)),
      method = "Glickman",
      metric = "Brier"
    ),
    tibble(
      scenario = names(brier_scores_spike_slab),
      value = as.numeric(unlist(brier_scores_spike_slab)),
      method = "Spike–Slab",
      metric = "Brier"
    ),
    tibble(
      scenario = names(brier_scores_const_var),
      value = as.numeric(unlist(brier_scores_const_var)),
      method = "Const-Var",
      metric = "Brier"
    )
  ) %>%
  filter(!is.na(value)) %>%
  mutate(scenario_label = dplyr::recode(scenario, !!!pretty_names, .default = scenario)) %>%
  filter(scenario_label %in% desired_order)


df_looic <-
  bind_rows(
    res_to_df(results_glick, "Glickman"),
    res_to_df(results_const_var, "Const-Var"),
    res_to_df(results_spike_slab, "Spike–Slab")
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    scenario_label = dplyr::recode(scenario, !!!pretty_names, .default = scenario)
  ) %>%
  filter(scenario_label %in% desired_order) %>%
  group_by(method, scenario_label, metric) %>%                  # de-duplicate aliases
  summarise(value = first(value), .groups = "drop")

# Combine metrics
df_all <-
  bind_rows(df_looic, df_brier) %>%
  mutate(
    scenario_label = factor(scenario_label, levels = desired_order),
    metric = factor(metric, levels = c("Brier", "LOOIC")) # show Brier first
  )

# Ribbon data (gap between methods) per facet
df_ribbon <-
  df_all %>%
  select(scenario_label, metric, method, value) %>%
  pivot_wider(names_from = method, values_from = value) %>%
  filter(!is.na(Glickman) & !is.na(`Spike–Slab`)) %>%
  mutate(
    ymin = pmin(Glickman, `Spike–Slab`),
    ymax = pmax(Glickman, `Spike–Slab`)
  )

# Label positions
df_labels <-
  df_all %>%
  group_by(metric) %>%
  mutate(
    label_text = ifelse(metric == "Brier",
      sprintf("%.3f", value),
      sprintf("%.1f", value)
    ),
    vjust = if_else(method == "Glickman", -2, 3)
  ) %>%
  ungroup()


#   ____________________________________________________________________________
#   Plot                                                                    ####

p_facet <-
  ggplot(df_all, aes(
    x = scenario_label, y = value, group = method,
    color = method, shape = method
  )) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = scenario_label, ymin = ymin, ymax = ymax, group = 1),
    alpha = 0.10,
    fill = "gray50",
    inherit.aes = FALSE
  ) +
  geom_line(size = 1.0, alpha = 0.85) +
  geom_point(size = 3, alpha = 0.95) +
  facet_wrap(
    ~metric,
    nrow = 1,
    scales = "free_y",
    labeller = labeller(metric = c(Brier = "Brier Score", LOOIC = "LOOIC"))
  ) +
  scale_color_manual(
    name = "Model:",
    values = c("Spike–Slab" = "#c41010", "Glickman" = "#56B4E9", "Const-Var" = "forestgreen"),
    breaks = c("Spike–Slab", "Glickman", "Const-Var"),
    labels = c("Spike–Slab" = "Proposal", "Glickman" = "SIV", "Const-Var" = "CIV")
  ) +
  scale_shape_manual(
    name = "Model:",
    values = c(
      "Spike–Slab" = 19,
      "Glickman" = 17,
      "Const-Var" = 15
    ),
    breaks = c("Spike–Slab", "Glickman", "Const-Var"),
    labels = c("Spike–Slab" = "Proposal", "Glickman" = "SIV", "Const-Var" = "CIV")
  ) +
  labs(
    x = "Prediction scenario",
    y = NULL
  ) +
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
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing = unit(1.2, "lines")
  )

print(p_facet)

ggsave(
  filename = "plot_brier_looic.pdf", path = "Plots",
  plot = p_facet,
  width = 16, height = 9, device = "pdf", dpi = 500
)
