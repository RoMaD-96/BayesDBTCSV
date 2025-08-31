# --- Plot LOOIC by prediction scenario for both methods -----------------------

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)

# Load saved result lists (adjust path if needed)
load("Results/results_glick.RData")
load("Results/results_spike_slab.RData")

# Pretty names + order
pretty_names <- c(
  "predict_first_round" = "First Round",
  "first_round"         = "First Round",
  "predict_semifinals"  = "Semifinals",
  "semifinals"          = "Semifinals",
  "predict_conf_finals" = "Conference Finals",
  "conference_finals"   = "Conference Finals",
  "predict_nba_finals"  = "NBA Finals",
  "nba_finals"          = "NBA Finals"
)
desired_order <- c("First Round", "Semifinals", "Conference Finals", "NBA Finals")

# Robust getter for LOOIC from a loo object
get_looic <- function(loo_obj) {
  if (is.null(loo_obj)) return(NA_real_)
  if (!is.null(loo_obj$looic)) return(as.numeric(loo_obj$looic))
  if (!is.null(loo_obj$estimates)) {
    est <- loo_obj$estimates
    rn <- rownames(est)
    if (!is.null(rn) && "looic" %in% rn)       return(as.numeric(est["looic", "Estimate"]))
    if (!is.null(rn) && "elpd_loo" %in% rn)    return(as.numeric(-2 * est["elpd_loo", "Estimate"]))
  }
  if (!is.null(loo_obj$elpd_loo)) return(as.numeric(-2 * loo_obj$elpd_loo))
  NA_real_
}

# Turn a results list (named by scenario) into a tidy df
results_to_df <- function(res_list, method_name) {
  tibble(
    scenario = names(res_list),
    looic    = map_dbl(res_list, ~ get_looic(.x$loo)),
    method   = method_name
  )
}

# Build, map to pretty labels, enforce order, and deduplicate aliases
df_plot <-
  bind_rows(
    results_to_df(results_glick, "Glickman"),
    results_to_df(results_spike_slab, "Spike–Slab")
  ) %>%
  filter(!is.na(looic)) %>%
  mutate(
    scenario = as.character(scenario),
    scenario_label = pretty_names[scenario],
    scenario_label = ifelse(is.na(scenario_label), scenario, scenario_label)
  ) %>%
  filter(scenario_label %in% desired_order) %>%
  group_by(method, scenario_label) %>%                 # in case both alias names exist
  summarise(looic = first(looic), .groups = "drop") %>%
  mutate(scenario_label = factor(scenario_label, levels = desired_order))

# Data for shaded ribbon showing method differences
df_ribbon <- df_plot %>%
  select(scenario_label, method, looic) %>%
  pivot_wider(names_from = method, values_from = looic) %>%
  filter(!is.na(Glickman) & !is.na(`Spike–Slab`)) %>%
  mutate(
    ymin = pmin(Glickman, `Spike–Slab`),
    ymax = pmax(Glickman, `Spike–Slab`)
  )

# Labels dataset (precompute text & vjust)
df_labels <- df_plot %>%
  mutate(
    label = sprintf("%.1f", looic),
    vjust = if_else(method == "Glickman", -2, 3)
  )

# Build plot
p_looic <-
  ggplot(df_plot, aes(x = scenario_label, y = looic, group = method, color = method)) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = scenario_label, ymin = ymin, ymax = ymax, group = 1),
    alpha = 0.1,
    fill = "gray50",
    inherit.aes = FALSE
  ) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text(
    data = df_labels,
    mapping = aes(label = label, vjust = vjust),
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Glickman" = "#E69F00", "Spike–Slab" = "#56B4E9"),
    labels = c("Glickman", "Spike–Slab")
  ) +
  scale_x_discrete(drop = FALSE) +  # keep all desired_order levels even if missing
  scale_y_continuous(
    limits = c(
      min(df_plot$looic, na.rm = TRUE) - 10,
      max(df_plot$looic, na.rm = TRUE) + 10
    )
  ) +
  labs(
    x = "Prediction scenario",
    y = "LOOIC",
    color = "Model:"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  )

print(p_looic)

ggsave(
  filename = "plot_looic.png", path = "Plots",
  plot = p_looic,
  width = 14, height = 10, device = "png", dpi = 500
)

