#   ____________________________________________________________________________
#   Facetted plot: Brier score + LOOIC (one ggplot, two facets)            ####

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# --- If not already in env, (re)define helpers -------------------------------
# Robust getter for LOOIC from a loo object
get_looic <- function(loo_obj) {
  if (is.null(loo_obj)) return(NA_real_)
  if (!is.null(loo_obj$looic)) return(as.numeric(loo_obj$looic))
  if (!is.null(loo_obj$estimates)) {
    est <- loo_obj$estimates
    rn  <- rownames(est)
    if (!is.null(rn) && "looic" %in% rn)    return(as.numeric(est["looic", "Estimate"]))
    if (!is.null(rn) && "elpd_loo" %in% rn) return(as.numeric(-2 * est["elpd_loo", "Estimate"]))
  }
  if (!is.null(loo_obj$elpd_loo)) return(as.numeric(-2 * loo_obj$elpd_loo))
  NA_real_
}

# Compute Brier score if not computed yet
if (!exists("brier_scores_glick") || !exists("brier_scores_spike_slab")) {
  library(posterior)
  compute_brier_score <- function(result) {
    fit       <- result$fit
    scenario  <- result$scenario
    stan_data <- result$stan_data
    if (stan_data$N_prev == 0) return(NA_real_)
    dm <- posterior::as_draws_matrix(fit$draws(variables = "y_prev"))
    p_hat_named <- colMeans(dm)
    idx <- as.integer(gsub("^y_prev\\[|\\]$", "", names(p_hat_named)))
    p_hat <- as.numeric(p_hat_named[order(idx)])
    y_oos <- as.integer(scenario$predict_data$homeScore > scenario$predict_data$awayScore)
    if (length(p_hat) != length(y_oos)) return(NA_real_)
    brier_per_obs <- (p_hat - y_oos)^2 + ((1-p_hat) - (1-y_oos))^2
    brier_score <- mean(brier_per_obs)
    return(brier_score)
  }
  brier_scores_glick      <- lapply(results_glick, compute_brier_score)
  brier_scores_spike_slab <- lapply(results_spike_slab, compute_brier_score)
}

# --- Pretty labels + order ---------------------------------------------------
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

# --- Build tidy data for LOOIC ----------------------------------------------
res_to_df <- function(res_list, method_name) {
  tibble(
    scenario = names(res_list),
    value    = map_dbl(res_list, ~ get_looic(.x$loo)),
    method   = method_name,
    metric   = "LOOIC"
  )
}

df_looic <-
  bind_rows(
    res_to_df(results_glick, "Glickman"),
    res_to_df(results_spike_slab, "Spike–Slab")
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    scenario_label = dplyr::recode(scenario, !!!pretty_names, .default = scenario)
  ) %>%
  filter(scenario_label %in% desired_order) %>%
  group_by(method, scenario_label, metric) %>%                  # de-duplicate aliases
  summarise(value = first(value), .groups = "drop")

# --- Build tidy data for Brier ----------------------------------------------
df_brier <-
  bind_rows(
    tibble(scenario = names(brier_scores_glick),
           value    = as.numeric(unlist(brier_scores_glick)),
           method   = "Glickman",
           metric   = "Brier"),
    tibble(scenario = names(brier_scores_spike_slab),
           value    = as.numeric(unlist(brier_scores_spike_slab)),
           method   = "Spike–Slab",
           metric   = "Brier")
  ) %>%
  filter(!is.na(value)) %>%
  mutate(scenario_label = dplyr::recode(scenario, !!!pretty_names, .default = scenario)) %>%
  filter(scenario_label %in% desired_order)

# --- Combine metrics ---------------------------------------------------------
df_all <-
  bind_rows(df_looic, df_brier) %>%
  mutate(
    scenario_label = factor(scenario_label, levels = desired_order),
    metric = factor(metric, levels = c("Brier", "LOOIC")) # show Brier first
  )

# --- Ribbon data (gap between methods) per facet -----------------------------
df_ribbon <-
  df_all %>%
  select(scenario_label, metric, method, value) %>%
  pivot_wider(names_from = method, values_from = value) %>%
  filter(!is.na(Glickman) & !is.na(`Spike–Slab`)) %>%
  mutate(
    ymin = pmin(Glickman, `Spike–Slab`),
    ymax = pmax(Glickman, `Spike–Slab`)
  )

# --- Label positions ---------------------------------------------------------
df_labels <-
  df_all %>%
  group_by(metric) %>%
  mutate(
    label_text = ifelse(metric == "Brier",
                        sprintf("%.3f", value),
                        sprintf("%.1f", value)),
    vjust = if_else(method == "Glickman", -2, 3)
  ) %>%
  ungroup()

# --- Plot --------------------------------------------------------------------
p_facet <-
  ggplot(df_all, aes(x = scenario_label, y = value, group = method,
                     color = method, shape = method)) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = scenario_label, ymin = ymin, ymax = ymax, group = 1),
    alpha = 0.10,
    fill  = "gray50",
    inherit.aes = FALSE
  ) +
  geom_line(size = 1.2, alpha = 0.85) +
  geom_point(size = 3, alpha = 0.95)+
  facet_wrap(
    ~ metric,
    nrow = 1,
    scales = "free_y",
    labeller = labeller(metric = c(Brier = "Brier Score", LOOIC = "LOOIC"))
  ) +
  scale_color_manual(
    values = c("Glickman" = "#56B4E9", "Spike–Slab" = "#c41010"),
    breaks = c("Glickman", "Spike–Slab"),
    labels = c("Glickman" = "Glickman (2001)", "Spike–Slab" = "Proposal")
  ) +
  scale_shape_manual(
    values = c("Glickman" = 17,  # solid circle
               "Spike–Slab" = 16) # solid triangle
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = label_number(accuracy = 0.001)) +
  ggnewscale::new_scale_color() +
  geom_text(
    data = df_labels,
    aes(label = label_text, vjust = vjust, color = method),
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Glickman"   = "#12486C", # darker orange
      "Spike–Slab" = "#581a15"  # darker blue
    )
  ) +
    labs(
    x = "Prediction scenario",
    y = NULL,
    color = "Model:"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing   = unit(1.2, "lines")
  ) +
  guides(shape = "none")

print(p_facet)

# Save
ggsave(
  filename = "plot_brier_looic.pdf", path = "Plots",
  plot = p_facet,
  width = 16, height = 9, device = "pdf", dpi = 500
)
