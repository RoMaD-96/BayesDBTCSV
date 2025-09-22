# Load required packages
library(cmdstanr)
library(posterior)
library(dplyr)
library(ggplot2)

# Function to compute Brier score for a single result
compute_brier_score <- function(result) {

  # Extract the fit and scenario data
  fit <- result$fit
  scenario <- result$scenario
  stan_data <- result$stan_data

  # Check if there are predictions to evaluate
  if (stan_data$N_prev == 0) {
    return(NA)
  }

  # 1) Extract posterior predictive draws for y_prev
  draws_y_prev <- fit$draws(variables = "y_prev")

  # 2) Combine chains/iterations, getting a draws_matrix
  dm <- posterior::as_draws_matrix(draws_y_prev)

  # 3) Posterior predictive probability for each held-out game
  p_hat_named <- colMeans(dm)

  # 4) Put probabilities in index order 1..N_prev
  idx <- as.integer(gsub("^y_prev\\[|\\]$", "", names(p_hat_named)))
  p_hat <- as.numeric(p_hat_named[order(idx)])

  # 5) Get true outcomes from the prediction data
  # Note: we need to ensure the same ordering as was used in prepare_stan_data
  y_oos <- as.integer(scenario$predict_data$homeScore > scenario$predict_data$awayScore)

  # 6) Sanity check lengths
  if (length(p_hat) != length(y_oos)) {
    warning("Length mismatch between predictions and outcomes")
    return(NA)
  }

  # 7) Calculate Brier score
  brier_per_obs <- (p_hat - y_oos)^2 + ((1-p_hat) - (1-y_oos))^2
  brier_score <- mean(brier_per_obs)

  return(brier_score)
}

# Compute Brier scores for all Glickman model fits
brier_scores_glick <- list()
for (scenario_name in names(results_glick)) {
  cat("\nComputing Brier score for Glickman model -", scenario_name)
  brier_scores_glick[[scenario_name]] <- compute_brier_score(results_glick[[scenario_name]])
  cat(" : ", round(brier_scores_glick[[scenario_name]], 3), "\n")
}

# Compute Brier scores for all Spike-Slab model fits
brier_scores_spike_slab <- list()
for (scenario_name in names(results_spike_slab)) {
  cat("\nComputing Brier score for Spike-Slab model -", scenario_name)
  brier_scores_spike_slab[[scenario_name]] <- compute_brier_score(results_spike_slab[[scenario_name]])
  cat(" : ", round(brier_scores_spike_slab[[scenario_name]], 3), "\n")
}

# Create a summary data frame for easy comparison
brier_summary <- data.frame(
  Scenario = names(brier_scores_glick),
  Glickman = unlist(brier_scores_glick),
  Spike_Slab = unlist(brier_scores_spike_slab),
  row.names = NULL
)

# Add difference column (lower is better for Brier score)
brier_summary$Difference <- brier_summary$Spike_Slab - brier_summary$Glickman
brier_summary$Better_Model <- ifelse(brier_summary$Difference < 0, "Spike_Slab", "Glickman")

# Print summary
cat("\n\n=== BRIER SCORE SUMMARY ===\n")
print(brier_summary)

# Calculate average Brier scores across all scenarios
cat("\n\n=== AVERAGE BRIER SCORES ===\n")
cat("Glickman model average:", round(mean(unlist(brier_scores_glick), na.rm = TRUE), 4), "\n")
cat("Spike-Slab model average:", round(mean(unlist(brier_scores_spike_slab), na.rm = TRUE), 4), "\n")

# --- Plot Brier score by prediction scenario for both methods ---------------

library(dplyr)
library(ggplot2)

df_brier <-
  bind_rows(
    tibble(
      scenario = names(brier_scores_glick),
      brier    = as.numeric(unlist(brier_scores_glick)),
      method   = "Glickman"
    ),
    tibble(
      scenario = names(brier_scores_spike_slab),
      brier    = as.numeric(unlist(brier_scores_spike_slab)),
      method   = "Spike–Slab"
    )
  ) %>%
  filter(!is.na(brier)) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = if (exists("pred_scenarios")) names(pred_scenarios) else unique(scenario)
    )
  )


# Map internal scenario names to pretty labels + set desired order
pretty_names <- c(
  "predict_first_round"       = "First Round",
  "first_round"               = "First Round",
  "predict_semifinals"        = "Semifinals",
  "semifinals"                = "Semifinals",
  "predict_conf_finals"       =     "Conference Finals",
  "conference_finals"         = "Conference Finals",
  "predict_nba_finals"        = "NBA Finals",
  "nba_finals"                = "NBA Finals"
)

desired_order <- c("First Round", "Semifinals", "Conference Finals", "NBA Finals")

df_brier <- df_brier %>%
  mutate(
    scenario_label = dplyr::recode(as.character(scenario), !!!pretty_names, .default = as.character(scenario)),
    scenario_label = factor(scenario_label, levels = desired_order)
  )

# Line plot
p_brier <- ggplot(df_brier, aes(x = scenario_label, y = brier, group = method, color = method)) +
  # Add shaded region to show difference
  geom_ribbon(
    data = df_brier %>%
      pivot_wider(names_from = method, values_from = brier) %>%
      mutate(ymin = pmin(Glickman, `Spike–Slab`, na.rm = TRUE),
             ymax = pmax(Glickman, `Spike–Slab`, na.rm = TRUE)),
    aes(x = scenario_label, ymin = ymin, ymax = ymax, group = 1),
    alpha = 0.1,
    fill = "gray50",
    inherit.aes = FALSE
  ) +
  # Thicker lines for better visibility
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  # Add value labels
  geom_text(
    aes(label = sprintf("%.3f", brier)),
    vjust = ifelse(df_brier$method == "Glickman", -2, 3),
    size = 4,
    show.legend = FALSE
  ) +
  # Custom color palette
  scale_color_manual(
    values = c("Glickman" = "#E69F00", "Spike–Slab" = "#56B4E9"),
    labels = c("Glickman", "Spike–Slab")
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.001),
    limits = c(
      min(df_brier$brier) * 0.95,
      max(df_brier$brier) * 1.05
    )
  ) +
  labs(
    x = "Prediction scenario",
    y = "Brier Score",
    color = "Model:",
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(size = 15))

print(p_brier)

ggsave(
  filename = "plot_brier_score.png", path = "Plots",
  plot = p_brier,
  width = 14, height = 10, device = "png", dpi = 500
)
