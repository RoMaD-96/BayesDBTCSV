# Posterior Predictive Checks for Glickman and Spike-and-Slab Models
# Load required libraries
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


# ============================================================================
# 1. EXTRACT POSTERIOR PREDICTIVE SAMPLES
# ============================================================================

# Extract draws from both models
draws_spike_slab <- fit_wbt_spike_slab$draws(format = "draws_df")

# Extract y_rep (posterior predictive samples)
y_rep_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("y_rep", format = "matrix"))

# Get observed outcomes
y_obs <- stan_data$y

# ============================================================================
# 2. BASIC POSTERIOR PREDICTIVE CHECKS
# ============================================================================
# Build facet labels in the same order used to create team1/team2
team_ids <- sort(unique(c(nba_data$hometeamId, nba_data$awayteamId)))

team_meta <- dplyr::bind_rows(
  nba_data %>% transmute(team_id = hometeamId,
                         team_label = paste0(hometeamCity, " ", hometeamName)),
  nba_data %>% transmute(team_id = awayteamId,
                         team_label = paste0(awayteamCity, " ", awayteamName))
) %>% distinct(team_id, .keep_all = TRUE)

# Vector of labels aligned to 1:nteams (i.e., the indices in stan_data$team1)
facet_labels <- team_meta$team_label[match(team_ids, team_meta$team_id)]

# Factor for grouping (this is the key line)
group_fac <- factor(stan_data$winner_team,
                    levels = seq_along(team_ids),
                    labels = facet_labels)

# Now plot using the factor instead of the numeric IDs
p_stat_win <- ppc_bars_grouped(
  y      = y_obs,
  yrep   = y_rep_spike_slab,
  group  = group_fac,           # <-- use the factor with team names
  prob   = 0.95,
  freq   = FALSE,
  facet_args = list(ncol = 6, nrow = 5)
) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),  # maybe a tad smaller if names are long
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0)
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Away Win", "Home Win")) +
  labs(y = "Proportion",
       x = "")

p_stat_win

ggsave(
  filename = "p_stat_win.pdf", path = "Plots",
  plot = p_stat_win,
  width = 17, height = 14, device = "pdf", dpi = 500
)



# Now plot using the factor instead of the numeric IDs
p_stat_mean <- ppc_stat(
  y      = y_obs,
  yrep   = y_rep_spike_slab,
  stat   = "mean",
  freq   = FALSE
) +
  theme_bw() +
  theme(
    strip.text.x   = element_text(size = 14),
    axis.text.y    = element_text(size = 12),
    axis.title.y   = element_text(size = 16),
    axis.title.x   = element_text(size = 16),
    legend.title   = element_blank(),
    legend.text    = element_text(size = 12),
    axis.text.x    = element_text(size = 12),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x  = unit(4, "mm"),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 18, hjust = 0)
  ) +
  labs(
    y = "Frequency",
    x = "Home win probability"
  ) +
  annotate(
    "text", x = -Inf, y = 60, label = "PPP = 0.501",
    hjust = -0.1, vjust = 1.2, size = 5
  )

ggsave(
  filename = "p_stat_mean.pdf", path = "Plots",
  plot = p_stat_mean,
  width = 12, height = 7, device = "pdf", dpi = 500
)




p_PIT <- ppc_pit_ecdf_grouped(y_obs, y_rep_spike_slab, stan_data$instants_rank)+ theme_bw() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 1),
    strip.background = element_rect(fill = "grey95"),
    panel.spacing.x = unit(4, "mm"),

    legend.position = "right",
    plot.title = element_text(face = "bold", size = 24, hjust = 0)  # left-aligned
  )

ggsave(
  filename = "p_stat_global_mean.pdf", path = "Plots",
  plot = p_stat_global_mean,
  width = 12, height = 9, device = "pdf", dpi = 500
)

# Function to create basic PPC plots
create_basic_ppc <- function(y, y_rep, model_name) {

  # Density overlay
  p1 <- ppc_dens_overlay(y, y_rep[1:min(100, nrow(y_rep)), ]) +
    ggtitle(paste(model_name, "- Density Overlay"))

  # Histogram comparison
  p2 <- ppc_hist(y, y_rep[1:8, ]) +
    ggtitle(paste(model_name, "- Histogram Comparison"))

  # Empirical CDF comparison
  p3 <- ppc_ecdf_overlay(y, y_rep[1:min(100, nrow(y_rep)), ]) +
    ggtitle(paste(model_name, "- ECDF Overlay"))

  # Proportion of wins (since y is binary)
  p4 <- ppc_stat(y, y_rep, stat = "mean") +
    ggtitle(paste(model_name, "- Proportion of Home Wins"))

  return(list(density = p1, hist = p2, ecdf = p3, prop = p4))
}

# Create basic PPCs for both models
ppc_glick <- create_basic_ppc(y_obs, y_rep_glick, "Glickman")
ppc_spike_slab <- create_basic_ppc(y_obs, y_rep_spike_slab, "Spike-and-Slab")

# Display basic checks
print("Glickman Model PPCs:")
(ppc_glick$density + ppc_glick$prop) / (ppc_glick$hist + ppc_glick$ecdf)

print("Spike-and-Slab Model PPCs:")
(ppc_spike_slab$density + ppc_spike_slab$prop) / (ppc_spike_slab$hist + ppc_spike_slab$ecdf)

# ============================================================================
# 3. CALIBRATION CHECKS
# ============================================================================

# Function to compute calibration statistics
compute_calibration <- function(y, y_rep, model_name) {

  # Compute posterior predictive probabilities
  pp_probs <- colMeans(y_rep)

  # Create calibration data frame
  calib_df <- data.frame(
    observed = y,
    predicted_prob = pp_probs
  ) %>%
    mutate(
      prob_bin = cut(predicted_prob,
                     breaks = seq(0, 1, by = 0.1),
                     include.lowest = TRUE)
    ) %>%
    group_by(prob_bin) %>%
    summarise(
      mean_predicted = mean(predicted_prob),
      mean_observed = mean(observed),
      n = n(),
      se = sqrt(mean_observed * (1 - mean_observed) / n)
    )

  # Calibration plot
  p_calib <- ggplot(calib_df, aes(x = mean_predicted, y = mean_observed)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = n)) +
    geom_errorbar(aes(ymin = mean_observed - 2*se,
                      ymax = mean_observed + 2*se),
                  width = 0.02) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Predicted Probability",
         y = "Observed Proportion",
         title = paste(model_name, "- Calibration Plot"),
         size = "Count") +
    coord_equal()

  return(list(plot = p_calib, data = calib_df))
}

# Compute calibration for both models
calib_glick <- compute_calibration(y_obs, y_rep_glick, "Glickman")
calib_spike_slab <- compute_calibration(y_obs, y_rep_spike_slab, "Spike-and-Slab")

# Display calibration plots
calib_glick$plot | calib_spike_slab$plot

# ============================================================================
# 4. TEST STATISTICS
# ============================================================================

# Function to compute various test statistics
compute_test_stats <- function(y, y_rep, model_name) {

  # Define test statistics
  test_stats <- list(
    mean = function(x) mean(x),
    variance = function(x) var(x),
    min = function(x) min(x),
    max = function(x) max(x),
    runs = function(x) {
      # Number of runs (consecutive sequences of same value)
      rle(x)$lengths %>% length()
    },
    autocorr = function(x) {
      # Lag-1 autocorrelation
      if(length(x) > 1) cor(x[-length(x)], x[-1]) else 0
    }
  )

  # Compute statistics for observed data
  obs_stats <- sapply(test_stats, function(f) f(y))

  # Compute statistics for each replicated dataset
  rep_stats <- apply(y_rep, 1, function(y_r) {
    sapply(test_stats, function(f) f(y_r))
  })

  # Compute p-values
  p_values <- sapply(names(test_stats), function(stat) {
    mean(rep_stats[stat, ] >= obs_stats[stat])
  })

  # Create plots
  plots <- list()
  for(stat in names(test_stats)) {
    plots[[stat]] <- ppc_stat(y, y_rep, stat = test_stats[[stat]]) +
      ggtitle(paste(model_name, "-", stat,
                    "(p-value:", round(p_values[stat], 3), ")"))
  }

  return(list(
    p_values = p_values,
    plots = plots,
    obs_stats = obs_stats,
    rep_stats = rep_stats
  ))
}

# Compute test statistics for both models
test_glick <- compute_test_stats(y_obs, y_rep_glick, "Glickman")
test_spike_slab <- compute_test_stats(y_obs, y_rep_spike_slab, "Spike-and-Slab")

# Display test statistic plots
print("Glickman Model Test Statistics:")
(test_glick$plots$mean + test_glick$plots$variance) /
  (test_glick$plots$runs + test_glick$plots$autocorr)

print("Spike-and-Slab Model Test Statistics:")
(test_spike_slab$plots$mean + test_spike_slab$plots$variance) /
  (test_spike_slab$plots$runs + test_spike_slab$plots$autocorr)

# ============================================================================
# 5. TEMPORAL PATTERNS
# ============================================================================

# Add temporal information to data
temporal_data <- data.frame(
  y = y_obs,
  time_period = stan_data$instants_rank,
  game_index = 1:length(y_obs)
)

# Function to check temporal patterns
check_temporal_patterns <- function(y, y_rep, temporal_data, model_name) {

  # Compute proportion of home wins by time period
  obs_by_time <- temporal_data %>%
    group_by(time_period) %>%
    summarise(obs_prop = mean(y))

  # Compute for replications
  rep_by_time <- apply(y_rep, 1, function(y_r) {
    temporal_data %>%
      mutate(y = y_r) %>%
      group_by(time_period) %>%
      summarise(prop = mean(y)) %>%
      pull(prop)
  })

  # Compute credible intervals
  time_ci <- apply(rep_by_time, 1, function(x) {
    quantile(x, probs = c(0.025, 0.5, 0.975))
  })

  # Create plot data
  plot_data <- data.frame(
    time_period = obs_by_time$time_period,
    observed = obs_by_time$obs_prop,
    median = time_ci[2, ],
    lower = time_ci[1, ],
    upper = time_ci[3, ]
  )

  # Create plot
  p_temporal <- ggplot(plot_data, aes(x = time_period)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
    geom_line(aes(y = median), color = "blue", size = 1) +
    geom_point(aes(y = observed), color = "red", size = 2) +
    labs(x = "Time Period",
         y = "Proportion of Home Wins",
         title = paste(model_name, "- Temporal Pattern Check"),
         subtitle = "Red points: observed, Blue line/ribbon: posterior predictive") +
    ylim(0, 1)

  return(p_temporal)
}

# Check temporal patterns
temporal_glick <- check_temporal_patterns(y_obs, y_rep_glick, temporal_data, "Glickman")
temporal_spike_slab <- check_temporal_patterns(y_obs, y_rep_spike_slab, temporal_data, "Spike-and-Slab")

temporal_glick | temporal_spike_slab

# ============================================================================
# 6. MODEL COMPARISON METRICS
# ============================================================================

# Function to compute predictive accuracy metrics
compute_accuracy_metrics <- function(y, y_rep, log_lik, model_name) {

  # Posterior predictive probabilities
  pp_probs <- colMeans(y_rep)

  # Binary predictions (threshold = 0.5)
  predictions <- as.integer(pp_probs > 0.5)

  # Accuracy
  accuracy <- mean(predictions == y)

  # Brier score
  brier_score <- mean((pp_probs - y)^2)

  # Log score (using log_lik from Stan)
  log_score <- mean(colMeans(log_lik))

  # Create confusion matrix
  conf_matrix <- table(Predicted = predictions, Actual = y)

  # Compute additional metrics
  tp <- conf_matrix[2, 2]
  tn <- conf_matrix[1, 1]
  fp <- conf_matrix[2, 1]
  fn <- conf_matrix[1, 2]

  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)

  return(list(
    model = model_name,
    accuracy = accuracy,
    brier_score = brier_score,
    log_score = log_score,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    confusion_matrix = conf_matrix
  ))
}

# Extract log-likelihood
log_lik_glick <- as.matrix(fit_glick$draws("log_lik", format = "matrix"))
log_lik_spike_slab <- as.matrix(fit_wbt_spike_slab$draws("log_lik", format = "matrix"))

# Compute metrics
metrics_glick <- compute_accuracy_metrics(y_obs, y_rep_glick, log_lik_glick, "Glickman")
metrics_spike_slab <- compute_accuracy_metrics(y_obs, y_rep_spike_slab, log_lik_spike_slab, "Spike-and-Slab")

# Create comparison table
comparison_df <- data.frame(
  Model = c(metrics_glick$model, metrics_spike_slab$model),
  Accuracy = c(metrics_glick$accuracy, metrics_spike_slab$accuracy),
  Brier_Score = c(metrics_glick$brier_score, metrics_spike_slab$brier_score),
  Log_Score = c(metrics_glick$log_score, metrics_spike_slab$log_score),
  Precision = c(metrics_glick$precision, metrics_spike_slab$precision),
  Recall = c(metrics_glick$recall, metrics_spike_slab$recall),
  F1_Score = c(metrics_glick$f1_score, metrics_spike_slab$f1_score)
)

print("Model Comparison Metrics:")
print(comparison_df)

# ============================================================================
# 7. LOO-CV AND WAIC
# ============================================================================

library(loo)

# Compute LOO for both models
loo_glick <- loo(log_lik_glick, save_psis = TRUE)
loo_spike_slab <- loo(log_lik_spike_slab, save_psis = TRUE)

# Compute WAIC
waic_glick <- waic(log_lik_glick)
waic_spike_slab <- waic(log_lik_spike_slab)

# Model comparison
loo_comparison <- loo_compare(list(
  Glickman = loo_glick,
  `Spike-and-Slab` = loo_spike_slab
))

print("LOO-CV Comparison:")
print(loo_comparison)

print("\nWAIC Comparison:")
print(data.frame(
  Model = c("Glickman", "Spike-and-Slab"),
  WAIC = c(waic_glick$estimates["waic", "Estimate"],
           waic_spike_slab$estimates["waic", "Estimate"]),
  SE = c(waic_glick$estimates["waic", "SE"],
         waic_spike_slab$estimates["waic", "SE"])
))

# ============================================================================
# 8. RESIDUAL ANALYSIS
# ============================================================================

# Function for residual analysis
analyze_residuals <- function(y, y_rep, model_name) {

  # Compute posterior mean predictions
  pp_mean <- colMeans(y_rep)

  # Compute residuals
  residuals <- y - pp_mean

  # Standardized residuals (for binary data)
  std_residuals <- residuals / sqrt(pp_mean * (1 - pp_mean))

  # Create diagnostic plots
  p1 <- ggplot(data.frame(x = pp_mean, y = residuals), aes(x, y)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "Fitted Values", y = "Residuals",
         title = paste(model_name, "- Residuals vs Fitted"))

  p2 <- ggplot(data.frame(x = 1:length(residuals), y = residuals), aes(x, y)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "Game Index", y = "Residuals",
         title = paste(model_name, "- Residuals vs Game Order"))

  p3 <- ggplot(data.frame(residuals = std_residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste(model_name, "- Q-Q Plot of Standardized Residuals"))

  p4 <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(x = "Residuals", y = "Count",
         title = paste(model_name, "- Residual Distribution"))

  return((p1 + p2) / (p3 + p4))
}

# Analyze residuals
resid_glick <- analyze_residuals(y_obs, y_rep_glick, "Glickman")
resid_spike_slab <- analyze_residuals(y_obs, y_rep_spike_slab, "Spike-and-Slab")

print("Glickman Residual Analysis:")
print(resid_glick)

print("Spike-and-Slab Residual Analysis:")
print(resid_spike_slab)

# ============================================================================
# 9. SUMMARY REPORT
# ============================================================================

cat("\n", rep("=", 60), "\n")
cat("POSTERIOR PREDICTIVE CHECK SUMMARY\n")
cat(rep("=", 60), "\n\n")

cat("1. TEST STATISTICS P-VALUES\n")
cat("   (Values close to 0 or 1 indicate model misfit)\n\n")
cat("   Glickman Model:\n")
print(test_glick$p_values)
cat("\n   Spike-and-Slab Model:\n")
print(test_spike_slab$p_values)

cat("\n2. PREDICTIVE ACCURACY METRICS\n\n")
print(comparison_df)

cat("\n3. MODEL COMPARISON (LOO-CV)\n\n")
print(loo_comparison)

cat("\n4. KEY FINDINGS:\n")
cat("   - Check calibration plots for probability calibration\n")
cat("   - Review temporal patterns for time-varying performance\n")
cat("   - Examine residual plots for systematic patterns\n")
cat("   - Compare test statistics to identify model strengths/weaknesses\n")

# Save all plots to PDF if desired
save_plots <- function() {
  pdf("posterior_predictive_checks.pdf", width = 12, height = 8)

  # Basic PPCs
  print((ppc_glick$density + ppc_glick$prop) / (ppc_glick$hist + ppc_glick$ecdf))
  print((ppc_spike_slab$density + ppc_spike_slab$prop) / (ppc_spike_slab$hist + ppc_spike_slab$ecdf))

  # Calibration
  print(calib_glick$plot | calib_spike_slab$plot)

  # Test statistics
  print((test_glick$plots$mean + test_glick$plots$variance) /
          (test_glick$plots$runs + test_glick$plots$autocorr))
  print((test_spike_slab$plots$mean + test_spike_slab$plots$variance) /
          (test_spike_slab$plots$runs + test_spike_slab$plots$autocorr))

  # Temporal patterns
  print(temporal_glick | temporal_spike_slab)

  # Residuals
  print(resid_glick)
  print(resid_spike_slab)

  dev.off()
  cat("\nPlots saved to 'posterior_predictive_checks.pdf'\n")
}

# Uncomment to save plots
# save_plots()

