# Packages
library(cmdstanr)
library(posterior)

# Inputs you need:
# - fit: CmdStanMCMC returned by model$sample(), which included N_prev > 0
#        so generated quantities contains y_prev.
# - y_oos: integer(0/1) vector of true out-of-sample outcomes, length N_prev.

# 1) Extract posterior predictive draws for y_prev
#    This returns draws as an array (iterations x chains x variables)
draws_y_prev <- fit_wbt$draws(variables = "y_prev")  # see cmdstanr docs on $draws() :contentReference[oaicite:1]{index=1}

# 2) Combine chains/iterations, getting a draws_matrix (rows = draws, cols = y_prev[i])
dm <- posterior::as_draws_matrix(draws_y_prev)

# 3) Posterior predictive probability for each held-out game = mean of Bernoulli draws
#    Columns are named like "y_prev[1]", "y_prev[2]", ...
p_hat_named <- colMeans(dm)

# 4) Put probabilities in index order 1..N_prev
idx <- as.integer(gsub("^y_prev\\[|\\]$", "", names(p_hat_named)))
p_hat <- as.numeric(p_hat_named[order(idx)])


data_test <- playoffs2 %>%
  mutate(
    team1 = team_lookup[hometeamId],
    team2 = team_lookup[awayteamId],
    y = as.integer(homeScore > awayScore)
  )
y_oos <- data_test$y
# 5) Sanity check lengths
stopifnot(length(p_hat) == length(y_oos))

# 6) Brier score (overall and per-observation)
brier_per_obs <- (p_hat - y_oos)^2
brier_score   <- mean(brier_per_obs)

round(brier_score, 3)
