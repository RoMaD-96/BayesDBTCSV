data {
    int<lower=1> N;          // Number of observations
    int<lower=0> N_prev;
    int<lower=1> nteams;          // Number of teams
    int<lower=1> ntimes_rank;      // Number of time points
    array[N] int<lower=1, upper=ntimes_rank> instants_rank;  // Time point of each observation
    array[N_prev] int instants_rank_prev;
    array[N] int<lower=1, upper=nteams> team1;  // Index of team1 in each observation
    array[N] int<lower=1, upper=nteams> team2;  // Index of team2 in each observation
    array[N_prev] int team1_prev;
    array[N_prev] int team2_prev;
    real mean_logStrength;                // Initial mean for logStrength
    array[N] int<lower=0, upper=1> y;      // Outcome: 1 if team1 beats team2, 3 if team2 beats team1, 2 for tie
    int<lower=0, upper=1> ind_home;        // Home effect indicator
    real mean_home;              // Mean for home effect
    real<lower=1e-8> sd_home;      // Standard deviation for home effect

    // Priors for stochastic variance parameters
    real<lower=0> s_prior_shape;     // Prior shape for s²
    real<lower=0> s_prior_rate;      // Prior rate for s²
}

parameters {
    matrix[ntimes_rank, nteams] logStrength_raw;     // Log strength parameters for each team over time
    real home;                  // Home team effect parameter
    real<lower=0> inv_tau2;   // Prior SD for initial log(σ²)
    real<lower=0> inv_omega2;
    // Stochastic variance parameters (following Glickman's formulation)
    matrix[ntimes_rank, nteams] log_sigma2;  // Log of variance parameters
}

transformed parameters {
    real adj_h_eff;
    matrix[ntimes_rank, nteams] logStrength;
    matrix[ntimes_rank, nteams] sigma = sqrt(exp(log_sigma2));          // sd parameters (σ)
    real<lower=0> tau = inv(sqrt(inv_tau2));   // Prior SD for initial log(σ²)
    real<lower=0> omega = inv(sqrt(inv_omega2));

    // Sum-to-zero constraint for log-strength parameters
    logStrength[1] = logStrength_raw[1] - mean(logStrength_raw[1]);
    for (t in 2:ntimes_rank) {
        logStrength[t] = logStrength_raw[t] - mean(logStrength_raw[t]);
    }

    adj_h_eff = home * ind_home;

}

model {
    // Prior for the home effect
    target += normal_lpdf(home | mean_home, sd_home);

    // Prior for global stochastic variance parameter
    target += gamma_lpdf(inv_tau2 | s_prior_shape, s_prior_rate);
    target += gamma_lpdf(inv_omega2 | s_prior_shape, s_prior_rate);

    // Priors for initial log(σ²) - Glickman's equation (4) equivalent
    for (k in 1:nteams) {
        target += normal_lpdf(log_sigma2[1, k] | 0, omega);
    }

    // Stochastic evolution of log(σ²) - This is Glickman's key innovation (equation 3)
    for (t in 2:ntimes_rank) {
        for (k in 1:nteams) {
            target += normal_lpdf(log_sigma2[t, k] | log_sigma2[t-1, k], tau);
        }
    }

    // AR(1) process for strength parameters using stochastic variances
    // This implements Glickman's equation (2): c^(t+1) ~ N(c^(t), σ²^(t+1))
    for(k in 1:nteams){
        target+=normal_lpdf(logStrength_raw[1, k] | mean_logStrength, sigma[1, k]);
    }

    for (t in 2:ntimes_rank) {
        for (k in 1:nteams) {
            target += normal_lpdf(logStrength_raw[t, k] | logStrength_raw[t-1, k], sigma[t, k]);
        }
    }

    for (n in 1:N) {
    target+= bernoulli_logit_lpmf(y[n]|logStrength[instants_rank[n],team1[n]] + adj_h_eff - logStrength[instants_rank[n],team2[n]]);
    }
}

generated quantities {
    // Log-likelihood vector
    vector[N] log_lik;
    array[N] int y_rep;
    array[N_prev] int y_prev;

 // Calculate log-likelihood for in-sample data
    for (n in 1:N) {
        real logit_p = logStrength[instants_rank[n], team1[n]] + adj_h_eff -
                      logStrength[instants_rank[n], team2[n]];
        log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p);

        // Generate in-sample replications
        y_rep[n] = bernoulli_logit_rng(logit_p);
    }

    // Generate out-of-sample predictions
    if (N_prev > 0) {
        for (n in 1:N_prev) {
            real logit_p_prev = logStrength[instants_rank_prev[n], team1_prev[n]] + adj_h_eff -
                               logStrength[instants_rank_prev[n], team2_prev[n]];

            // Generate prediction
            y_prev[n] = bernoulli_logit_rng(logit_p_prev);
        }
    }
}
