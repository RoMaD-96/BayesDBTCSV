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
    array[N] int<lower=0, upper=1> y;      // Outcome
    int<lower=0, upper=1> ind_home;        // Home effect indicator
    real mean_home;              // Mean for home effect
    real<lower=1e-8> sd_home;      // Standard deviation for home effect

    // Priors for stochastic variance parameters
    real<lower=0> s_prior_shape;     // Prior shape
    real<lower=0> s_prior_rate;      // Prior rate
}

parameters {
    matrix[ntimes_rank, nteams] logStrength_raw;     // Log strength parameters for each team over time
    real home;                  // Home team effect parameter
    real<lower=0> tau2;   // Prior SD for initial log(sigma2)
    real<lower=0> omega2;
    matrix[ntimes_rank, nteams] log_sigma2; // Stochastic log-variance
}

transformed parameters {
    real adj_h_eff;
    matrix[ntimes_rank, nteams] logStrength;
    matrix[ntimes_rank, nteams] sigma = sqrt(exp(log_sigma2));          // sd parameters
    real<lower=0> tau = sqrt(tau2);   // Prior SD for initial log(sigma2)
    real<lower=0> omega = sqrt(omega2);

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
    target += inv_gamma_lpdf(tau2 | s_prior_shape, s_prior_rate);
    target += inv_gamma_lpdf(omega2 | s_prior_shape, s_prior_rate);

    // Priors for initial log(sigma2)
    for (k in 1:nteams) {
        target += normal_lpdf(log_sigma2[1, k] | 0, omega);
    }

    // Stochastic evolution of log(sigma2)
    for (t in 2:ntimes_rank) {
        for (k in 1:nteams) {
            target += normal_lpdf(log_sigma2[t, k] | log_sigma2[t-1, k], tau);
        }
    }

    // AR(1) process for strength parameters using stochastic variances
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

 // Log-likelihood for in-sample data
    for (n in 1:N) {
        real logit_p = logStrength[instants_rank[n], team1[n]] + adj_h_eff -
                      logStrength[instants_rank[n], team2[n]];
        log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p);

        // In-sample replications
        y_rep[n] = bernoulli_logit_rng(logit_p);
    }

    // Out-of-sample predictions
    if (N_prev > 0) {
        for (n in 1:N_prev) {
            real logit_p_prev = logStrength[instants_rank_prev[n], team1_prev[n]] + adj_h_eff -
                               logStrength[instants_rank_prev[n], team2_prev[n]];

            // Prediction
            y_prev[n] = bernoulli_logit_rng(logit_p_prev);
        }
    }
}
