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
    real mu_spike;
    real<lower=0> sd_spike;
    real mu_slab;
    real<lower=0> sd_slab;
    real<lower=0, upper=1> p_spike;
}

parameters {
    matrix[ntimes_rank, nteams] logStrength_raw;     // Log strength parameters for each team over time
    real home;                // Home team effect parameter
    // Only declare p_mix for times 2:ntimes_rank (not for time 1)
    array[ntimes_rank - 1, nteams] real<lower = 0, upper = 1> p_mix_free;
}

transformed parameters {
    real adj_h_eff;
    matrix[ntimes_rank, nteams] logStrength;
    // Create full p_mix array with first time period set to 0
    array[ntimes_rank, nteams] real<lower = 0, upper = 1> p_mix;

    // Set first time period to 0 (no mixing)
    for (k in 1:nteams) {
        p_mix[1, k] = 0;
    }

    // Copy free parameters for times 2:ntimes_rank
    for (t in 2:ntimes_rank) {
        for (k in 1:nteams) {
            p_mix[t, k] = p_mix_free[t-1, k];
        }
    }

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

    // First time period - no mixing (p_mix[1,k] = 0 already set in transformed parameters)
    for(k in 1:nteams){
        target += normal_lpdf(logStrength_raw[1, k] | mean_logStrength, 10);
    }

    // Subsequent time periods with mixing
    for (t in 2:ntimes_rank) {
        for (k in 1:nteams) {
            // Prior on mixing probability (using p_mix_free which gets mapped to p_mix)
            target += beta_lpdf(p_mix_free[t-1, k] | 1, 1);
            // Mixture model for logStrength
            target += log_mix(p_mix[t, k],
                            normal_lpdf(logStrength_raw[t, k] | logStrength_raw[t-1, k], 0.1),
                            normal_lpdf(logStrength_raw[t, k] | 0, 10));
        }
    }

    // Likelihood
    for (n in 1:N) {
        target += bernoulli_logit_lpmf(y[n] | logStrength[instants_rank[n], team1[n]] + adj_h_eff -
                                              logStrength[instants_rank[n], team2[n]]);
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
