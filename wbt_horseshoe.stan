data {
    int<lower=1> N;                        // Number of observations
    int<lower=0> N_prev;
    int<lower=1> nteams;                   // Number of teams
    int<lower=1> ntimes_rank;              // Number of time points
    array[N] int<lower=1, upper=ntimes_rank> instants_rank;  // Time point of each observation
    array[N_prev] int instants_rank_prev;
    array[N] int<lower=1, upper=nteams> team1;  // Index of team1 in each observation
    array[N] int<lower=1, upper=nteams> team2;  // Index of team2 in each observation
    array[N_prev] int team1_prev;
    array[N_prev] int team2_prev;
    real mean_logStrength;                 // Initial mean for logStrength
    array[N] int<lower=0, upper=1> y;      // Outcome: 1 if team1 beats team2, 0 otherwise
    int<lower=0, upper=1> ind_home;        // Home effect indicator
    real mean_home;                        // Mean for home effect
    real<lower=1e-8> sd_home;              // Standard deviation for home effect
    // Regularized horseshoe hyperparameters
    real<lower=0> scale_global;            // Global scale parameter for horseshoe (tau_0)
    real<lower=0> slab_scale;              // Slab scale for regularization (c)
    real<lower=0> slab_df;                 // Degrees of freedom for slab (recommended: 4)
}

transformed data {
    // Pre-compute for efficiency
    real slab_scale2 = square(slab_scale);
}

parameters {
    matrix[ntimes_rank, nteams] logStrength_raw;     // Log strength parameters for each team over time
    real home;                                        // Home team effect parameter

    // Regularized horseshoe parameters
    real<lower=0> tau;                               // Global shrinkage parameter
    matrix<lower=0>[ntimes_rank, nteams] lambda;     // Local shrinkage parameters
    real<lower=0> caux;                              // Auxiliary variable for regularization
}

transformed parameters {
    real adj_h_eff;
    matrix[ntimes_rank, nteams] logStrength;
    matrix<lower=0>[ntimes_rank, nteams] lambda_tilde;  // Regularized local shrinkage
    matrix<lower=0>[ntimes_rank, nteams] sd_logStrength; // Standard deviations for evolution

    // Regularization of local shrinkage parameters
    real c = slab_scale * sqrt(caux);
    for (t in 1:ntimes_rank) {
        for (k in 1:nteams) {
            lambda_tilde[t, k] = sqrt(c^2 * square(lambda[t, k]) /
                                     (c^2 + square(tau) * square(lambda[t, k])));
            // Transform to standard deviation (inverse of precision)
            sd_logStrength[t, k] = tau * lambda_tilde[t, k];
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

    // Regularized horseshoe priors
    // Global shrinkage parameter (half-Cauchy)
    target += cauchy_lpdf(tau | 0, scale_global) + log(2);

    // Local shrinkage parameters (half-Cauchy)
    for (t in 1:ntimes_rank) {
        for (k in 1:nteams) {
            target += cauchy_lpdf(lambda[t, k] | 0, 1) + log(2);
        }
    }

    // Slab regularization (inverse-gamma)
    target += inv_gamma_lpdf(caux | 0.5 * slab_df, 0.5 * slab_df);

    // AR(1) process for strength parameters using stochastic variances
    // Initial time point
    for(k in 1:nteams){
        target += normal_lpdf(logStrength_raw[1, k] | mean_logStrength, sd_logStrength[1, k]);
    }

    // Evolution over time
    for (t in 2:ntimes_rank) {
        for (k in 1:nteams) {
            target += normal_lpdf(logStrength_raw[t, k] | logStrength_raw[t-1, k], sd_logStrength[t, k]);
        }
    }

    // Likelihood
    for (n in 1:N) {
        target += bernoulli_logit_lpmf(y[n] | logStrength[instants_rank[n], team1[n]] +
                                              adj_h_eff -
                                              logStrength[instants_rank[n], team2[n]]);
    }
}

generated quantities {
    // Log-likelihood vector
    vector[N] log_lik;
    array[N] int y_rep;
    array[N_prev] int y_prev;

    // Effective number of non-zero precision parameters (for diagnostics)
    real m_eff = 0;
    for (t in 1:ntimes_rank) {
        for (k in 1:nteams) {
            m_eff += (1.0 / (1.0 + square(tau * lambda_tilde[t, k])));
        }
    }

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
