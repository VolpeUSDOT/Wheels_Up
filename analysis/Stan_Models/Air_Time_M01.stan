// Model 01: Carrier as fixed effect
data {
	int N; 					// Number of observations
	vector[N] y; 			// Response variable: log Air time
	int K;					// Number of predictors
	matrix[N, K] X;	 		// Design matrix
	vector[K] scale_beta;   // Vector of beta priors for each predictors
	real scale_alpha;		// Prior for overall intercept
	real loc_sigma;			// Prior for sigma 
	
	// keep responses
	int use_y_rep;
	int use_log_lik;
}

parameters {
	real alpha;
	vector[K] beta;
	real sigma;
}

transformed parameters {
	vector[N] mu;
	mu = alpha + X * beta;
}

model {
	alpha ~ normal(0, scale_alpha); // Prior for overall intercept
	beta ~ normal(0, scale_beta);	// Priors for each predictors
	sigma ~ exponential(loc_sigma);
	y ~ normal(mu, sigma);
}

generated quantities {
	vector[N * use_y_rep] y_rep;	
	vector[N * use_log_lik] log_lik; 
	
	// Posterior predicted values
	for (i in 1:num_elements(y_rep)) { 	
		y_rep[i] = normal_rng(mu[i], sigma);
	}
	
	// Posterior log-likelihood
	for (i in 1:num_elements(log_lik)) {
		log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
	}
}
