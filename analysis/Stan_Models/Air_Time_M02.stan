// Model 02: Carrier as random effect
data {
	int N; 					                                // Number of observations
	int N_carrier;                                  // Number of carriers as grouping variable, i.e. J
	int<lower=1, upper=N_carrier> Carrier[N];       // Vector of carrier IDs 
	vector[N] y; 	                            	    // Response variable: log Air time
	int K;				                                  // Number of predictors
	matrix[N, K] X;	 	                              // Design matrix
//vector[K] scale_beta;                           // Vector of beta priors for each predictors
	real loc_sigma;		                            	// Prior for sigma 
	
	}

parameters {
  vector[K] gamma;                                // Population level coef
	vector<lower=0>[K] tau;                         // SD of regression coef
	vector[K] beta_raw[N_carrier];                  // Matrix of group-level regression coefs
	real sigma;                                     // SD of individual obs
}

transformed parameters {
  vector[K] beta[N_carrier];                     // Matrix of group-level regression coefs, after non-centered parameterization
  
  // Group-level regression coefficients, created with non-centered parameterization
	for(j in 1:N_carrier){
	  beta[j] = gamma + tau .* beta_raw[j];
	}
	

}

model {
  vector[N] mu;

  // Priors
	gamma ~ normal(0, 5); 
	tau ~ cauchy(0, 2.5);	
	sigma ~ exponential(loc_sigma);
	
	for(j in 1:N_carrier){
	  beta_raw[j] ~ normal(0, 1);
	}
	
	// Model for each flight, using carrier-specific regression coefficients
	for(n in 1:N){
	  mu[n] = X[n] * beta[Carrier[n]];
	}
	
	y ~ normal(mu, sigma);
}

/*
generated quantities {
	vector[N] y_rep;	
	vector[N] log_lik; 
  vector[N] mu;

	// Posterior predicted values
	for (i in 1:num_elements(y_rep)) { 	
		y_rep[i] = normal_rng(mu[i], sigma);
	}
	
	// Posterior log-likelihood
	for (i in 1:num_elements(log_lik)) {
		log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
	}
}
*/
