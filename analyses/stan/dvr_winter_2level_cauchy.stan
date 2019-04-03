// Chill Freeze Analysis
// 1 April 2019 - Started by Cat
// Looking at the effects of false springs on duration of vegetative risk
// Level: Species on INTERCEPTS and SLOPES
// This model has interactions! The two-way type. 

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] tx; 	// predictor
	vector[N] chill1; 	// predictor
	vector[N] chill2; 	// predictor
		
	}

transformed data {
  vector[N] inter_txchill1;
  vector[N] inter_txchill2;

  inter_txchill1    = tx .* chill1;
  inter_txchill2    = tx .* chill2;
}

parameters {
  real mu_a_sp;   
  real mu_b_tx_sp;   
  real mu_b_chill1_sp;   
  real mu_b_chill2_sp;
  real mu_b_txchill1_sp;   
  real mu_b_txchill2_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_tx_sp; 
  real<lower=0> sigma_b_chill1_sp; 
  real<lower=0> sigma_b_chill2_sp;
  real<lower=0> sigma_b_txchill1_sp;
  real<lower=0> sigma_b_txchill2_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_tx[n_sp]; // slope of forcing effect 
  real b_chill1[n_sp]; // slope of photoperiod effect
  real b_chill2[n_sp]; // slope of chill effect
  real b_txchill1[n_sp]; // slope of lat effect
  real b_txchill2[n_sp]; // slope of chill x force effect

	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_tx[sp[i]] * tx[i] + 
	      	b_chill1[sp[i]] * chill1[i] +
		b_chill2[sp[i]] * chill2[i] +
		      b_txchill1[sp[i]] * inter_txchill1[i] +
                b_txchill2[sp[i]] *  inter_txchill2[i];
	}
}

model {

	a_sp ~ cauchy(mu_a_sp, sigma_a_sp); 
	b_tx ~ cauchy(mu_b_tx_sp, sigma_b_tx_sp); 
	b_chill1 ~ cauchy(mu_b_chill1_sp, sigma_b_chill1_sp); 
	b_chill2 ~ cauchy(mu_b_chill2_sp, sigma_b_chill2_sp);
	b_txchill1 ~ cauchy(mu_b_txchill1_sp, sigma_b_txchill1_sp);
	b_txchill2 ~ cauchy(mu_b_txchill2_sp, sigma_b_txchill2_sp); 

        mu_a_sp ~ cauchy(0, 50);
        sigma_a_sp ~ cauchy(0, 10);

        mu_b_tx_sp ~ cauchy(0, 50);
        sigma_b_tx_sp ~ cauchy(0, 10);
        mu_b_chill1_sp ~ cauchy(0, 50);
        sigma_b_chill1_sp ~ cauchy(0, 10);
        mu_b_chill2_sp ~ cauchy(0, 50);
        sigma_b_chill2_sp ~ cauchy(0, 10);
        mu_b_txchill1_sp ~ cauchy(0, 50);
        sigma_b_txchill1_sp ~ cauchy(0, 10);

        mu_b_txchill2_sp ~ cauchy(0, 50);
        sigma_b_txchill2_sp ~ cauchy(0, 10);

	y ~ cauchy(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_tx[sp[n]] * tx[n] + 
	      	b_chill1[sp[n]] * chill1[n] +
		b_chill2[sp[n]] * chill2[n] +
		      b_txchill1[sp[n]] * inter_txchill1[n] +
		b_txchill2[sp[n]] * inter_txchill2[n];
    for (n in 1:N)
      y_ppc[n] = cauchy_rng(y_ppc[n], sigma_y);

}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_rng(y[n], sigma_y);
    
  }
}