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
	vector[N] drought1;
	vector[N] drought2;
		
	}

transformed data {
  vector[N] inter_txchill1;
  vector[N] inter_txchill2;
  vector[N] inter_txdrought1;
  vector[N] inter_txdrought2;

  inter_txchill1    = tx .* chill1;
  inter_txchill2    = tx .* chill2;
  
  inter_txdrought1    = tx .* drought1;
  inter_txdrought2    = tx .* drought2;
}

parameters {
  real mu_a_sp;   
  real mu_b_tx_sp;   
  real mu_b_chill1_sp;   
  real mu_b_chill2_sp;
  real mu_b_drought1_sp;   
  real mu_b_drought2_sp;
  real mu_b_txchill1_sp;   
  real mu_b_txchill2_sp; 
  real mu_b_txdrought1_sp;   
  real mu_b_txdrought2_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_tx_sp; 
  real<lower=0> sigma_b_chill1_sp; 
  real<lower=0> sigma_b_chill2_sp;
  real<lower=0> sigma_b_txchill1_sp;
  real<lower=0> sigma_b_txchill2_sp;
  real<lower=0> sigma_b_drought1_sp; 
  real<lower=0> sigma_b_drought2_sp;
  real<lower=0> sigma_b_txdrought1_sp;
  real<lower=0> sigma_b_txdrought2_sp;
  real<lower=0> sigma_y; 

  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_tx; // slope of forcing effect 
  vector[n_sp] b_chill1; // slope of photoperiod effect
  vector[n_sp] b_chill2; // slope of chill effect
  vector[n_sp] b_txchill1_ncp; // slope of lat effect
  vector[n_sp] b_txchill2_ncp; // slope of chill x force effect
  vector[n_sp] b_drought1; // slope of photoperiod effect
  vector[n_sp] b_drought2; // slope of chill effect
  vector[n_sp] b_txdrought1_ncp; // slope of lat effect
  vector[n_sp] b_txdrought2_ncp; // slope of chill x force effect

	}

transformed parameters {
  vector[n_sp] b_txchill1; // slope of interaction 
  vector[n_sp] b_txchill2; // slope of interaction 
  vector[n_sp] b_txdrought1; // slope of interaction 
  vector[n_sp] b_txdrought2; // slope of interaction 
  vector[N] yhat;
  
  b_txchill1 = mu_b_txchill1_sp + sigma_b_txchill1_sp*b_txchill1_ncp;
  b_txchill2 = mu_b_txchill2_sp + sigma_b_txchill2_sp*b_txchill2_ncp;
  
  b_txdrought1 = mu_b_txdrought1_sp + sigma_b_txdrought1_sp*b_txdrought1_ncp;
  b_txdrought2 = mu_b_txdrought2_sp + sigma_b_txdrought2_sp*b_txdrought2_ncp;
  
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_tx[sp[i]] * tx[i] + 
	      	b_chill1[sp[i]] * chill1[i] +
		b_chill2[sp[i]] * chill2[i] +
		  b_drought1[sp[i]] * drought1[i] +
		b_drought2[sp[i]] * drought2[i] +
		      b_txchill1[sp[i]] * inter_txchill1[i] +
                b_txchill2[sp[i]] *  inter_txchill2[i] +
		      b_txdrought1[sp[i]] * inter_txdrought1[i] +
                b_txdrought2[sp[i]] *  inter_txdrought2[i];
	}
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_tx ~ normal(mu_b_tx_sp, sigma_b_tx_sp); 
	b_chill1 ~ normal(mu_b_chill1_sp, sigma_b_chill1_sp); 
	b_chill2 ~ normal(mu_b_chill2_sp, sigma_b_chill2_sp);
	
	b_drought1 ~ normal(mu_b_drought1_sp, sigma_b_drought1_sp); 
	b_drought2 ~ normal(mu_b_drought2_sp, sigma_b_drought2_sp);
	//b_txchill1 ~ normal(mu_b_txchill1_sp, sigma_b_txchill1_sp);
	//b_txchill2 ~ normal(mu_b_txchill2_sp, sigma_b_txchill2_sp); 
	
	b_txchill1_ncp ~ skew_normal(0, 10, 5);
	b_txchill2_ncp ~ skew_normal(0, 10, 5);
	b_txdrought1_ncp ~ skew_normal(0, 10, 5);
	b_txdrought2_ncp ~ skew_normal(0, 10, 5);

        mu_a_sp ~skew_normal(0, 50, 5);
        sigma_a_sp ~ skew_normal(0, 10, 5);

        mu_b_tx_sp ~ skew_normal(0, 50, 5);
        sigma_b_tx_sp ~ skew_normal(0, 10, 5);
        mu_b_chill1_sp ~ skew_normal(0, 50, 5);
        sigma_b_chill1_sp ~ skew_normal(0, 10, 5);
        mu_b_chill2_sp ~ skew_normal(0, 50, 5);
        sigma_b_chill2_sp ~ skew_normal(0, 10, 5);
        mu_b_txchill1_sp ~ skew_normal(0, 50, 5);
        sigma_b_txchill1_sp ~ skew_normal(0, 10, 5);
        mu_b_txchill2_sp ~ skew_normal(0, 50, 5);
        sigma_b_txchill2_sp ~ skew_normal(0, 10, 5);
        
        mu_b_drought1_sp ~ skew_normal(0, 50, 5);
        sigma_b_drought1_sp ~ skew_normal(0, 10, 5);
        mu_b_drought2_sp ~ skew_normal(0, 50, 5);
        sigma_b_drought2_sp ~ skew_normal(0, 10, 5);
        mu_b_txdrought1_sp ~ skew_normal(0, 50, 5);
        sigma_b_txdrought1_sp ~ skew_normal(0, 10, 5);
        mu_b_txdrought2_sp ~ skew_normal(0, 50, 5);
        sigma_b_txdrought2_sp ~ skew_normal(0, 10, 5);

	y ~ normal(yhat, sigma_y);

}

/*
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
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}
*/

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_rng(y[n], sigma_y);
    
  }
}

