
data {
  int<lower=1> N;  // total number of observations (all subjects)
  array[N] int npx;  // total number of observations per X
  
  int<lower=0> S; //Total number of subjects
  array[N] int S_id;  // total number of observations (per subjects)

  array[N] int Y;  // response variable
  matrix[N, 2] X;  // design matrix (first column being intercept i.e. 1)

}

parameters {
  // hierarchical group level means 
  vector [3] gm;
  // hierarchical group level deviations
  vector<lower = 0>[3]  tau_u;
  // Subject-level estimate matrix 
  matrix[2, S] z_expo;
  vector[S] alpha;
  }



transformed parameters{
  //getting subject level estimates for ease.
  //vector[S] alpha = rep_vector(0, S);
  vector<lower = 0>[S] beta = rep_vector(0, S);
  vector<lower = 0, upper = 1>[S] lapse = rep_vector(0, S);
  vector<lower = 0>[S] real_alpha = rep_vector(0, S);
  //trial level parameters
  vector[N] X_weibull = rep_vector(0, N);
  vector[N] mu = rep_vector(0, N);

  
  real_alpha = exp(alpha);
  //alpha = (gm[1]+(tau_u[1] * z_expo[1,]))';
  beta = exp(gm[2]+(tau_u[2] * z_expo[1,]))';
  lapse = (inv_logit(gm[3]+(tau_u[3] * z_expo[2,])) / 2)';
  

  
  //model
  for (n in 1:N){
      X_weibull[n] = 1-exp(-((X[n,2]/real_alpha[S_id[n]])^beta[S_id[n]]));
      
      mu[n] = lapse[S_id[n]] + (1 - 2 * lapse[S_id[n]]) * X_weibull[n];
    }
}
  
  



model{
  
  //priors
  // target += normal_lpdf(gm[1] | 0,4);
  // target += normal_lpdf(gm[2] |  0,3);
  // //target += normal_lpdf(gm_lapse | -4, 2);
  // target += normal_lpdf(gm[3] | -4, 2);
  
  // target += normal_lpdf(gm[4] | -3, 2);
  target += std_normal_lpdf(to_vector(z_expo));
  // target += normal_lpdf(tau_u[1] | 0, 20)-normal_lccdf(0 | 0, 20);
  // target += normal_lpdf(tau_u[2] | 0, 5)-normal_lccdf(0 | 0, 5);
  // target += normal_lpdf(tau_u[3] | 0, 5)-normal_lccdf(0 | 0, 5);
  
  target += normal_lpdf(alpha | gm[1], tau_u[1]);


  //target += bernoulli_lpmf(Y | mu);
  target += binomial_lpmf(Y |npx ,mu);
}


generated quantities{

  //prior posterior updates

  // real mu_alpha;
  // real mu_beta;
  // real mu_guess;
  // real mu_lapse;
  // real prior_mu_alpha;
  // real prior_mu_beta;
  // real prior_mu_guess;
  // real prior_mu_lapse;
  
  // mu_alpha = gm[1];
  // mu_beta = exp(gm[2]);
  // mu_guess = inv_logit(gm[3]);
  // mu_lapse = inv_logit(gm[4]);
  // 
  // 
  // prior_mu_alpha = normal_rng(0,10);
  // prior_mu_beta = exp(normal_rng(log(10),0.6));
  // prior_mu_guess = inv_logit(normal_rng(-3,1));
  // prior_mu_lapse = inv_logit(normal_rng(-3,1));
  // 

  vector[N] log_lik;
  
  for (n in 1:N){
    log_lik[n] = binomial_lpmf(Y[n] |npx[n] , mu[n]);
  
  }
  
}
