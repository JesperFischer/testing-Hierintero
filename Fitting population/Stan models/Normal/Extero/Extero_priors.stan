
data {
  int<lower=1> N;  // total number of observations (all subjects)
  array[N] int npx;  // total number of observations per X
  
  int<lower=0> S; //Total number of subjects
  array[N] int S_id;  // total number of observations (per subjects)


  vector[N] condition;
  
  array[N] int Y;  // response variable
  
  vector[N] X;  // design matrix (first column being intercept i.e. 1)

}

parameters {
  // hierarchical group level means 
  vector [6] gm;
  // hierarchical group level deviations
  vector<lower = 0>[6]  tau_u;
  // Subject-level estimate matrix 
  matrix[6, S] z_expo;
  }



transformed parameters{
  
  
  //getting subject level estimates for ease.
  vector[S] alpha = rep_vector(0, S);
  vector[S] beta = rep_vector(0, S);
  vector[S] lapse = rep_vector(0, S);

  vector[N] alphas;
  vector[N] betas;
  vector[N] lapses;

  
  vector[S] alpha_dif = rep_vector(0, S);
  vector[S] beta_dif = rep_vector(0, S);
  vector[S] lapse_dif = rep_vector(0, S);

  //trial level parameters
  vector[N] X_normal = rep_vector(0, N);
  vector<lower=0>[N] mu = rep_vector(0, N);
  

  alpha = gm[1]+(tau_u[1] * z_expo[1,])';
  beta = gm[2]+(tau_u[2] * z_expo[2,])';
  lapse = gm[3]+(tau_u[3] * z_expo[3,])';
  
  alpha_dif = gm[4]+(tau_u[4] * z_expo[4,])';
  beta_dif = gm[5]+(tau_u[5] * z_expo[5,])';
  lapse_dif = gm[6]+(tau_u[6] * z_expo[6,])';
  
  
  


  for (n in 1:N){
    
      alphas[n] = (alpha[S_id[n]] + condition[n] .* alpha_dif[S_id[n]]);
      betas[n] = (beta[S_id[n]]+ condition[n] .* beta_dif[S_id[n]]);
      lapses[n] = (lapse[S_id[n]] + condition[n] .* lapse_dif[S_id[n]]);


      X_normal[n] = 0.5+0.5*erf((X[n]-alphas[n])/(exp(betas[n])*sqrt(2)));
      
      mu[n] = (inv_logit(lapses[n]) / 2) + (1 - 2 * (inv_logit(lapses[n])/ 2)) * X_normal[n];
    }
  }
  
  



model{
  
  //priors
  target += normal_lpdf(gm[1] | 0,50);
  target += normal_lpdf(gm[2] |  0,3);
  target += normal_lpdf(gm[3] | -4, 2);
  target += normal_lpdf(gm[4] | 0, 20);
  target += normal_lpdf(gm[5] | 0, 2);
  target += normal_lpdf(gm[6] | 0, 2);
  
  
  // // target += normal_lpdf(gm[4] | -3, 2);
  target += std_normal_lpdf(to_vector(z_expo));

  target += normal_lpdf(tau_u[1] | 0, 20)-normal_lccdf(0 | 0, 20);
  target += normal_lpdf(tau_u[2] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[3] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[4] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[5] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[6] | 0, 5)-normal_lccdf(0 | 0, 5);
  
  // // 
  // target += normal_lpdf(alpha | gm[1], tau_u[1]);
  // target += normal_lpdf(alpha_dif | gm[4], tau_u[4]);


  //target += bernoulli_lpmf(Y | mu);
  target += binomial_lpmf(Y |npx ,mu);
}


generated quantities{


  vector[N] log_lik;
  
  for (n in 1:N){
    log_lik[n] = binomial_lpmf(Y[n] |npx[n] ,mu[n]);
  
  }
  
}
