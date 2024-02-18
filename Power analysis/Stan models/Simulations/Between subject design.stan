
data {
  int<lower=1> N;  // total number of observations (all subjects)
  int<lower=1> C;  // total number of conditions (all subjects) (for now 2)
  
  int<lower=0> S; //Total number of subjects
  array[N] int S_id;

  array[N,C] int Y;
  
  matrix[N, C] X;  // design matrix (first column being intercept i.e. 1)

}

parameters {
  // hierarchical group level means 
  vector [9] gm;
  // hierarchical group level deviations
  vector<lower = 0>[9]  tau_u;
  // Subject-level estimate matrix 
  matrix[9, S] z_expo;
  // for the cholesky decomposition
  cholesky_factor_corr[9] L_u;
  
}



transformed parameters{
  //getting subject level estimates for ease.
  matrix[N,C] X_normal = rep_matrix(0, N,C);
  matrix[N,C] mu = rep_matrix(0, N,C);
  
  matrix[S,C] alpha = rep_matrix(0, S,C);
  matrix<lower = 0>[S,C] beta = rep_matrix(0, S,C);
  matrix<lower = 0, upper = 1>[S,C] lapse = rep_matrix(0, S,C);

  matrix[S,9] indi_dif = (diag_pre_multiply(tau_u, L_u) * z_expo)';
  
  alpha[,1] = (gm[1]+(indi_dif[,1]));
  beta[,1] = exp(gm[2]+(indi_dif[,2]));
  lapse[,1] = inv_logit(gm[3]+(indi_dif[,3]));
    
  alpha[,2] = (gm[4]+(indi_dif[,4]));
  beta[,2] = exp(gm[5]+(indi_dif[,5]));
  lapse[,2] = inv_logit(gm[6]+(indi_dif[,6]));

  alpha[,3] = (gm[7]+(indi_dif[,7]));
  beta[,3] = exp(gm[8]+(indi_dif[,8]));
  lapse[,3] = inv_logit(gm[9]+(indi_dif[,9]));

  
    for (n in 1:N){
      for(c in 1:C){
      X_normal[n,c] = 0.5+0.5*erf((X[n,c]-alpha[S_id[n],c]) / (beta[S_id[n],c]*sqrt(2)));
      mu[n,c] = lapse[S_id[n],c] + (1- 2 * lapse[S_id[n],c]) * X_normal[n,c];
    }
  
    }
}


model{
  
  //priors
  target += normal_lpdf(gm[1] | 0,20);
  target += normal_lpdf(gm[2] | log(10),0.6);
  target += normal_lpdf(gm[3] | -3, 1);
  
  target += normal_lpdf(gm[4] | 0,20);
  target += normal_lpdf(gm[5] | log(10),0.6);
  target += normal_lpdf(gm[6] | -3, 1);
  
  
  target += normal_lpdf(gm[7] | 0,20);
  target += normal_lpdf(gm[8] | log(10),0.6);
  target += normal_lpdf(gm[9] | -3, 1);
  

  target += std_normal_lpdf(to_vector(z_expo));
  target += normal_lpdf(tau_u[1] | 0, 10)-normal_lccdf(0 | 0, 10);
  target += normal_lpdf(tau_u[2] | 0, 2)-normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_u[3] | 0, 3)-normal_lccdf(0 | 0, 3);

  target += normal_lpdf(tau_u[4] | 0, 10)-normal_lccdf(0 | 0, 10);
  target += normal_lpdf(tau_u[5] | 0, 2)-normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_u[6] | 0, 3)-normal_lccdf(0 | 0, 3);

  target += normal_lpdf(tau_u[7] | 0, 10)-normal_lccdf(0 | 0, 10);
  target += normal_lpdf(tau_u[8] | 0, 2)-normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_u[9] | 0, 3)-normal_lccdf(0 | 0, 3);

  target += lkj_corr_cholesky_lpdf(L_u | 2);
  //model
    for (c in 1:C){
    
      target += bernoulli_lpmf(Y[,c] | mu[,c]);

  }

}


generated quantities{
  

  matrix[N,C] log_lik;
  
  real mu_cond;
  real mu_cond2;
  
  real mu_cond_beta;
  real mu_cond_beta2;
  
  mu_cond = gm[1]-gm[7];
  mu_cond2 = gm[1]-gm[4];
  
  mu_cond_beta = gm[2]-gm[8];
  mu_cond_beta2 = gm[2]-gm[5];
  
  
    for (n in 1:N){
      for (c in 1:C){
      log_lik[n,c] = bernoulli_lpmf(Y[n,c] | mu[n,c]);
    }

  }
  
}
