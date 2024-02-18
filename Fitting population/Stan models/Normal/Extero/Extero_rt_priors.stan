
data {
  int<lower=1> N;  // total number of observations (all subjects)
  array[N] int npx;  // total number of observations per X
  
  int<lower=0> S; //Total number of subjects
  array[N] int S_id;  // total number of observations (per subjects)

  vector[S] min_RT;
  //vector[S] min_RT_dif;
  
  vector <lower = 0> [N] RT;


  array[N] int Y;  // response variable
  matrix[N, 3] X;  // design matrix (first column being intercept i.e. 1)

}

parameters {
  // hierarchical group level means 
  vector [7] gm;
  // hierarchical group level deviations
  vector<lower = 0>[7]  tau_u;
  // Subject-level estimate matrix 
  matrix[6, S] z_expo;
  vector[S] alpha;
  
  
  vector[7] gm_dif;
  vector <lower = 0> [7] tau_u_dif;
  
  matrix[6,S] z_expo_dif;
  vector[S] alpha_dif;
  }



transformed parameters{
  //getting subject level estimates for ease.
  // vector[S] alpha = rep_vector(0, S);
  vector[S] beta = rep_vector(0, S);
  vector[S] lapse = rep_vector(0, S);
  vector[S] intercept = rep_vector(0, S);
  vector[S] beta_rt = rep_vector(0, S);
  vector[S] sigma = rep_vector(0, S);
  vector[S] ndt = rep_vector(0, S);
  
  
  // vector[S] alpha_dif = rep_vector(0, S);
  vector[S] beta_dif = rep_vector(0, S);
  vector[S] lapse_dif = rep_vector(0, S);
  vector[S] intercept_dif = rep_vector(0, S);
  vector[S] beta_rt_dif = rep_vector(0, S);
  vector[S] sigma_dif = rep_vector(0, S);
  vector[S] ndt_dif = rep_vector(0, S);



  // alpha = (gm[1]+(tau_u[1] * z_expo[1,]))';
  beta = (gm[2]+(tau_u[2] * z_expo[2,]))';
  lapse = (gm[3]+(tau_u[3] * z_expo[3,]))';
  
  intercept = (gm[4]+(tau_u[4] * z_expo[3,]))';
  beta_rt = (gm[5]+(tau_u[5] * z_expo[4,]))';
  sigma = (gm[6]+(tau_u[6] * z_expo[5,]))';
  ndt = (gm[7]+(tau_u[7] * z_expo[6,]))';
  
  
  
  // alpha_dif = (gm_dif[1]+(tau_u_dif[1] * z_expo_dif[1,]))';
  beta_dif = (gm_dif[2]+(tau_u_dif[2] * z_expo_dif[2,]))';
  lapse_dif = (gm_dif[3]+(tau_u_dif[3] * z_expo_dif[3,]))';
  
  intercept_dif = (gm_dif[4]+(tau_u_dif[4] * z_expo_dif[3,]))';
  beta_rt_dif = (gm_dif[5]+(tau_u_dif[5] * z_expo_dif[4,]))';
  
  sigma_dif = (gm_dif[6]+(tau_u_dif[6] * z_expo_dif[5,]))';
  
  
  ndt_dif = (gm_dif[7]+(tau_u_dif[7] * z_expo[6,]))';
  


  }
  
  



model{
  
  vector[N] X_normal;
  vector[N] mu;
  vector[N] mu_rt;
  
  //priors
  target += normal_lpdf(gm[1] | 0,50);
  target += normal_lpdf(gm[2] |  0,3);
  target += normal_lpdf(gm[3] | -4, 2);
  
  target += normal_lpdf(gm[4] | 0, 10);
  //beta
  target += normal_lpdf(gm[5] | 0, 5);
  //sigma
  target += normal_lpdf(gm[6] | 0, 5);
  //ndt
  target += normal_lpdf(gm[7] | -2, 5);
  // 
  target += normal_lpdf(gm_dif[1] | 0, 10);
  target += normal_lpdf(gm_dif[2] | 0, 2);
  target += normal_lpdf(gm_dif[3] | 0, 1);
  target += normal_lpdf(gm_dif[4] | 0, 2);
  target += normal_lpdf(gm_dif[5] | 0, 1);
  target += normal_lpdf(gm_dif[6] | 0, 2);
  target += normal_lpdf(gm_dif[7] | 0, 1);
  // // target += normal_lpdf(gm[4] | -3, 2);
  target += std_normal_lpdf(to_vector(z_expo));
  target += std_normal_lpdf(to_vector(z_expo_dif));

  target += normal_lpdf(tau_u[1] | 0, 20)-normal_lccdf(0 | 0, 20);
  target += normal_lpdf(tau_u[2] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[3] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[4] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[5] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[6] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u[7] | 0, 5)-normal_lccdf(0 | 0, 5);
  
  
  target += normal_lpdf(tau_u_dif[1] | 0, 5)-normal_lccdf(0 | 0, 5);
  target += normal_lpdf(tau_u_dif[2] | 0, 1)-normal_lccdf(0 | 0, 1);
  target += normal_lpdf(tau_u_dif[3] | 0, 1)-normal_lccdf(0 | 0, 1);
  target += normal_lpdf(tau_u_dif[4] | 0, 1)-normal_lccdf(0 | 0, 1);
  target += normal_lpdf(tau_u_dif[5] | 0, 1)-normal_lccdf(0 | 0, 1);
  target += normal_lpdf(tau_u_dif[6] | 0, 1)-normal_lccdf(0 | 0, 1);
  target += normal_lpdf(tau_u_dif[7] | 0, 1)-normal_lccdf(0 | 0, 1);
  
  
  // 
  target += normal_lpdf(alpha | gm[1], tau_u[1]);
  target += normal_lpdf(alpha_dif | gm_dif[1], tau_u_dif[1]);


    for (n in 1:N){
    
      X_normal[n] = 0.5+0.5*erf((X[n,2]-(alpha[S_id[n]] + X[n,3]*alpha_dif[S_id[n]]))/(exp((beta[S_id[n]]+ X[n,3]*beta_dif[S_id[n]]))*sqrt(2)));
      
      mu[n] = (inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2) + (1 - 2 * (inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2)) * X_normal[n];
      
      mu_rt[n] = (intercept[S_id[n]] + X[n,3]*intercept_dif[S_id[n]]) + (beta_rt[S_id[n]]+ X[n,3]*beta_rt_dif[S_id[n]])*(mu[n]*(1-mu[n]));
  
      target += bernoulli_lpmf(Y[n] | mu[n]);
      target += lognormal_lpdf(RT[n] - ((inv_logit((ndt[S_id[n]] + ndt_dif[S_id[n]]))).* min_RT[S_id[n]]) | mu_rt[n], exp((sigma[S_id[n]]+ sigma_dif[S_id[n]])));

    }


}


generated quantities{


  vector[N] log_lik;

  for (n in 1:N){
    log_lik[n] = bernoulli_lpmf(Y[n] |((inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2) + (1 - 2 * (inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2)) * (0.5+0.5*erf((X[n,2]-(alpha[S_id[n]] + X[n,3]*alpha_dif[S_id[n]]))/(exp((beta[S_id[n]]+ X[n,3]*beta_dif[S_id[n]]))*sqrt(2)))))) +
                 lognormal_lpdf(RT[n] - ((inv_logit((ndt[S_id[n]] + ndt_dif[S_id[n]]))).* min_RT[S_id[n]]) | ((intercept[S_id[n]] + X[n,3]*intercept_dif[S_id[n]]) + (beta_rt[S_id[n]]+ X[n,3]*beta_rt_dif[S_id[n]])*(((inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2) + (1 - 2 * (inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2)) * (0.5+0.5*erf((X[n,2]-(alpha[S_id[n]] + X[n,3]*alpha_dif[S_id[n]]))/(exp((beta[S_id[n]]+ X[n,3]*beta_dif[S_id[n]]))*sqrt(2)))))*(1-((inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2) + (1 - 2 * (inv_logit((lapse[S_id[n]]+ X[n,3]*lapse_dif[S_id[n]])) / 2)) * (0.5+0.5*erf((X[n,2]-(alpha[S_id[n]] + X[n,3]*alpha_dif[S_id[n]]))/(exp((beta[S_id[n]]+ X[n,3]*beta_dif[S_id[n]]))*sqrt(2)))))))), exp((sigma[S_id[n]]+ sigma_dif[S_id[n]])));

  }
  
}
