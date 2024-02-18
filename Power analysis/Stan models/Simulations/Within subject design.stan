data{
  //Constants
  int<lower=0> T; //n Trials per session
  int<lower=1> S; //n sessions
  int<lower=1> P; //n participants

  matrix[S*P,T] X;
  array[T*S*P] int Y;
  
  
  vector[S*P] condition1;

  //array[T*S*P] int condition1;
  //array[T*S*P] int condition2;
  

  // array[T*S*P] int<lower=1,upper=P*S> P_id; //participant (on a trial level)


}
transformed data{
  int<lower=1> N=6; //n free parameters

}
parameters{
  // Group means 
  vector[N] mu_g;
  // Between participant scales
  vector<lower = 0>[N]  tau_b;
  // Between participant cholesky decomposition
  cholesky_factor_corr[N] L_b;
  // Participant deviation 
  matrix[N, P] z_p;  

}
transformed parameters{
  ///Recomposition
  matrix[N, P] delta_mu_p;  
  matrix[N, P] mu_p;  
  matrix[N, S*P] mu_p_rep;  
  

  vector[S*P] psyalpha;
  vector[S*P] psybeta;
  vector[S*P] psylapse;

  vector[S*P] psyalpha_dif1;
  vector[S*P] psybeta_dif1;
  vector[S*P] psylapse_dif1;
  
  
  vector[S*P] alphas;
  vector<lower=0>[S*P] betas;
  vector<lower=0, upper = 0.5>[S*P] lapses;
  
  
  ///Model
  //Prediction  
  matrix[S*P,T] mu;  
  
  /// Reformatted values for sampling
  vector[S*P*T] mu_prob;  
  
  ///Recomposition
  delta_mu_p = diag_pre_multiply(tau_b, L_b) * z_p;
  for(idx in 1:P){
    mu_p[,idx] = mu_g + delta_mu_p[,idx];
  }
  
  mu_p_rep=append_col(mu_p,mu_p);
  
    ///within subject deviations
//  fp_s = mu_p_rep + diag_pre_multiply(tau_w, L_w) * z_s;
  

  psyalpha = to_vector(mu_p_rep[1,]);
  psybeta = to_vector(mu_p_rep[2,]);
  psylapse = to_vector(mu_p_rep[3,]);

  psyalpha_dif1 = to_vector(mu_p_rep[4,]);
  psybeta_dif1 = to_vector(mu_p_rep[5,]);
  psylapse_dif1 = to_vector(mu_p_rep[6,]);
  


  alphas = psyalpha+condition1 .* psyalpha_dif1;
  betas = exp(psybeta+condition1 .* psybeta_dif1);
  lapses = inv_logit(psylapse+condition1 .* psylapse_dif1) / 2;
  
  
  //Trial loop
  for(idx in 1:T){
   
      mu[,idx] = lapses + (1 - 2 * lapses) .* ((0.5+0.5 * erf((X[,idx]-alphas) ./ (betas * sqrt(2)))));
      
  }
  mu_prob = to_vector(mu');
 
}
model{

  mu_g[1] ~ normal(0,20);
  mu_g[2] ~ normal(0,3);
  mu_g[3] ~ normal(-3,2);
  mu_g[4] ~ normal(0,5);
  mu_g[5] ~ normal(0,5);
  mu_g[6] ~ normal(0,5);
 
  
  tau_b[1] ~ normal(0,10);
  tau_b[2] ~ normal(0,3);
  tau_b[3] ~ normal(0,3);
  
  //differences
  tau_b[4] ~ normal(0,3); 
  tau_b[5] ~ normal(0,3); 
  tau_b[6] ~ normal(0,3); 
  
  //Between participant cholesky decomposition
  L_b ~ lkj_corr_cholesky(2);
  
  // Participant deviation 
  to_vector(z_p) ~ std_normal();  
  
 


  //Mapping on observed responses
   target += bernoulli_lpmf(Y | mu_prob);
}
