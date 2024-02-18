data{
  //Constants
  int<lower=1> T; //n Trials
  int<lower=1> S; //n participants
  array[T] int S_id; //n participants
  
  int<lower=1> N_alpha;
  int<lower=1> N_beta;
  int<lower=1> N_lapse;
  

  matrix[N_alpha,T] X_alpha;
  matrix[N_beta,T] X_beta;
  matrix[N_lapse,T] X_lapse;
  
  
  array[T] int Y;
  vector[T] X;
  

}
transformed data{
  int<lower=1> N=N_alpha+N_beta+N_lapse; //n free parameters

}
parameters{
  // Group means 
  vector[N] gm;
  // Between participant scales
  vector<lower = 0>[N]  tau_u;
  // Between participant cholesky decomposition
  cholesky_factor_corr[N] L_u;
  // Participant deviation 
  matrix[N, S] z_expo;  

}
transformed parameters{


  ///Recomposition

  vector[T] alpha;
  vector[T] beta;
  vector[T] lapse;
  
  matrix[S, N] indi_dif = (diag_pre_multiply(tau_u, L_u) * z_expo)';
  
  matrix[S, N] param;
  
  for(n in 1:N){
    param[,n]= gm[n] + indi_dif[,n];
  }
  

  matrix[S,N_alpha] alpha_p = param[,1:N_alpha];
  
  matrix[S,N_beta] beta_p = param[,(N_alpha+1):(N_alpha+N_beta)];
  
  matrix[S,N_lapse] lapse_p = param[,(N_alpha+N_beta+1):N];
  
  
  
  for(n in 1:T){

    alpha[n] = dot_product(X_alpha[,n], alpha_p[S_id[n],]);
    
    beta[n] = exp(dot_product(X_beta[,n], beta_p[S_id[n],]));
    
    lapse[n] = inv_logit(dot_product(X_lapse[,n], lapse_p[S_id[n],])) / 2;
    
    }
  


}
model{


  
 

  target += normal_lpdf(gm[1:N_alpha] | 0, 20); //global mean of alpha
  target += normal_lpdf(gm[(N_alpha+1):(N_alpha+N_beta)] | 0, 3); //global mean of beta
  target += normal_lpdf(gm[(N_alpha+N_beta+1):N] | -5.5,1.5); //global mean of lapse


  target += std_normal_lpdf(to_vector(z_expo));
  target += normal_lpdf(tau_u | 0, 3)-normal_lccdf(0 | 0, 3);
  target += lkj_corr_cholesky_lpdf(L_u | 2);



  //Mapping on observed responses
   target += bernoulli_lpmf(Y | lapse + (1 - 2 * lapse) .* ((0.5+0.5 * erf((X-alpha) ./ (beta * sqrt(2))))));
}
