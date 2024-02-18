

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] x;
  array[N] int y;
  int<lower=0> nT;
  int<lower=0> nS;
  matrix[N,nT*nS] TnS;
  // 
  // vector[nS] Subs;
  // vector[nT] Trials;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[nT*nS] alphas;
  vector[nT*nS] betas;
  
  // real <lower = 0> expo_alpha;
  // real <lower = 0> expo_beta;
  // 
  // real <lower = 0> int_alpha;
  // real <lower = 0> int_beta;

}


transformed parameters {
  
  vector[N] X_norm;
  vector[N] alpha;
  //vector[N] alpha1;
  
  vector<lower=0>[N] beta;

  alpha = TnS * alphas;
  //alpha1 = TnS * alphas;
  
  beta = exp(TnS * betas);
  

  X_norm = 0.5+0.5 * erf((x-alpha) ./ (beta * sqrt(2)));
  
  //alpha1 = int_alpha * exp(-expo_alpha * Subs);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += normal_lpdf(alphas | 0, 10);
  target += normal_lpdf(betas | 0, 2);
  
   y ~ bernoulli(X_norm);
}

