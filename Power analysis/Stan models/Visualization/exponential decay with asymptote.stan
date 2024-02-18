

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] x;
  array[N] int y;
  // 
  vector[N] Subs;
  vector[N] Trials;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {


  real <lower = 0> expo_alpha;
  real <lower = 0> expo_beta;

  real <lower = 0> int_alpha;
  real <lower = 0> int_beta;

}


transformed parameters {
  
  vector[N] X_norm;
  vector[N] alpha;
  
  vector<lower=0>[N] beta;

  real <lower = 0> asym_alpha = 0;
  real <lower = 0> asym_beta = 0;

  alpha =  int_alpha * exp(-expo_alpha * Subs) + asym_alpha;

  beta =  int_beta * exp(-expo_beta * Subs) + asym_beta;
  

  X_norm = 0.5+0.5 * erf((x-alpha) ./ (beta * sqrt(2)));
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  target += normal_lpdf(expo_alpha | 0, 5);
  target += normal_lpdf(expo_beta | 0, 5);
  
  target += normal_lpdf(int_alpha | 0, 5);
  target += normal_lpdf(int_beta | 0, 5);
  
  target += normal_lpdf(asym_alpha | 0, 5);
  target += normal_lpdf(asym_beta | 0, 5);
  

   y ~ bernoulli(X_norm);
}

