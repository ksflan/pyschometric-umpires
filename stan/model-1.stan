data {
  int<lower=1> N; // num obs
  int<lower=1> U; // num umpires
  int<lower=1,upper=U> umpire_index[N];
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
}
parameters {
  real mu_alpha;
  real sigma_alpha;
  
  real mu_beta;
  real sigma_beta;
  
  real alpha[U];
  real beta[U];
  
  real x0;
  real y0;
}
model {
  real theta[N];
  
  mu_beta ~ normal(0, 10);
  mu_alpha ~ normal(0, 1);
  
  x0 ~ normal(0, 1);
  y0 ~ normal(2.5, 1);
  
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0) ^ 2.0 + fabs(y[n] - y0) ^ 2.0) ^ (1.0 / 2) - alpha[umpire_index[n]]);
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }
