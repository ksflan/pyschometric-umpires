// Model 1 plus fitted r

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
  real<lower=0> sigma_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  real alpha_tilde[U];
  real beta_tilde[U];
  
  real r;
  
  real x0;
  real y0;
}
transformed parameters {
  real alpha[U];
  real beta[U];
  
  real r_exp;
  
  r_exp = exp(r);
  
  for (u in 1:U) {
    alpha[u] = mu_alpha + sigma_alpha * alpha_tilde[u];
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
  }
}
model {
  real theta[N];
  
  mu_beta ~ normal(0, 10);
  mu_alpha ~ normal(0, 1);
  
  x0 ~ normal(0, 1);
  y0 ~ normal(2.5, 1);
  
  r ~ normal(0,5);
  
  alpha_tilde ~ normal(0, 1);
  beta_tilde ~ normal(0, 1);
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0) ^ r_exp + fabs(y[n] - y0) ^ r_exp) ^ (1.0 / r_exp) - alpha[umpire_index[n]]);
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }
