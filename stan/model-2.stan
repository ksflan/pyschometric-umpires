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
  
  
  
  // Strike zone center parameters
  
  real mu_x0;
  real<lower=0> sigma_x0;
  real mu_y0;
  real<lower=0> sigma_y0;
  
  real x0_tilde[U];
  real y0_tilde[U];
}
transformed parameters {
  real alpha[U];
  real beta[U];
  
  real x0[U];
  real y0[U];
  
  for (u in 1:U) {
    alpha[u] = mu_alpha + sigma_alpha * alpha_tilde[u];
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
    
    // Strike zone center parameters
    x0[u] = mu_x0 + sigma_x0 * x0_tilde[u];
    y0[u] = mu_y0 + sigma_y0 * y0_tilde[u];
  }
  
}
model {
  real theta[N];
  
  mu_beta ~ normal(0, 10);
  mu_alpha ~ normal(0, 1);
  
  mu_x0 ~ normal(0, 1);
  mu_y0 ~ normal(2.5, 1);
  
  x0_tilde ~ normal(0, 1);
  y0_tilde ~ normal(0, 1);
  
  alpha_tilde ~ normal(0, 1);
  beta_tilde ~ normal(0, 1);
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0[umpire_index[n]]) ^ 2.0 + fabs(y[n] - y0[umpire_index[n]]) ^ 2.0) ^ (1.0 / 2) - alpha[umpire_index[n]]);
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }
