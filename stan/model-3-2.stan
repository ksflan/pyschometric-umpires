// Model 3 with covariates of count and platoon for alpha parameters


data {
  int<lower=1> N; // num obs
  int<lower=1> U; // num umpires
  int K; // number of covariate parameters
  int<lower=1,upper=U> umpire_index[N];
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  matrix[N,K] model_matrix;
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
}
parameters {
  vector[K] mu_alpha;
  vector<lower=0>[K] sigma_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  vector[K] alpha_tilde[U];
  real beta_tilde[U];
  
  real r;
  
  real x0;
  real y0;
}
transformed parameters {
  vector[K] alpha[U];
  real beta[U];
  
  real r_exp;
  
  r_exp = exp(r);
  
  for (u in 1:U) {
    alpha[u] = mu_alpha + to_row_vector(sigma_alpha) * alpha_tilde[u];
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
  }
}
model {
  real theta[N];
  
  mu_beta ~ normal(0,10);
  mu_alpha ~ normal(0,1);
  mu_alpha[1] ~ normal(17.0 / 24.0,0.5);
  
  x0 ~ normal(0,1);
  y0 ~ normal(2.5,1);
  
  r ~ normal(0,5);
  
  for(u in 1:U) {
    alpha_tilde[u] ~ normal(0,1);
    beta_tilde[u] ~ normal(0,1);
  }
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0) ^ r_exp + fabs(y[n] - y0) ^ r_exp) ^ (1.0 / r_exp) - (to_row_vector(alpha[umpire_index[n]]) * to_vector(model_matrix[n])));
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }
