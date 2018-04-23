// Model 7 plus different x0 parameters for batter-handedness

// Parameters:
// hierarchical alpha
// hierarchical beta
// hierarchical Minkowski
// hierarchical scale
// hierarchical x0 (with handedness)
// hierarchical y0


// Model 6 plus hierarchical strike zone center parameters



data {
  int<lower=1> N; // num obs
  int<lower=1> U; // num umpires
  int<lower=1,upper=U> umpire_index[N];
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int<lower=1,upper=2> batter_stance[N]; // 1 for right-handed, 2 for left-handed
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
}
parameters {
  
  // strike zone dimension parameters
  
  real mu_alpha;
  real<lower=0> sigma_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  real alpha_tilde[U];
  real beta_tilde[U];
  
  // minkowski parameters
  
  real mu_r;
  real<lower=0> sigma_r;
  real r_tilde[U];
  
  // strike zone center parameters
  
  real mu_scale;
  real<lower=0> sigma_scale;
  
  real scale_tilde[U];
  
  vector[2] mu_x0; // 2, for the number of batter handednesses (R and L)
  vector<lower=0>[2] sigma_x0; // 2, for the number of batter handednesses (R and L)
  real mu_y0;
  real<lower=0> sigma_y0;
  
  vector[2] x0_tilde[U]; // 2, for the number of batter handednesses (R and L)
  real y0_tilde[U];
}
transformed parameters {
  real alpha[U];
  real beta[U];
  
  real r_exp[U];
  
  vector[2] x0[U]; // 2, for the number of batter handednesses (R and L)
  real y0[U];
  
  real<lower=0> scale_exp[U];
  
  // scale_exp = exp(scale);
  
  for (u in 1:U) {
    alpha[u] = mu_alpha + sigma_alpha * alpha_tilde[u];
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
    r_exp[u] = exp(mu_r + sigma_r * r_tilde[u]);
    x0[u] = mu_x0 + sigma_x0 * x0_tilde[u];
    y0[u] = mu_y0 + sigma_y0 * y0_tilde[u];
    scale_exp[u] = exp(mu_scale + sigma_scale * scale_tilde[u]);
  }
}
model {
  real theta[N];
  
  mu_beta ~ normal(0,10);
  mu_alpha ~ normal(0,1);
  
  mu_x0 ~ normal(0,1);
  mu_y0 ~ normal(2.5,1);
  
  mu_r ~ normal(0,10);
  
  mu_scale ~ normal(1,1);
  scale_tilde ~ normal(0,1);
  
  x0_tilde ~ normal(0,1);
  y0_tilde ~ normal(0,1);
  
  r_tilde ~ normal(0,1);
  
  alpha_tilde ~ normal(0,1);
  beta_tilde ~ normal(0,1);
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0[umpire_index[n],batter_stance[n]]) ^ r_exp[umpire_index[n]] + (fabs(y[n] - y0[umpire_index[n]]) / scale_exp[umpire_index[n]]) ^ r_exp[umpire_index[n]]) ^ (1.0 / r_exp[umpire_index[n]]) - alpha[umpire_index[n]]);
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }






