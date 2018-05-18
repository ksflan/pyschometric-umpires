// Model 7 plus different x0 parameters for batter-handedness

// Parameters:
// hierarchical alpha (with handedness)
// hierarchical beta
// hierarchical Minkowski
// hierarchical scale (with handedness)
// hierarchical x0 (with handedness)
// hierarchical y0


// Model 6 plus hierarchical strike zone center parameters



data {
  int<lower=1> N; // num obs
  int<lower=1> predict_N; // number of points to predict
  int<lower=1> U; // num umpires
  int<lower=1,upper=U> umpire_index[N];
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int<lower=1,upper=4> batter_stance[N]; // 1 for right-handed, 2 for left-handed
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
  
  real predict_x[predict_N];
  real predict_y[predict_N];
  int<lower=1,upper=4> predict_platoon[predict_N];
}
parameters {
  
  // strike zone dimension parameters
  
  vector[4] mu_alpha;
  vector<lower=0>[4] sigma_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  vector[4] alpha_tilde[U];
  real beta_tilde[U];
  
  // minkowski parameters
  
  real mu_r;
  real<lower=0> sigma_r;
  real r_tilde[U];
  
  // strike zone center parameters
  
  vector[4] mu_scale;
  vector<lower=0>[4] sigma_scale;
  
  vector[4] scale_tilde[U];
  
  vector[4] mu_x0; // 2, for the number of batter handednesses (R and L) // or 4 for the number of distinct platoons
  vector<lower=0>[4] sigma_x0; // 2, for the number of batter handednesses (R and L)
  real mu_y0;
  real<lower=0> sigma_y0;
  
  vector[4] x0_tilde[U]; // 2, for the number of batter handednesses (R and L)
  real y0_tilde[U];
}
transformed parameters {
  vector[4] alpha[U];
  real beta[U];
  
  real r_exp[U];
  
  vector[4] x0[U]; // 2, for the number of batter handednesses (R and L)
  real y0[U];
  
  vector<lower=0>[4] scale_exp[U];
  
  // scale_exp = exp(scale);
  
  for (u in 1:U) {
    alpha[u] = mu_alpha + to_row_vector(sigma_alpha) * alpha_tilde[u];
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
    r_exp[u] = exp(mu_r + sigma_r * r_tilde[u]);
    x0[u] = mu_x0 + to_row_vector(sigma_x0) * x0_tilde[u];
    y0[u] = mu_y0 + sigma_y0 * y0_tilde[u];
    for(i in 1:4)
      scale_exp[u][i] = exp(mu_scale[i] + sigma_scale[i] * scale_tilde[u][i]); // cannot use vector multiplication because of the exp() call
  }
}
model {
  real theta[N];
  
  // sigma_beta ~ normal(2,0.0001);
  // sigma_alpha ~ normal(2,0.0001);
  // sigma_scale ~ normal(2,0.0001);
  // sigma_r ~ normal(2,0.0001);
  
  mu_beta ~ normal(0,10);
  mu_alpha ~ normal(0,1);
  
  mu_x0 ~ normal(0,1);
  mu_y0 ~ normal(2.5,1);
  
  mu_r ~ normal(0,10);
  
  mu_scale ~ normal(1,1);
  for(u in 1:U) // does it matter whether this is a univariate normal or multivariate, or if the sampling statement is separate for each platoon parameter?
    scale_tilde[u] ~ normal(0,1);
  
  for(u in 1:U)
    x0_tilde[u] ~ normal(0,1);
  y0_tilde ~ normal(0,1);
  
  r_tilde ~ normal(0,1);
  
  for(u in 1:U)
    alpha_tilde[u] ~ normal(0,1);
  beta_tilde ~ normal(0,1);
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0[umpire_index[n],batter_stance[n]]) ^ r_exp[umpire_index[n]] + (fabs(y[n] - y0[umpire_index[n]]) / scale_exp[umpire_index[n],batter_stance[n]]) ^ r_exp[umpire_index[n]]) ^ (1.0 / r_exp[umpire_index[n]]) - alpha[umpire_index[n],batter_stance[n]]);
  
  call ~ bernoulli_logit(theta);
}
generated quantities {
  real predict_theta[predict_N];
  
  for(n in 1:predict_N) {
    predict_theta[n] = inv_logit(
      mu_beta * ((fabs(predict_x[n] - mu_x0[predict_platoon[n]]) ^ exp(mu_r) + (fabs(predict_y[n] - mu_y0) / exp(mu_scale[predict_platoon[n]])) ^ exp(mu_r)) ^ (1.0 / exp(mu_r)) - mu_alpha[predict_platoon[n]])
    );
  }
}






