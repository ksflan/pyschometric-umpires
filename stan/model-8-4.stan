// experimental changes to model 8.2

// Added multivariate prior to alpha and lambda







// Parameters:
// hierarchical alpha (with handedness and count)
// hierarchical beta
// hierarchical Minkowski
// hierarchical scale (with handedness and count)
// hierarchical x0 (with handedness and count)
// hierarchical y0


functions {
  
  // This function scales the y values and then calculates the Minkowski distance.
  real minkowski_distance(real x0, real y0, real x, real y, real lambda, real r) {
    real d;
    
    d = (fabs(x - x0) ^ r + (fabs(y - y0) / (lambda)) ^ r) ^ (1.0 / r);
    return d;
  }
  
  // This function takes the distance, the intercept, and the discriminability and plugs them into the psychometric function.
  real psychometric_function(real beta, real alpha, real d) {
    return beta * (d - alpha);
  }
}
data {
  int<lower=1> N; // num obs
  int<lower=1> predict_N; // number of points to predict
  int<lower=1> U; // num umpires
  int<lower=1,upper=U> umpire_index[N];
  int K; // number of covariate parameters
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int<lower=1,upper=4> batter_stance[N]; // for each platoon
  int<lower=1,upper=12> count[N]; // ball/strike count
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
  matrix[N,K] model_matrix;
  
  // real predict_x[predict_N];
  // real predict_y[predict_N];
  // int<lower=1,upper=4> predict_platoon[predict_N];
}
parameters {
  
  // strike zone dimension parameters
  
  // vector[4] mu_alpha_platoon;
  // vector<lower=0>[4] sigma_alpha_platoon;
  
  // vector[12] mu_alpha_count;
  // vector<lower=0>[12] sigma_alpha_count;
  vector[K] mu_alpha;
  row_vector<lower=0>[K] sigma_alpha;
  // corr_matrix[K] omega_alpha;
  // vector<lower=0>[K] tau_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  // vector[4] alpha_platoon_tilde[U];
  // vector[12] alpha_count_tilde[U];
  // vector[K] alpha_tilde[U];
  real beta_tilde[U];
  
  // minkowski parameters
  
  real mu_r;
  real<lower=0> sigma_r;
  real r_tilde[U];
  
  // strike zone center parameters
  
  // vector[4] mu_lambda_platoon;
  // vector<lower=0>[4] sigma_lambda_platoon;
  
  // vector[12] mu_lambda_count;
  // vector<lower=0>[12] sigma_lambda_count;
  vector[K] mu_lambda;
  row_vector<lower=0>[K] sigma_lambda;
  // corr_matrix[K] omega_lambda;
  // vector<lower=0>[K] tau_lambda;
  
  // vector[4] lambda_platoon_tilde[U];
  // vector[12] lambda_count_tilde[U];
  vector[K] lambda_tilde[U];
  
  vector[4] mu_x0; // 2, for the number of batter handednesses (R and L) // or 4 for the number of distinct platoons
  row_vector<lower=0>[4] sigma_x0; // 2, for the number of batter handednesses (R and L)
  real mu_y0;
  real<lower=0> sigma_y0;
  
  vector[4] x0_tilde[U]; // 2, for the number of batter handednesses (R and L)
  real y0_tilde[U];
}
transformed parameters {
  // vector[4] alpha_platoon[U];
  // vector[12] alpha_count[U];
  vector[K] alpha[U];
  
  real beta[U];
  
  real<lower=0> r_exp[U];
  
  vector[4] x0[U]; // xxxxxxxxx4, for the number of platoons
  real y0[U];
  
  // vector<lower=0>[4] lambda_platoon_exp[U];
  // vector<lower=0>[12] lambda_count_exp[U];
  // vector[4] lambda_platoon_exp[U];
  // vector[12] lambda_count_exp[U];
  vector[K] lambda_exp[U];
  
  real theta[N];
  
  real alpha_star[N];
  real lambda_star[N];
  real d[N]; // distance calculated from minkowski_distance
  
  // scale_exp = exp(scale);
  
  
  // alpha = mu_alpha + to_row_vector(sigma_alpha) * alpha_tilde[u];
  
  for (u in 1:U) {
    // alpha_platoon[u] = mu_alpha_platoon + to_row_vector(sigma_alpha_platoon) * alpha_platoon_tilde[u];
    // alpha_count[u] = mu_alpha_count + to_row_vector(sigma_alpha_count) * alpha_count_tilde[u];
    // alpha[u] = mu_alpha + sigma_alpha * alpha_tilde[u];
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
    r_exp[u] = mu_r + sigma_r * r_tilde[u];
    x0[u] = mu_x0 + sigma_x0 * x0_tilde[u];
    y0[u] = mu_y0 + sigma_y0 * y0_tilde[u];
    lambda_exp[u] = mu_lambda + sigma_lambda * lambda_tilde[u];
    // for(i in 1:4)
    //   lambda_platoon_exp[u][i] = mu_lambda_platoon[i] + sigma_lambda_platoon[i] * lambda_platoon_tilde[u][i]; // cannot use vector multiplication because of the exp() call
    // for(i in 1:12)
    //   lambda_count_exp[u][i] = mu_lambda_count[i] + sigma_lambda_count[i] * lambda_count_tilde[u][i]; // cannot use vector multiplication because of the exp() call
    // for(i in 1:K)
    //   lambda_exp[u][i] = exp(mu_lambda[i] + sigma_lambda[i] * lambda_tilde[u][i]);
  }
  
  
  for(n in 1:N) { // possibly move the exp() call to here
    alpha_star[n] = model_matrix[n] * alpha[umpire_index[n]];
    lambda_star[n] = model_matrix[n] * exp(lambda_exp[umpire_index[n]]);
    
    d[n] = minkowski_distance(x0[umpire_index[n],batter_stance[n]], y0[umpire_index[n]], x[n], y[n], lambda_star[n], exp(r_exp[umpire_index[n]]));
    
    // theta[n] = beta[umpire_index[n]] * ((fabs(x[n] - x0[umpire_index[n],batter_stance[n]]) ^ r_exp[umpire_index[n]] + (fabs(y[n] - y0[umpire_index[n]]) / (lambda_star[n])) ^ r_exp[umpire_index[n]]) ^ (1.0 / r_exp[umpire_index[n]]) - (alpha_star[n]));
  }
  
  theta = psychometric_function(beta[umpire_index], alpha_star, d);
}
model {
  
  
  // sigma_beta ~ normal(2,0.0001);
  // sigma_alpha ~ normal(2,0.0001);
  // sigma_scale ~ normal(2,0.0001);
  // sigma_r ~ normal(2,0.0001);
  
  sigma_alpha ~ cauchy(0,5);
  sigma_lambda ~ cauchy(0,5);
  // tau_alpha ~ cauchy(0, 2.5);
  // omega_alpha ~ lkj_corr(2);
  
  // alpha ~ multi_normal(mu_x0, quad_form_diag(omega, tau))
  
  sigma_beta ~ cauchy(0,5);
  sigma_r ~ cauchy(0,5);
  
  mu_beta ~ normal(0,10);
  // mu_alpha_platoon ~ normal(0,1);
  // mu_alpha_count ~ normal(0,1);
  mu_alpha ~ normal(0,3);
  mu_alpha[1] ~ normal(17.0 / 24.0,0.5);
  
  mu_lambda ~ normal(0,3);
  mu_lambda[1] ~ normal(1,0.5);
  
  mu_x0 ~ normal(0,1);
  mu_y0 ~ normal(2.5,1);
  
  alpha ~ normal(mu_alpha, sigma_alpha);
  for(u in 1:U) {
    // alpha_tilde[u] ~ normal(0,1);
    lambda_tilde[u] ~ normal(0,1);
  }
  
  mu_r ~ normal(0,5);
  
  // mu_lambda_count ~ normal(0,10);
  // mu_lambda_platoon ~ normal(0,10);
  // for(u in 1:U) { // does it matter whether this is a univariate normal or multivariate, or if the sampling statement is separate for each platoon parameter?
  //   lambda_platoon_tilde[u] ~ normal(0,1);
  //   lambda_count_tilde[u] ~ normal(0,1);
  // }
  
  for(u in 1:U)
    x0_tilde[u] ~ normal(0,1);
  y0_tilde ~ normal(0,1);
  
  r_tilde ~ normal(0,1);
  
  // for(u in 1:U) {
  //   alpha_count_tilde[u] ~ normal(0,1);
  //   alpha_platoon_tilde[u] ~ normal(0,1);
  // }
  
  beta_tilde ~ normal(0,1);
  
  
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   real predict_theta[predict_N];
//   
//   for(n in 1:predict_N) {
//     predict_theta[n] = inv_logit(
//       mu_beta * ((fabs(predict_x[n] - mu_x0[predict_platoon[n]]) ^ exp(mu_r) + (fabs(predict_y[n] - mu_y0) / exp(mu_scale[predict_platoon[n]])) ^ exp(mu_r)) ^ (1.0 / exp(mu_r)) - mu_alpha[predict_platoon[n]])
//     );
//   }
// }

