// 

// Model 8.4 but non-hierarchical effects on count and platoon




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
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  real beta_tilde[U];
  
  real mu_alpha;
  real<lower=0> sigma_alpha;
  vector[K] alpha;
  real alpha_tilde[U];
  
  real mu_lambda;
  real<lower=0> sigma_lambda;
  vector[K] lambda_exp;
  real lambda_tilde[U];
  
  // minkowski parameters
  
  real mu_r;
  real<lower=0> sigma_r;
  real r_tilde[U];
  
  vector[4] mu_x0; // 2, for the number of batter handednesses (R and L) // or 4 for the number of distinct platoons
  row_vector<lower=0>[4] sigma_x0; // 2, for the number of batter handednesses (R and L)
  real mu_y0;
  real<lower=0> sigma_y0;
  
  vector[4] x0_tilde[U]; // 2, for the number of batter handednesses (R and L)
  real y0_tilde[U];
}
transformed parameters {
  real alpha_umpire[U];
  real lambda_umpire[U];
  
  real beta[U];
  
  real r_exp[U];
  
  vector[4] x0[U];
  real y0[U];
  
  real theta[N];
  
  real alpha_star[N];
  real lambda_star[N];
  real d[N]; // distance calculated from minkowski_distance
  

  for (u in 1:U) {
    alpha_umpire[u] = mu_alpha + sigma_alpha * alpha_tilde[u];
    lambda_umpire[u] = mu_lambda + sigma_lambda * lambda_tilde[u];
    
    beta[u] = mu_beta + sigma_beta * beta_tilde[u];
    r_exp[u] = mu_r + sigma_r * r_tilde[u];
    x0[u] = mu_x0 + sigma_x0 * x0_tilde[u];
    y0[u] = mu_y0 + sigma_y0 * y0_tilde[u];
  }
  
  
  for(n in 1:N) { // possibly move the exp() call to here
    alpha_star[n] = model_matrix[n] * alpha + alpha_umpire[umpire_index[n]];
    lambda_star[n] = exp(model_matrix[n] * lambda_exp + lambda_umpire[umpire_index[n]]);
    
    d[n] = minkowski_distance(x0[umpire_index[n],batter_stance[n]], y0[umpire_index[n]], x[n], y[n], lambda_star[n], exp(r_exp[umpire_index[n]]));
    
    theta[n] = psychometric_function(beta[umpire_index[n]], alpha_star[n], d[n]);
  }
  
}
model {
  
  sigma_alpha ~ cauchy(0,3);
  sigma_lambda ~ cauchy(0,3);

  sigma_beta ~ cauchy(0,3);
  sigma_r ~ cauchy(0,3);
  
  sigma_y0 ~ cauchy(0,3);
  sigma_x0 ~ cauchy(0,3);
  
  mu_beta ~ normal(-5,5);
  
  mu_alpha ~ normal(0,1);
  mu_lambda ~ normal(0,1);
  
  alpha ~ normal(0,1);
  alpha[1] ~ normal(1,1);
  lambda_exp ~ normal(0,1);
  
  mu_x0 ~ normal(0,1);
  mu_y0 ~ normal(2.5,1);
  
  for(u in 1:U) {
    alpha_tilde[u] ~ normal(0,1);
    lambda_tilde[u] ~ normal(0,1);
  }
  
  mu_r ~ normal(1.5,0.5);
  
  for(u in 1:U)
    x0_tilde[u] ~ normal(0,1);
  y0_tilde ~ normal(0,1);
  
  r_tilde ~ normal(0,1);

  
  beta_tilde ~ normal(0,1);
  
  
  
  call ~ bernoulli_logit(theta);
}
generated quantities {
  real height[U];
  
  for(u in 1:U)
    height[u] = (alpha[1] + alpha_umpire[u]) / exp(lambda_exp[1] + lambda_umpire[u]);
  
//   real predict_theta[predict_N];
//   
//   for(n in 1:predict_N) {
//     predict_theta[n] = inv_logit(
//       mu_beta * ((fabs(predict_x[n] - mu_x0[predict_platoon[n]]) ^ exp(mu_r) + (fabs(predict_y[n] - mu_y0) / exp(mu_scale[predict_platoon[n]])) ^ exp(mu_r)) ^ (1.0 / exp(mu_r)) - mu_alpha[predict_platoon[n]])
//     );
//   }
}

