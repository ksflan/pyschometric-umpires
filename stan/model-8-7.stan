// 

// Model 8.6 but non-hierarchical effects for all parameters




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
  // int<lower=1,upper=U> umpire_index[N];
  int K; // number of covariate parameters
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int<lower=1,upper=4> batter_stance[N]; // for each platoon
  int<lower=1,upper=12> count[N]; // ball/strike count
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
  matrix[N,K] model_matrix;
  matrix[N,U-1] umpire_matrix; // indicator variables for umpire effects
  
  // real predict_x[predict_N];
  // real predict_y[predict_N];
  // int<lower=1,upper=4> predict_platoon[predict_N];
}
parameters {
  // real mu_beta;
  real<lower=0> sigma_beta;
  vector[U-1] beta_tilde;
  vector[K] beta;
  
  // real mu_alpha;
  real<lower=0> sigma_alpha;
  vector[K] alpha;
  vector[U-1] alpha_tilde;
  
  // real mu_lambda;
  real<lower=0> sigma_lambda;
  vector[K] lambda;
  vector[U-1] lambda_tilde;
  
  // real mu_r;
  real<lower=0> sigma_r;
  vector[U-1] r_tilde;
  vector[K] r;
  
  // real mu_x0;
  real<lower=0> sigma_x0;
  // real mu_y0;
  real<lower=0> sigma_y0;
  
  vector[U-1] x0_tilde;
  vector[U-1] y0_tilde;
  
  vector[K] x0;
  vector[K] y0;
  
  
}
transformed parameters {
  vector[U-1] alpha_umpire;
  vector[U-1] lambda_umpire;
  
  vector[U-1] beta_umpire;
  
  vector[U-1] r_umpire;
  
  vector[U-1] x0_umpire;
  vector[U-1] y0_umpire;
  
  // vector[4] x0[U];
  // real y0[U];
  
  real theta[N];
  
  vector[N] alpha_star;
  real<lower=0> lambda_star[N];
  vector[N] x0_star;
  vector[N] y0_star;
  real<lower=0> r_star[N];
  vector[N] beta_star;
  real d[N]; // distance calculated from minkowski_distance
  

  for (u in 1:(U-1)) {
    // alpha_umpire[u] = mu_alpha + sigma_alpha * alpha_tilde[u];
    // lambda_umpire[u] = mu_lambda + sigma_lambda * lambda_tilde[u];
    // beta_umpire[u] = mu_beta + sigma_beta * beta_tilde[u];
    // r_umpire[u] = mu_r + sigma_r * r_tilde[u];
    // x0_umpire[u] = mu_x0 + sigma_x0 * x0_tilde[u];
    // y0_umpire[u] = mu_y0 + sigma_y0 * y0_tilde[u];
    alpha_umpire[u] = sigma_alpha * alpha_tilde[u];
    lambda_umpire[u] = sigma_lambda * lambda_tilde[u];
    beta_umpire[u] = sigma_beta * beta_tilde[u];
    r_umpire[u] = sigma_r * r_tilde[u];
    x0_umpire[u] = sigma_x0 * x0_tilde[u];
    y0_umpire[u] = sigma_y0 * y0_tilde[u];
  }

  alpha_star = model_matrix * alpha + umpire_matrix * alpha_umpire;
  x0_star = model_matrix * x0 + umpire_matrix * x0_umpire;
  y0_star = model_matrix * y0 + umpire_matrix * y0_umpire;
  beta_star = model_matrix * beta + umpire_matrix * beta_umpire;  
  
  for(n in 1:N) { // possibly move the exp() call to here
    // alpha_star[n] = model_matrix[n] * alpha + umpire_matrix[n] * alpha_umpire;
    // x0_star[n] = model_matrix[n] * x0 + umpire_matrix[n] * x0_umpire;
    // y0_star[n] = model_matrix[n] * y0 + umpire_matrix[n] * y0_umpire;
    // beta_star[n] = model_matrix[n] * beta + umpire_matrix[n] * beta_umpire;
    lambda_star[n] = exp(model_matrix[n] * lambda + umpire_matrix[n] * lambda_umpire);
    r_star[n] = exp(model_matrix[n] * r + umpire_matrix[n] * r_umpire);
    
    d[n] = minkowski_distance(x0_star[n], y0_star[n], x[n], y[n], lambda_star[n], r_star[n]);
    
    theta[n] = psychometric_function(beta_star[n], alpha_star[n], d[n]);
  }
  
}
model {
  // mu_beta ~ normal(0,1);
  sigma_beta ~ cauchy(0,3);
  for(k in 2:K) {
    beta[k] ~ normal(0,1);
    r[k] ~ normal(0,1);
    alpha[k] ~ normal(0,1);
    lambda[k] ~ normal(0,1);
    x0[k] ~ normal(0,1);
    y0[k] ~ normal(0,1);
  }
  
  beta[1] ~ normal(-10,5);
  
  // mu_r ~ normal(0,1);
  sigma_r ~ cauchy(0,3);
  // r ~ normal(0,1);
  r[1] ~ normal(1,5);
  
  // mu_alpha ~ normal(0,1);
  sigma_alpha ~ cauchy(0,3);
  // alpha ~ normal(0,1);
  alpha[1] ~ normal(1,1);
  
  // mu_lambda ~ normal(0,1);
  sigma_lambda ~ cauchy(0,3);
  // lambda ~ normal(0,1);
  lambda[1] ~ normal(1,1);
  
  // x0 ~ normal(0,1);
  // y0 ~ normal(0,1);
  x0[1] ~ normal(0,1);
  y0[1] ~ normal(2.5,1);
  
  // mu_x0 ~ normal(0,1);
  sigma_x0 ~ cauchy(0,3);
  // mu_y0 ~ normal(0,1);
  sigma_y0 ~ cauchy(0,3);
  
  alpha_tilde ~ normal(0,1);
  lambda_tilde ~ normal(0,1);
  x0_tilde ~ normal(0,1);
  y0_tilde ~ normal(0,1);
  r_tilde ~ normal(0,1);
  beta_tilde ~ normal(0,1);
  
  call ~ bernoulli_logit(theta);
}
generated quantities {
  real height[U-1];
  // vector[K] r_exp;
  
  for(u in 1:(U-1))
    height[u] = (alpha[1] + alpha_umpire[u]) / exp(lambda[1] + lambda_umpire[u]);
    
    
    
    // Create calculated umpire measurements. Now that there is a reference umpire, this must be done separately.
    
    
    
    
  // for(k in 1:K)
  //   r_exp[k] = exp(r[k]);
  
//   real predict_theta[predict_N];
//   
//   for(n in 1:predict_N) {
//     predict_theta[n] = inv_logit(
//       mu_beta * ((fabs(predict_x[n] - mu_x0[predict_platoon[n]]) ^ exp(mu_r) + (fabs(predict_y[n] - mu_y0) / exp(mu_scale[predict_platoon[n]])) ^ exp(mu_r)) ^ (1.0 / exp(mu_r)) - mu_alpha[predict_platoon[n]])
//     );
//   }
}

