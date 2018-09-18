// This is model 8.4 but with the centered parameterization
//  (to see if there is enough data that this version is more efficient)




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
  vector[K] mu_alpha;
  row_vector<lower=0>[K] sigma_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;

  vector[K] alpha[U];
  real beta[U];
  
  // minkowski parameters
  
  real mu_r;
  real<lower=0> sigma_r;
  real r[U];

  vector[K] mu_lambda;
  row_vector<lower=0>[K] sigma_lambda;
  vector[K] lambda[U];
  
  // strike zone center parameters
  
  vector[4] mu_x0; // 2, for the number of batter handednesses (R and L) // or 4 for the number of distinct platoons
  row_vector<lower=0>[4] sigma_x0; // 2, for the number of batter handednesses (R and L)
  real mu_y0;
  real<lower=0> sigma_y0;
  
  vector[4] x0[U]; // 2, for the number of batter handednesses (R and L)
  real y0[U];
}
transformed parameters {
  real theta[N];
  
  real alpha_star[N];
  real lambda_star[N];
  real d[N]; // distance calculated from minkowski_distance
  
  
  for(n in 1:N) { // possibly move the exp() call to here
    alpha_star[n] = model_matrix[n] * alpha[umpire_index[n]];
    lambda_star[n] = exp(model_matrix[n] * lambda[umpire_index[n]]);
    
    d[n] = minkowski_distance(x0[umpire_index[n],batter_stance[n]], y0[umpire_index[n]], x[n], y[n], lambda_star[n], exp(r[umpire_index[n]]));

    theta[n] = psychometric_function(beta[umpire_index[n]], alpha_star[n], d[n]);
  }
  
}
model {
  
  sigma_alpha ~ cauchy(0,5);
  sigma_lambda ~ cauchy(0,5);

  sigma_beta ~ cauchy(0,5);
  sigma_r ~ cauchy(0,5);
  
  sigma_y0 ~ cauchy(0,5);
  sigma_x0 ~ cauchy(0,5);
  
  mu_beta ~ normal(-5,5);
  
  mu_alpha ~ normal(0,1);
  mu_alpha[1] ~ normal(17.0 / 24.0,0.5);
  
  mu_lambda ~ normal(-0.5,0.5);
  mu_lambda[1] ~ normal(0,0.5);
  
  mu_x0 ~ normal(0,1);
  mu_y0 ~ normal(2.5,1);
  
  mu_r ~ normal(1.5,0.5);
  
  for(u in 1:U) {
    for(k in 1:K) {
      alpha[u][k] ~ normal(mu_alpha[k],sigma_alpha[k]);
      lambda[u][k] ~ normal(mu_lambda[k],sigma_lambda[k]);
    }
    for(i in 1:4)
      x0[u][i] ~ normal(mu_x0[i],sigma_x0[i]);
  }
  
  beta ~ normal(mu_beta,sigma_beta);
  r ~ normal(mu_r,sigma_r);
  y0 ~ normal(mu_y0,sigma_y0);
  
  for(n in 1:N)
    call[n] ~ bernoulli_logit(theta[n]);
}

