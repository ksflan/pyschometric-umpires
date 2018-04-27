// model 1 with dynamic parameters


data {
  int<lower=1> N; // num obs
  int<lower=1> U; // num umpires
  int<lower=1> T; // number of time periods
  int<lower=1,upper=U> umpire_index[N];
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int<lower=1,upper=T> period[N]; // index of periods
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
}
parameters {
  real mu_alpha;//[T];
  real<lower=0> sigma_alpha;//[T];
  
  real mu_beta;//[T];
  real<lower=0> sigma_beta;//[T];
  
  real alpha_tilde[U,T]; // this is assuming that each umpire appears in each timepoint
  real beta_tilde[U,T];
  
  real<lower=0> phi;
  
  // real x0;
  // real y0;
}
transformed parameters {
  real alpha[U,T];
  real beta[U,T];
  
  for(t in 1:T) {
    for (u in 1:U) {
      alpha[u,t] = mu_alpha + sigma_alpha * alpha_tilde[u,t];
      beta[u,t] = mu_beta + sigma_beta * beta_tilde[u,t];
    }
  }
}
model {
  real theta[N];
  
  mu_beta ~ normal(0,10);
  mu_alpha ~ normal(0,1);
  
  phi ~ normal(0.25,0.01);
  
  // x0 ~ normal(0,1);
  // y0 ~ normal(2.5,1);
  
  for(u in 1:U) {
    alpha_tilde[u,1] ~ normal(0,1);
    beta_tilde[u,1] ~ normal(0,1);
  }
  
  for(t in 2:T) {
    for(u in 1:U) {
      alpha_tilde[u,t] ~ normal(alpha_tilde[u,t-1],phi);
      beta_tilde[u,t] ~ normal(beta_tilde[u,t-1],phi);
    }
  }
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n],period[n]] * ((fabs(x[n] - 0) ^ 2.0 + fabs(y[n] - 2.5) ^ 2.0) ^ (1.0 / 2) - alpha[umpire_index[n],period[n]]);
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }

