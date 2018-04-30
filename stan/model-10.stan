// model 4 plus dynamic components


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
  real mu_alpha[T];
  real<lower=0> sigma_alpha[T];
  
  real mu_beta[T];
  real<lower=0> sigma_beta[T];
  
  real alpha_tilde[U,T];
  real beta_tilde[U,T];
  
  real<lower=0> phi[3]; // first component is transition for alpha, second for beta, third for r_tilde
  
  real mu_r[T];
  real<lower=0> sigma_r[T];
  real r_tilde[U,T];
  
  real x0;
  real y0;
}
transformed parameters {
  real alpha[U,T];
  real beta[U,T];
  
  real<lower=0> r_exp[U,T];
  
  for(u in 1:U) {
    for (t in 1:T) {
      alpha[u,t] = mu_alpha[t] + sigma_alpha[t] * alpha_tilde[u,t];
      beta[u,t] = mu_beta[t] + sigma_beta[t] * beta_tilde[u,t];
      r_exp[u,t] = exp(mu_r[t] + sigma_r[t] * r_tilde[u,t]);
    }
  }
}
model {
  real theta[N];
  
  mu_beta ~ normal(0, 10);
  mu_alpha ~ normal(0, 1);
  
  x0 ~ normal(0, 1);
  y0 ~ normal(2.5, 1);
  
  for(u in 1:U) {
    alpha_tilde[u,1] ~ normal(0,1);
    beta_tilde[u,1] ~ normal(0,1);
    r_tilde[u,1] ~ normal(0,1);
  }
  
  mu_r ~ normal(0,10);
  
  phi ~ normal(0.25,0.01);
  
  for(t in 2:T) {
    for(u in 1:U) {
      alpha_tilde[u,t] ~ normal(alpha_tilde[u,t-1],phi[1]);
      beta_tilde[u,t] ~ normal(beta_tilde[u,t-1],phi[2]);
      r_tilde[u,t] ~ normal(r_tilde[u,t-1],phi[3]);
    }
  }
  
  for(n in 1:N)
    theta[n] = beta[umpire_index[n],period[n]] * ((fabs(x[n] - x0) ^ r_exp[umpire_index[n],period[n]] + fabs(y[n] - y0) ^ r_exp[umpire_index[n],period[n]]) ^ (1.0 / r_exp[umpire_index[n],period[n]]) - alpha[umpire_index[n],period[n]]);
  
  call ~ bernoulli_logit(theta);
}
// generated quantities {
//   add log-likelihood calculation
// }


