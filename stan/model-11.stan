// Model 10 but coded with the database format rather than a matrix.
// Periods refer to the period within each umpire, not in absolute time.

data {
  int<lower=1> N; // num obs
  int<lower=1> U; // num umpires
  // int<lower=1> T; // number of time periods
  int<lower=1> T; // total number of umpire-period parameters
  int<lower=1,upper=U> umpire_index[N];
  vector[N] x; // x-coord.
  vector[N] y; // y-coord.
  int s[U]; // number of periods for each umpire
  int<lower=1,upper=T> period[N]; // index of periods
  int<lower=0,upper=1> call[N]; // 0 = ball; 1 = strike
}
transformed data {
  int index[U]; // the index of the first parameter for each umpire
  
  index[1] = 1;
  // index[2] = 1 + s[1];
  for(u in 2:U)
    index[u] = index[u-1] + s[u-1];
}
parameters {
  real mu_alpha; // for now, the top-level parameters will be non-dynamic
  real<lower=0> sigma_alpha;
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  real alpha_tilde[T];
  real beta_tilde[T];
  
  real<lower=0> phi[3]; // first component is transition for alpha, second for beta, third for r_tilde
  
  real mu_r;
  real<lower=0> sigma_r;
  real r_tilde[T];
  
  real x0;
  real y0;
}
transformed parameters {
  real alpha[T];
  real beta[T];
  
  real<lower=0> r_exp[T];
  
  // for(u in 1:U) {
    for (t in 1:T) {
      alpha[t] = mu_alpha + sigma_alpha * alpha_tilde[t];
      beta[t] = mu_beta + sigma_beta * beta_tilde[t];
      r_exp[t] = exp(mu_r + sigma_r * r_tilde[t]);
    }
  // }
}
model {
  real theta[N];
  int pos;
  pos = 1;
  
  mu_beta ~ normal(0, 10);
  mu_alpha ~ normal(0, 1);
  
  x0 ~ normal(0, 1);
  y0 ~ normal(2.5, 1);
  
  for(t in 2:T) {// first set up the time-based priors
    alpha_tilde[t] ~ normal(alpha_tilde[t-1],phi[1]);
    beta_tilde[t] ~ normal(beta_tilde[t-1],phi[2]);
    r_tilde[t] ~ normal(r_tilde[t-1],phi[3]);
  }
  
  for(u in 1:U) { // then rewrite the first parameter for each umpire to be drawn from the initial-state prior
    alpha_tilde[pos] ~ normal(0,1);
    beta_tilde[pos] ~ normal(0,1);
    r_tilde[pos] ~ normal(0,1);
    
    pos = pos + s[u];
  }
  
  mu_r ~ normal(0,10);
  
  phi ~ normal(0.25,0.01);
  
  // for(t in 2:T) {
  //   mu_r[t] ~ normal(mu_r[t-1],0.25);
  //   for(u in 1:U) {
  //     alpha_tilde[u,t] ~ normal(alpha_tilde[u,t-1],phi[1]);
  //     beta_tilde[u,t] ~ normal(beta_tilde[u,t-1],phi[2]);
  //     r_tilde[u,t] ~ normal(r_tilde[u,t-1],phi[3]);
  //   }
  // }
  
  for(n in 1:N)
    theta[n] = beta[index[umpire_index[n]] + (period[n] - 1)] * ((fabs(x[n] - x0) ^ r_exp[index[umpire_index[n]] + (period[n] - 1)] + fabs(y[n] - y0) ^ r_exp[index[umpire_index[n]] + (period[n] - 1)]) ^ (1.0 / r_exp[index[umpire_index[n]] + (period[n] - 1)]) - alpha[index[umpire_index[n]] + (period[n] - 1)]);
  
  call ~ bernoulli_logit(theta);
}

