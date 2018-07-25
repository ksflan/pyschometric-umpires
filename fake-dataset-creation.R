


# Script to create a fake umpire dataset to be used in debugging the model. This script
#   creates a population of umpires with artificially determined parameters and uses
#   those parameters to generate a fake dataset of pitches.

library(tidyverse)

set.seed(108)

N_umpire <- 20
N_observations <- 100

umpires <- 1:N_umpire

mu_alpha <- seq(0.6, 1.5, length.out = 4) # 4 is currently the number of alpha sub-parameters
sigma_alpha <- rep(0.5, N_umpire)

mu_beta <- -10
sigma_beta <- 1

mu_x0 <- c(0.8, 0.9, 1, 1.1)
sigma_x0 <- rep(0.1, 4)

mu_y0 <- 1.5
sigma_y0 <- 0.2

mu_lambda <- c(0.8, 0.9, 1, 1.1)
sigma_lambda <- rep(0.2, 4)

mu_r <- 4
sigma_r <- 0.5


umpire_parameters <- data.frame(
  umpire_id = rep(umpires, each = 4),
  platoon = rep(1:4, times = N_umpire),
  alpha = c(rnorm(N_umpire, mu_alpha[1], sigma_alpha[1]),
            rnorm(N_umpire, mu_alpha[2], sigma_alpha[2]),
            rnorm(N_umpire, mu_alpha[3], sigma_alpha[3]),
            rnorm(N_umpire, mu_alpha[4], sigma_alpha[4])),
  beta = rep(rnorm(20, mu_beta, sigma_beta), each = 4),
  x0 = c(rnorm(N_umpire, mu_x0[1], sigma_x0[1]),
         rnorm(N_umpire, mu_x0[2], sigma_x0[2]),
         rnorm(N_umpire, mu_x0[3], sigma_x0[3]),
         rnorm(N_umpire, mu_x0[4], sigma_x0[4])),
  y0 = rep(rnorm(20, mu_y0, sigma_y0), each = 4),
  lambda = c(rnorm(N_umpire, mu_lambda[1], sigma_lambda[1]),
             rnorm(N_umpire, mu_lambda[2], sigma_lambda[2]),
             rnorm(N_umpire, mu_lambda[3], sigma_lambda[3]),
             rnorm(N_umpire, mu_lambda[4], sigma_lambda[4])),
  r = rep(rnorm(N_umpire, mu_r, sigma_r), each = 4)
)


situations <- data.frame(
  umpire_id = rep(1:N_umpire, each = N_observations),
  platoon = sample(1:4, N_observations * N_umpire, replace = TRUE),
  x = rnorm(N_observations * N_umpire, 0, 2),
  y = rnorm(N_observations * N_umpire, 2.5, 2)
)

dataset <- situations %>%
  left_join(umpire_parameters) %>%
  mutate(
    theta = beta * ((abs(x - x0) ^ (r) + (abs(y - y0) / lambda) ^ (r)) ^ (1.0 / r) - alpha),
    prob = exp(theta) / (1 + exp(theta)),
    strike = rbinom(N_observations * N_umpire, 1, prob)
  )

predict_grid <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2),
                            platoon = 1:4)

data <- list(
  N = nrow(dataset),
  U = N_umpire,
  # K = ncol(model.matrix(strike ~ platoon + count, data = pre_data)),
  umpire_index = dataset$umpire_id,
  x = dataset$x,
  y = dataset$y,
  batter_stance = dataset$platoon,
  # count = as.numeric(factor(pre_data$count)),
  call = dataset$strike,
  # model_matrix = model.matrix(strike ~ platoon + count, data = pre_data),
  predict_N = nrow(predict_grid),
  predict_x = predict_grid$x,
  predict_y = predict_grid$y,
  predict_platoon = predict_grid$platoon
)



