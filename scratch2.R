source("utils.R")
set.seed(4321)




full_data <- readRDS("data/final-dataset2.rds")

twenty_umpires <- as.character(sample(unique(full_data$UmpName), 20))

pre_data <- full_data %>%
  filter(game_year %in% 2008:2015,
         !is.na(p_throws),
         !is.na(stand),
         p_throws %in% c("R", "L"),
         stand %in% c("R", "L"),
         !is.na(plate_x),
         !is.na(plate_z),
         !is.na(balls),
         !is.na(strikes),
         !is.na(strike)) %>%
  mutate(platoon = paste0(stand, "-", p_throws),
         centered_height = height - mean(height),
         inning_bottom = inning_topbot == "Bot") %>%
  filter(pitch_type == "FF",
         # UmpName %in% sample(twenty_umpires,10)
         UmpName %in% twenty_umpires
         ) %>%
  group_by(UmpName, game_year) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 100,
         game_year == 2014) %>%
  ungroup()


predict_grid <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2),
                            platoon = 1:4)

m_matrix <- model.matrix(strike ~ count + platoon + inning_bottom + UmpName, # + centered_height, # + count
                         data = pre_data)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$UmpName)),
  K = ncol(m_matrix) - length(unique(pre_data$UmpName)) + 1,
  # T = max(pre_data$game_year) - min(pre_data$game_year),
  # T = sum((pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n),
  # umpire_index = as.numeric(factor(pre_data$UmpName)),
  x = pre_data$plate_x,
  y = pre_data$plate_z,
  # s = (pre_data %>% group_by(UmpName) %>% summarise(n = length(unique(game_year))))$n,
  # batter_stance = as.numeric(factor(pre_data$platoon)),
  # count = as.numeric(factor(pre_data$count)),
  # period = pre_data$game_year,
  call = pre_data$strike,
  model_matrix = as.matrix(m_matrix[,1:(ncol(m_matrix) - length(unique(pre_data$UmpName)) + 1)]),
  umpire_matrix = m_matrix[,-(ncol(m_matrix) - length(unique(pre_data$UmpName)) + 1):-1],
  predict_N = nrow(predict_grid),
  predict_x = predict_grid$x,
  predict_y = predict_grid$y,
  predict_platoon = predict_grid$platoon
)




# model8_v5 <- stan(file = "stan/model-8-5.stan",
#                   data = data,
#                   iter = 1000,
#                   chains = 2,
#                   include = FALSE,
#                   pars = c("theta", "d"),
#                   control = list(
#                     adapt_delta = 0.80, max_treedepth = 13
#                   ))

# model8_v6 <- stan(file = "stan/model-8-6.stan",
#                   data = data,
#                   iter = 1000,
#                   chains = 2,
#                   include = FALSE,
#                   pars = c("theta", "d", "alpha_star", "lambda_star"),
#                   control = list(
#                     adapt_delta = 0.80, max_treedepth = 10
#                   ))

model8_v7 <- stan(file = "stan/model-8-7.stan",
                  data = data,
                  iter = 1000,
                  chains = 4,
                  # include = FALSE,
                  # pars = c("d", "alpha_star", "lambda_star",
                  #          "beta_star", "x0_star", "y0_star", "r_star"),
                  control = list(
                    adapt_delta = 0.80, max_treedepth = 13
                  ))

# model8_v4 <- stan(file = "stan/model-8-4.stan",
#                   data = data,
#                   iter = 1000,
#                   chains = 2,
#                   include = FALSE,
#                   pars = c("theta", "d"),
#                   control = list(
#                     adapt_delta = 0.80, max_treedepth = 13
#                   ))


# model3_v2 <- stan(file = "stan/model-3-2.stan",
#                   data = data,
#                   iter = 500,
#                   chains = 2,
#                   include = FALSE,
#                   pars = "theta",
#                   control = list(
#                     max_treedepth = 10
#                   ))

model_summary <- rstan::summary(model8_v7)$summary %>%
  as.data.frame() %>%
  mutate(parameter = rownames(.))

prob <- model_summary %>%
  filter(grepl("prob", parameter)) %>%
  cbind(pre_data)

prob %>%
  # filter(count %in% c("0-1", "2-1")) %>%
  ggplot(aes(plate_x, plate_z, color = `97.5%`)) +
  geom_point() +
  coord_equal() +
  facet_wrap(~platoon)



# pars <- extract(model8)

#### Evaluation ----

# Calibration

pred_actual <- apply(pars$theta, FUN = mean, MARGIN = 2)
post_data <- pre_data
post_data$pred <- pred_actual

post_data %>%
  group_by(pred = round((1 / (1 + exp(-pred))), 1),
           platoon) %>%
  summarise(freq = mean(strike)) %>%
  ggplot(aes(pred, freq)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0)) +
  facet_wrap(~platoon) +
  xlim(c(0,1)) +
  ylim(c(0,1))




#########
pre_data %>%
  filter(count=="0-1") %>%
  ggplot(aes(plate_x,plate_z,color=strike)) +
  geom_point() +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 1.6, yend = 1.6), color = "black") +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 2.5+(2.5-1.6), yend = 2.5+(2.5-1.6)), color = "black") +
  geom_segment(aes(x = -17/24, xend = -17/24, y = 1.6, yend = 2.5+(2.5-1.6)), color = "black") +
  geom_segment(aes(x = 17/24, xend = 17/24, y = 1.6, yend = 2.5+(2.5-1.6)), color = "black") +
  coord_equal()


