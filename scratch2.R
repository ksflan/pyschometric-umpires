source("utils.R")
set.seed(4321)




full_data <- readRDS("data/final-dataset.rds")

twenty_umpires <- as.character(sample(unique(full_data$UmpName), 20))

pre_data <- full_data %>%
  filter(game_year %in% 2008:2015,
         !is.na(p_throws),
         !is.na(stand),
         !is.na(plate_x),
         !is.na(plate_z),
         !is.na(balls),
         !is.na(strikes),
         !is.na(strike)) %>%
  mutate(platoon = paste0(stand, "-", p_throws)) %>%
  filter(UmpName %in% twenty_umpires,
         pitch_type == "FF") %>%
  group_by(UmpName, game_year) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 400,
         game_year == 2009) %>%
  ungroup()


predict_grid <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2),
                            platoon = 1:4)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$UmpName)),
  T = max(pre_data$game_year) - min(pre_data$game_year),
  # T = sum((pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n),
  umpire_index = as.numeric(factor(pre_data$UmpName)),
  x = pre_data$plate_x,
  y = pre_data$plate_z,
  s = (pre_data %>% group_by(UmpName) %>% summarise(n = length(unique(game_year))))$n,
  batter_stance = as.numeric(factor(pre_data$platoon)),
  period = pre_data$game_year,
  call = pre_data$strike,
  predict_N = nrow(predict_grid),
  predict_x = predict_grid$x,
  predict_y = predict_grid$y,
  predict_platoon = predict_grid$platoon
)




model8 <- stan(file = "stan/model-8.stan",
               data = data,
               iter = 2000,
               chains = 4)

pars <- extract(model8)

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



