source("utils.R")
set.seed(4321)




full_data <- readRDS("data/final-dataset.rds")

twenty_umpires <- as.character(sample(unique(full_data$UmpName), 20))

pre_data <- full_data %>%
  filter(game_year %in% 2008:2015) %>%
  mutate(platoon = paste0(stand, "-", p_throws)) %>%
  filter(UmpName %in% twenty_umpires,
         pitch_type == "FF") %>%
  group_by(UmpName, game_year) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 400) %>%
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
