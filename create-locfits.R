

source("utils.R")

library(locfit)
library(gam)

full_data <- readRDS("data/final-dataset.rds")

predict_grid_nonparam <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2))

umpire_names <- unique(full_data$UmpName)

ump1 <- full_data %>%
  filter(pitch_type == "FF",
         UmpName == "Joe West",
         # year == 2015,
         p_throws == "R",
         stand == "R") %>%
  mutate(x = plate_x,
         y = plate_z)

ump1_fit <- mgcv::gam(strike ~ s(x) + s(y), data = ump1, family = binomial)
ump1_pred <- cbind(predict_grid_nonparam, pred_lo = predict(ump1_fit, predict_grid_nonparam)) %>%
  mutate(pred = exp(pred_lo) / (1 + exp(pred_lo)))


ump1_pred %>%
  ggplot(aes(x, y, z = pred)) +
  geom_contour() +
  geom_segment(aes(x = -17/24, xend = -17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 1.6, yend = 1.6)) +
  geom_segment(aes(x = 17/24, xend = 17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 3.4, yend = 3.4)) +
  coord_equal()


binned_result <- full_data %>%
  mutate(x = plate_x,
         y = plate_z,
         umpire_name = UmpName,
         year = game_year) %>%
  group_by(umpire_name,
           platoon,
           year,
           x = round(x, 1),
           y = round(y, 1)) %>%
  summarise(n = n(),
            freq = mean(strike))

binned_result %>%
  filter(UmpName == "Joe West") %>%
  ggplot(aes(x, y, fill = n)) +
  geom_tile() +
  geom_segment(aes(x = -17/24, xend = -17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 1.6, yend = 1.6)) +
  geom_segment(aes(x = 17/24, xend = 17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 3.4, yend = 3.4)) +
  coord_equal() +
  facet_grid(platoon ~ year)

####

ump_all <- NULL

for(u in umpire_names) {
  ump_year_all <- NULL
  
  for(y in 2008:2015) {
    ump_year_platoon_all <- NULL
    
    for(p in c("R-R", "R-L", "L-R", "L-L")) {
      
      ump_data <- full_data %>%
        filter(pitch_type == "FF",
               UmpName == u,
               game_year == y,
               paste0(stand, "-", p_throws) == p) %>%
        mutate(x = plate_x,
               y = plate_z)
      
      if(nrow(ump_data) < 100) next
      
      ump_fit <- mgcv::gam(I(strike == 1) ~ s(x) + s(y), data = ump_data, family = "binomial")
      ump_pred <- cbind(predict_grid_nonparam,
                        pred_lo = predict(ump_fit, predict_grid_nonparam),
                        umpire_name = u,
                        year = y,
                        platoon = p) %>%
        mutate(pred = exp(pred_lo) / (1 + exp(pred_lo)))
      
      ump_year_platoon_all <- rbind(ump_year_platoon_all, ump_pred)
    }
    
    ump_year_all <- rbind(ump_year_all, ump_year_platoon_all)
  }
  
  ump_all <- rbind(ump_all, ump_year_all)
}


saveRDS(ump_all, "../psychometric-umpires-model-comparison-tool/test-umpire-locfits.rds")

ump_all %>%
  ggplot(aes(x, y, z = pred)) +
  geom_contour() +
  geom_segment(aes(x = -17/24, xend = -17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 1.6, yend = 1.6)) +
  geom_segment(aes(x = 17/24, xend = 17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 3.4, yend = 3.4)) +
  coord_equal() +
  facet_grid(platoon ~ year + umpire_name)

