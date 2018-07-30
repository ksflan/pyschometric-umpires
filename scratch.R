

source("utils.R")

set.seed(4321)

ump_data <- fread("data/umpire-data-2014-2015.csv")
ump_data2 <- fread("data/twenty-umpires.csv")

umpires <- ump_data2 %>%
  count(umpire_id) %>%
  arrange(desc(n))

umpire_list <- umpires$umpire_id[1:20]

pre_data <- ump_data2 %>%
  filter(year(original_date) %in% 2010:2010,
         month(original_date) >= 4,
         month(original_date) <= 9) %>%
  # group_by(umpire_id) %>%
  # mutate(period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1) %>%
  mutate(test = as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365,
         #(as.numeric(ymd(original_date)) - min(as.numeric(ymd(original_date)))) %/% 30 + 1) %>%
         period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1,
         platoon = paste0(stand, "-", p_throws)
  ) %>%
  filter(umpire_id %in% umpire_list,
         period <= 6000,
         pitch_type == "FF") %>%
  group_by(umpire_id, period) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 400) %>%
  ungroup() #%>%
  # filter(period == 3)

# platoon = case_when(
#   pre_data$stand == "R" && pre_data$p_throws == "R" ~ 1,
#   pre_data$stand == "R" && pre_data$p_throws == "L" ~ 2,
#   pre_data$stand == "L" && pre_data$p_throws == "R" ~ 3,
#   pre_data$stand == "L" && pre_data$p_throws == "L" ~ 4
# )

predict_grid <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2),
                            platoon = 1:4)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$umpire_id)),
  T = max(pre_data$period),
  # T = sum((pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n),
  umpire_index = as.numeric(factor(pre_data$umpire_id)),
  x = pre_data$px,
  y = pre_data$pz,
  s = (pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n,
  batter_stance = as.numeric(factor(pre_data$platoon)),
  period = pre_data$period,
  call = pre_data$strike,
  predict_N = nrow(predict_grid),
  predict_x = predict_grid$x,
  predict_y = predict_grid$y,
  predict_platoon = predict_grid$platoon
)

model1 <- stan(file = "stan/model-1.stan",
               data = data,
               iter = 2000,
               chains = 2)

model2 <- stan(file = "stan/model-2.stan",
               data = data,
               iter = 2000,
               chains = 2)

model3 <- stan(file = "stan/model-3.stan",
               data = data,
               iter = 2000,
               chains = 2)

model4 <- stan(file = "stan/model-4.stan",
               data = data,
               iter = 2000,
               chains = 2)

model5 <- stan(file = "stan/model-5.stan",
               data = data,
               iter = 2000,
               chains = 2)

model6 <- stan(file = "stan/model-6.stan",
               data = data,
               iter = 2000,
               chains = 2)

model7 <- stan(file = "stan/model-7.stan",
               data = data,
               iter = 2000,
               chains = 2)

model8 <- stan(file = "stan/model-8.stan",
               data = data,
               iter = 2000,
               chains = 2)

model9 <- stan(file = "stan/model-9.stan",
               data = data,
               iter = 2000,
               chains = 2)

model10 <- stan(file = "stan/model-10.stan",
               data = data,
               iter = 2000,
               chains = 2,
               control = list(
                 max_treedepth = 20
               ))

model11 <- stan(file = "stan/model-11.stan",
                data = data,
                iter = 2000,
                chains = 2,
                control = list(
                  max_treedepth = 20
                ))

model12 <- stan(file = "stan/model-12.stan",
               data = data,
               iter = 2000,
               chains = 2)












# Manipulate samples

pars <- rstan::extract(model_list[[4]])
# alpha_p <- pars$alpha
all_alpha <- NULL
for(i in 1:dim(pars$r_exp)[2]) {
  alpha_temp <- pars$r_exp[,i,] %>%
    as.data.frame() %>%
    gather(period, value) %>%
    mutate(period = as.integer(gsub("V", "", period)),
           umpire = levels(factor(pre_data$umpire_id))[i],
           parameter = "alpha_tilde")
  all_alpha <- rbind(all_alpha, alpha_temp)
}
plot_data <- all_alpha %>%
  group_by(umpire, period) %>%
  summarise(q10 = quantile(value, 0.10),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q90 = quantile(value, 0.90))

plot_data %>%
  # filter(period != 7) %>%
  ggplot(aes(period, q50, color = as.factor(umpire))) +
  geom_point() +
  geom_errorbar(aes(ymax = q90, ymin = q10)) +
  geom_line() +
  # facet_wrap(~umpire) +
  theme(legend.position = "none")

# Fitting Model 4 for 2013-2015 separately by year in order to compare to the DLM version (Model 10)
model_list <- NULL
for(i in 1) {
  temp_data <- pre_data %>%
    filter(period == i)
  data <- list(
    N = nrow(temp_data),
    U = length(unique(temp_data$umpire_id)),
    T = max(temp_data$period),
    umpire_index = as.numeric(factor(temp_data$umpire_id)),
    x = temp_data$px,
    y = temp_data$pz,
    batter_stance = ifelse(temp_data$stand == "R", 1, 2),
    period = temp_data$period,
    call = temp_data$strike
  )
  model_temp <- stan(file = "stan/model-12.stan",
                   data = data,
                   iter = 2000,
                   chains = 2)
  model_list <- append(model_list,
                       model_temp)
}


# Non-DLM version

pars <- rstan::extract(model_list[[6]])
# alpha_p <- pars$alpha
all_alpha <- NULL
for(i in 1:dim(pars$r_exp)[2]) {
  alpha_temp <- pars$r_exp[,i] %>%
    as.data.frame() %>%
    gather(period, value) %>%
    mutate(#period = as.integer(gsub("V", "", period)),
           umpire = levels(factor(pre_data$umpire_id))[i],
           parameter = "alpha_tilde")
  all_alpha <- rbind(all_alpha, alpha_temp)
}
plot_data <- all_alpha %>%
  group_by(umpire) %>%
  summarise(q10 = quantile(value, 0.10),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q90 = quantile(value, 0.90))

plot_data$period <- 3
# plot_data_com <- plot_data
plot_data_com <- rbind(plot_data_com, plot_data)

plot_data %>%
  ggplot(aes(as.character(umpire), q50)) +
  geom_point() +
  geom_errorbar(aes(ymax = q90, ymin = q10)) +
  coord_flip()

plot_data_com %>%
  ggplot(aes(period, q50, color = as.character(umpire))) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymax = q90, ymin = q10)) +
  theme(legend.position = "none")






####
pars <- rstan::extract(model8)

pred <- apply(pars$predict_theta, FUN = mean, MARGIN = 2)
predict_grid$pred <- pred

predict_grid %>%
  ggplot(aes(x, y, z = pred)) +
  geom_contour() +
  facet_wrap(~platoon, nrow = 2) +
  geom_segment(aes(x = -17/24, xend = -17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 1.6, yend = 1.6)) +
  geom_segment(aes(x = 17/24, xend = 17/24, y = 1.6, yend = 3.4)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 3.4, yend = 3.4)) +
  coord_equal()


pred_actual <- apply(pars$theta, FUN = mean, MARGIN = 2)
post_data <- pre_data
post_data$pred <- pred_actual

hist(post_data$strike - (1 / (1 + exp(-post_data$pred))))
plot(post_data$plate_x, post_data$strike - (1 / (1 + exp(-post_data$pred))))

post_data %>%
  ggplot(aes(plate_x, plate_z)) +
  # geom_density2d(lev) +
  # geom_hex(aes(z = strike - (1 / (1 + exp(-pred))))) +
  geom_point(aes(color = strike - (1 / (1 + exp(-pred)))), cex = 0.5) +
  facet_wrap(~platoon) +
  coord_equal()

post_data %>%
  group_by(x = round(plate_x, 1), y = round(plate_z,1), platoon) %>%
  summarise(n = n(),
            error = mean(strike - (1 / (1 + exp(-pred))))) %>%
  ggplot(aes(x, y)) +
  geom_contour(aes(z = error)) +
  facet_wrap(~platoon)




###### Set up for remote machine

pre_data <- ump_data2 %>%
  filter(year(original_date) %in% 2010:2015,
         month(original_date) >= 4,
         month(original_date) <= 9) %>%
  # group_by(umpire_id) %>%
  # mutate(period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1) %>%
  mutate(test = as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365,
         #(as.numeric(ymd(original_date)) - min(as.numeric(ymd(original_date)))) %/% 30 + 1) %>%
         period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1,
         platoon = paste0(stand, "-", p_throws)
  ) %>%
  filter(umpire_id %in% umpire_list,
         period <= 6000) %>%
  group_by(umpire_id, period) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 9999999) %>%
  ungroup() #%>%
# filter(period == 3)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$umpire_id)),
  T = max(pre_data$period),
  # T = sum((pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n),
  umpire_index = as.numeric(factor(pre_data$umpire_id)),
  x = pre_data$px,
  y = pre_data$pz,
  s = (pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n,
  batter_stance = as.numeric(factor(pre_data$platoon)),
  period = pre_data$period,
  call = pre_data$strike,
  predict_N = nrow(predict_grid),
  predict_x = predict_grid$x,
  predict_y = predict_grid$y,
  predict_platoon = predict_grid$platoon
)

pre_data2010 <- pre_data %>% filter(year(original_date) == 2010)
model8_2010 <- stan(file = "stan/model-8.stan",
                    data = data_2010,
                    iter = 4000,
                    chains = 4)

pre_data2011 <- pre_data %>% filter(year(original_date) == 2011)
model8_2011 <- stan(file = "stan/model-8.stan",
                    data = data_2011,
                    iter = 4000,
                    chains = 4)

pre_data2012 <- pre_data %>% filter(year(original_date) == 2012)
model8_2012 <- stan(file = "stan/model-8.stan",
                    data = data_2012,
                    iter = 4000,
                    chains = 4)

pre_data2013 <- pre_data %>% filter(year(original_date) == 2013)
model8_2013 <- stan(file = "stan/model-8.stan",
                    data = data_2013,
                    iter = 4000,
                    chains = 4)

pre_data2014 <- pre_data %>% filter(year(original_date) == 2014)
model8_2014 <- stan(file = "stan/model-8.stan",
                    data = data_2014,
                    iter = 4000,
                    chains = 4)

pre_data2015 <- pre_data %>% filter(year(original_date) == 2015)
model8_2015 <- stan(file = "stan/model-8.stan",
                    data = data_2015,
                    iter = 4000,
                    chains = 4)



### Misc.

test_dat <- twenty_umpires_full %>%
  mutate(px = plate_x,
         pz = plate_z,
         call = as.numeric(strike)) %>%
  filter(#pitch_type == "FF",
         !is.na(pz),
         !is.na(px),
         stand %in% c("R", "L"),
         p_throws %in% c("R", "L")) %>%
  group_by(UmpName) %>%
  mutate(rnum = row_number()) %>%
  filter(rnum <= 400)

test_dat$umpire_id <- as.numeric(factor(test_dat$UmpName))
test_dat$period <- 1
pre_data <- test_dat

model8_SL <- stan(file = "stan/model-8.stan",
                  data = data,
                  iter = 2000,
                  chains = 2)




#### Automated fitting ----

year <- 2010


pre_data <- ump_data2 %>%
  filter(year(original_date) %in% year:year,
         month(original_date) >= 4,
         month(original_date) <= 9) %>%
  # group_by(umpire_id) %>%
  # mutate(period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1) %>%
  mutate(test = as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365,
         #(as.numeric(ymd(original_date)) - min(as.numeric(ymd(original_date)))) %/% 30 + 1) %>%
         period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1,
         platoon = paste0(stand, "-", p_throws)
  ) %>%
  filter(umpire_id %in% umpire_list,
         period <= 6000) %>%
  group_by(umpire_id, period) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 9999999) %>%
  ungroup() #%>%
# filter(period == 3)

predict_grid <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2),
                            platoon = 1:4)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$umpire_id)),
  T = max(pre_data$period),
  # T = sum((pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n),
  umpire_index = as.numeric(factor(pre_data$umpire_id)),
  x = pre_data$px,
  y = pre_data$pz,
  s = (pre_data %>% group_by(umpire_id) %>% summarise(n = length(unique(period))))$n,
  batter_stance = as.numeric(factor(pre_data$platoon)),
  period = pre_data$period,
  call = pre_data$strike,
  predict_N = nrow(predict_grid),
  predict_x = predict_grid$x,
  predict_y = predict_grid$y,
  predict_platoon = predict_grid$platoon
)

model8 <- stan(file = "stan/model-8.stan",
               data = data,
               iter = 4000,
               chains = 4,
               include = FALSE,
               pars = "theta")





