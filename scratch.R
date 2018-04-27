

source("utils.R")

set.seed(4321)

ump_data <- fread("data/umpire-data-2014.csv")

umpires <- ump_data %>%
  count(umpire_id) %>%
  arrange(desc(n))

umpire_list <- umpires$umpire_id[1:20]

pre_data <- ump_data %>%
  mutate(
    #(as.numeric(ymd(original_date)) - min(as.numeric(ymd(original_date)))) %/% 30 + 1) %>%
         period = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 365) + 1) %>%
  filter(umpire_id %in% umpire_list,
         period <= 6000,
         year(original_date) %in% 2014:2015) %>%
  group_by(umpire_id, period) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 1000) %>%
  ungroup() #%>%
  # filter(period == 3)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$umpire_id)),
  T = max(pre_data$period),
  umpire_index = as.numeric(factor(pre_data$umpire_id)),
  x = pre_data$px,
  y = pre_data$pz,
  batter_stance = ifelse(pre_data$stand == "R", 1, 2),
  period = pre_data$period,
  call = pre_data$strike
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












# Manipulate samples

pars <- rstan::extract(model10)
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
  facet_wrap(~umpire) +
  theme(legend.position = "none")






