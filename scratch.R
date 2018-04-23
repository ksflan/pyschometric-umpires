

source("utils.R")

set.seed(4321)

ump_data <- fread("data/umpire-data-2014.csv")

umpires <- unique(ump_data$umpire_id)[1:50]

pre_data <- ump_data %>%
  filter(umpire_id %in% umpires) %>%
  group_by(umpire_id) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 100)

data <- list(
  N = nrow(pre_data),
  U = length(unique(pre_data$umpire_id)),
  umpire_index = as.numeric(factor(pre_data$umpire_id)),
  x = pre_data$px,
  y = pre_data$pz,
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
