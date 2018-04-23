

source("utils.R")

ump_data <- fread("data/umpire-data-2014.csv")

pre_data <- ump_data %>%
  sample_n(1000)

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

