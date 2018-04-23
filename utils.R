try(library(tidyverse))
library(rstan)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(pitchRx)
library(data.table)
library(dplyr)
library(ggplot2)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


