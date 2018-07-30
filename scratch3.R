
source("utils.R")


s2008 <- readRDS("mybox-selected/model8-summary-2008.rds")$summary %>% as.data.frame()
s2009 <- readRDS("mybox-selected/model8-summary-2009.rds")$summary %>% as.data.frame()
s2010 <- readRDS("mybox-selected/model8-summary-2010.rds")$summary %>% as.data.frame()
s2011 <- readRDS("mybox-selected/model8-summary-2011.rds")$summary %>% as.data.frame()
s2012 <- readRDS("mybox-selected/model8-summary-2012.rds")$summary %>% as.data.frame()
s2013 <- readRDS("mybox-selected/model8-summary-2013.rds")$summary %>% as.data.frame()
s2014 <- readRDS("mybox-selected/model8-summary-2014.rds")$summary %>% as.data.frame()
s2015 <- readRDS("mybox-selected/model8-summary-2015.rds")$summary %>% as.data.frame()

full_data <- readRDS("data/final-dataset.rds")

predict_grid <- expand.grid(x = seq(-2, 2, 0.2),
                            y = seq(0, 6, 0.2),
                            platoon = 1:4)

alpha_all <- NULL
beta_all <- NULL
x0_all <- NULL
y0_all <- NULL
theta_all <- NULL
r_all <- NULL
top_all <- NULL
scale_all <- NULL
theta_standard_all <- NULL

for(year in c(2008:2015)) {
  
  s <- readRDS(paste0("mybox-selected/model8-summary-", year, ".rds"))$summary %>% as.data.frame()
  if(year %in% c(2010,2012)) s <- readRDS(paste0("mybox-selected/model8-summary-", year, "-v2.rds"))$summary %>% as.data.frame()
  
  s$variable <- rownames(s)
  
  year_data <- full_data %>%
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
    mutate(platoon = paste0(stand, "-", p_throws)) %>%
    filter(pitch_type == "FF") %>%
    group_by(UmpName, game_year) %>%
    mutate(row_num = row_number()) %>%
    filter(game_year == year) %>%
    ungroup()
  
  umpire_names <- unique(year_data$UmpName)[order(unique(year_data$UmpName))]
  
  
  # Alpha
  alpha_temp <- s %>%
    filter(grepl("alpha", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable)) %>%
    mutate(umpire_name = rep(umpire_names, each = 4),
           platoon = rep(c("L-L", "L-R", "R-L", "R-R"), length(umpire_names)),
           year = year)
  
  alpha_all <- rbind(alpha_all, alpha_temp)
  
  
  # Beta
  beta_temp <- s %>%
    filter(grepl("beta", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable)) %>%
    mutate(umpire_name = rep(umpire_names),
           platoon = rep(NA, length(umpire_names)),
           year = year)
  
  beta_all <- rbind(beta_all, beta_temp)
  
  # x0
  x0_temp <- s %>%
    filter(grepl("x0", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable)) %>%
    mutate(umpire_name = rep(umpire_names, each = 4),
           platoon = rep(c("L-L", "L-R", "R-L", "R-R"), length(umpire_names)),
           year = year)
  
  x0_all <- rbind(x0_all, x0_temp)
  
  # y0
  y0_temp <- s %>%
    filter(grepl("y0", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable)) %>%
    mutate(umpire_name = rep(umpire_names),
           platoon = rep(NA, length(umpire_names)),
           year = year)
  
  y0_all <- rbind(y0_all, y0_temp)
  
  # Theta_rep
  if(year != 2010) {
    theta_temp <- s %>%
      filter(grepl("theta", variable),
             !grepl("mu", variable),
             !grepl("sigma", variable),
             !grepl("tilde", variable),
             !grepl("predict", variable)) %>%
      mutate(mean_prob = exp(mean) / (exp(mean) + 1)) %>%
      cbind(year_data)
    
    theta_all <- rbind(theta_all, theta_temp)
  }
  
  # R param
  r_temp <- s %>%
    filter(grepl("r", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable),
           !grepl("predict", variable)) %>%
    mutate(umpire_name = rep(umpire_names),
           platoon = rep(NA, length(umpire_names)),
           year = year)
  
  r_all <- rbind(r_all, r_temp)
  
  # Top-level
  top_temp <- s %>%
    filter(grepl("mu", variable) |
           grepl("sigma", variable),
           !grepl("tilde", variable),
           !grepl("predict", variable)) %>%
    mutate(umpire_name = NA,
           platoon = NA,
           year = year)
  
  top_all <- rbind(top_all, top_temp)
  
  # Scale
  scale_temp <- s %>%
    filter(grepl("scale", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable)) %>%
    mutate(umpire_name = rep(umpire_names, each = 4),
           platoon = rep(c("L-L", "L-R", "R-L", "R-R"), length(umpire_names)),
           year = year)
  
  scale_all <- rbind(scale_all, scale_temp)
  
  # Theta standard
  theta_standard_temp <- s %>%
    filter(grepl("theta", variable),
           grepl("predict", variable),
           !grepl("mu", variable),
           !grepl("sigma", variable),
           !grepl("tilde", variable)) %>%
    mutate(mean_prob = exp(mean) / (exp(mean) + 1),
           year = year) %>%
    cbind(predict_grid)
  
  theta_standard_all <- rbind(theta_standard_all, theta_standard_temp)
}

alpha_all %>%
  ggplot(aes(year, mean, color = platoon)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) +
  facet_wrap(~umpire_name)

scale_all %>%
  ggplot(aes(year, mean, color = platoon)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) +
  # facet_wrap(~umpire_name)
facet_wrap(~platoon)

beta_all %>%
  ggplot(aes(year, mean)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) +
  facet_wrap(~umpire_name)

x0_all %>%
  ggplot(aes(year, mean, color = platoon)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) #+
  # facet_wrap(~umpire_name)

y0_all %>%
  ggplot(aes(year, mean)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) #+
  # facet_wrap(~umpire_name)


theta_all %>%
  group_by(game_year,
           platoon,
           prediction = round(mean_prob, 2)) %>%
  summarise(rate = mean(strike),
            n = n()) %>%
  ggplot(aes(prediction, rate, color = platoon)) +
  geom_point() +
  facet_wrap(~game_year)

r_all %>%
  ggplot(aes(year, mean, color = umpire_name)) +
  # ggplot(aes(year, mean)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) +
  # facet_wrap(~umpire_name) +
  theme(legend.position = "none")

top_all %>%
  filter(grepl("sigma", variable)) %>%
  ggplot(aes(year, mean, color = variable)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`)) +
  facet_wrap(~variable)
  # theme(legend.position = "none")

theta_standard_all %>%
  ggplot(aes(x, y, z = mean_prob, color = as.factor(platoon))) +
  geom_contour() +
  geom_segment(aes(x = -17/24, xend = -17/24, y = 1.6, yend = 3.4, color = NA)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 1.6, yend = 1.6, color = NA)) +
  geom_segment(aes(x = 17/24, xend = 17/24, y = 1.6, yend = 3.4, color = NA)) +
  geom_segment(aes(x = -17/24, xend = 17/24, y = 3.4, yend = 3.4, color = NA)) +
  coord_equal() +
  facet_grid(platoon ~ year)



# TODO: add umpire debut years in the for loop above






