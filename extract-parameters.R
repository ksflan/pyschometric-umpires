# Code (hopefully with functions) for extracting parameters from the model objects


source("utils.R")



### Model 8

# Read in model objects
model8_files <- list.files("./model8-fits")
model8_list <- lapply(model8_files, function(x) readRDS(paste0("model8-fits/", x)))

# model_files <- list.files("./model-fits")
# model_objects <- lapply(model_files, function(x) readRDS(paste0("model-fits/", x)))

# Individual parameters

FIRST_YEAR <- 2010
LAST_YEAR <- 2015

output <- NULL
for(i in 1:(LAST_YEAR - FIRST_YEAR)) {
  
  pars <- rstan::extract(model_objects[[i]])
  
  parameter_names <- names(pars)
  
  for(p in parameter_names) {
    # Logic to handle 3 types of parameters: top-level, individual (single), individual (multiple) ----
    
    if(dim(pars[[p]]) == 1) { # top-level
      output_temp <- pars[[p]] %>%
        as.data.frame() %>%
        # setNames(levels(factor(pre_data$umpire_id))) %>%
        gather(umpire, value) %>%
        mutate(period = FIRST_YEAR + i - 1,
               #umpire = as.numeric(umpire),
               umpire = -999, # code for top-level
               parameter = p)
    }
      
      output_temp <- pars$y0 %>%
        as.data.frame() %>%
        setNames(levels(factor(pre_data$umpire_id))) %>%
        gather(umpire, value) %>%
        mutate(period = FIRST_YEAR + i - 1,
               umpire = as.numeric(umpire),
               parameter = "y0")
  }
  
  output <- rbind(output, output_temp)
}









