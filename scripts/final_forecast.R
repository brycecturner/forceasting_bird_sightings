
# Setup -------------------------------------------------------------------

library(tidyverse)
library(stats)
library(usmap)

setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# Plot Options ------------------------------------------------------------
source("scripts/plot_options.R")




# QOL Functions -----------------------------------------------------------
bird_abb_to_name <- function(bird_abb){
  name <- ifelse(bird_abb=="eagles", "Bald Eagle", "Red Winged Black Bird")
  return(name)
}

state_abb_to_name <- function(state_abb){
  return(state.name[match(state_abb, state.abb)])
}


# Forecast_Data -----------------------------------------------------------
forecast_data <-
  read_csv("intermediate_data/forecast_data.csv")


state_series <- 
  lapply(X=forecast_data$STATE_CODE %>% unique,
         FUN=function(X){
           forecast_data %>% filter(STATE_CODE==X) %>% arrange(date)
         }) 


# Actual Forecast ---------------------------------------------------------

march2023_fcst <- function(state_df){
  
  eagles.ts <- ts(state_df$number_of_eagles, 
                  start=as.Date("1950-01-01"),
                  frequency = 12)
  
  eagles_fit = StructTS(eagles.ts, "BSM")
  eagles_fcst <- forecast(eagles_fit, 110)$mean %>% tail(1)
  
  
  
  rwbb.ts <- ts(state_df$number_of_rwbb, 
                  start=as.Date("1950-01-01"),
                  frequency = 12)
  
  rwbb_fit = StructTS(rwbb.ts, "BSM")
  rwbb_fcst <- forecast(rwbb_fit, 110)$mean %>% tail(1)
  
  out_matrix<- matrix(nrow=2, ncol=3)
  colnames(out_matrix) <- c("STATE_CODE", "Bird", "Forecast")
  out_matrix[1:2, 1] <- state_df$STATE_CODE %>% unique
  out_matrix[1:2, 2] <- c("eagles", "rwbb")
  out_matrix[1:2, 3] <- c(eagles_fcst, rwbb_fcst)
  return(out_matrix)

}


march2023_values  <- 
  mcmapply(state_df=state_series,
           FUN=march2023_fcst,
           SIMPLIFY = F) %>% 
  lapply(X=., FUN=data.frame) %>% 
  bind_rows %>% 
  mutate(Forecast=as.numeric(Forecast))




# rwbb US Map -------------------------------------------------------------


plot_usmap(data = march2023_values %>% 
                  filter(Bird=="rwbb") %>% 
                  mutate(Bird=bird_abb_to_name(Bird),
                         state=STATE_CODE,
                         Forecast=log(Forecast+1)), 
           values = "Forecast", 
           include = march2023_values$STATE_CODE %>% unique) + 
  scale_fill_gradient(name = "Log of Forecast", 
                      low="white",
                      high=bird_colors[2],
                      label = scales::comma
                      ) + 
  theme(legend.position = "right")

ggsave("figures/rwbb_map.jpeg")


# eagles US Map -----------------------------------------------------------


plot_usmap(data = march2023_values %>% 
             filter(Bird=="eagles") %>% 
             mutate(Bird=bird_abb_to_name(Bird),
                    state=STATE_CODE,
                    Forecast=log(Forecast+1)), 
           values = "Forecast", 
           include = march2023_values$STATE_CODE %>% unique) + 
  scale_fill_gradient(name = "Log of Forecast", 
                      low="white",
                      high=bird_colors[1],
                      label = scales::comma
  ) + 
  theme(legend.position = "right")

ggsave("figures/eagles_map.jpeg")