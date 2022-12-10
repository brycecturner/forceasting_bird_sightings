
# Setup -------------------------------------------------------------------
rm(list=ls())


library(tidyverse)
library(parallel)
library(knitr)
library(urca) #for ur.df tests
library(dynlm) #for dynlm model
library(lmtest) # for coeftest
library(sandwich) #for NeweyWest
library(forecast) # for auto.arima
library(Metrics) #for RMSE

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

# ARIMA -------------------------------------------------------------------

arima_prediction <- function(state_df){

out_matrix<-matrix(nrow=2, ncol=4) 
colnames(out_matrix) <- c("STATE_CODE", "Bird",  "insample_rmse", 
                          "outsample_rmse")

out_matrix[1:2, 1] <- state_df$STATE_CODE %>% unique
out_matrix[1:2, 2] <- c("eagles", "rwbb")

## Eagles
  eagles_train <- 
    state_df$number_of_eagles[state_df$date<as.Date(as.Date("2013-01-01"))]
  eagles_test <-
    state_df$number_of_eagles[state_df$date>=as.Date(as.Date("2013-01-01"))]
  
  eagles_model <- 
    auto.arima(eagles_train, stepwise=F) 
  out_matrix[1,3] <- round(rmse(eagles_train, eagles_model$fitted), 3)

  eagles_fcast <- 
    forecast(eagles_model, 13)
  out_matrix[1,4] <- round(rmse(eagles_fcast$mean, eagles_test), 3)

## RWBB

  rwbb_train <- 
    state_df$number_of_rwbb[state_df$date<as.Date(as.Date("2013-01-01"))]
  rwbb_test <-
    state_df$number_of_rwbb[state_df$date>=as.Date(as.Date("2013-01-01"))]
  
  rwbb_model <- 
    auto.arima(rwbb_train, stepwise=F) 
  out_matrix[2,3] <- round(rmse(rwbb_train, rwbb_model$fitted), 3)
  
  
  rwbb_fcast <- 
    forecast(rwbb_model, 13)
  out_matrix[2,4] <- round(rmse(rwbb_fcast$mean, rwbb_test), 3)

  return(out_matrix)
}

arima_results  <- 
  mcmapply(state_df=state_series,
           FUN=arima_prediction,
           SIMPLIFY = F) %>% 
  lapply(X=., FUN=data.frame) %>% 
  bind_rows %>% 
  mutate(
    insample_rmse = as.numeric(insample_rmse),
    outsample_rmse = as.numeric(outsample_rmse),
    rmse_increase = round(outsample_rmse/insample_rmse,2),
    Bird =bird_abb_to_name(Bird))


## Raw ARIMA Results ------------------------------------------------------


arima_results %>% 
  arrange(Bird, STATE_CODE) %>% 
  pivot_wider(id_cols="STATE_CODE", 
              names_from="Bird",
              values_from=c("insample_rmse", "outsample_rmse")) %>% 
  select(State=STATE_CODE, 
         ends_with("Eagle"), 
         ends_with("Bird")) %>% 

  kable(format="latex",
        digits=2,
        row.names=F) %>% 
  cat(., file="tables/arima_rmse_results.tex")


# Hist Increase in RMSE ---------------------------------------------------

arima_results %>% 
  select(STATE_CODE, Bird, rmse_increase) %>% 
  
  ggplot +
  geom_histogram(aes(x=rmse_increase, color=Bird, fill=Bird), alpha=0.2) +
  facet_wrap(~Bird) +
  theme_bw() +
  scale_fill_manual(values=bird_colors) +
  scale_color_manual(values=bird_colors) +
  ylab("Count") +
  xlab("Percentage Increase in RMSE from In-Sample Prediction (ARIMA)") +
  theme(legend.position = "top")

ggsave("figures/hist_of_rmse_increase.jpeg")


# Point Increase in RMSE --------------------------------------------------
arima_results %>% 
  pivot_longer(cols=c("insample_rmse", "outsample_rmse"),
               names_to=c("Sample", "Drop"),
               names_sep = "_",
               values_to="Value")  %>% 
  select(-Drop) %>% 
  mutate(Value=log(Value+1),
         Sample=sub("insample", "In Sample", Sample),
         Sample=sub("outsample", "Forecast", Sample)) %>% 
  #head
  
ggplot(aes(x=Value, y=STATE_CODE, group=STATE_CODE, color=Sample)) +
geom_point() +
geom_line(color="black", alpha=0.2) +
facet_wrap(~Bird, scales = "free_x") +
theme_bw() +
theme(axis.text.y=element_text(size=4)) +
ylab("State") + xlab("Log RMSE")

ggsave("figures/full_rmse_reporting_arima.jpeg")


