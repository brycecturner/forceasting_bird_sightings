
# Setup -------------------------------------------------------------------
rm(list=ls())


library(tidyverse)
library(knitr)
library(urca) #for ur.df tests
library(dynlm) #for dynlm model
library(lmtest) # for coeftest
library(sandwich) #for NeweyWest

setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# Plot Options ------------------------------------------------------------
source("plot_options.R")

# QOL Functions -----------------------------------------------------------
bird_abb_to_name <- function(bird_abb){
  name <- ifelse(bird_abb=="eagles", "Bald Eagle", "Red Winged Black Bird")
  return(name)
}

state_abb_to_name <- function(state_abb){
  return(state.name[match(state_abb, state.abb)])
}


# Pull Analysis Data ------------------------------------------------------

analysis_data <- 
  read_csv("intermediate_data/analysis_data.csv") 

state_series <- 
  lapply(X=analysis_data$STATE_CODE %>% unique,
         FUN=function(X){
           analysis_data %>% filter(STATE_CODE==X) %>% arrange(date)
         })

# Stationarity ------------------------------------------------------------


## Level Stationarity ------------------------------------------------------


state_urdf_tests <- function(state_df){
  #print(state_df$STATE_CODE %>% unique)
  return_matrix <- matrix(nrow=2, ncol=4)
  colnames(return_matrix) <- c("STATE_CODE", "Bird",  "tau2", "phi1")
  rwbb_urdf <-
    ur.df(state_df$number_of_rwbb, lags = 12, selectlags = "AIC", type = "trend")
  
  eagles_urdf <- 
    ur.df(state_df$number_of_eagles, lags = 12, selectlags = "AIC", type = "trend")

  rwbb_tau_significance <- ifelse(abs(rwbb_urdf@teststat[1])>abs(rwbb_urdf@cval[1,1]), "0.01",
                      ifelse(abs(rwbb_urdf@teststat[1])>abs(rwbb_urdf@cval[1,2]), "0.05",
                      ifelse(abs(rwbb_urdf@teststat[1])>abs(rwbb_urdf@cval[1,3]), "0.10",
                      "1")))
  rwbb_phi_significance <- ifelse(abs(rwbb_urdf@teststat[2])>abs(rwbb_urdf@cval[2,1]), "0.01",
                      ifelse(abs(rwbb_urdf@teststat[2])>abs(rwbb_urdf@cval[2,2]), "0.05",
                      ifelse(abs(rwbb_urdf@teststat[2])>abs(rwbb_urdf@cval[2,3]), "0.10",
                      "1")))
  
  eagles_tau_significance <- ifelse(abs(eagles_urdf@teststat[1])>abs(eagles_urdf@cval[1,1]), "0.01",
                                  ifelse(abs(eagles_urdf@teststat[1])>abs(eagles_urdf@cval[1,2]), "0.05",
                                         ifelse(abs(eagles_urdf@teststat[1])>abs(eagles_urdf@cval[1,3]), "0.10",
                                                "1")))
  eagles_phi_significance <- ifelse(abs(eagles_urdf@teststat[2])>abs(eagles_urdf@cval[2,1]), "0.01",
                                  ifelse(abs(eagles_urdf@teststat[2])>abs(eagles_urdf@cval[2,2]), "0.05",
                                         ifelse(abs(eagles_urdf@teststat[2])>abs(eagles_urdf@cval[2,3]), "0.10",
                                                "1")))
  
  return_matrix[1:2, 1] <- state_df$STATE_CODE %>% unique
  return_matrix[1:2, 2] <- c("rwbb", "eagles")
  return_matrix[1, 3] <- rwbb_tau_significance
  return_matrix[1, 4] <- rwbb_phi_significance
  return_matrix[2, 3] <- eagles_tau_significance
  return_matrix[2, 4] <- eagles_phi_significance 
  
  return(return_matrix)
}

  lapply(X=state_series,
         FUN=state_urdf_tests) %>% 
  lapply(X=.,
         FUN=data.frame) %>% 
  bind_rows %>% 
  mutate(tau2=abs(as.numeric(tau2)),
         phi1=abs(as.numeric(phi1)),
         Bird=bird_abb_to_name(Bird)) %>% 
  pivot_longer(cols=c("tau2", "phi1"),
               names_to="statistic",
               values_to="significance_level") %>% 
  group_by(Bird, statistic, significance_level) %>% 
  summarise(count=n()) %>% 
  
  pivot_wider(id_cols=c("Bird", "statistic"),
              names_from="significance_level",
              names_prefix = "sig_level_",
              values_from="count",
              values_fill =0)  %>% 
  select(Bird, statistic, 
         sig_level_0.01, sig_level_0.05, 
         sig_level_0.1, non_sig=sig_level_1) %>% 
  
  kable(format="latex",
        digits=2,
        row.names=F) %>% 
    cat(., file="tables/stationarity_tests.tex")



# Cointegreation ----------------------------------------------------------
state_df <- analysis_data %>% filter(STATE_CODE=="CA") %>% arrange(date)
state_df$number_of_eagles

rwbb.ts <- ts(state_df$number_of_rwbb)
eagles.ts <- ts(state_df$number_of_eagles)
rain.ts <- ts(state_df$percipitation)
temp.ts <- ts(state_df$temperature)

eq01 = dynlm(d(rwbb.ts)
             ~ L(d(rain.ts), 1:24) 
             + L(d(temp.ts), 1:24) )
tmp <- 
coeftest(eq01, vcov. = NeweyWest(eq01, prewhite = F, adjust = T)) #%>% 
  cat(., "tables/test.tex")
