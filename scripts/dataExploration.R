
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
source("scripts/plot_options.R")

# QOL Functions -----------------------------------------------------------
bird_abb_to_name <- function(bird_abb){
  name <- ifelse(bird_abb=="eagles", "Bald Eagle", "Red Winged Black Bird")
  return(name)
}

state_abb_to_name <- function(state_abb){
  return(state.name[match(state_abb, state.abb)])
}


# Pull Analysis Data ------------------------------------------------------

exploration_data <- 
  read_csv("intermediate_data/exploration_data.csv") %>% 
  mutate(d_eagles=number_of_eagles-lag(number_of_eagles),
         d_rwbb=number_of_rwbb-lag(number_of_rwbb))



# Stationarity ------------------------------------------------------------


## Level Stationarity ------------------------------------------------------

state_series <- 
  lapply(X=exploration_data$STATE_CODE %>% unique,
         FUN=function(X){
           exploration_data %>% filter(STATE_CODE==X) %>% arrange(date)
         })

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

exploration_data <- 
  lapply(X=state_series,
         FUN=state_urdf_tests) %>% 
  lapply(X=.,
         FUN=data.frame) %>% 
  bind_rows %>% 
  mutate(is_stationary=ifelse(as.numeric(tau2)<=0.1, T, F)) %>% 
  pivot_wider(id_cols=c("STATE_CODE"),
              values_from=c("tau2", "phi1", "is_stationary"),
              names_from="Bird") %>% 
  right_join(y=exploration_data, by="STATE_CODE")

  
  exploration_data %>% 
  select(STATE_CODE, starts_with("tau"), starts_with("phi")) %>% 
  unique %>% 
  pivot_longer(cols=c(starts_with("tau"), starts_with("phi")),
               names_to=c("statistic", "Bird"),
               names_sep="_",
               values_to="significance_level") %>% 
  mutate(Bird=bird_abb_to_name(Bird)) %>% 
  group_by(Bird, statistic, significance_level) %>% 
  summarise(count=n()) %>% 
  ungroup %>% 
  pivot_wider(id_cols=c("Bird", "statistic"),
              names_from="significance_level",
              names_prefix = "sig_level_",
              values_from="count",
              values_fill =0)  %>% 
  select(Bird, statistic, 
         sig_level_0.01, sig_level_0.05, 
         sig_level_0.10, non_sig=sig_level_1) %>% 
  
  kable(format="latex",
        digits=2,
        row.names=F) %>% 
    cat(., file="tables/stationarity_tests.tex")


  write_csv(exploration_data,
            "intermediate_data/forecast_data.csv")

# Cointegreation ----------------------------------------------------------
  state_series <- 
    lapply(X=exploration_data$STATE_CODE %>% unique,
           FUN=function(X){
             exploration_data %>% filter(STATE_CODE==X) %>% arrange(date)
           })

  
cointegration_test <- function(state_df){ 
  

eagles_series_loc <- ifelse(state_df$is_stationary_eagles[1], 
                        10, 
                        18)

rwbb_series_loc <- ifelse(state_df$is_stationary_rwbb[1], 
                     9, 
                     19) 




rwbb.ts <- ts(state_df[[rwbb_series_loc]], 
              start=c(1950,1), frequency=12)
eagles.ts <-ts(state_df[[eagles_series_loc]], 
               start=c(1950,1), frequency=12)
rain.ts <- ts(state_df$percipitation, 
              start=c(1950,1), frequency=12)
temp.ts <- ts(state_df$temperature, 
              start=c(1950,1), frequency=12)


rwbb_coint <- dynlm(rwbb.ts 
             ~ L(rain.ts, 1:3)
             + L(temp.ts, 1:3))
rwbb_test <- 
  coeftest(rwbb_coint, vcov. = NeweyWest(rwbb_coint, prewhite = F, adjust = T))



eagles_coint <- dynlm(eagles.ts 
                      ~ L(rain.ts, 1:3)
                      + L(temp.ts, 1:3)  )

eagles_test <- coeftest(eagles_coint, vcov. = NeweyWest(eagles_coint, prewhite = F, adjust = T))

out_matrix <- matrix(nrow=6, ncol=4)
colnames(out_matrix) <- c("STATE_CODE", "var", "rwbb", "eagles")
out_matrix[1:6, 1] <- state_df$STATE_CODE %>% unique
out_matrix[1:6, 2] <- c("rain_1", "rain_2", "rain_3",
                        "temp_1", "temp_2", "temp_3")

out_matrix[1:6, 3]<- round(rwbb_test[1:6, 4], 2)
out_matrix[1:6, 4]<- round(eagles_test[1:6, 4], 2)
out_matrix

return(out_matrix)
}

check_sig <- function(p_value){
  ifelse(p_value<0.01, 0.01,
         ifelse(p_value<0.05, 0.05,
                ifelse(p_value<0.1, 0.1,
                       1)))
}

lapply(X=state_series, 
       FUN=cointegration_test) %>% 
  lapply(X=.,
         FUN=data.frame) %>% 
  bind_rows %>%
  separate(var, into=c("variable", "lag"), sep="_") %>% 
  mutate(rwbb_sig = check_sig(as.numeric(rwbb)), 
         eagles_sig=check_sig(as.numeric(eagles))) %>% 
  pivot_longer(cols=c("rwbb_sig", "eagles_sig"),
               names_to="Bird",
               values_to = "significance") %>% 
  group_by(variable,lag, Bird, significance) %>% 
  summarise(n=n()) %>% 
  ungroup %>% 
  mutate(significance=factor(significance, 
                             levels=c("0.01", "0.05", "0.1", "1")),
         Bird=bird_abb_to_name(gsub("_sig", "", Bird)),
         variable=gsub("rain", "Precp.", variable),
         variable=gsub("temp", "Temp.", variable)) %>% 
  
  ggplot +
  geom_col(aes(x=significance, y=n,
               group=Bird, color=Bird, fill=Bird),
           position="identity", alpha=0.3) +
  facet_grid(variable+lag~Bird) +
  geom_text(aes(label=n, x=significance, y=25)) +
  scale_color_manual(values=bird_colors) +
  scale_fill_manual(values=bird_colors) +
  theme_bw() +
  theme(legend.position = "none",
        strip.placement = "outside") +
  xlab("Significance Level") +
  ylab("Number of Series")

ggsave("figures/cointegration_results.jpeg", width=5, height=5)

