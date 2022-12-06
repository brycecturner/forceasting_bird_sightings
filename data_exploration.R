

# Setup -------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(readxl)
library(glue)
library(stargazer)
library(urca) # For Dickey-Fuller Test


setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")

options(scipen=999) #to remove scientific notation
# Source Rest of Pipeline -------------------------------------------------
source("annual_weather_data.R")
source("bird_descriptive_stats.R")


# Stationarity of Bird Data -----------------------------------------------

stationarity_tests <- data.frame()

for (using_id in base_df$series_id %>% unique){
  
  if(using_id%%100==0){
    print(using_id)
  }
  
  using_series <- 
    base_df %>% 
    filter(series_id==using_id)
  
  
  tryCatch(
      {mean_test <- ur.df(using_series$mean,lags=3, selectlags = "AIC", type="drift")
       mean_stat <- mean_test@teststat[1]
       },
    error=function(cond){
      message(using_id)
      message("error in mean series")
      mean_stat <- NA
    })

  tryCatch(
    {max_test <- ur.df(using_series$max,lags=3, selectlags = "AIC", type="drift")
    max_stat <- max_test@teststat[1]
    },
    error=function(cond){
      message(using_id)
      message("error in max series")
      max_stat <- NA
    })

  tryCatch(
    {sum_test <- ur.df(using_series$sum,lags=3, selectlags = "AIC", type="drift")
    sum_stat <- sum_test@teststat[1]
    },
    error=function(cond){
      message(using_id)
      message("error in sum series")
      sum_stat <- NA
    })
  

  tmp_df <- 
    data.frame(series_id=using_id,
               mean_stat=mean_stat,
               max_stat=max_stat,
               sum_stat=sum_stat
               )
  
  stationarity_tests <- bind_rows(stationarity_tests, tmp_df)
}


stationarity_tests %>% 
  pivot_longer(cols=ends_with("stat"),
               names_to=c("Series","Drop"),
               names_sep="_",
               values_to="Test_Statistic") %>% 
  mutate(Test_Statistic=ifelse(abs(Test_Statistic)>4, 4, abs(Test_Statistic))) %>% 
  filter(Series!="sum") %>% 
  
  
  ggplot +
  geom_histogram(aes(x=Test_Statistic, group=Series, color=Series, fill=Series), 
                 alpha=0.3,
                 position = "identity") +
  theme_bw() +
  xlab("Value of Stationarity Test Statistic")+
  ylab("Count")

# Conintegration: Bird Sightings and Annual Rain --------------------------
eq01 = dynlm(d(morts) 
             ~ L(d(treas), 1:3) 
             + L(d(morts), 1:3) 
             + L(ec,1)
)
coeftest(eq01, vcov. = NeweyWest(eq01, prewhite = F, adjust = T))

# Cointegration: Bird Sightings and Day-of-Sighting Data ------------------


