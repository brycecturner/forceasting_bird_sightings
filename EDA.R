

# Setup -------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(readxl)
library(urca) # For Dickey-Fuller Test


setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# Notes to Self -----------------------------------------------------------
#General Idea:
# I'm going to predict each bird-plot-pair... 
# ...then aggregate up to get a sense of what park I need to go to



# Source Cleaned Bird Data ------------------------------------------------

source("bird_data_preprocessing.R")
 # bird_data <- information about bird types
 # observation_weather_data <- weather from day of observation
 # count_data <- main data 
 # plot_breed_pairs <- df containing of each plot-breed pairing



# Year Coverage Hist ------------------------------------------------------
hist_data <-
  count_data %>%  
  group_by(Plot_Name, AOU_Code) %>% 
  summarize(num_years = n()) %>% 
  filter(num_years > 7)

ggplot(hist_data) +
  geom_histogram(aes(x=num_years), binwidth=1, color="black", fill="grey") +
  theme_bw() +
  xlab("Number of Years with Non-Zero Bird-Plot Observations") +
  ylab("Count")

# Stationarity Testing ----------------------------------------------------
using_plot <- "ANTI-0007"
using_bird <- "AMCR"

base_df <- 
  data.frame(Year=seq(2007, 2017, 1), 
             Plot_Name=using_plot,
             AOU_Code=using_bird) %>% 
  left_join(count_data %>% 
              filter(Plot_Name==using_plot,
                     AOU_Code==using_bird) %>% 
              select(-Park_Code)) %>% 
  mutate_all(~replace(., is.na(.), 0)) 
 
  df_test_results <- ur.df(base_df$mean, type="trend")

  