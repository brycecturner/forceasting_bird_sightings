

# Options -----------------------------------------------------------------
setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# Make Exploration Data ---------------------------------------------------

source("scripts/eBird_preprocessing.R")
  # will source the following files...
    # full_noaa_weather.R
  # will write the following data sets...
    # intermediate_data/exploration_data.csv


# Describe the Raw Data ---------------------------------------------------

source("scripts/dataDescription.R")
  # will create the following plots/figures...
    # figures/national_bird_sightings.jpeg
    # figures/bird_sightings_in_select_states.jpeg
    # tables/bird_summary_stats.tex
    # figures/national_weather_averages.jpeg
    # figures/weather_in_select_states.jpeg
    # tables/weather_sum_stats.tex


# Conduct Statistical Exploration of Data ---------------------------------
source("scripts/dataExploration.R")
  # will write the following data sets...
    #intermediate_data/forecast_data.csv

  # will create the following plots/figures...
    # tables/stationarity_tests.tex
    # figures/cointegration_results.jpeg
        

# Forecasting Exercises ---------------------------------------------------

source("scripts/forecasts.R")
