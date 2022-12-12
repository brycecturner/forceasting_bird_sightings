

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

source("scripts/arima_models.R")
  # will write the following data sets...
    # intermediate_data/arima_results.csv

  # will create the following plots/figures...
    # tables/arima_rmse_results.tex
    # figures/hist_of_rmse_increase_arima.jpeg
    # figures/full_rmse_reporting_arima.jpeg

source("scripts/structural_models.R")
  # will write the following data sets...
    # intermediate_data/strct_results.csv

  # will create the following plots/figures...
    # tables/strct_rmse_results.tex
    # figures/hist_of_rmse_increase_strct.jpeg
    # figures/full_rmse_reporting_strct.jpeg


# Final Forecast ----------------------------------------------------------

source("scripts/final_forecast.R")
  # will create the following plots/figures...
    # figures/rwbb_map.jpeg
    # figures/eagles_map.jpeg
