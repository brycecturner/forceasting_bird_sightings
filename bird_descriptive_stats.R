

# Setup -------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(readxl)
library(glue)
library(stargazer)
library(urca) # For Dickey-Fuller Test


setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# Notes to Self -----------------------------------------------------------
#General Idea:
# I'm going to predict each plot-species series... 
# ...then aggregate up to get a sense of what park I need to go to



# Source Cleaned Bird Data ------------------------------------------------

source("bird_data_preprocessing.R")
 # bird_data <- information about bird types
 # observation_weather_data <- weather from day of observation
 # count_data <- main data 
 # plot_breed_pairs <- df containing of each plot-breed pairing




# Total Year Coverage Hist ------------------------------------------------


hist_data <-
  count_data %>%  
  group_by(Plot_Name, AOU_Code) %>% 
  summarize(num_years = n()) 

ggplot(hist_data) +
  geom_histogram(aes(x=num_years), binwidth=1, color="black", fill="grey") +
  theme_bw() +
  xlab("Number of Years with Non-Zero Bird-Plot Observations") +
  ylab("Count")

ggsave("figures/total_hist_speciesPlot_years.jpeg")



# Using Sample Coverage Hist ----------------------------------------------
sample_series <- 
  hist_data %>% 
  filter(num_years>=7)

num_birds <-  sample_series$AOU_Code %>% unique %>% length
num_plots <- sample_series$Plot_Name %>% unique %>% length
num_series <- sample_series$Plot_Name %>% length

ggplot(sample_series) +
  geom_histogram(aes(x=num_years), binwidth=1, color="black", fill="grey") +
  theme_bw() +
  xlab("Number of Years with Non-Zero Bird-Plot Observations") +
  ylab("Count") +
  ylim(0, 1000) +
  annotate("text", 
           label=glue("Number of Distinct Species: {num_birds}"),
           x=11.5, 
           y=1000,
           hjust=1) +
  annotate("text", 
           label=glue("Number of Distinct Plots: {num_plots}"),
           x=11.5, 
           y=920,
           hjust=1) +
  annotate("text", 
           label=glue("Number of Distinct Series: {num_series}"),
           x=11.5, 
           y=840,
           hjust=1) 
  
ggsave("figures/usingSample_hist_speciesPlot_years.jpeg")


# Generate Full Using Sample ----------------------------------------------
sample_series <- 
  sample_series %>% 
  select(Plot_Name, AOU_Code) %>% 
  distinct

base_df <- data.frame()

for (row_num in seq(1, nrow(sample_series))){
  
  if (row_num%%100==0){
    print(row_num)
  }
  
  tmp_df  <- 
    data.frame(Year=seq(2007, 2017, 1), 
                Plot_Name=sample_series[row_num,1],
                AOU_Code=sample_series[row_num,2]) %>% 
    left_join(count_data %>% select(-Park_Code),
              by = c("Year", "Plot_Name", "AOU_Code")) %>% 
    mutate_all(~replace(., is.na(.), 0)) 
  
  base_df <- rbind(base_df, tmp_df)
}



# Full Sample Descriptive Stats -------------------------------------------
descriptive_stats <- 
  base_df %>% 
  group_by(Year) %>% 
  summarize(sum_mean = mean(sum),
            sum_p90 = quantile(sum, probs=0.90),
            sum_p10 = quantile(sum, probs=0.10),
            max_mean = mean(max),
            max_p90 = quantile(max, probs=0.90),
            max_p10 = quantile(max, probs=0.10),
            mean_mean = mean(mean),
            mean_p90 = quantile(mean, probs=0.90),
            mean_p10 = quantile(mean, probs=0.10)) %>% 
  pivot_longer(cols=c(starts_with("sum"), 
                      starts_with("max"), 
                      starts_with("mean")),
               names_sep = "_",
               names_to=c("series", "statistic")
  ) %>% 
  mutate(statistic=factor(statistic,
                          levels=c("p90", "mean", "p10")),
         series=factor(series, 
                       levels=c("mean", "max", "sum")),
         Year=as.integer(Year))


ggplot(descriptive_stats) + 
  geom_line(aes(x=Year, y=value, group=statistic, color=statistic)) +
  facet_wrap(~series) +
  theme_bw() +
  ylab("") +
  scale_x_continuous(breaks=seq(2007, 2018, 2)) +
  theme(axis.text.x=element_text(angle=90)) +
  scale_color_discrete(name="Summary Statistic",
                       labels = c("90th Ptile", "Mean", "10th Ptile")) +
  theme(legend.position = "left") +
  ggtitle("Series Type")



ggsave("figures/usingSample_summaryStats.jpeg")


  