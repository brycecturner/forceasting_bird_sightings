

# Setup -------------------------------------------------------------------


library(tidyverse)
library(readxl)

setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# Read crosswalk ----------------------------------------------------------

station_zip_crosswalk <- 
  read_excel(path="Input/Park_Info.xlsx",
             sheet="StationZipCrosswalk")


# Prep Annual Weather Data ------------------------------------------------

# Only Keeping the rainfall data if there is sufficient data points
annual_rainfall <- 
  read_csv("Input/Weather_Data.csv",
           show_col_types = FALSE) %>%
  mutate(Year=str_sub(DATE,start=-4)) %>% 
  mutate(completeness_check=ifelse(is.na(PRCP), 0,1)) %>% 
  group_by(STATION, Year) %>% 
  summarize(rainfall=sum(PRCP, na.rm=TRUE),
            completeness_check=mean(completeness_check)) %>%
  filter(completeness_check>0.85) %>% 
  
  left_join(y=station_zip_crosswalk,
            by = "STATION") %>%
  group_by(Year, Park_Zip) %>% 
  summarize(rainfall=mean(rainfall)) %>% 
  ungroup %>% 
  mutate(Year=as.numeric(Year),
         Park_Zip=factor(Park_Zip)) %>% 
  pivot_wider(id_cols="Park_Zip",
              names_from=Year,
              names_prefix="Year",
              values_from=rainfall) %>% 
  
  #Replacing missing values with the mean from the observed data
  mutate_all(~replace(., is.na(.), mean(., na.rm=TRUE))) %>% 
  pivot_longer(cols=starts_with("Year"), 
               names_to="Year",
               names_prefix = "Year",
               values_to="rainfall")


# Save Resulting Weather Data ---------------------------------------------

write_csv(annual_rainfall, "intermediate_data/annual_rainfall.csv")


# Annual Rainfall Plot ----------------------------------------------------



ggplot(annual_rainfall, aes(x=Year, y=rainfall, group=Park_Zip)) +
  geom_line(aes(color=Park_Zip, linetype=Park_Zip)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

ggsave("figures/annual_rainfall_by_zipcode.jpeg")
