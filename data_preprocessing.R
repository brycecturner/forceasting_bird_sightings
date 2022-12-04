

# Setup -------------------------------------------------------------------


library(tidyverse)
library(readxl)

setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")

# Data Ingest -------------------------------------------------------------

raw_data_path <- "input/NCRN LAND Bird Monitoring Data 2007 - 2017_Public.xlsx"
all_sheets <- excel_sheets(raw_data_path)


read_data <- function(excel_file, sheet) {
  
  raw <-  read_excel(excel_file,
                     sheet = sheet)
  return(raw)
 
}

full_data <- 
  mapply(read_data, 
         sheet=all_sheets, 
         MoreArgs = list(excel_file=raw_data_path),
         SIMPLIFY = FALSE) %>% 
  bind_rows 


# Create Bird Data --------------------------------------------------------

bird_data <- 
  full_data %>% 
  select(Common_Name,
         Scientific_Name,
         AcceptedTSN,
         NPSTaxonCode,
         AOU_Code) %>% 
  unique


# Create Weather Data -----------------------------------------------------
observation_weather_data <- 
  full_data %>% 
  select(Admin_Unit_Code,
         Date, 
         Temperature, 
         Humidity, 
         Sky, 
         Wind) %>% 
  unique %>% 
  mutate(Sky=factor(Sky),
         Wind=factor(Wind,
                    levels = c("Calm (< 1 mph) smoke rises vertically",
                               "Light air movement (1-3 mph) smoke drifts",
                               "Light breeze (4-7 mph) wind felt on face",
                               "Gentle breeze (8-12 mph), leaves in motion",
                               "Moderate breeze (13-18 mph), moves small branches",
                               "Fresh breeze (19-24 mph), small trees sway" )))

# Create Count Data -------------------------------------------------------

#Doing this flip-thing to fill in the missing observations
#  in so doing, this shows that there are 5 or so plots...
#  that received more than 2 visits
# Thus, to calculate the MEAN, ...
#   I will use the sum and divide by two 

count_data <- 
  full_data %>% 
  
  group_by(Plot_Name ,
           Year, 
           Visit,
           AOU_Code) %>% 
  summarise(count = n()) %>% 
  pivot_wider(id_cols = c(Plot_Name , AOU_Code, Year),
              names_from=Visit,
              names_prefix="count_",
              values_from = count,
              values_fill=0) %>% 
  pivot_longer(cols = starts_with("count_"),
               names_prefix="count_",
               names_to="visit_number",
               values_to="count") %>% 
  group_by(Plot_Name, AOU_Code, Year) %>% 
  summarise(sum=sum(count),
            max=max(count)) %>% 
  mutate(mean = sum/2) %>% 
  mutate(Park_Code=sub("-[0-9]+", "", Plot_Name),
         AOU_Code=factor(AOU_Code))



# Weather Data ------------------------------------------------------------

station_zip_crosswalk <- 
  read_excel(path="Input/Park_Info.xlsx",
             sheet="StationZipCrosswalk")

park_info <- 
  read_excel(path="Input/Park_Info.xlsx",
             sheet="LocationInformation")

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
  mutate_all(vars(starts_with("Year")) ,~replace(., is.na(.), mean(., na.rm=TRUE))) %>% 
  pivot_longer(cols=starts_with("Year"), 
               names_to="Year",
               names_prefix = "Year",
               values_to="rainfall")
  
  
ggplot(annual_rainfall, aes(x=Year, y=rainfall, group=Park_Zip)) +
  geom_line(aes(color=Park_Zip, linetype=Park_Zip)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

ggsave("figures/annual_rainfall_by_zipcode.jpeg")
  




