

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

rm(list=c("all_sheets", "raw_data_path", "read_data"))

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


# Breed-Plot Pairs --------------------------------------------------------

plot_breed_pairs <- 
  count_data %>% 
  select(Plot_Name, AOU_Code) %>% 
  unique
