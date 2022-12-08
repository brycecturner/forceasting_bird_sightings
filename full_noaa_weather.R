

# Setup -------------------------------------------------------------------


library(tidyverse)

setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")


# based on https://www.ncei.noaa.gov/pub/data/cirs/drd/state.README


# Prep Adjoining Data -----------------------------------------------------

weather_codes <- 
  data.frame(element_code=c("01", "02", "03", "04", "05", "06", "07", "08"),
             element_name=c("percipitation", "temperature", "heating_deg_days", 
                            "cooling_def_days", "PDSI", "PHDI", "ZNDX", "PMDI"))

region_codes <- 
  data.frame(
    region_code=
      c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010",
        "011", "012", "013", "014", "015", "016", "017", "018", "019", "020",
        "021", "022", "023", "024", "025", "026", "027", "028", "029", "030",
        "031", "032", "033", "034", "035", "036", "037", "038", "039", "040",
        "041", "042", "043", "044", "045", "046", "047", "048", 
        "101", "102", "103", "104", "105", "106", "107", "108", "109", "110"),
    region_name=c(
      "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", "IN", 
      "IO", "KA", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
      "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PE",
      "RI", "SC", "SD", "TE", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
      "Northeast_Region", "East_North_Central_Region", "Central_Region",
      "Southeast_Region", "West_North_Central_Region", "South_Region", 
      "Southwest_Region", "Northwest_Region", "West_Region", "National"),
    region_type=c(
      "state", "state", "state", "state", "state", "state", "state", "state",
      "state", "state", "state", "state", "state", "state", "state", "state",
      "state", "state", "state", "state", "state", "state", "state", "state", 
      "state", "state", "state", "state", "state", "state", "state", "state",
      "state", "state", "state", "state", "state", "state", "state", "state",
      "state", "state", "state", "state", "state", "state", "state", "state",
      "region", "region", "region", "region", "region", "region", "region", 
      "region", "region", "national")
  )

# Read Weather Data -------------------------------------------------------
all_weather_files <- c("drd964x.cddst.txt",
                       "drd964x.hddst.txt",
                       "drd964x.pcpst.txt",
                       "drd964x.pdsist.txt",
                       "drd964x.phdist.txt",
                       "drd964x.pmdist.txt",
                       "drd964x.tmpst.txt",
                       "drd964x.zndxst.txt") %>% 
  paste0("https://www.ncei.noaa.gov/pub/data/cirs/drd/", .)



read_noaa_weather_data <- function(file_string){
  tmp <- read_lines(file_string) %>% 
    gsub("\\s+", "\t", .) %>% 
    gsub("\t$", "", .)
  
  clean_df <- read_table(tmp,
                     col_names = c("metadata", 
                                   "Jan",  "Feb", "Mar", 
                                   "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", 
                                   "Oct", "Nov", "Dec")) %>% 
    mutate(region_code=substr(metadata, 1,3),
           division=substr(metadata, 4, 4),
           element_code=substr(metadata, 5, 6),
           year=substr(metadata, 7, 10)) %>% 
    select(-division) %>% 
    pivot_longer(cols=c("Jan",  "Feb", "Mar", 
                        "Apr", "May", "Jun", 
                        "Jul", "Aug", "Sep", 
                        "Oct", "Nov", "Dec"),
                 names_to="month")
  
  return(clean_df)
  
}


all_weather <- 
  lapply(X = all_weather_files,
         FUN=read_noaa_weather_data) %>% 
  bind_rows %>% 
  left_join(y=weather_codes,
            by="element_code")  %>% 
  pivot_wider(id_cols=c("region_code", "year", "month"),
              names_from = "element_name",
              values_from = "value") %>% 
  mutate(date=as.Date(paste0("01-", month, "-", year),
                      format="%d-%b-%Y")) %>% 
  left_join(y=region_codes,
            by="region_code") 



