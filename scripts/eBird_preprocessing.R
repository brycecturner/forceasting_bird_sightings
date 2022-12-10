


# Setup -------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(lubridate)
library(parallel)

setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")
source("scripts/full_noaa_weather.R")

# Bald Eagle Data ---------------------------------------------------------
eagles <-
  "input/ebd_baleag_190001_201401_relOct-2022/ebd_baleag_190001_201401_relOct-2022.txt" %>% 
  read_delim(.) %>% 
  filter(COUNTRY=="United States") 

colnames(eagles) <- gsub(" ", "_", colnames(eagles))

eagles <- 
  eagles %>% 
  mutate(STATE_CODE=gsub("US-", "", STATE_CODE)) %>% 
  filter(STATE_CODE %in% region_codes$region_name[1:48]) %>% 
  mutate(date=floor_date(OBSERVATION_DATE, "month")) %>% 
  group_by(STATE_CODE, date) %>% 
  summarize(number_of_eagles=sum(as.numeric(OBSERVATION_COUNT), na.rm = T)) %>%  
  ungroup


# RWBB Data ---------------------------------------------------------------
rwbb_raw <- 
  "input/ebd_rewbla_190001_201401_relOct-2022/ebd_rewbla_190001_201401_relOct-2022.txt" %>% 
  read_delim(.) %>% 
  filter(COUNTRY=="United States") 

colnames(rwbb_raw) <- gsub(" ", "_", colnames(rwbb_raw))

rwbb_raw <- 
  rwbb_raw %>% 
  mutate(STATE_CODE=gsub("US-", "", STATE_CODE))

state_dfs <- 
  lapply(X=region_codes$region_name[1:48],
         FUN=function(X){
           rwbb_raw %>% filter(STATE_CODE==X)
          })

exploration_data <- 
  mcmapply(X=state_dfs,
         FUN=function(X){
           X %>% 
             mutate(date=floor_date(OBSERVATION_DATE, "month")) %>% 
             group_by(date) %>% 
             summarize(number_of_rwbb=sum(as.numeric(OBSERVATION_COUNT), na.rm = T)) %>% 
             mutate(STATE_CODE=X$STATE_CODE %>% unique) %>% 
             ungroup
         },
         mc.cores=1, 
         SIMPLIFY = FALSE) %>% 
  bind_rows  %>% 
  full_join(y=eagles,
             by=c("STATE_CODE", "date")) %>% 
  right_join(y=all_weather %>% 
                filter(year>=1950 & year<=2014) %>% 
                select(date,STATE_CODE=region_name,
                       cooling_def_days, heating_deg_days, 
                       percipitation, PDSI, PHDI, PMDI, temperature),
            by=c("STATE_CODE", "date")) %>% 
  replace_na(list(number_of_rwbb=0,
                  number_of_eagles=0)) %>% 
  filter(STATE_CODE %in% region_codes$region_name[1:48])   %>% 
  filter(date<=as.Date("2014-01-01"))      

write_csv(exploration_data, 
          "intermediate_data/exploration_data.csv")