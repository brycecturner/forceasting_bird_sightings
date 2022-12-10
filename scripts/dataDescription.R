
# Setup -------------------------------------------------------------------

rm(list=ls())


library(tidyverse)
library(ggpubr)
library(knitr)


setwd("/Users/Bryce Turner/Documents/GitHub/bird_sightings_dmv/")
# QOL Functions -----------------------------------------------------------
bird_abb_to_name <- function(bird_abb){
  name <- ifelse(bird_abb=="eagles", "Bald Eagle", "Red Winged Black Bird")
  return(name)
}

state_abb_to_name <- function(state_abb){
  return(state.name[match(state_abb, state.abb)])
}


# Pull Analysis Data ------------------------------------------------------

exploration_data <- 
  read_csv("intermediate_data/exploration_data.csv")

# Plot Options ------------------------------------------------------------
source("scripts/plot_options.R")
# National Bird Sightings -------------------------------------------------

exploration_data %>% 
  group_by(date) %>% 
  summarise(number_of_rwbb=sum(number_of_rwbb),
            number_of_eagles=sum(number_of_eagles)) %>% 
  pivot_longer(cols=starts_with("number"),
               values_to="count",
               names_to="Bird",
               names_prefix = "number_of_") %>% 
  mutate(count=log(count+1),
         Bird=bird_abb_to_name(Bird)) %>% 
  
  ggplot +
  geom_line(aes(x=date, y=count, group=Bird, color=Bird)) +
  theme_bw() +
  scale_color_manual(values=bird_colors) +
  ylab("Log Count")+
  xlab("Date") + 
  theme(legend.position="top",
        legend.title = element_blank())

ggsave("figures/national_bird_sightings.jpeg")



# Bird Sightings in Select States -----------------------------------------
select_states <- c("CA", "NY", "TX", "FL")

exploration_data %>% 
  select(STATE_CODE, date, "number_of_rwbb", "number_of_eagles") %>% 
  pivot_longer(cols=starts_with("number"),
               values_to="count",
               names_to="Bird",
               names_prefix = "number_of_") %>% 
  mutate(count=log(count+1),
         Bird=bird_abb_to_name(Bird),
         State=state_abb_to_name(STATE_CODE)) %>%
  filter(STATE_CODE %in% select_states) %>% 
  
  
  ggplot(aes(x=date)) +
  geom_line(aes(y=count, group=Bird, color=Bird)) +
  facet_wrap(~State)+
  theme_bw() +
  scale_color_manual(values=bird_colors) +
  ylab("Log Count")+
  xlab("Date")+ 
  theme(legend.position="top",
        legend.title = element_blank())


ggsave("figures/bird_sightings_in_select_states.jpeg")

# Bird Sighting Summary Stats ---------------------------------------------
exploration_data %>% 
  select(date, number_of_eagles, number_of_rwbb) %>% 
  pivot_longer(cols=starts_with("number"),
               names_to = "Bird", 
               names_prefix="number_of_",
               values_to="Count") %>% 
  group_by(Bird) %>% 
  summarize(mean=mean(Count),
            median=median(Count),
            p25=quantile(Count, .25),
            p75=quantile(Count, .75),
            max=max(Count)
            ) %>% 
  mutate(Bird=bird_abb_to_name(Bird)) %>% 
  
  kable(format="latex",
        digits=2,
        row.names=F) %>% 
  cat(., file="tables/bird_summary_stats.tex")



# National Weather Trends -------------------------------------------------

exploration_data %>% 
  group_by(date) %>% 
  summarise(temperature=mean(temperature),
            percipitation=mean(percipitation)) %>% 
  pivot_longer(c("temperature", "percipitation"),
               names_to="Weather",
               values_to="National_Average") %>% 
  
  ggplot +
  geom_line(aes(x=date, y=National_Average, color=Weather)) +
  facet_wrap(~Weather, scales = "free", nrow = 2) +
  scale_color_manual(values=weather_colors) +
  xlab("Date")+
  ylab("National Average") +
  theme_bw() + 
  theme(legend.position="none",
        legend.title = element_blank())

ggsave("figures/national_weather_averages.jpeg")


# Weather Trends in Select States -----------------------------------------

select_rain_plot <- 
  exploration_data %>% 
  filter(STATE_CODE %in% select_states) %>% 
  select(STATE_CODE, date, percipitation) %>%
  
  ggplot +
  geom_line(aes(x=date, y=percipitation), color=rain_color) +
  scale_color_manual(values=rain_color) +
  facet_wrap(~STATE_CODE, nrow=1) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        legend.position = "none") +
  ylab("Percipitation")

select_temp_plot <- 
  exploration_data %>% 
  filter(STATE_CODE %in% select_states) %>% 
  select(STATE_CODE, date, temperature) %>%
  
  ggplot +
  geom_line(aes(x=date, y=temperature), color=temp_color) +
  facet_wrap(~STATE_CODE, nrow=1) +
  xlab("Date") +
  ylab("Temp") +
  theme_bw()+
  theme(legend.position = "none") 

ggarrange(select_rain_plot, select_temp_plot,
          ncol=1, nrow=2)

ggsave("figures/weather_in_select_states.jpeg")

# Weather Summary Stats ---------------------------------------------------
exploration_data %>% 
  select(date, percipitation, temperature) %>% 
  pivot_longer(cols=c("percipitation", "temperature"),
               names_to = "Weather", 
               values_to="Count") %>% 
  group_by(Weather) %>% 
  summarize(mean=mean(Count),
            median=median(Count),
            p25=quantile(Count, .25),
            p75=quantile(Count, .75),
            max=max(Count)
  ) %>% 
  mutate(Weather = str_to_title(Weather)) %>% 
  
  kable(format="latex",
        digits=2,
        row.names=F) %>% 
  cat(., file="tables/weather_sum_stats.tex")

