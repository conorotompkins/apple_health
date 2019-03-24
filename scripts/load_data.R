library(tidyverse)
library(lubridate)
library(XML)
library(janitor)
library(scales)

theme_set(theme_bw())
options(scipen = 999, digits = 4)

xml <- xmlParse("data/apple_health_export/export.xml")
df <- XML:::xmlAttrsToDataFrame(xml["//Record"]) %>% 
  filter(type == "HKQuantityTypeIdentifierDistanceWalkingRunning") %>% 
  mutate(value_char = as.character(value),
         value_num = as.numeric(value_char)) %>% 
  as_tibble() %>% 
  clean_names()
glimpse(df)


df_distance <- df %>% 
  select(type, creation_date, value_num) %>% 
  mutate(ymd_hour = ymd_h(str_sub(creation_date, 1, 14))) %>% 
  filter(value_num < 5) %>% 
  group_by(ymd_hour) %>% 
  summarize(distance = sum(value_num),
            observations = n()) %>% 
  ungroup() %>% 
  mutate(ymd = ymd(str_sub(ymd_hour, 1, 10)),
         year = year(ymd_hour),
         month = month(ymd_hour, label = TRUE),
         week = week(ymd_hour),
         wday = wday(ymd_hour, label = TRUE),
         yday = yday(ymd_hour),
         hour = hour(ymd_hour))

df_distance_day <- df_distance %>%
  group_by(ymd, year, month, wday) %>% 
  summarize(distance = sum(distance),
            observations = sum(observations))

df_distance_day <- df_distance_day %>% 
  group_by(wday) %>% 
  mutate(distance_median = median(distance)) %>% 
  ungroup() %>% 
  mutate(distance_imputed = case_when(distance > 6 ~ distance_median,
                                      distance <= 6 ~ distance),
         quincy = case_when(ymd >= "2018-04-15" ~ TRUE,
                            ymd < "2018-04-15" ~ FALSE),
         location = case_when(ymd >= "2018-10-13" ~ "Suburban",
                              ymd < "2018-10-13" ~ "Urban"),
         location = factor(location, levels = c("Urban", "Suburban")))
