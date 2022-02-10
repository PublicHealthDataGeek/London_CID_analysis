################################################################################
#                     Summary statistics for Paper 1                           #
#                                Part 3                                        #
#                     ##############################                           #
#                                                                              #
# This code generates the summary statistics used in Paper 1                   #
# 1) Calculate total number of assets and total length                         #
# 2) Calculate dates and year of surveying                                     #
# 3) URL NAs
# 4) Summary of 5 safety datasets (counts and % and then lengths % for CLT too)
# 5) Comparison of variables of on v off road infrastructure


# load packages
library(tidyverse)
library(sf)
library(summarytools)
library(units)

# load datasets
c_asl = readRDS(file = "data/cleansed_asl_24_01_2022")
c_crossings = readRDS(file = "data/cleansed_crossings_24_01_2022")
c_cyclelanetrack = readRDS(file = "data/cleansed_cycle_lane_track_24_01_2022")
c_signals = readRDS(file = "data/cleansed_signals_24_01_2022")
c_trafficcalming = readRDS(file = "data/cleansed_trafficcalming_24_01_2022")
c_restrictedroutes = readRDS(file = "data/cleansed_restricted_route_24_01_2022")
c_restrictedpoints = readRDS(file = "data/cleansed_restrictedpoints_24_01_2022")
c_signage = readRDS(file = "data/cleansed_signage_24_01_2022")
c_parking = readRDS(file = "data/cleansed_parking_24_01_2022")

# # These datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
# CID_borough_count = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_count_by_borough")
# CID_borough_length = readRDS(file = "/home/bananafan/Documents/PhD/Paper1/data/CID_length_by_borough")
# 
# ################################################################################
# # 1) Calculate total number of assets and total length                         #
# 
# 
# totals_counts = CID_borough_count %>%
#   select(-c("BOROUGH"))
# colSums(totals_counts)
# 
# totals_lengths = CID_borough_length %>%
#   select(-c("BOROUGH"))
# colSums(totals_lengths)

################################################################################
# 2) Calculate dates/years of surveying

survey_dates = c_asl %>% st_drop_geometry() %>%
  select(c("FEATURE_ID", "SVDATE")) %>%
  rbind(c_crossings %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_cyclelanetrack %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_parking %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_restrictedpoints %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_restrictedroutes %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_signage %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_signals %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE"))) %>%
  rbind(c_trafficcalming %>% st_drop_geometry() %>%
          select(c("FEATURE_ID", "SVDATE")))
dim(survey_dates)  # [1]234251      3

# min/max survey date
min(survey_dates$SVDATE) #"2017-01-06"
max(survey_dates$SVDATE) "6482-04-01"

# remove observation with inappropriate year
survey_dates = survey_dates[survey_dates$FEATURE_ID != "RWG999394",]  # n = 234950
max(survey_dates$SVDATE) #"2019-09-02"

# obtain year of survey
survey_dates = survey_dates %>%
  mutate(year = lubridate::year(SVDATE))

#summarise
survey_years = survey_dates %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/233950*100), digits = 2))
# year  count percentage
# <dbl>  <int>      <dbl>
# 1  2017 177692      76.0 
# 2  2018  56541      24.2 
# 3  2019     17       0.01


################################################################################
# 3) Examine URL NAs

URL_NAs = c_asl %>% st_drop_geometry() %>%
  select(c("FEATURE_ID", "PHOTO1_URL", "PHOTO2_URL")) %>%
  mutate(type = "asl") %>%
  rbind(c_crossings %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "crossings")) %>%
  rbind(c_cyclelanetrack %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "cyclelanetrack")) %>%
  rbind(c_parking %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "parking")) %>%
  rbind(c_restrictedpoints %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "restrictedpoints")) %>%
  rbind(c_restrictedroutes %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "restrictedroutes")) %>%
  rbind(c_signage %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "signage")) %>%
  rbind(c_signals %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "signals")) %>%
  rbind(c_trafficcalming %>% st_drop_geometry() %>%
          select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
          mutate(type = "trafficcalming"))
dim(URL_NAs)  # [1] 234251      4

# drop observations that have same original FEATURE_ID (ie drop the ones I created by spatial splitting on boundaries)
original_URL_NAs = URL_NAs %>%
  filter(!str_detect(FEATURE_ID, "_ 2|_ 3|_2")) # n = 233623 observations
# ie the 233951 - the extra 3 crossings, 339 clt and 21 restricted routes

# Checking total of clt, rr and crossings created 
# crossings_created_n = c_crossings %>% st_drop_geometry() %>%
#   select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
#   filter(stringr::str_detect(FEATURE_ID, '_2')) # n = 3
# 
# clt_created_n = c_cyclelanetrack %>% st_drop_geometry() %>%
#   select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
#   filter(str_detect(FEATURE_ID, "_ 2|_ 3")) # n= 339
# 
# rr_created_n = c_restrictedroutes %>% st_drop_geometry() %>%
#   select(c("FEATURE_ID","PHOTO1_URL", "PHOTO2_URL")) %>%
#   filter(str_detect(FEATURE_ID, "_ 2|_ 3")) # n= 21

# calculate number of NAs by type
count_photo1 =  original_URL_NAs %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  filter(str_detect(PHOTO1_URL, "no_asset_photo.png")) %>%
  mutate(photo_1_na_count = n()) %>%
  group_by(type)

totals = original_URL_NAs %>%
  group_by(type) %>%
  summarise(total = n())
photo_1_na_count = original_URL_NAs %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  filter(str_detect(PHOTO1_URL, "no_asset_photo.png")) %>%
  summarise(photo_1_na_count = n()) 
photo_2_na_count = original_URL_NAs %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  filter(str_detect(PHOTO2_URL, "no_asset_photo.png")) %>%
  summarise(photo_2_na_count = n()) 

summary_URL_NAs = left_join(totals, photo_1_na_count) %>%
  left_join(photo_2_na_count) %>%
  mutate(prop_P1_NA = round((photo_1_na_count/total*100), digit = 1)) %>%
  mutate(prop_P2_NA = round((photo_2_na_count/total*100), digit = 1)) %>%
  mutate(total_2 = 2*total) %>%
  mutate(overall_prop_NA = round(((photo_1_na_count + photo_2_na_count)/total_2 * 100), digit = 1))

# type          total photo_1_na_count photo_2_na_count prop_P1_NA prop_P2_NA total_2 overall_prop_NA
# <chr>         <int>            <int>            <int>      <dbl>      <dbl>   <dbl>           <dbl
# 1 asl            3775               48               51        1.3        1.4    7550             1.3
# 2 crossings      1687               31               32        1.8        1.9    3374             1.9
# 3 cyclelanetr…  24976              588              605        2.4        2.4   49952             2.4
# 4 parking       23758              299              298        1.3        1.3   47516             1.3
# 5 restrictedp…    180               12               12        6.7        6.7     360             6.7
# 6 restrictedr…   1378               71               52        5.2        3.8    2756             4.5
# 7 signage      118826             1347             1336        1.1        1.1  237652             1.1
# 8 signals         443                8                8        1.8        1.8     886             1.8
# 9 trafficcalm…  58565              805              810        1.4        1.4  117130             1.4

overall_prop = (sum(summary_URL_NAs$photo_1_na_count) + sum(summary_URL_NAs$photo_2_na_count)) / sum(summary_URL_NAs$total_2) *100
p1 = sum(summary_URL_NAs$photo_1_na_count)
