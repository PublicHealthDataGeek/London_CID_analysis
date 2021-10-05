################################################################################
#                                                                              #
#                     Summary statistics for Paper 1                           #
#                                 Part 2                                       #
#                     ##############################                           #
#                                                                              #
# This code generates the summary statistics used in Paper 1                   #
# 1) Detailed examination of variables for each of the 5 safety datasets with 
#     cross tabs
# 2) Summary statistics for Borough level safety infrastructure
# 3) Borough level data for safety infrastructure




# load packages
library(tidyverse)
library(sf)
library(summarytools)
library(units)

# load datasets
# These datasets were downloaded from TFL 25th February 2021 and data cleansed
c_asl = readRDS(file = "data/cleansed_asl")
c_crossings = readRDS(file = "data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "data/cleansed_cycle_lane_track")
c_signals = readRDS(file = "data/cleansed_signals")
c_trafficcalming = readRDS(file = "data/cleansed_trafficcalming")

################################################################################
#                     
#                             ASL (n = 3775)
#
################################################################################
c_asl %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

# ASL_FDR       1. FALSE                        1992 (52.8%)               
# [factor]      2. TRUE                         1783 (47.2%)        
# 
# ASL_FDRLFT    1. FALSE                        2080 (55.1%)              
# [factor]      2. TRUE                         1695 (44.9%)          
# 
# ASL_FDCENT    1. FALSE                        3697 (97.9%)                
# [factor]      2. TRUE                           78 ( 2.1%)               
# 
# ASL_FDRIGH    1. FALSE                        3748 (99.3%)                
# [factor]      2. TRUE                           27 ( 0.7%)                    
# 
# ASL_SHARED    1. FALSE                        3768 (99.8%)                 
# [factor]      2. TRUE                            7 ( 0.2%)                            
# 
# ASL_COLOUR    1. BLUE                           85 ( 2.3%)                                
# [factor]      2. BUFF/YELLOW                    21 ( 0.6%)                                   
#               3. GREEN                         889 (23.5%)                                              
#               4. NONE                         2713 (71.9%)                                   
#               5. OTHER                           4 ( 0.1%)                                                    
#               6. RED                            63 ( 1.7%)                                                    
       

# Are these unique signals or do some signals have more than one characteristic? 
print(ctable(x = c_asl$ASL_FDR, y = c_asl$ASL_FDRLFT))

# some definately have more than one characteristic so investigate below

# # Convert Factors to numeric (converts all False to 1 and True to 2)
asl_numeric = c_asl %>%
  mutate(ASL_FDR_NUMERIC = as.numeric(c_asl$ASL_FDR)) %>%
  mutate(ASL_FDRLFT_NUMERIC = as.numeric(c_asl$ASL_FDRLFT)) %>%
  mutate(ASL_FDCENT_NUMERIC = as.numeric(c_asl$ASL_FDCENT)) %>%
  mutate(ASL_FDRIGH_NUMERIC = as.numeric(c_asl$ASL_FDRIGH)) %>%
  mutate(ASL_SHARED_NUMERIC = as.numeric(c_asl$ASL_SHARED)) %>%
  mutate(ASL_COLOUR_NUMERIC = as.numeric(c_asl$ASL_COLOUR))

# Convert 1(false) to 0 and 2(true) to 1
asl_numeric$ASL_FDR_NUMERIC = ifelse(asl_numeric$ASL_FDR_NUMERIC == 1, 0, 1)
asl_numeric$ASL_FDRLFT_NUMERIC = ifelse(asl_numeric$ASL_FDRLFT_NUMERIC == 1, 0, 1)
asl_numeric$ASL_FDCENT_NUMERIC = ifelse(asl_numeric$ASL_FDCENT_NUMERIC == 1, 0, 1)
asl_numeric$ASL_FDRIGH_NUMERIC = ifelse(asl_numeric$ASL_FDRIGH_NUMERIC == 1, 0, 1)
asl_numeric$ASL_SHARED_NUMERIC = ifelse(asl_numeric$ASL_SHARED_NUMERIC == 1, 0, 1)
asl_numeric$ASL_COLOUR_NUMERIC = ifelse(asl_numeric$ASL_COLOUR_NUMERIC == 4, 0, 1) # if coded 4 (no colour) then 0 so coloured = 1

# # # Check now gives the count that I expect (sum will count all the ones)  -YES THEY DO
# sum(asl_numeric$ASL_FDR_NUMERIC) # n = 1783
# sum(asl_numeric$ASL_FDRLFT_NUMERIC) # n = 1695
# sum(asl_numeric$ASL_FDCENT_NUMERIC) # n = 78
# sum(asl_numeric$ASL_FDRIGH_NUMERIC) # n = 27
# sum(asl_numeric$ASL_SHARED_NUMERIC) # n = 7
# sum(asl_numeric$ASL_COLOUR_NUMERIC) # n = 1062

# # Recode to give weighted value with so can distinguish between different characteristics
asl_numeric$ASL_FDR__weight = ifelse(asl_numeric$ASL_FDR_NUMERIC == 1, 10000, 0)
asl_numeric$ASL_FDRLFT_weight = ifelse(asl_numeric$ASL_FDRLFT_NUMERIC == 1, 1000, 0)
asl_numeric$ASL_FDCENT_weight = ifelse(asl_numeric$ASL_FDCENT_NUMERIC == 1, 100, 0)
asl_numeric$ASL_FDRIGH_weight = ifelse(asl_numeric$ASL_FDRIGH_NUMERIC == 1, 10, 0)
asl_numeric$ASL_SHARED_weight = ifelse(asl_numeric$ASL_SHARED_NUMERIC == 1, 1, 0)
asl_numeric$ASL_COLOUR_weight = ifelse(asl_numeric$ASL_COLOUR_NUMERIC == 1, 5, 0)

# # Create new column with the value of characterisation
asl_numeric = asl_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(ASL_FDR__weight:ASL_COLOUR_weight)))

asl_characteristics = asl_numeric %>%
  st_drop_geometry() %>%
  group_by(weight_5) %>%
  summarise(count = n())
sum(asl_characteristics$count) # n = 3775
   # weight_5 count
#  1        0  1568 No characteristics
#  2        1     2 Shared
#  3        5   405 coloured
#  4        6     5 coloured and shared
#  5     1000    11 feeder left
#  6     1005     1 feeder left and coloured
#  7    10000     1 feeder present
#  8    10005     1 feeder present & coloured
#  9    10010    15 feeder present & right
# 10    10015     8 feeder present right & coloured
# 11    10100    45 feeder present & in centre
# 12    10105    30 feeder present, in centre and coloured
# 13    11000  1067 feeder present & on left
# 14    11005   609 feeder prsent, on left & coloured
# 15    11010     3 feeder present, left and right
# 16    11015     1 feeder present, left, right and coloured
# 17    11100     1 feeder present, on left & centre
# 18    11105     2 feeder present, on left& centre, and coloured

# # Create new column with count of left, right and centre lanes
asl_numeric = asl_numeric %>%
  rowwise() %>%
  mutate(lane_number = sum(c_across(ASL_FDRLFT_NUMERIC:ASL_FDRIGH_NUMERIC)))
# number of lanes
asl_feeder_lanes = asl_numeric %>%
  st_drop_geometry() %>%
  group_by(lane_number) %>%
  summarise(count = n())
# lane_number count
# 1           0  1982
# 2           1  1786
# 3           2     7

################################################################################
#                     
#                             Crossings (n = 1990)
#
################################################################################

c_crossings %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

# CRS_SIGNAL    1. FALSE                        470 (23.6%)                                
# [factor]      2. TRUE                        1520 (76.4%)            
# 
# CRS_SEGREG    1. FALSE                       1652 (83.0%)           
# [factor]      2. TRUE                          338 (17.0%)          
# 
# CRS_CYGAP     1. FALSE                        1856 (93.3%)         
# [factor]      2. TRUE                          134 ( 6.7%)        

# CRS_PEDEST    1. FALSE                        1942 (97.6%)        
# [factor]      2. TRUE                           48 ( 2.4%)           
# 
# CRS_LEVEL     1. FALSE                          1969 (98.9%)    
# [factor]      2. TRUE                           21 ( 1.1%)       

# Are these unique signals or do some signals have more than one characteristic? 
print(ctable(x = c_crossings$CRS_SIGNAL, y = c_crossings$CRS_SEGREG))
print(ctable(x = c_crossings$CRS_SIGNAL, y = c_crossings$CRS_CYGAP))
# some definately have more than one characteristic so investigate below

# # Convert Factors to numeric (converts all False to 1 and True to 2)
# # e.g.  CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
crossings_numeric = c_crossings %>%
  mutate(CRS_SIGNAL_NUMERIC = as.numeric(c_crossings$CRS_SIGNAL))  %>%
  mutate(CRS_SEGREG_NUMERIC = as.numeric(c_crossings$CRS_SEGREG))  %>%
  mutate(CRS_CYGAP_NUMERIC = as.numeric(c_crossings$CRS_CYGAP))  %>%
  mutate(CRS_PEDEST_NUMERIC = as.numeric(c_crossings$CRS_PEDEST))  %>%
  mutate(CRS_LEVEL_NUMERIC = as.numeric(c_crossings$CRS_LEVEL))

# Convert 1(false) to 0 and 2(true) to 1
crossings_numeric$CRS_SIGNAL_NUMERIC = ifelse(crossings_numeric$CRS_SIGNAL_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_SEGREG_NUMERIC = ifelse(crossings_numeric$CRS_SEGREG_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_CYGAP_NUMERIC = ifelse(crossings_numeric$CRS_CYGAP_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_PEDEST_NUMERIC = ifelse(crossings_numeric$CRS_PEDEST_NUMERIC == 1, 0, 1)
crossings_numeric$CRS_LEVEL_NUMERIC = ifelse(crossings_numeric$CRS_LEVEL_NUMERIC == 1, 0, 1)

# # Check now gives the count that I expect  -YES THEY DO
# sum(crossings_numeric$CRS_SIGNAL_NUMERIC) # n = 1520
# sum(crossings_numeric$CRS_SEGREG_NUMERIC) # n = 338
# sum(crossings_numeric$CRS_CYGAP_NUMERIC) # n = 134
# sum(crossings_numeric$CRS_PEDEST_NUMERIC) # n = 48
# sum(crossings_numeric$CRS_LEVEL_NUMERIC) # n = 21

# Recode to give weighted value with so can distinguish between different characteristics
crossings_numeric$CRS_SIGNAL_weight = ifelse(crossings_numeric$CRS_SIGNAL_NUMERIC == 1, 10000, 0)
crossings_numeric$CRS_SEGREG_weight = ifelse(crossings_numeric$CRS_SEGREG_NUMERIC == 1, 1000, 0)
crossings_numeric$CRS_CYGAP_weight = ifelse(crossings_numeric$CRS_CYGAP_NUMERIC == 1, 100, 0)
crossings_numeric$CRS_PEDEST_weight = ifelse(crossings_numeric$CRS_PEDEST_NUMERIC == 1, 10, 0)
crossings_numeric$CRS_LEVEL_weight = ifelse(crossings_numeric$CRS_LEVEL_NUMERIC == 1, 1, 0)

# Create new column with the value of characterisation
crossings_numeric = crossings_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(CRS_SIGNAL_weight:CRS_LEVEL_weight)))

crossings_characteristics = crossings_numeric %>%
  st_drop_geometry() %>%
  group_by(weight_5) %>%
  summarise(count = n())
sum(crossings_characteristics$count) # n = 1990
#     weight_5 count
# 1         0   224  Not signal controlled, not segre, no gap, not ped only, no level crossing
# 2         1    15 Just level cross
# 3        10    17 Just pedestrian only
# 4        11     6  Level crossing and pedestrian only
# 5       100    15  Just cycle gao
# 6      1000   121  Just segreg
# 7      1100    72  Segreg with gap
# 8     10000  1348  Just signal controlled
# 9     10010    25  Signal controlled by pedestrian only crossing
# 10    10100     2  Signal controlled with gap
# 11    11000    100  Signal controlled with segregation
# 12    11100    45  Signal controlled with segreg and gap




################################################################################
#                     
#          Cycle lanes and tracks  (n = 25315, 2903.5 km total length)
#
################################################################################

c_cyclelanetrack %>%
  st_drop_geometry() %>%
  select(-c("length_m", "length_km")) %>%
  summarytools::dfSummary()

# CLT_CARR      1. FALSE                        11350 (44.8%)                 
# [factor]      2. TRUE                         13965 (55.2%)        
# 
# CLT_SEGREG    1. FALSE                        23384 (92.4%)      
# [factor]      2. TRUE                          1931 ( 7.6%)        
# 
# CLT_STEPP     1. FALSE                        25211 (99.6%)                
# [factor]      2. TRUE                           104 ( 0.4%)                             
# 
# CLT_PARSEG    1. FALSE                        21732 (85.8%)            
# [factor]      2. TRUE                          3583 (14.2%)            
# 
# CLT_SHARED    1. FALSE                        14924 (59.0%)        
# [factor]      2. TRUE                         10391 (41.0%)           
# 
# CLT_MANDAT    1. FALSE                        23458 (92.7%)             
# [factor]      2. TRUE                          1857 ( 7.3%)        
# 
# CLT_ADVIS     1. FALSE                        18038 (71.3%)              
# [factor]      2. TRUE                          7277 (28.7%)          
# 
# CLT_PRIORI    1. FALSE                        23029 (91.0%)             
# [factor]      2. TRUE                          2286 ( 9.0%)         
# 
# CLT_CONTRA    1. FALSE                        23822 (94.1%)             
# [factor]      2. TRUE                          1493 ( 5.9%)           
# 
# CLT_BIDIRE    1. FALSE                        14883 (58.8%)              
# [factor]      2. TRUE                         10432 (41.2%)          
# 
# CLT_CBYPAS    1. FALSE                        25252 (99.8%)               
# [factor]      2. TRUE                            63 ( 0.2%)                                   
# 
# CLT_BBYPAS    1. FALSE                        25183 (99.5%)           
# [factor]      2. TRUE                           132 ( 0.5%)                   
# 
# CLT_PARKR     1. FALSE                        21121 (83.4%)               
# [factor]      2. TRUE                          4194 (16.6%)        
# 
# CLT_WATERR    1. FALSE                        24704 (97.6%)               
# [factor]      2. TRUE                           611 ( 2.4%)                             
# 
# CLT_PTIME     1. FALSE                        22515 (88.9%)       
# [factor]      2. TRUE                          2800 (11.1%)         

# CLT_COLOUR    1. BLUE                           951 ( 3.8%)             
# [factor]      2. BUFF                             1 ( 0.0%)         
#               3. BUFF/YELLOW                    322 ( 1.3%)                                                    
#               4. GREEN                         3215 (12.7%)                                             
#               5. NONE                         19124 (75.5%)                                    
#               6. OTHER                           20 ( 0.1%)                                                    
#               7. RED                           1682 ( 6.6%)                                             


# Convert Factors to numeric and add variable to distinguish on/off road
# e.g.  CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# create new variable which is clearly on/off road
clt_numeric = c_cyclelanetrack %>%
  mutate(on_off = case_when(CLT_CARR == 'TRUE' ~ "onroad", TRUE ~ "offroad")) %>%
  mutate(CLT_SEGREG_NUMERIC = as.numeric(c_cyclelanetrack$CLT_SEGREG)) %>%
  mutate(CLT_STEPP_NUMERIC = as.numeric(c_cyclelanetrack$CLT_STEPP))  %>%
  mutate(CLT_PARSEG_NUMERIC = as.numeric(c_cyclelanetrack$CLT_PARSEG))  %>%
  mutate(CLT_SHARED_NUMERIC = as.numeric(c_cyclelanetrack$CLT_SHARED)) %>%
  mutate(CLT_MANDAT_NUMERIC = as.numeric(c_cyclelanetrack$CLT_MANDAT))  %>%
  mutate(CLT_ADVIS_NUMERIC = as.numeric(c_cyclelanetrack$CLT_ADVIS)) %>%
  mutate(CLT_PRIORI_NUMERIC = as.numeric(c_cyclelanetrack$CLT_PRIORI)) %>%
  mutate(CLT_CONTRA_NUMERIC = as.numeric(c_cyclelanetrack$CLT_CONTRA)) %>%
  mutate(CLT_BIDIRE_NUMERIC = as.numeric(c_cyclelanetrack$CLT_BIDIRE)) %>%  
  mutate(CLT_CBYPAS_NUMERIC = as.numeric(c_cyclelanetrack$CLT_CBYPAS)) %>%  
  mutate(CLT_BBYPAS_NUMERIC = as.numeric(c_cyclelanetrack$CLT_BBYPAS)) %>%  
  mutate(CLT_PARKR_NUMERIC = as.numeric(c_cyclelanetrack$CLT_PARKR)) %>%  
  mutate(CLT_WATERR_NUMERIC = as.numeric(c_cyclelanetrack$CLT_WATERR)) %>%
  mutate(CLT_PTIME_NUMERIC = as.numeric(c_cyclelanetrack$CLT_PTIME)) %>%  
  mutate(CLT_COLOUR_NUMERIC = as.numeric(c_cyclelanetrack$CLT_COLOUR))
# converts all False to 1 and True to 2

# Convert 1(false) to 0 and 2(true) to 1
clt_numeric$CLT_SEGREG_NUMERIC = ifelse(clt_numeric$CLT_SEGREG_NUMERIC == 1, 0, 1)
clt_numeric$CLT_STEPP_NUMERIC = ifelse(clt_numeric$CLT_STEPP_NUMERIC == 1, 0, 1)
clt_numeric$CLT_PARSEG_NUMERIC = ifelse(clt_numeric$CLT_PARSEG_NUMERIC == 1, 0, 1)
clt_numeric$CLT_SHARED_NUMERIC = ifelse(clt_numeric$CLT_SHARED_NUMERIC == 1, 0, 1)
clt_numeric$CLT_MANDAT_NUMERIC = ifelse(clt_numeric$CLT_MANDAT_NUMERIC == 1, 0, 1)
clt_numeric$CLT_ADVIS_NUMERIC = ifelse(clt_numeric$CLT_ADVIS_NUMERIC == 1, 0, 1)
clt_numeric$CLT_PRIORI_NUMERIC = ifelse(clt_numeric$CLT_PRIORI_NUMERIC == 1, 0, 1)
clt_numeric$CLT_CONTRA_NUMERIC = ifelse(clt_numeric$CLT_CONTRA_NUMERIC == 1, 0, 1)
clt_numeric$CLT_BIDIRE_NUMERIC = ifelse(clt_numeric$CLT_BIDIRE_NUMERIC == 1, 0, 1)
clt_numeric$CLT_CBYPAS_NUMERIC = ifelse(clt_numeric$CLT_CBYPAS_NUMERIC == 1, 0, 1)
clt_numeric$CLT_BBYPAS_NUMERIC = ifelse(clt_numeric$CLT_BBYPAS_NUMERIC == 1, 0, 1)
clt_numeric$CLT_PARKR_NUMERIC = ifelse(clt_numeric$CLT_PARKR_NUMERIC == 1, 0, 1)
clt_numeric$CLT_WATERR_NUMERIC = ifelse(clt_numeric$CLT_WATERR_NUMERIC == 1, 0, 1)
clt_numeric$CLT_PTIME_NUMERIC = ifelse(clt_numeric$CLT_PTIME_NUMERIC == 1, 0, 1)
clt_numeric$CLT_COLOUR_NUMERIC = ifelse(clt_numeric$CLT_COLOUR_NUMERIC == 5, 0, 1) # no colour converted to 0, any colour converted to 1

# Check now gives the count that I expect (NB these are both on and off road)
sum(clt_numeric$CLT_SEGREG_NUMERIC) # n = 1931
sum(clt_numeric$CLT_STEPP_NUMERIC) # n = 104
sum(clt_numeric$CLT_PARSEG_NUMERIC) # n = 3583
sum(clt_numeric$CLT_SHARED_NUMERIC) # n = 10391
sum(clt_numeric$CLT_MANDAT_NUMERIC) # n = 1857
sum(clt_numeric$CLT_ADVIS_NUMERIC) # n = 7277
sum(clt_numeric$CLT_PRIORI_NUMERIC) # n = 2286
sum(clt_numeric$CLT_CONTRA_NUMERIC) # n = 1493
sum(clt_numeric$CLT_BIDIRE_NUMERIC) # n = 10432
sum(clt_numeric$CLT_CBYPAS_NUMERIC) # n = 63
sum(clt_numeric$CLT_BBYPAS_NUMERIC) # n = 132
sum(clt_numeric$CLT_PARKR_NUMERIC) # n = 4194
sum(clt_numeric$CLT_WATERR_NUMERIC) # n = 611
sum(clt_numeric$CLT_PTIME_NUMERIC) # n = 2800
sum(clt_numeric$CLT_COLOUR_NUMERIC) # n = 6191

# Recode to give different values so can identify clt that has multiple characteristics
clt_numeric$CLT_SEGREG_weight = ifelse(clt_numeric$CLT_SEGREG_NUMERIC == 1, 10000, 0)
clt_numeric$CLT_STEPP_weight = ifelse(clt_numeric$CLT_STEPP_NUMERIC == 1, 1000, 0)
clt_numeric$CLT_PARSEG_weight = ifelse(clt_numeric$CLT_PARSEG_NUMERIC == 1, 100, 0)
clt_numeric$CLT_MANDAT_weight = ifelse(clt_numeric$CLT_MANDAT_NUMERIC == 1, 10, 0)
clt_numeric$CLT_ADVIS_weight = ifelse(clt_numeric$CLT_ADVIS_NUMERIC == 1, 1, 0)

clt_numeric$CLT_PRIORI_weight = ifelse(clt_numeric$CLT_PRIORI_NUMERIC == 1, 5000, 0)
clt_numeric$CLT_CONTRA_weight = ifelse(clt_numeric$CLT_CONTRA_NUMERIC == 1, 500, 0)
clt_numeric$CLT_BIDIRE_weight = ifelse(clt_numeric$CLT_BIDIRE_NUMERIC == 1, 50, 0)
clt_numeric$CLT_COLOUR_weight = ifelse(clt_numeric$CLT_COLOUR_NUMERIC == 1, 5, 0)

clt_numeric$CLT_CBYPAS_weight = ifelse(clt_numeric$CLT_CBYPAS_NUMERIC == 1, 20000, 0)
clt_numeric$CLT_BBYPAS_weight = ifelse(clt_numeric$CLT_BBYPAS_NUMERIC == 1, 2000, 0)
clt_numeric$CLT_PARKR_weight = ifelse(clt_numeric$CLT_PARKR_NUMERIC == 1, 200, 0)
clt_numeric$CLT_WATERR_weight = ifelse(clt_numeric$CLT_WATERR_NUMERIC == 1, 20, 0)
clt_numeric$CLT_PTIME_weight = ifelse(clt_numeric$CLT_PTIME_NUMERIC == 1, 2, 0)

# Create new column with the values of characterisation
clt_numeric = clt_numeric %>%
  rowwise() %>%
  mutate(values = sum(c_across(CLT_SEGREG_weight:CLT_PTIME_weight)))


# Examine characteristics by total length  
# a) Onroad
clt_values_length_onroad = clt_numeric %>%
  st_drop_geometry() %>%
  filter(on_off == "onroad") %>%
  group_by(values) %>%
  summarise(length_onroad = sum(length_km))
clt_values_length_top20_onroad = clt_values_length_onroad %>%
  arrange(desc(length_onroad)) %>%
  mutate(rank = 1:nrow(clt_values_length_onroad)) %>%
  slice_head(n = 20) # take top 20 combined characteristics that have the longest length
unique(clt_values_length_top20_onroad$values)
# [1]      1  5001     2   500     7     6  5006    10     0    15
# [11]     5   501   201 10050   510 10005   250    17   110 10000

clt_values_length_top20_onroad = clt_values_length_top20_onroad %>%
  mutate(Characteristics_on = case_when(values == 1 ~ "Advisory cycle lane",
                           values == 5001 ~ "Advisory cycle lane with Cyclists prioritised",
                           values == 2 ~ "Part-time cycle lane",
                           values == 500 ~ "Contraflow cycle lane",
                           values == 7 ~ "Part-time cycle lane with Coloured tarmac",
                           values == 6 ~ "Advisory cycle lane with Coloured tarmac",
                           values == 5006 ~ "Advisory cycle lane with Cyclists prioritised and Coloured tarmac",
                           values == 10 ~ "Mandatory cycle lane", 
                           values == 0 ~ "No characteristics", 
                           values == 15 ~ "Mandatory cycle lane with Coloured tarmac", 
                           values == 5 ~ "Coloured tarmac", 
                           values == 501 ~ "Contraflow and Advisory cycle lane", 
                           values == 201 ~ "Park route and Advisory cycle lane",
                           values == 10050 ~ "Segregated and Bidirectional cycle lane",
                           values == 510 ~ "Contraflow and Mandatory cycle lane",
                           values == 10005 ~ "Segregated cycle lane with Coloured tarmac", 
                           values == 250 ~ "Park route and Bidirectional cycle lane", 
                           values == 17 ~ "Part-time mandatory cycle lane with Coloured tarmac",
                           values == 110 ~ "Partially segregated, mandatory cycle lane",
                           values == 10000 ~ "Segregated cycle lane"
                             ))
# add column for proportion of length compared to total onroad length
sum(clt_values_length_onroad$length_onroad) #944.0157 [km]

clt_values_length_top10_onroad = clt_values_length_top20_onroad %>%
  mutate(Prop_of_total_onroad_length = drop_units(round(((length_onroad/944.0157)*100), digit = 1))) %>%
  mutate(rounded_length_onroad = drop_units(round(length_onroad, digit = 1))) %>%
  select(c("rank", "Characteristics_on", "rounded_length_onroad", "Prop_of_total_onroad_length")) %>%
  slice_head(n = 11)  # take top 11 as need 11 rows to join to the top 10 + 0 characterstics in the offroad df

# b) Offroad
clt_values_length_offroad = clt_numeric %>%
  st_drop_geometry() %>%
  filter(on_off == "offroad") %>%
  group_by(values) %>%
  summarise(length_offroad = sum(length_km))
clt_values_length_top20_offroad = clt_values_length_offroad %>%
  arrange(desc(length_offroad)) %>%
  mutate(rank = 1:nrow(clt_values_length_offroad)) %>%
  slice_head(n = 20) # take top 20 combined characteristics that have the longest length
unique(clt_values_length_top20_offroad$values)
# [1]    250    50   252   270    70   150   155   255   350   100
# [11] 10050   272   275   105 10055   200 10250    55   355     0

clt_values_length_top20_offroad = clt_values_length_top20_offroad %>%
  mutate(Characteristics_off = case_when(values == 250 ~ "Park route and Bidirectional", 
                                     values == 50 ~ "Bidirectional", 
                                     values == 252 ~ "Park route, Bidirectional and Part-time",
                                     values == 270 ~ "Park route, Waterside and Bidirectional",
                                     values == 70 ~ "Waterside route and Bidirectional",
                                     values == 150 ~ "Partially segregated and Bidirectional",
                                     values == 155 ~ "Partially segregated, Bidirectional with Coloured tarmac",
                                     values == 255 ~ "Partially segregated, Park route with Coloured tarmac",
                                     values == 350 ~ "Partially segregated, Bidirectional, Park route",
                                     values == 100 ~ "Partially segregated",
                                     values == 10050 ~ "Segregated and Bidirectional",
                                     values == 0 ~ "No characteristics"
  ))

#  add column for proportion of length compared to total offroad length
sum(clt_values_length_offroad$length_offroad) # 1959.577 [km]

clt_values_length_top10_offroad = clt_values_length_top20_offroad %>%
  mutate(Prop_of_total_offroad_length = drop_units(round(((length_offroad/1959.577)*100), digit = 1))) %>%
  mutate(rounded_length_offroad = drop_units(round(length_offroad, digit = 1))) %>%
  select(c("rank", "Characteristics_off", "rounded_length_offroad", "Prop_of_total_offroad_length")) %>%
  filter(rank <11 | rank ==20)

# join on and off road comparison together then save
clt_values_length_top10 = cbind(clt_values_length_top10_onroad, clt_values_length_top10_offroad)

# save output as table for use in paper
write_csv2(clt_values_length_top10, file = "/home/bananafan/Downloads/clt_values_length_top10.csv", col_names = TRUE)


################################################################################
#                     
#                             Signals (n = 443)
#
################################################################################
c_signals %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

# SIG_HEAD      1. FALSE                          5 ( 1.1%)            
# [factor]      2. TRUE                         438 (98.9%)            
# 
# SIG_SEPARA    1. FALSE                        187 (42.2%)        
# [factor]      2. TRUE                         256 (57.8%)        
# 
# SIG_EARLY     1. FALSE                        363 (81.9%)             
# [factor]      2. TRUE                          80 (18.1%)          
# 
# SIG_TWOSTG    1. FALSE                        415 (93.7%)             
# [factor]      2. TRUE                          28 ( 6.3%)          
# 
# SIG_GATE      1. FALSE                        415 (93.7%)              
# [factor]      2. TRUE                          28 ( 6.3%)         

# Convert Factors to numeric (converts all False to 1 and True to 2)
signals_numeric = c_signals %>%
  mutate(SIG_HEAD_NUMERIC = as.numeric(c_signals$SIG_HEAD)) %>%
  mutate(SIG_SEPARA_NUMERIC = as.numeric(c_signals$SIG_SEPARA)) %>%
  mutate(SIG_EARLY_NUMERIC = as.numeric(c_signals$SIG_EARLY)) %>%
  mutate(SIG_TWOSTG_NUMERIC = as.numeric(c_signals$SIG_TWOSTG)) %>%
  mutate(SIG_GATE_NUMERIC = as.numeric(c_signals$SIG_GATE)) 

# Convert 1(false) to 0 and 2(true) to 1
signals_numeric$SIG_HEAD_NUMERIC = ifelse(signals_numeric$SIG_HEAD_NUMERIC == 1, 0, 1)
signals_numeric$SIG_SEPARA_NUMERIC = ifelse(signals_numeric$SIG_SEPARA_NUMERIC == 1, 0, 1)
signals_numeric$SIG_EARLY_NUMERIC = ifelse(signals_numeric$SIG_EARLY_NUMERIC == 1, 0, 1)
signals_numeric$SIG_TWOSTG_NUMERIC = ifelse(signals_numeric$SIG_TWOSTG_NUMERIC == 1, 0, 1)
signals_numeric$SIG_GATE_NUMERIC = ifelse(signals_numeric$SIG_GATE_NUMERIC == 1, 0, 1)

## Check now gives the count that I expect (sum will count all the ones)  -YES THEY DO
# sum(signals_numeric$SIG_HEAD_NUMERIC) # n = 483
# sum(signals_numeric$SIG_SEPARA_NUMERIC) # n = 256
# sum(signals_numeric$SIG_EARLY_NUMERIC) # n = 80
# sum(signals_numeric$SIG_TWOSTG_NUMERIC) # n = 28
# sum(signals_numeric$SIG_GATE_NUMERIC) # n = 28

# Recode to give weighted value with so can distinguish between different characteristics
signals_numeric$SIG_HEAD_weight = ifelse(signals_numeric$SIG_HEAD_NUMERIC == 1, 10000, 0)
signals_numeric$SIG_SEPARA_weight = ifelse(signals_numeric$SIG_SEPARA_NUMERIC == 1, 1000, 0)
signals_numeric$SIG_EARLY_weight = ifelse(signals_numeric$SIG_EARLY_NUMERIC == 1, 100, 0)
signals_numeric$SIG_TWOSTG_weight = ifelse(signals_numeric$SIG_TWOSTG_NUMERIC == 1, 10, 0)
signals_numeric$SIG_GATE_weight = ifelse(signals_numeric$SIG_GATE_NUMERIC == 1, 1, 0)

# # Create new column with the values of characterisation
signals_numeric = signals_numeric %>%
  rowwise() %>%
  mutate(values = sum(c_across(SIG_HEAD_weight:SIG_GATE_weight)))

signals_characteristics = signals_numeric %>%
  st_drop_geometry() %>%
  group_by(values) %>%
  summarise(count = n())
sum(signals_characteristics$count) # n = 443
print(signals_characteristics)
#      values count
#  1        0     4 No characteristics
#  2      100     1 Signal gate
#  3    10000   132 cycle symbol on lights
#  4    10001     2 cycle symbol on lights and signal gate
#  5    10010     8 cycle symbol on lights and 2 stage
#  6    10100    36 cycle symbol on lights and early release
#  7    10101     1 cycle symbol on lights, early release and signal gate
#  8    10110     3 cycle symbol on lights early release and 2 stage
#  9    11000   186 cycle symbol on lights and separate stage
# 10    11001    15 cycle symbol on lights, separate stage and signal gate
# 11    11010    16 cycle symbol on lights, separate stage and 2 stage
# 12    11100    28 cycle symbol on lights, separate stage and early release
# 13    11101    10 cycle symbol on lights, separate stage, early release and signal gate
# 14    11110     1 cycle symbol on lights, separate stage, early release and 2 stage

signals_characteristics = signals_characteristics %>%
  mutate(Characteristics = case_when(values == 100 ~ "Early cyclist release", 
                                     values == 10000 ~ "Cycle symbol on signal lights", 
                                     values == 10001 ~ "Cycle symbol on lights and Cyclist signal gate",
                                     values == 10010 ~ "Cycle symbol on lights and Two stage right turn",
                                     values == 10100 ~ "Cycle symbol on lights and Early cyclist release",
                                     values == 10101 ~ "Cycle symbol on lights, Early cyclist release and Signal gate",
                                     values == 10110 ~ "Cycle symbol on lights, Early cyclist release and Two stage right turn",
                                     values == 11000 ~ "Cycle symbol on lights and Separate cyclist stage",
                                     values == 11001 ~ "Cycle symbol on lights, Separate cyclist stage and Signal gate",
                                     values == 11010 ~ "Cycle symbol on lights, Separate cyclist stage and Two stage right turn",
                                     values == 11100 ~ "Cycle symbol on lights, Separate cyclist stage and Early cyclist release",
                                     values == 11101 ~ "Cycle symbol on lights, Separate cyclist stage, Early cyclist release and Signal gate",
                                     values == 11110 ~ "Cycle symbol on lights, Separate cyclist stage, Early cyclist release and Two stage right turn",
                                     values == 0 ~ "No characteristics")) %>%
  select(-c(values))

# save output as table for use in paper
write_csv2(signals_characteristics, file = "/home/bananafan/Downloads/signals_characteristics.csv", col_names = TRUE)



################################################################################
#                     
#                             Traffic calming (n = 58565)
#
################################################################################
c_trafficcalming %>%
  st_drop_geometry() %>%
  summarytools::dfSummary()

# TRF_RAISED    1. FALSE                        55793 (95.3%)               
# [factor]      2. TRUE                          2772 ( 4.7%)                       
# 
# TRF_ENTRY     1. FALSE                        50984 (87.1%)            
# [factor]      2. TRUE                          7581 (12.9%)         
# 
# TRF_CUSHI     1. FALSE                        45939 (78.4%)             
# [factor]      2. TRUE                         12626 (21.6%)          
# 
# TRF_HUMP      1. FALSE                        25294 (43.2%)             
# [factor]      2. TRUE                         33271 (56.8%)         
# 
# TRF_SINUSO    1. FALSE                        51845 (88.5%)            
# [factor]      2. TRUE                          6720 (11.5%)           
# 
# TRF_BARIER    1. FALSE                        57630 (98.4%)              
# [factor]      2. TRUE                           935 ( 1.6%)                              
# 
# TRF_NAROW     1. FALSE                        57903 (98.9%)             
# [factor]      2. TRUE                           662 ( 1.1%)                                
# 
# TRF_CALM      1. FALSE                        57844 (98.8%)               
# [factor]      2. TRUE                           721 ( 1.2%)                           

# # Convert Factors to numeric (converts all False to 1 and True to 2)
calming_numeric = c_trafficcalming %>%
  mutate(TRF_RAISED_NUMERIC = as.numeric(c_trafficcalming$TRF_RAISED)) %>%
  mutate(TRF_ENTRY_NUMERIC = as.numeric(c_trafficcalming$TRF_ENTRY)) %>%
  mutate(TRF_CUSHI_NUMERIC = as.numeric(c_trafficcalming$TRF_CUSHI)) %>%
  mutate(TRF_HUMP_NUMERIC = as.numeric(c_trafficcalming$TRF_HUMP)) %>%
  mutate(TRF_SINUSO_NUMERIC = as.numeric(c_trafficcalming$TRF_SINUSO)) %>%
  mutate(TRF_BARIER_NUMERIC = as.numeric(c_trafficcalming$TRF_BARIER)) %>%
  mutate(TRF_NAROW_NUMERIC = as.numeric(c_trafficcalming$TRF_NAROW)) %>%
  mutate(TRF_CALM_NUMERIC = as.numeric(c_trafficcalming$TRF_CALM))
  
# Convert 1(false) to 0 and 2(true) to 1
calming_numeric$TRF_RAISED_NUMERIC = ifelse(calming_numeric$TRF_RAISED_NUMERIC == 1, 0, 1)
calming_numeric$TRF_ENTRY_NUMERIC= ifelse(calming_numeric$TRF_ENTRY_NUMERIC == 1, 0, 1)
calming_numeric$TRF_CUSHI_NUMERIC = ifelse(calming_numeric$TRF_CUSHI_NUMERIC == 1, 0, 1)
calming_numeric$TRF_HUMP_NUMERIC = ifelse(calming_numeric$TRF_HUMP_NUMERIC == 1, 0, 1)
calming_numeric$TRF_SINUSO_NUMERIC = ifelse(calming_numeric$TRF_SINUSO_NUMERIC == 1, 0, 1)
calming_numeric$TRF_BARIER_NUMERIC = ifelse(calming_numeric$TRF_BARIER_NUMERIC == 1, 0, 1)
calming_numeric$TRF_NAROW_NUMERIC = ifelse(calming_numeric$TRF_NAROW_NUMERIC == 1, 0, 1)
calming_numeric$TRF_CALM_NUMERIC = ifelse(calming_numeric$TRF_CALM_NUMERIC == 1, 0, 1)

# ## Check now gives the count that I expect (sum will count all the ones)  -YES THEY DO
# sum(calming_numeric$TRF_RAISED_NUMERIC) # n = 2772
# sum(calming_numeric$TRF_ENTRY_NUMERIC) # n = 7581
# sum(calming_numeric$TRF_CUSHI_NUMERIC) # n = 12626
# sum(calming_numeric$TRF_HUMP_NUMERIC) # n = 33271
# sum(calming_numeric$TRF_SINUSO_NUMERIC) # n = 6720
# sum(calming_numeric$TRF_BARIER_NUMERIC) # n = 935
# sum(calming_numeric$TRF_NAROW_NUMERIC) # n = 662
# sum(calming_numeric$TRF_CALM_NUMERIC) # n = 721

# Recode to give weighted value with so can distinguish between different characteristics
calming_numeric$TRF_RAISED_weight = ifelse(calming_numeric$TRF_RAISED_NUMERIC == 1, 10000, 0)
calming_numeric$TRF_ENTRY_weight = ifelse(calming_numeric$TRF_ENTRY_NUMERIC == 1, 5000, 0)
calming_numeric$TRF_CUSHI_weight = ifelse(calming_numeric$TRF_CUSHI_NUMERIC == 1, 1000, 0)
calming_numeric$TRF_HUMP_weight = ifelse(calming_numeric$TRF_HUMP_NUMERIC == 1, 500, 0)
calming_numeric$TRF_SINUSO_weight = ifelse(calming_numeric$TRF_SINUSO_NUMERIC == 1, 100, 0)
calming_numeric$TRF_BARIER_weight = ifelse(calming_numeric$TRF_BARIER_NUMERIC == 1, 50, 0)
calming_numeric$TRF_NAROW_weight = ifelse(calming_numeric$TRF_NAROW_NUMERIC == 1, 10, 0)
calming_numeric$TRF_CALM_weight = ifelse(calming_numeric$TRF_CALM_NUMERIC == 1, 1, 0)

# # Create new column with the values of characterization
calming_numeric = calming_numeric %>%
  rowwise() %>%
  mutate(values = sum(c_across(TRF_RAISED_weight:TRF_CALM_weight)))

calming_characteristics = calming_numeric %>%
  st_drop_geometry() %>%
  group_by(values) %>%
  summarise(count = n())
sum(calming_characteristics$count) # n = 443
print(calming_characteristics)
# #  values count
# 1       0    12 no characteristics
# 2       1   721 other traffic calming measure
# 3      10   652 road narrowing
# 4      50   935 barrier
# 5     500 26948 hump
# 6     510     1 hump and road narrowing
# 7     600  6319 hump and sinusoidal
# 8    1000 12217 cushion
# 9    1010     8 cushion and road narrowing
# 10   1100   400 cushion and sinusoidal
# 11   1110     1 cushion and sinusoidal and narrowing
# 12   5000  7576 side entry treatment
# 13   5500     3 side entry treatment and hump
# 14  10000  2770 raised table at junction
# 15  15000     2 raised table at junction and side entry treatment


# save output as table for use in paper
write_csv2(calming_characteristics, file = "/home/bananafan/Downloads/calming_characteristics.csv", col_names = TRUE)



###############################################################################
#                       Summary stats at Borough level                       #
#                                                                             #
# - safety infrastructure at Borough level                                    #
 

# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "data/CID_count_by_borough")
CID_length = readRDS(file = "data/CID_length_by_borough_on_off")

# Join df  
safe_cnt = CID_count %>%
  select(c(BOROUGH, ASL, Crossings, Signals, TrafficCalming))
safe_ln = CID_length %>%
  select(BOROUGH, clt_total_length_km, length_km_onroad, length_km_offroad) %>%
  drop_units()
safe_df = left_join(safe_cnt, safe_ln, by = "BOROUGH")

# add column to compare ratio on on and off road clt
safe_df = safe_df %>%
  mutate(ratio_on_off = length_km_onroad/length_km_offroad) %>%
  mutate(ratio_off_on = length_km_offroad/length_km_onroad)

# Functions to create summary data how I want it
my.summary <- function(x,...){ 
  c("Range" = paste0(round(min(x, ...), digits = 1), " - ", paste0(round(max(x,...), digits = 1))),
    "Mean (SD)" = paste0(round(mean(x, ...), digits = 1), " (", paste0(round(sd(x, ...), digits = 1)), ")"),
    "Median (IQR)" = paste0(round(median(x, ...), digits = 1), "  (", paste0(round((quantile(x, 0.25)), digits = 1), " - ",  
                                                                             round((quantile(x, 0.75)), digits = 1)), ")"))
} 

my.summary_clt <- function(x,...){ 
  c("Range" = paste0(round(min(x, ...), digits = 1), " - ", paste0(round(max(x,...), digits = 1), " km")),
    "Mean (SD)" = paste0(round(mean(x, ...), digits = 1), " km", " (", paste0(round(sd(x, ...), digits = 1)), " km)"),
    "Median (IQR)" = paste0(round(median(x, ...), digits = 1), " km", "  (", paste0(round((quantile(x, 0.25)), digits = 1), " - ",  
                                                                                    round((quantile(x, 0.75)), digits = 1)), " km)"))
} 

# Calculate summary statistics for asset type 
borough_summary = data.frame(
  Measure = c("Range", "Mean (SD)", "Median (IQR)"), 
  ASL = my.summary(safe_df2$ASL),  
  Crossings = my.summary(safe_df2$Crossings),
  clt_total_length_km = my.summary_clt(safe_df2$clt_total_length_km),
  length_km_onroad = my.summary_clt(safe_df2$length_km_onroad),
  length_km_offroad = my.summary_clt(safe_df2$length_km_offroad),
  Signals = my.summary(safe_df2$Signals),
  TrafficCalming = my.summary(safe_df2$TrafficCalming))

borough_summary_transpose = t(borough_summary) # get in format suitable for paper



###############################################################################
#                       Borough level data and ranking                        #
#                                                                             #
# - safety infrastructure at Borough level  - table by Borough                #

# Load datasets - these datasets were created 2_3_2021 from TFL datasets downloaded 25/2/21
CID_count = readRDS(file = "data/CID_count_by_borough")
CID_length = readRDS(file = "data/CID_length_by_borough_on_off")

# Get safety infrastructure in one dataset 
safe_cnt = CID_count %>%
  select(c(BOROUGH, ASL, Crossings, Signals, TrafficCalming))
safe_ln = CID_length %>%
  select(BOROUGH, clt_total_length_km, length_km_onroad, length_km_offroad) %>%
  drop_units()
safe_df2 = left_join(safe_cnt, safe_ln, by = "BOROUGH")

# Add column for inner/outer london
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

safe_df2 = safe_df2 %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) # 9 variables

# Create ranking for Count dataset
# rank each borough
# use rank(-.x) to get ranking so that borough with most assets has highest ranking

safe_df2_rank = safe_df2 %>%
  mutate(across(.cols = c("ASL", "Crossings", "Signals", "TrafficCalming", 
                          "clt_total_length_km", "length_km_onroad", "length_km_offroad"),
                .fns = ~rank(-.x, ties.method = "min"),  # set so that obs with same rank at given the minimum rank
                .names = "{.col}_count_rank")) # now 12 variables

# mutate to get rank in with number ie n(rank)
safe_df2_rank$ASL = paste(safe_df2_rank$ASL, "(", safe_df2_rank$ASL_count_rank, ")")
safe_df2_rank$Crossings = paste(safe_df2_rank$Crossings, "(", safe_df2_rank$Crossings_count_rank, ")")
safe_df2_rank$Signals = paste(safe_df2_rank$Signals, "(", safe_df2_rank$Signals_count_rank, ")")
safe_df2_rank$TrafficCalming = paste(safe_df2_rank$TrafficCalming, "(", safe_df2_rank$TrafficCalming_count_rank, ")")
safe_df2_rank$clt_total_length_km = paste(safe_df2_rank$clt_total_length_km, "km (", safe_df2_rank$clt_total_length_km_count_rank, ")")
safe_df2_rank$length_km_onroad = paste(safe_df2_rank$length_km_onroad, "km (", safe_df2_rank$length_km_onroad_count_rank, ")")
safe_df2_rank$length_km_offroad = paste(safe_df2_rank$length_km_offroad, "km (", safe_df2_rank$length_km_offroad_count_rank, ")")

# Select columns I want to keep
safe_borough_rank = safe_df2_rank %>%
  select(c("London", "BOROUGH", "ASL", "Crossings", "Signals", "TrafficCalming", 
           "clt_total_length_km", "length_km_onroad", "length_km_offroad")) %>%
  arrange(London, by_group = TRUE)

# Save dataset so can send to journal/have it in editable format
write_csv2(safe_borough_rank, 
           file = "output/summary_stats/borough_safety_table.csv")



