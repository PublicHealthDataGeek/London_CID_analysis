###################################################################################
# Data cleaning CID - Cycle Parking                                               #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that has no Borough assigned                        #
# It also checks that the Boroughs are correctly assigned                         #
# 37 cycle parking sites incorrectly located and reallocated                      #
###################################################################################

######################################
# Install packages and load datasets #
######################################

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(forcats)
library(units)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE)

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# download CID data using the Cycle Infra Lnd package
cycle_parking = get_cid_points(type = "cycle_parking")

# Convert CRS so matches ONS boundary data CRS
cycle_parking = st_transform(cycle_parking, crs=27700) 
st_crs(cycle_parking) # PROJCRS["OSGB 1936 / British National Grid"

###################################
# Check completeness of variables #
###################################
unique(cycle_parking$FEATURE_ID) # 23758 unique variables
unique(cycle_parking$BOROUGH) # 33 Boroughs no NAS
unique(cycle_parking$SVDATE) # 331 unique survey dates, all of which are valid date
# the below all have just true and false unless stated
unique(cycle_parking$PRK_CARR)
unique(cycle_parking$PRK_COVER)
unique(cycle_parking$PRK_SECURE)
unique(cycle_parking$PRK_LOCKER)
unique(cycle_parking$PRK_SHEFF)
unique(cycle_parking$PRK_MSTAND)
unique(cycle_parking$PRK_PSTAND)
unique(cycle_parking$PRK_HOOP)
unique(cycle_parking$PRK_POST)
unique(cycle_parking$PRK_BUTERF)
unique(cycle_parking$PRK_WHEEL)
unique(cycle_parking$PRK_HANGAR)
unique(cycle_parking$PRK_TIER)
unique(cycle_parking$PRK_OTHER)
unique(cycle_parking$PRK_CPT) # contains NA
unique(cycle_parking$PRK_PROVIS) # contains NA

# examine URL data
count_photo1 =  cycle_parking %>%
  count(PHOTO1_URL) # 299 have no asset photo 1
count_photo2 =  cycle_parking %>%
  count(PHOTO2_URL) # 298 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

#### 1) Convert certain columns to factors
# Create list of columns to factor
f_variables = c("PRK_CARR", "PRK_COVER", "PRK_SECURE", "PRK_LOCKER", "PRK_SHEFF", "PRK_MSTAND",
                "PRK_PSTAND", "PRK_HOOP", "PRK_POST", "PRK_BUTERF", "PRK_WHEEL", "PRK_HANGAR",
                "PRK_TIER", "PRK_OTHER", "BOROUGH")

## Factor columns
f_cycle_parking = cycle_parking %>%
  mutate_at(f_variables, as.factor)

#### 2) Sort out numerical NAs for PRK_PROVIS and PRK_CPT
# PRK_PROVIS (number of stands etc) and PRK_CPT (number of bikes that can be 
# parked) have NAs.  These need correcting before can calculate no of 
# bikes that can be parked per borough

# sum(is.na(f_cycle_parking$PRK_PROVIS)) # 2 NA
# sum(is.na(f_cycle_parking$PRK_CPT)) # 2 NAs
# # CPcount_borough_NA = f_cycle_parking %>% # identify which observations are NA
# #   st_drop_geometry() %>%
# #   filter(is.na(PRK_CPT)) # 2 observations have NA - both for PROVIS & CPT

# Asset images reviewed - able to see how many stands and spaces each have
# RWG999580 4 stands (PROVIS) 8 spaces (CPT)
# RWG999458 3 stands, 6 spaces
# therefore can correct missing data
f_cycle_parking$PRK_PROVIS[f_cycle_parking$FEATURE_ID == "RWG999580"] = 4
f_cycle_parking$PRK_CPT[f_cycle_parking$FEATURE_ID == "RWG999580"] = 8
f_cycle_parking$PRK_PROVIS[f_cycle_parking$FEATURE_ID == "RWG999458"] = 3
f_cycle_parking$PRK_CPT[f_cycle_parking$FEATURE_ID == "RWG999458"] = 6

# # check coded correctly
# parking_check = f_cycle_parking %>%
#   select(c("FEATURE_ID", "PRK_PROVIS", "PRK_CPT")) %>%
#   filter(FEATURE_ID == "RWG999580" | FEATURE_ID == "RWG999458") # = yes coded correctly


######################################
# Check to see if BOROUGH is correct #
######################################

# Visually inspect 
mapview(f_cycle_parking, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
          lwd = 1, legend = FALSE) # Visual inspection all appears to be fine but lots of obs

# Spatially join parking dataset to ONS dataset
joined = st_join(f_cycle_parking, lon_lad_2020)

# check borough.x (CID) matches borough.y (ONS)
nonmatch_pre = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 37 observations

## 2) Run loop to run through and recode BOROUGH.x (CID) with BOROUGH.y (ONS)
counter = 0 # create counter so can check how many observations are changed
for (i in seq_along(joined$BOROUGH.x)) {
  if(joined$BOROUGH.y[[i]] != joined$BOROUGH.x[[i]]) {
    counter = counter + 1
  }
  joined$BOROUGH.y[[i]] = joined$BOROUGH.x[[i]]
}

# check worked
nonmatch_post = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 0 observations

# Finalise dataset
parking_corrected = joined %>%
  select(c("FEATURE_ID", "SVDATE", "PRK_CARR", "PRK_COVER", "PRK_SECURE", 
           "PRK_LOCKER", "PRK_SHEFF", "PRK_MSTAND", "PRK_PSTAND", "PRK_HOOP", 
           "PRK_POST", "PRK_BUTERF", "PRK_WHEEL", "PRK_HANGAR", "PRK_TIER", 
           "PRK_OTHER","PRK_PROVIS", "PRK_CPT", "BOROUGH.x", "PHOTO1_URL", 
           "PHOTO2_URL", "geometry")) %>%
  rename("BOROUGH" = "BOROUGH.x")

######################
# SAVE CLEAN DATASET #
######################
saveRDS(parking_corrected, file = "data/cleansed_parking")


# Check use st_Contains to see if boroughs in CID are within ONS Boroughs 
# - outcome is that they are - see commented out appendix
# 37 cycle poarking sites allocated to different Boroughs

# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                               which(f_cycle_parking$FEATURE_ID == "RWG260429"), 
#                               values = "Harrow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG204478"), 
#                                   values = "Croydon")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG281472"), 
#                                   values = "Brent")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG058214"), 
#                                   values = "City of London")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG056999"), 
#                                   values = "Westminster")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG058103"), 
#                                   values = "Westminster")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG057782"), 
#                                   values = "Westminster")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG058206"), 
#                                   values = "Camden")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG057800"), 
#                                   values = "Tower Hamlets")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG057803"), 
#                                   values = "Tower Hamlets")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG057805"), 
#                                   values = "Tower Hamlets")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232761"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232762"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232768"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232769"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232774"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232775"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232770"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG231840"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232763"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232773"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232760"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232765"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232766"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG197541"), 
#                                   values = "Lewisham")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG171415"), 
#                                   values = "Bexley")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG004681"), 
#                                   values = "Islington")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG044634"), 
#                                   values = "Hounslow")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232651"), 
#                                   values = "Ealing")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG232030"), 
#                                   values = "Hammersmith & Fulham")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG056800"), 
#                                   values = "City of London")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG056777"), 
#                                   values = "Hackney")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG118004"), 
#                                   values = "Westminster")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG098368"), 
#                                   values = "Southwark")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG199655"), 
#                                   values = "Southwark")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG291875"), 
#                                   values = "Bromley")
# f_cycle_parking$BOROUGH = replace(f_cycle_parking$BOROUGH, 
#                                   which(f_cycle_parking$FEATURE_ID == "RWG067682"), 
#                                   values = "Hackney")
# 
# # Visually inspect again 
# mapview(f_cycle_parking, zcol = "BOROUGH") + 
#   mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#           lwd = 1, legend = FALSE) # No obvious issue seen from recoding




# APPENDIX 1 - the observations that need recoding

## RWG197541 Lewisham
## RWG171415 Bexley
## RWG004681 Islington
## RWG044634 Hounslow
## RWG232651 Ealing
## RWG232030 Hammersmith & Fulham
## RWG056800 City of London
## RWG056777 Hackney
## RWG118004 Westminster
## RWG098368 Southwark
## RWG199655 Southwark
## RWG291875 Bromley
## RWG067682 Hackney

## RWG260429 needs to be harrow not brent
## RWG204478 should be in Croydon not Bromley
## RWG281472 should be Brent
## RWG058214 shoudl be City of London
## RWG056999 Westminster
## RWG058103 Westminster
## RWG057782 Westminster
## RWG058206 Camden
## RWG057800 Tower Hamlets
## RWG057803 Tower Hamlets
## RWG057805 Tower Hamlets

# these 13 all to Hounslow
## [1] "RWG232761" 
## "RWG232762" 
##"RWG232768" 
##"RWG232769" 
##"RWG232774" 
## "RWG232775" 
## "RWG232770" 
## "RWG231840"
## [9] "RWG232763" 
## "RWG232773" 
## "RWG232760" 
## "RWG232765" 
##"RWG232766"






# APPENDIX 2 - checking that cycle parking within specific Boroughs are within the same ONS borough
# 
parking_boroughs = str_sort(as.character(unique(f_cycle_parking$BOROUGH))) # 23 Boroughs, no NAS
str_sort(parking_boroughs)
# [1] "Barking & Dagenham"   "Barnet"               "Bexley"               "Brent"               
# [5] "Bromley"              "Camden"               "City of London"       "Croydon"             
# [9] "Ealing"               "Enfield"              "Greenwich"            "Hackney"             
# [13] "Hammersmith & Fulham" "Haringey"             "Harrow"               "Havering"            
# [17] "Hillingdon"           "Hounslow"             "Islington"            "Kensington & Chelsea"
# [21] "Kingston upon Thames" "Lambeth"              "Lewisham"             "Merton"              
# [25] "Newham"               "Redbridge"            "Richmond upon Thames" "Southwark"           
# [29] "Sutton"               "Tower Hamlets"        "Waltham Forest"       "Wandsworth"          
# [33] "Westminster"         
# 

# # Barking & Dagenham
# CID_bark = f_cycle_parking %>%
#   filter(BOROUGH == "Barking & Dagenham") # 265 observations
# ONS_bark = lon_lad_2020 %>%
#   filter(BOROUGH == "Barking & Dagenham")
# all(st_contains(ONS_bark, CID_bark, sparse = FALSE))  # all TRUE
# 
# # Barnet
# CID_barn = f_cycle_parking %>%
#   filter(BOROUGH == "Barnet") # 410 observations
# ONS_barn = lon_lad_2020 %>%
#   filter(BOROUGH == "Barnet")
# all(st_contains(ONS_barn, CID_barn, sparse = FALSE))  # all TRUE
# 
# # Bexley
# CID_bex = f_cycle_parking %>%
#   filter(BOROUGH == "Bexley") # 179 observations
# ONS_bex = lon_lad_2020 %>%
#   filter(BOROUGH == "Bexley")
# all(st_contains(ONS_bex, CID_bex, sparse = FALSE))  # all TRUE
# 
# # Brent
# CID_bren = f_cycle_parking %>%
#   filter(BOROUGH == "Brent") # 489 observations
# ONS_bren = lon_lad_2020 %>%
#   filter(BOROUGH == "Brent")
# all(st_contains(ONS_bren, CID_bren, sparse = FALSE))  # FALSE ie one observation is incorrectly labelled
# 
# bren_matrix = st_contains(ONS_bren, CID_bren, sparse = FALSE) # 489 columns
# row_id_FALSE = as.data.frame(t(bren_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE")
# bren_false = CID_bren %>%
#   filter(row_number() == 99)
# 
# st_contains(ONS_bren, bren_false, sparse = FALSE) # check got right one
# mapview(bren_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                               lwd = 1, legend = FALSE)
# # Yes - should be in Harrow not Brent.  Recode this observation as Harrow. 
#  
# # Bromley
# CID_brom = f_cycle_parking %>%
#   filter(BOROUGH == "Bromley") # 299 observations
# ONS_brom = lon_lad_2020 %>%
#   filter(BOROUGH == "Bromley")
# all(st_contains(ONS_brom, CID_brom, sparse = FALSE))  # FALSE 
# 
# brom_matrix = st_contains(ONS_brom, CID_brom, sparse = FALSE) # 489 columns
# row_id_FALSE = as.data.frame(t(brom_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE")
# brom_false = CID_brom %>%
#   filter(row_number() == 58)
# 
# st_contains(ONS_brom, brom_false, sparse = FALSE) # check got right one
# mapview(brom_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                               lwd = 1, legend = FALSE)
# # Yes RWG204478 should be in Croydon not Bromley
# 
# # Camden
# CID_camden = f_cycle_parking %>%
#   filter(BOROUGH == "Camden") # 1622 observations
# ONS_camden = lon_lad_2020 %>%
#   filter(BOROUGH == "Camden")
# all(st_contains(ONS_camden, CID_camden, sparse = FALSE))  # FALSE
# 
# cam_matrix = st_contains(ONS_camden, CID_camden, sparse = FALSE) # 1622 columns
# row_id_FALSE = as.data.frame(t(cam_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 5
# # 1 FALSE   1043
# # 2 FALSE   1089
# # 3 FALSE   1260
# # 4 FALSE   1266
# # 5 FALSE   1622
# cam_false= CID_camden[c(1043, 1089, 1260, 1266, 1622),]
# st_contains(ONS_camden, cam_false, sparse = FALSE) # all FALSE
# mapview(cam_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                               lwd = 1, legend = FALSE)
# # RWG281472 should be Brent
# # RWG058214 shoudl be City of London
# # RWG056999 Westminster
# # RWG058103 Westminster
# # RWG057782 Westminster
# 
# # City of London
# CID_city = f_cycle_parking %>%
#   filter(BOROUGH == "City of London") # 306 observations
# ONS_city = lon_lad_2020 %>%
#   filter(BOROUGH == "City of London")
# all(st_contains(ONS_city, CID_city, sparse = FALSE))  # FALSE
# 
# city_matrix = st_contains(ONS_city, CID_city, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(city_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 4
# row_id_FALSE
# #      V1 row_id
# # 1 FALSE    117
# # 2 FALSE    279
# # 3 FALSE    280
# # 4 FALSE    302
# city_false= CID_city[c(117, 279, 280, 302),]
# st_contains(ONS_city, city_false, sparse = FALSE) # all FALSE
# mapview(city_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG058206 Camden
# # RWG057800 Tower Hamlets
# # RWG057803 Tower Hamlets
# # RWG057805 Tower Hamlets
# 
# # Croydon
# CID_croy = f_cycle_parking %>%
#   filter(BOROUGH == "Croydon") # 414 observations
# ONS_croy = lon_lad_2020 %>%
#   filter(BOROUGH == "Croydon")
# all(st_contains(ONS_croy, CID_croy, sparse = FALSE))  # all TRUE
# 
# # Ealing
# CID_eal = f_cycle_parking %>%
#   filter(BOROUGH == "Ealing") # 980 observations
# ONS_eal = lon_lad_2020 %>%
#   filter(BOROUGH == "Ealing")
# all(st_contains(ONS_eal, CID_eal, sparse = FALSE))  # FALSE
# 
# eal_matrix = st_contains(ONS_eal, CID_eal, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(eal_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 13
# 
# row_id_FALSE
# # V1 row_id
# # 1  FALSE    130
# # 2  FALSE    241
# # 3  FALSE    246
# # 4  FALSE    247
# # 5  FALSE    305
# # 6  FALSE    306
# # 7  FALSE    358
# # 8  FALSE    378
# # 9  FALSE    492
# # 10 FALSE    533
# # 11 FALSE    634
# # 12 FALSE    651
# # 13 FALSE    652
# eal_false= CID_eal[c(130, 241, 246, 247, 305, 306, 358, 378, 492, 533, 634, 651, 652),]
# st_contains(ONS_eal, eal_false, sparse = FALSE) # all FALSE
# mapview(eal_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                               lwd = 1, legend = FALSE)
# eal_false_list = pull(eal_false, FEATURE_ID)
# print(eal_false_list)
# # all to be Hounslow
# # [1] "RWG232761" "RWG232762" "RWG232768" "RWG232769" "RWG232774" "RWG232775" "RWG232770" "RWG231840"
# # [9] "RWG232763" "RWG232773" "RWG232760" "RWG232765" "RWG232766"
# 
# # Enfield
# CID_enf = f_cycle_parking %>%
#   filter(BOROUGH == "Enfield") # 355 observations
# ONS_enf = lon_lad_2020 %>%
#   filter(BOROUGH == "Enfield")
# all(st_contains(ONS_enf, CID_enf, sparse = FALSE))  # all TRUE
# 
# # Greenwich
# CID_gren = f_cycle_parking %>%
#   filter(BOROUGH == "Greenwich") # 353 observations
# ONS_gren = lon_lad_2020 %>%
#   filter(BOROUGH == "Greenwich")
# all(st_contains(ONS_gren, CID_gren, sparse = FALSE))  # all FALSE
# 
# gren_matrix = st_contains(ONS_gren, CID_gren, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(gren_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 2
# 
# row_id_FALSE
# #      V1 row_id
# # 1 FALSE    184
# # 2 FALSE    289
# gren_false= CID_gren[c(184, 289),]
# st_contains(ONS_gren, gren_false, sparse = FALSE) # all FALSE
# mapview(gren_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG197541 Lewisham
# # RWG171415 Bexley
# 
# # "Hackney"
# CID_hac = f_cycle_parking %>%
#   filter(BOROUGH == "Hackney") # 2292 observations
# ONS_hac = lon_lad_2020 %>%
#   filter(BOROUGH == "Hackney")
# all(st_contains(ONS_hac, CID_hac, sparse = FALSE))  # FALSE
# 
# hac_matrix = st_contains(ONS_hac, CID_hac, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(hac_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 1
# 
# row_id_FALSE # row 120
# hac_false= CID_hac[120,]
# st_contains(ONS_hac, hac_false, sparse = FALSE) # FALSE
# mapview(hac_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                               lwd = 1, legend = FALSE)
# # RWG004681 Islington
# 
# # Hammersmith & Fulham
# CID_hf = f_cycle_parking %>%
#   filter(BOROUGH == "Hammersmith & Fulham") # 1500 observations
# ONS_hf = lon_lad_2020 %>%
#   filter(BOROUGH == "Hammersmith & Fulham")
# all(st_contains(ONS_hf, CID_hf, sparse = FALSE))  # FALSE
# 
# hf_matrix = st_contains(ONS_hf, CID_hf, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(hf_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 1
# 
# row_id_FALSE # row 1000
# hf_false= CID_hf[1000,]
# st_contains(ONS_hf, hf_false, sparse = FALSE) # FALSE
# mapview(hf_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG044634 Hounslow
# 
# # Haringey
# CID_hari = f_cycle_parking %>%
#   filter(BOROUGH == "Haringey") # 620 observations
# ONS_hari = lon_lad_2020 %>%
#   filter(BOROUGH == "Haringey")
# all(st_contains(ONS_hari, CID_hari, sparse = FALSE))  # TRUE
# 
# # Harrow
# CID_harr = f_cycle_parking %>%
#   filter(BOROUGH == "Harrow") # 298 observations
# ONS_harr = lon_lad_2020 %>%
#   filter(BOROUGH == "Harrow")
# all(st_contains(ONS_harr, CID_harr, sparse = FALSE))  # TRUE
# 
# # Havering 
# CID_hav = f_cycle_parking %>%
#   filter(BOROUGH == "Havering") # 275 observations
# ONS_hav = lon_lad_2020 %>%
#   filter(BOROUGH == "Havering")
# all(st_contains(ONS_hav, CID_hav, sparse = FALSE))  # TRUE
# 
# # Hillingdon
# CID_hil = f_cycle_parking %>%
#   filter(BOROUGH == "Hillingdon") # 294 observations
# ONS_hil = lon_lad_2020 %>%
#   filter(BOROUGH == "Hillingdon")
# all(st_contains(ONS_hil, CID_hil, sparse = FALSE))  # TRUE
# 
# # Hounslow
# CID_hou = f_cycle_parking %>%
#   filter(BOROUGH == "Hounslow") # 294 observations
# ONS_hou = lon_lad_2020 %>%
#   filter(BOROUGH == "Hounslow")
# all(st_contains(ONS_hou, CID_hou, sparse = FALSE))  # FALSE
# 
# hou_matrix = st_contains(ONS_hou, CID_hou, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(hou_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 2
# row_id_FALSE
# # V1 row_id
# # 1 FALSE    299
# # 2 FALSE    637
# 
# hou_false= CID_hou[c(299, 637),]
# st_contains(ONS_hou, hou_false, sparse = FALSE) # FALSE
# mapview(hou_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                             lwd = 1, legend = FALSE)
# # RWG232651 Ealing
# # RWG232030 Hammersmith & Fulham
# 
# # Islington
# CID_isl = f_cycle_parking %>%
#   filter(BOROUGH == "Islington") # 1246 observations
# ONS_isl = lon_lad_2020 %>%
#   filter(BOROUGH == "Islington")
# all(st_contains(ONS_isl, CID_isl, sparse = FALSE))  # FALSE
# 
# isl_matrix = st_contains(ONS_isl, CID_isl, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(isl_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 2
# row_id_FALSE
# # V1 row_id
# # 1 FALSE   1091
# # 2 FALSE   1210
# isl_false= CID_isl[c(1091, 1210),]
# st_contains(ONS_isl, isl_false, sparse = FALSE) # FALSE
# mapview(isl_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG056800 City of London
# # RWG056777 Hackney
# 
# # Kensington & Chelsea
# CID_ken = f_cycle_parking %>%
#   filter(BOROUGH == "Kensington & Chelsea") # 1209 observations
# ONS_ken = lon_lad_2020 %>%
#   filter(BOROUGH == "Kensington & Chelsea")
# all(st_contains(ONS_ken, CID_ken, sparse = FALSE))  # FALSE
# 
# ken_matrix = st_contains(ONS_ken, CID_ken, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(ken_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 1
# row_id_FALSE # = 939
# 
# ken_false= CID_ken[939,]
# st_contains(ONS_ken, ken_false, sparse = FALSE) # FALSE
# mapview(ken_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG118004 Westminster
# 
# # Kingston upon Thames
# CID_king = f_cycle_parking %>%
#   filter(BOROUGH == "Kingston upon Thames") # 357 observations
# ONS_king= lon_lad_2020 %>%
#   filter(BOROUGH == "Kingston upon Thames")
# all(st_contains(ONS_king, CID_king, sparse = FALSE))  # TRUE
# 
# # Lambeth
# CID_lam = f_cycle_parking %>%
#   filter(BOROUGH == "Lambeth") # 1429 observations
# ONS_lam = lon_lad_2020 %>%
#   filter(BOROUGH == "Lambeth")
# all(st_contains(ONS_lam, CID_lam, sparse = FALSE))  # FALSE
# 
# lam_matrix = st_contains(ONS_lam, CID_lam, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(lam_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 2
# row_id_FALSE
# # V1 row_id
# # 1 FALSE    963
# # 2 FALSE   1175
# 
# lam_false= CID_lam[c(963, 1175),]
# st_contains(ONS_lam, lam_false, sparse = FALSE) # FALSE
# mapview(lam_false) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG098368 Southwark
# # RWG199655 Southwark
#  
# # Lewisham
# CID_lew = f_cycle_parking %>%
#   filter(BOROUGH == "Lewisham") # 483 observations
# ONS_lew = lon_lad_2020 %>%
#   filter(BOROUGH == "Lewisham")
# all(st_contains(ONS_lew, CID_lew, sparse = FALSE))  # FALSE
# 
# lew_matrix = st_contains(ONS_lew, CID_lew, sparse = FALSE) 
# row_id_FALSE = as.data.frame(t(lew_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 1
# row_id_FALSE  # 459
# 
# lew_false= CID_lew[459,]
# st_contains(ONS_lew, lew_false, sparse = FALSE) # FALSE
# mapview(lew_false) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# # RWG291875 Bromley
# 
# # Merton
# CID_mer = f_cycle_parking %>%
#   filter(BOROUGH == "Merton") # 498 observations
# ONS_mer = lon_lad_2020 %>%
#   filter(BOROUGH == "Merton")
# all(st_contains(ONS_mer, CID_mer, sparse = FALSE))  # all TRUE

# # Newham
# CID_new = f_cycle_parking %>%
#   filter(BOROUGH == "Newham") # 557 observations
# ONS_new = lon_lad_2020 %>%
#   filter(BOROUGH == "Newham")
# all(st_contains(ONS_new, CID_new, sparse = FALSE))  # TRUE
# 
# # # Redbridge
# CID_red = f_cycle_parking %>%
#    filter(BOROUGH == "Redbridge") # 163 observations
# ONS_red = lon_lad_2020 %>%
#   filter(BOROUGH == "Redbridge")
# all(st_contains(ONS_red, CID_red, sparse = FALSE))  # TRUE
# 
# # Richmond upon Thames
# CID_rich = f_cycle_parking %>%
#   filter(BOROUGH == "Richmond upon Thames") # 512 observations
# ONS_rich = lon_lad_2020 %>%
#   filter(BOROUGH == "Richmond upon Thames")
# all(st_contains(ONS_rich, CID_rich, sparse = FALSE))  # TRUE
# 
# # Southwark
# CID_south = f_cycle_parking %>%
#   filter(BOROUGH == "Southwark") # 1511 observations
# ONS_south = lon_lad_2020 %>%
#   filter(BOROUGH == "Southwark")
# all(st_contains(ONS_south, CID_south, sparse = FALSE))  # TRUE
# 
# # Sutton
# CID_sut = f_cycle_parking %>%
#   filter(BOROUGH == "Sutton") # 307 observations
# ONS_sut = lon_lad_2020 %>%
#   filter(BOROUGH == "Sutton")
# all(st_contains(ONS_sut, CID_sut, sparse = FALSE))  # TRUE
# 
# # Tower Hamlets
# CID_tower = f_cycle_parking %>%
#   filter(BOROUGH == "Tower Hamlets") # 851 observations
# ONS_tower = lon_lad_2020 %>%
#   filter(BOROUGH == "Tower Hamlets")
# all(st_contains(ONS_tower, CID_tower, sparse = FALSE))  # FALSE
# 
# tower_matrix = st_contains(ONS_tower, CID_tower, sparse = FALSE)
# row_id_FALSE = as.data.frame(t(tower_matrix)) %>%
#   mutate(row_id = row_number()) %>%
#   filter(V1 == "FALSE") # n = 1
# row_id_FALSE  # 350
# 
# tower_false= CID_tower[350,]
# st_contains(ONS_tower, tower_false, sparse = FALSE) # FALSE
# mapview(tower_false) + mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH",
#                              lwd = 1, legend = FALSE)
# RWG067682 Hackney

# # Waltham Forest
# CID_walt = f_cycle_parking %>%
#   filter(BOROUGH == "Waltham Forest") # 627 observations
# ONS_walt = lon_lad_2020 %>%
#   filter(BOROUGH == "Waltham Forest")
# all(st_contains(ONS_walt, CID_walt, sparse = FALSE))  # TRUE
# 
# # Wandsworth
# CID_wan = f_cycle_parking %>%
#   filter(BOROUGH == "Wandsworth") # 759 observations
# ONS_wan = lon_lad_2020 %>%
#   filter(BOROUGH == "Wandsworth")
# all(st_contains(ONS_wan, CID_wan, sparse = FALSE))  # TRUE

# # "Westminster"
# CID_wes = f_cycle_parking %>%
#   filter(BOROUGH == "Westminster") # 1603 observations
# ONS_wes = lon_lad_2020 %>%
#   filter(BOROUGH == "Westminster")
# all(st_contains(ONS_wes, CID_wes, sparse = FALSE))  # TRUE
