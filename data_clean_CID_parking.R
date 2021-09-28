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
#saveRDS(parking_corrected, file = "data/cleansed_parking")

