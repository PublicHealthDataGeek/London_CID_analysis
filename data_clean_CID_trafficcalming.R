###################################################################################
# Data cleaning CID - Traffic calming                                             #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #                        #
# It also checks that the Boroughs are correctly assigned                         #
# 138 observations incorrectly located and reallocated                            #
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
traffic_calming = get_cid_points(type = "traffic_calming")

# check and convert CRS so matches ONS boundary data CRS
traffic_calming = st_transform(traffic_calming, crs=27700) 
# st_crs(traffic_calming) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################
unique(traffic_calming$FEATURE_ID) # 58565 unique variables
unique(traffic_calming$BOROUGH) # 33 Boroughs, no NAs
unique(traffic_calming$SVDATE) # 334 unique survey dates, all of which are valid date

# the below all have just true and false
unique(traffic_calming$TRF_RAISED)
unique(traffic_calming$TRF_ENTRY)
unique(traffic_calming$TRF_CUSHI)
unique(traffic_calming$TRF_HUMP)
unique(traffic_calming$TRF_SINUSO)
unique(traffic_calming$TRF_BARIER)
unique(traffic_calming$TRF_NAROW)
unique(traffic_calming$TRF_CALM)

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################
# convert certain columns to factors
f_variables = c("TRF_RAISED", "TRF_ENTRY", "TRF_CUSHI", "TRF_HUMP", "TRF_SINUSO",
                "TRF_BARIER", "TRF_NAROW", "TRF_CALM", "BOROUGH")

# convert columns to factors 
f_traffic_calming = traffic_calming %>%
  mutate_at(f_variables, as.factor)

######################################
# Check to see if BOROUGH is correct #
######################################

# Visually inspect 
mapview(f_traffic_calming, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
          lwd = 1, legend = FALSE) # Visual inspection all appears to be fine but lots of obs

# Spatially join parking dataset to ONS dataset
joined = st_join(f_traffic_calming, lon_lad_2020)

# check borough.x (CID) matches borough.y (ONS)
nonmatch_pre = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 138 observations

# Run loop to run through and recode BOROUGH.x (CID) with BOROUGH.y (ONS)
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
traffic_calming_corrected = joined %>%
  select(c("FEATURE_ID", "SVDATE", "TRF_RAISED", "TRF_ENTRY", "TRF_CUSHI", 
           "TRF_HUMP", "TRF_SINUSO", "TRF_BARIER", "TRF_NAROW", "TRF_CALM",
           "BOROUGH.x", "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  rename("BOROUGH" = "BOROUGH.x")

######################
# SAVE CLEAN DATASET #
######################
#saveRDS(traffic_calming_corrected, file = "data/cleansed_trafficcalming")
