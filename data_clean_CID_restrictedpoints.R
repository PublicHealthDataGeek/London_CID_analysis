###################################################################################
# Data cleaning CID - Restricted Points                                           #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #                        
# It also checks that the Boroughs are correctly assigned                         #                                                      #
# 2 observations incorrectly located and reallocated.                             #
#                                                                                 #
# Code rerun Jan 2022 to ensure that spatial data is not affected by gdal library #
# linux issue.                                                                    #
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
mapviewOptions(native.crs = TRUE, fgb = FALSE)

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# download CID data using the Cycle Infra Lnd package
restricted_points = get_cid_points(type = "restricted_point")

# Convert CRS so matches ONS boundary data CRS
restricted_points = st_transform(restricted_points, crs=27700) 
st_crs(restricted_points) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################
unique(restricted_points$FEATURE_ID) # 180 unique variables
unique(restricted_points$BOROUGH) # 27 Boroughs, no NAS
unique(restricted_points$SVDATE) # 73 unique survey dates, all of which are valid dates

# the below all have just true and false
unique(restricted_points$RST_STEPS)
unique(restricted_points$RST_LIFT)

# examine URL data
count_photo1 =  restricted_points %>%
  st_drop_geometry() %>%
  count(PHOTO1_URL) # 12 have no asset photo 1
count_photo2 =  restricted_points %>%
  st_drop_geometry() %>%
  count(PHOTO2_URL) # 12 have no asset photo 2


###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

# convert certain columns to factors
f_variables = c("RST_STEPS", "RST_LIFT")

# convert columns to factors 
f_restricted_points = restricted_points %>%
  mutate_at(f_variables, as.factor)

# create levels for Borough 
borough_levels = pull(lon_lad_2020, BOROUGH) 
f_restricted_points$BOROUGH = factor(f_restricted_points$BOROUGH, levels = borough_levels)


######################################
# Check to see if BOROUGH is correct #
######################################

# Visually inspect 
mapview(f_restricted_points, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
          lwd = 1, legend = FALSE) # Visual inspection all appears to be fine

# Spatially join RP dataset to ONS dataset
joined = st_join(f_restricted_points, lon_lad_2020)

# check borough.x (CID) matches borough.y (ONS)
nonmatch_pre = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 2 observations

# Run loop to run through and recode BOROUGH.x (CID) with BOROUGH.y (ONS)
counter = 0 # create counter so can check how many observations are changed
for (i in seq_along(joined$BOROUGH.x)) {
  if(joined$BOROUGH.y[[i]] != joined$BOROUGH.x[[i]]) {
    counter = counter + 1
  }
  joined$BOROUGH.y[[i]] = joined$BOROUGH.x[[i]]
}
# counter = 2

# check worked
nonmatch_post = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 0 observations

# Finalise dataset
restrictedpoints_corrected = joined %>%
  select(c("FEATURE_ID", "SVDATE", "RST_STEPS", "RST_LIFT", 
           "BOROUGH.x", "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  rename("BOROUGH" = "BOROUGH.x")

######################
# SAVE CLEAN DATASET #
######################
#saveRDS(restrictedpoints_corrected, file = "data/cleansed_restrictedpoints_24_01_2022")

