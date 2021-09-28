###################################################################################
# Data cleaning CID - Signals                                                     #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that has no Borough assigned                        #
# It also checks that the Boroughs are correctly assigned                         #
# 2 were reassigned                                                               #
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
signal = get_cid_points(type = "signal") #n = 443

# Convert CRS so matches ONS boundary data CRS
signal = st_transform(signal, crs=27700) 
st_crs(signal) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################
unique(signal$FEATURE_ID) # 443 unique variables
unique(signal$BOROUGH) # 23 Boroughs, no NAS
unique(signal$SVDATE) # 111 unique survey dates, all of which are valid dates

# the below all have just true and false
unique(signal$SIG_HEAD)
unique(signal$SIG_SEPARA)
unique(signal$SIG_EARLY)
unique(signal$SIG_TWOSTG)
unique(signal$SIG_GATE)

# examine URL data
count_photo1 =  f_signal %>%
  count(PHOTO1_URL) # 8 have no asset photo 1
count_photo2 =  signal %>%
  count(PHOTO2_URL) # 8 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################
# Convert columns to factors 
### create list of columns to factor (Borough included as no NAs)
f_variables = c("SIG_HEAD", "SIG_SEPARA", "SIG_EARLY", "SIG_TWOSTG", "SIG_GATE")

### factor columns
f_signal = signal %>%
  mutate_at(f_variables, as.factor)

# create levels for Borough and factor 
borough_levels = pull(lon_lad_2020, BOROUGH) 
f_signal$BOROUGH = factor(f_signal$BOROUGH, levels = borough_levels)

######################################
# Check to see if BOROUGH is correct #
######################################
# Visually inspect 
mapview(f_signal, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
          lwd = 1, legend = FALSE) # Visual inspection all appears to be fine

# Spatially join parking dataset to ONS dataset
joined = st_join(f_signal, lon_lad_2020)

# check borough.x (CID) matches borough.y (ONS)
nonmatch_pre = joined %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y", "geometry")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 2 observations

# mapview(nonmatch_pre) + 
#   mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#           lwd = 1, legend = FALSE) 

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
signals_corrected = joined %>%
  select(c("FEATURE_ID", "SVDATE", "SIG_HEAD", "SIG_SEPARA", "SIG_EARLY",
           "SIG_TWOSTG", "SIG_GATE", "BOROUGH.x", "PHOTO1_URL", "PHOTO2_URL", 
           "geometry")) %>%
  rename("BOROUGH" = "BOROUGH.x")



######################
# SAVE CLEAN DATASET #
######################
#saveRDS(f_signal, file = "data/cleansed_signals")

