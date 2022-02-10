###################################################################################
# Data cleaning CID - restricted routes                                           #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that has no Borough assigned                        #
# It also checks that the Boroughs are correctly assigned where they exist        #
# Where restricted routes cross more than one London Borough they are split       #
# into multiple observations based on their Borough location                      #
#                                                                                 #
# Code rerun Jan 2022 to ensure that the spatial data issue affecting gdal        #
# libraries and linux has not affected this spatial data.                         #
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
restricted_route = get_cid_lines(type = "restricted_route")

# Convert CRS so matches ONS boundary data CRS
restricted_route = st_transform(restricted_route, crs=27700) 
st_crs(restricted_route) # PROJCRS["OSGB 1936 / British National Grid"

###################################
# Check completeness of variables #
###################################
unique(restricted_route$FEATURE_ID) # 1378 unique variables
unique(restricted_route$BOROUGH) # 33 Boroughs plus a NA group
unique(restricted_route$SVDATE) # 196 unique survey dates, all of which are valid date

# the below all have just true and false 
unique(restricted_route$RES_PEDEST)
unique(restricted_route$RES_BRIDGE)
unique(restricted_route$RES_TUNNEL)
unique(restricted_route$RES_STEPS)
unique(restricted_route$RES_LIFT)

# examine URL data
count_photo1 =  restricted_route %>%
  st_drop_geometry() %>%
  count(PHOTO1_URL) # 71 have no asset photo 1
count_photo2 =  restricted_route %>%
  st_drop_geometry() %>%
  count(PHOTO2_URL) # 52 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################
# Convert certain columns to factors (CLT_ACCESS and BOROUGH not done)
f_variables = c("RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", "RES_STEPS", "RES_LIFT")

f_restricted_route = restricted_route %>%
  mutate_at(f_variables, as.factor)

anyNA(f_restricted_route$BOROUGH) # = TRUE

###################
# Tidy up BOROUGH #
###################
# Mapping the restricted routes shows that some have no Borough (NA), other 
# restricted routes cross into other Boroughs but they are not assigned to that
# Borough
pre_borough_cleanse_map = mapview(f_restricted_route, zcol = "BOROUGH", na.colour = "red") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)
## NA not showing as red???


# As comparing Boroughs by count and length, need to ensure that each bit of
# restricted route is correctly assigned to Borough.  

# Will deal with observations that have a Borough coded first then move onto sorting
# out those without a Borough. 

# Create datasets for those with and without a Borough coded - totals add up to 1378 (correct!)
rr_borough = f_restricted_route %>%
  filter(!is.na(BOROUGH)) # 1360 observations 

rr_borough_NA = f_restricted_route %>%
  filter(is.na(BOROUGH)) # 18 observations ie 18 restricted routes have no Borough

##### PART 1 - ensure that all observations have the correct Borough attached
rr_borough_split = st_intersection(lon_lad_2020, rr_borough) 
# n = 1363 ie 3 more than 1360
# all observations in this dataset have BOROUGH from the ONS dataset and BOROUGH.1 from the CID 
# there are no missing values in this dataset
#anyNA(rr_borough_split$BOROUGH) # FALSE
#anyNA(rr_borough_split$BOROUGH.1) # FALSE

# find newly created segments by identifying FEATURE_IDs with more than 1 observation
multi_feature_id = rr_borough_split %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs == 2) %>%  # checked that there were no observations split into 3, max is 2
  ungroup() 

# create list of FEATURE_IDs that have 2 segments
multi_feature_id_list = pull(multi_feature_id, FEATURE_ID) # create list of the FEATURE_ID 

# create dataset containing the observations with these FEATURE_IDs
new_segments = rr_borough_split %>%
  filter(FEATURE_ID %in% multi_feature_id_list) # n = 6, the 3 original ones 
# plus the additional 3 created when they were split

mapview(new_segments, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE, lwd = 1)

# therefore, would expect that for 3 the BOROUGH will match BOROUGH.1 but for 3 it wont.
new_segment_match_pre = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1"))  # Yes - this what we see.

# Now to ensure the BOROUGH.1 (CID) are correctly coded with the ONS BOROUGH 
## 1) Need to factor BOROUGH.1 
borough_levels = c("Barking & Dagenham", "Barnet", "Bexley", "Brent",  
                   "Bromley", "Camden", "City of London", "Croydon", 
                   "Ealing", "Enfield", "Greenwich", "Hackney",  
                   "Hammersmith & Fulham", "Haringey", "Harrow", 
                   "Havering", "Hillingdon", "Hounslow", "Islington", 
                   "Kensington & Chelsea", "Kingston upon Thames",  
                   "Lambeth", "Lewisham", "Merton", "Newham", 
                   "Redbridge", "Richmond upon Thames", "Southwark",  
                   "Sutton", "Tower Hamlets", "Waltham Forest",   
                   "Wandsworth", "Westminster") 
new_segments$BOROUGH.1 = factor(new_segments$BOROUGH.1, levels = borough_levels)

## 2) Run loop to run through and recode BOROUGH.1 (CID) with BOROUGH (ONS)
for (i in seq_along(new_segments$BOROUGH)) {
  new_segments$BOROUGH.1[[i]] = new_segments$BOROUGH[[i]]
}

## 3) Check segments have been recoded correctly
new_segment_match_post = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1")) # Now all match

# Relabel FEATURE_ID so each segment can be identified
##  Group 144 by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1 or 2)
new_segments_corrected = new_segments %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(count = cumsum(n)) %>%
  select(-n)

## relabel the FEATURE_ID with the cumulative number so each observation has a unique ID
new_segments_corrected$FEATURE_ID = 
  paste(new_segments_corrected$FEATURE_ID, "_", new_segments_corrected$count)

## Check that the correct number should have 1 or 2 appended to the FEATURE_ID
new_segments_corrected %>%
  st_drop_geometry() %>%
  group_by(count) %>%
  count()
#   count   n
#       1   3
#       2   3

# check geometry 
st_geometry_type(new_segments_corrected) # all multilinestring

# Create df of all the observations with correct Boroughs that can be joined
rr_borough_corrected = new_segments_corrected %>%
  select(c("FEATURE_ID", "SVDATE", "RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", 
           "RES_STEPS", "RES_LIFT","BOROUGH", 
           "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  mutate(BOROUGH = as.character(BOROUGH)) # change borough to character so can match back to f_restricted_routes

# Remove the 3 observations from f_restricted_route that have been replaced by the 6 new observations
f_restricted_route = f_restricted_route %>%
  filter(!FEATURE_ID %in% multi_feature_id_list) # n = 1375 ie 1378 - 3 THis is correct

# Join rr_borough_corrected to f_restricted_route - will then have 1381 observations
f_restricted_route = rbind(f_restricted_route, rr_borough_corrected) # n= 1381 (1375+6)

# If visualise again, now looks correct - just need to sort out observations with BOROUGH NAs
mid_borough_cleanse_map = mapview(f_restricted_route, zcol = "BOROUGH", na.color = "red") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)

##### PART 2 - Add Borough for those that are NA

# 1) identify missing Borough details
restricted_route_borough_NA = f_restricted_route %>%
  filter(is.na(BOROUGH)) # 18 observations ie 18 restricted routes no Borough

# 2) Split each observation into segments using ONS borough boundaries 
restricted_route_borough_NA_i = st_intersection(lon_lad_2020, restricted_route_borough_NA) # 36 observations, 
# geometry column is from the lanes dataset
# this has broken each unique FEATURE_ID into segments based on whether they cross a borough line NB each segment may contain a MLS
# but contains multiple geometry types
# summary(restricted_route_borough_NA_i$geometry) # -> 27 linestrings and 9 multilinestrings
restricted_route_borough_NA_i = restricted_route_borough_NA_i %>%
  st_cast("MULTILINESTRING") # convert geometry type to MLS for all so can mapview
# summary(restricted_route_borough_NA_i$geometry) # -> 36 MLS.  Now can mapview(lanes_borough_NA_i)

rr_NA_map = mapview(restricted_route_borough_NA_i, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE)

# 3) Count number of observations for each FEATURE_ID
count_obs = restricted_route_borough_NA_i %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n()) %>%
  group_by(num_obs) %>%
  count()
#   num_obs     n
#1       1      1 # 1 FEATURE_ID has one observation
#2       2     16 # 16 FEATURE_IDs have two observations
#3       3      1  #  1 FEATURE_ID has 3 observations


# 4) Relabel FEATURE_ID so each segment can be identified
# Group 36 by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1, 2 or 3)
restricted_route_NA_corrected = restricted_route_borough_NA_i %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(cum_count = cumsum(n)) %>%
  select(-n)

# relabel the FEATURE_ID with the cumulative number so each observation has a unique ID
restricted_route_NA_corrected$FEATURE_ID = paste(restricted_route_NA_corrected$FEATURE_ID, "_", 
                                                 restricted_route_NA_corrected$cum_count)

# 5) Check that the correct number have 1, 2 or 3 appended to the FEATURE_ID
restricted_route_NA_corrected %>%
  st_drop_geometry() %>%
  group_by(cum_count) %>%
  count()

# cum_count     n   # This looks to be correct
#         1    18  = 1 + 16 + 1
#         2    17  = 1 + 16
#         3     1

# 6) Create df of all the observations with correct Boroughs that can be joined
restricted_route_NA_corrected = restricted_route_NA_corrected %>%
  select(c("FEATURE_ID", "SVDATE", "RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", 
           "RES_STEPS", "RES_LIFT", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL", 
           "geometry", )) %>%
  mutate(BOROUGH = as.character(BOROUGH))
# change borough to character so can match back to f_restricted_route
# NB geometry is already Multiline string so no need to convert

# 7) Validate that have corrected NAs so count NAs before transformation
count_restricted_route_borough = f_restricted_route %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Count = n()) 

# 8) Join main lanes dataset to corrected borough dataset
#  8a) drop obs with no boroughs from f_cycle_lane_track
f_restricted_route = f_restricted_route %>%
  filter(!is.na(BOROUGH)) # 1363 observations ie  the 1381 - 18 NAs
anyNA(f_restricted_route$BOROUGH) # = FALSE ie all dropped

#  8b) join corrected observations to the f_restricted_route
f_restricted_route = rbind(f_restricted_route, restricted_route_NA_corrected) # = 1399 (1963 + 36) 
anyNA(f_restricted_route$BOROUGH) # = FALSE

# 9) Validate have correctly transformed
#  9a) Recount Boroughs after transformation
recount_restricted_route_borough = f_restricted_route %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Recount = n()) 

#  9b) Check how many recoded
number_recoded = restricted_route_NA_corrected %>%  
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(Number_recoded = n())

#  9c) Create dataframe of counts 
restricted_route_boroughs = left_join(count_restricted_route_borough, number_recoded) %>%
  left_join(recount_restricted_route_borough) # This looks to be correct

# 10) Visually inspect restricted routes to check now coded correct Borough
post_borough_cleanse_map = mapview(f_restricted_route, zcol = "BOROUGH", na.colour = "red") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)

######################
# SAVE CLEAN DATASET #
######################
saveRDS(f_restricted_route, file = "data/cleansed_restricted_route_24_01_2022")


