###################################################################################
# Data cleaning CID - ASL                                                         #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observation (n = 1) that has no Borough assigned                 #
# It also checks that the Boroughs are correctly assigned for the rest of the ASL #
#                                                                                 #
###################################################################################

######################################
# Install packages and load datasets #
######################################

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(mapview)
library(forcats)
library(units)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE)

# Import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Download ASL CID data using the Cycle Infra Lnd package
advanced_stop_line = get_cid_lines(type = "advanced_stop_line") # n = 3775

# Convert CRS so matches ONS boundary data CRS
advanced_stop_line = st_transform(advanced_stop_line, crs=27700) 
st_crs(advanced_stop_line) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################

unique(advanced_stop_line$FEATURE_ID) # 3775 unique variables
unique(advanced_stop_line$BOROUGH) # 33 Boroughs plus a NA group
unique(advanced_stop_line$SVDATE) # 290 unique survey dates, all of which are valid date

# the below all have just true and false apart from colour that has 6 options
unique(advanced_stop_line$ASL_FDR)
unique(advanced_stop_line$ASL_FDRLFT)
unique(advanced_stop_line$ASL_FDCENT)
unique(advanced_stop_line$ASL_FDRIGH)
unique(advanced_stop_line$ASL_SHARED)
unique(advanced_stop_line$ASL_COLOUR)  # "NONE" "GREEN" "RED" "BUFF/YELLOW" "BLUE" "OTHER"

# examine URL data
count_photo1 =  advanced_stop_line %>%
  count(PHOTO1_URL) # 48 have no asset photo 1
count_photo2 =  advanced_stop_line %>%
  count(PHOTO2_URL) # 51 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

# Convert character columns to factors
## Create list of columns to factor
f_variables = c("ASL_FDR", "ASL_FDRLFT", "ASL_FDCENT", "ASL_FDRIGH", 
                "ASL_SHARED", "ASL_COLOUR")

# Factor columns (CLT_ACCESS not converted as 721 different values)
f_advanced_stop_line = advanced_stop_line %>%
  mutate_at(f_variables, as.factor)

######################################################
# Check to make sure all line strings ie single ASLs #
######################################################

# Examine original 1687 obs
unique(st_geometry_type(f_advanced_stop_line)) # linestring
sum(st_length(f_advanced_stop_line)) # = 17352.14 [m]


###################
# Tidy up BOROUGH #
###################

anyNA(f_advanced_stop_line$BOROUGH) # = TRUE

asl_borough_split = st_intersection(lon_lad_2020, f_advanced_stop_line) # n = 3777 ie 2 extra

anyNA(asl_borough_split$BOROUGH) # FALSE
anyNA(asl_borough_split$BOROUGH.1) # TRUE

multi_feature_id = asl_borough_split %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs == 2) %>%
  ungroup() 
# FEATURE_ID num_obs
# RWG055663        2
# RWG199118        2

# create list of FEATURE_IDs that have 2 segments
multi_feature_id_list = pull(multi_feature_id, FEATURE_ID) # create list of the FEATURE_ID 

# create dataset containing the observations with these FEATURE_IDs
new_segments = asl_borough_split %>%
  filter(FEATURE_ID %in% multi_feature_id_list) # n = 4, the 2 original ones 
# plus the additional 2 created when they were split

segments = mapview(new_segments, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE, lwd = 1)
leafem::addStaticLabels(segments, label = new_segments$FEATURE_ID) # plot segments with the FEATURE_IDs labelled

# Can see that RWG199118 starts in Greenwich and RWG055663 starts in Lambeth   
# The traffic lights that these ASLs relate to also are located in these Boroughs 
# RWG055663 is already correctly coded as Lambeth
# Just correct RWG199118 as Greenwich and factor the BOROUGH variable
# Correct NA as Greenwich and factor the BOROUGH variable - SEE correct_borough_NAs file for why Greenwich
f_advanced_stop_line$BOROUGH = factor(f_advanced_stop_line$BOROUGH) %>%
  fct_explicit_na(na_level = "Greenwich")
#anyNA(f_advanced_stop_line$BOROUGH) # = FALSE so no NAs

# Code to check missing url data post correction of Borough etc
# f_advanced_stop_line = readRDS(file = "data/cleansed_asl")
# count_photo1 =  f_advanced_stop_line %>%
#   st_drop_geometry() %>%
#   count(PHOTO1_URL) # 48 have no asset photo 1
# count_photo2 =  f_advanced_stop_line %>%
#   st_drop_geometry() %>%
#   count(PHOTO2_URL) # 51 have no asset photo 2

######################
# SAVE CLEAN DATASET #
######################
#saveRDS(f_advanced_stop_line, file = "data/cleansed_asl")

