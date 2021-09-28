###################################################################################
# Data cleaning CID - Signage                                                     #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #                        
# It also checks that the Boroughs are correctly assigned                         #
# 2 NAs outside London so dropped, 6 further observations had boroughs but were   #
# outside london so dropped.                                                      #
# 398 observations incorrectly located and reallocated                            #
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
signage = get_cid_points(type = "signage") # n = 118834

# Convert CRS so matches ONS boundary data CRS
signage = st_transform(signage, crs=27700) 
st_crs(signage) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################
unique(signage$FEATURE_ID) # 118833 unique variables RWG999275 is present twice. 
unique(signage$BOROUGH) # 33 Boroughs plus a NA group, 2 observation have NA for Borough 
unique(signage$SVDATE) # 349 unique survey dates, all of which are valid date

# the below all have just true and false except where specified:
unique(signage$SS_ROAD) # TRUE FALSE NA - 1 observations has NA
unique(signage$SS_PATCH)
unique(signage$SS_FACING)
unique(signage$SS_NOCYC)
unique(signage$SS_NOVEH)
unique(signage$SS_CIRC)
unique(signage$SS_EXEMPT)
unique(signage$SS_NOLEFT)
unique(signage$SS_NORIGH)
unique(signage$SS_LEFT)
unique(signage$SS_RIGHT)
unique(signage$SS_NOEXCE)
unique(signage$SS_DISMOU)
unique(signage$SS_END)
unique(signage$SS_CYCSMB) # TRUE FALSE FASLE
unique(signage$SS_PEDSMB)
unique(signage$SS_BUSSMB)
unique(signage$SS_SMB)
unique(signage$SS_LNSIGN)
unique(signage$SS_ARROW)
unique(signage$SS_NRCOL)
unique(signage$SS_NCN)
unique(signage$SS_LCN)
unique(signage$SS_SUPERH)
unique(signage$SS_QUIETW)
unique(signage$SS_GREENW)
unique(signage$SS_ROUTEN)  # 429 different names including 105883 observations with NA and 3 wih no data
unique(signage$SS_DESTN)
unique(signage$SS_ACCESS) # 802 different names including 115602 observations with NA and 5 with no data
unique(signage$SS_NAME) # 65 unique labels including 3156 NA plus 2648 obs with no data
unique(signage$SS_COLOUR) #  NONE GREEN RED BLUE NA <Null> BUFF/YELLOW: 3 NA, 2 <NULL>, 117188 NONE

# examine URL data
count_photo1 =  signage %>%
  count(PHOTO1_URL) # 1347 have no asset photo 1
count_photo2 =  signage %>%
  count(PHOTO2_URL) # 1336 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

# convert certain columns to factors
f_variables = c("SS_ROAD", "SS_PATCH", "SS_FACING", "SS_NOCYC", "SS_NOVEH",
                "SS_NOLEFT", "SS_NORIGH", "SS_LEFT", "SS_RIGHT", "SS_NOEXCE",
                "SS_DISMOU", "SS_END", "SS_CYCSMB", "SS_PEDSMB", "SS_BUSSMB",
                "SS_SMB", "SS_LNSIGN", "SS_ARROW", "SS_NRCOL", "SS_NCN",
                "SS_LCN", "SS_SUPERH", "SS_QUIETW", "SS_GREENW",
                "SS_DESTN", "SS_CIRC", "SS_EXEMPT")

# convert columns to factors 
f_signage = signage %>%
  mutate_at(f_variables, as.factor)

# Factor these other columns separately
f_signage$SS_ROAD = factor(signage$SS_ROAD, exclude = NULL) 
f_signage$SS_COLOUR = factor(signage$SS_COLOUR, exclude = NULL)

# recode SS_CYCSMB where values include "FALSE" and "FASLE"
f_signage$SS_CYCSMB= fct_collapse(f_signage$SS_CYCSMB, 
                                  "FALSE" = c("FALSE", "FASLE"))

# examine duplicate FEATURE_ID RWG999275
# RWG_duplicate = f_signage %>%
#   filter(FEATURE_ID == "RWG999275")
# ## visually inspect - they are in different Boroughs
# mapview(RWG_duplicate) + mapview(lon_lad_2020, alpha.regions = 0.5, zcol = "BOROUGH",
#                                  lwd = 1, legend = FALSE) 
## create new FEATURE_IDs so that observations can be differentiated
f_signage$FEATURE_ID[f_signage$BOROUGH == "Barnet"] = "RWG999275a"
f_signage$FEATURE_ID[f_signage$BOROUGH == "Haringey"] = "RWG999275b"

###########################
# Correct Boroughs inc NA #
###########################

signage_NA = f_signage %>%
  filter(is.na(BOROUGH)) # 2

signage_not_NA = f_signage %>%
  filter(!is.na(BOROUGH)) # 118832

# 1) Review NAs first
pre_borough_cleanse_map = mapview(signage_NA, zcol = "BOROUGH", na.color = "red") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)         
# These 2 observations are outside London Boroughs so drop
drop_ids = pull(signage_NA, FEATURE_ID) 

# drop these FEATURE_IDs from dataset
f_signage = f_signage %>%
  filter(!FEATURE_ID %in% drop_ids) # n = 118832 (ie 2 less) 

# 2) Check to see if Boroughs correctly assigned
# Spatially join parking dataset to ONS dataset
joined = st_join(f_signage, lon_lad_2020) # n = 118832

# initial attempts to looping code resulted in 6 observations being identified 
# as being outside London according to ONS despite having Borough coded in CID
# these are then dropped.  
# x = joined %>%
#   filter(is.na(BOROUGH.y)) # n = 6, they have 
# unique(x$BOROUGH.x) # Bexley and Croydon
# unique(x$BOROUGH.y) #  <NA>
# 
# mapview(x) + 
#   mapview(lon_lad_2020, alpha.regions = 0.1, legend = FALSE, lwd = 1)     
# # These 6 according to ONS are outside London Boroughs so need to drop
drop_ids_2 = pull(x, FEATURE_ID)

# drop these FEATURE_IDs from dataset
f_signage = f_signage %>%
  filter(!FEATURE_ID %in% drop_ids_2) # n = 118826 (ie 6 less) 

# Now rerun join 
joined = st_join(f_signage, lon_lad_2020) # n = 118826

# check borough.x (CID) matches borough.y (ONS)
nonmatch_pre = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 398 observations

## Run loop to run through and recode BOROUGH.x (CID) with BOROUGH.y (ONS)
counter = 0 # create counter so can check how many observations are changed
for (i in seq_along(joined$BOROUGH.x)) {
  if(joined$BOROUGH.y[[i]] != joined$BOROUGH.x[[i]]) {
    counter = counter + 1
  }
  joined$BOROUGH.y[[i]] = joined$BOROUGH.x[[i]]
}
#counter = 398

# run post-transformation check
nonmatch_post = joined %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID", "BOROUGH.x", "BOROUGH.y")) %>%
  mutate(match = ifelse(BOROUGH.x == BOROUGH.y, TRUE, FALSE)) %>%
  rename(c("CID" = "BOROUGH.x", "ONS" = "BOROUGH.y")) %>%
  filter(match == "FALSE") # 0 observations

# Finalise dataset
signage_corrected = joined %>%
  select(c("FEATURE_ID", "SVDATE", "SS_ROAD", "SS_PATCH", "SS_FACING", "SS_NOCYC", "SS_NOVEH",
           "SS_CIRC", "SS_EXEMPT", "SS_NOLEFT", "SS_NORIGH", "SS_LEFT", "SS_RIGHT", "SS_NOEXCE",
           "SS_DISMOU", "SS_END", "SS_CYCSMB", "SS_PEDSMB", "SS_BUSSMB",
           "SS_SMB", "SS_LNSIGN", "SS_ARROW", "SS_NRCOL", "SS_NCN",
           "SS_LCN", "SS_SUPERH", "SS_QUIETW", "SS_GREENW", "SS_ROUTEN",
           "SS_DESTN", "SS_ACCESS", "SS_NAME",  "SS_COLOUR",
           "BOROUGH.x", "PHOTO1_URL", "PHOTO2_URL", "geometry")) %>%
  rename("BOROUGH" = "BOROUGH.x")

######################
# SAVE CLEAN DATASET #
######################

#saveRDS(signage_corrected, file = "data/cleansed_signage")
