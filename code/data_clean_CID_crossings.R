###################################################################################
# Data cleaning CID - crossings                                                   #
#                                                                                 #
# This code downloads the CID and cleans up the variables                         #
# It recodes the observations that have no Borough assigned (n = 29)              #
# It also checks that the Boroughs are correctly assigned where they exist        #
#                                                                                 #
# Code rerun in Jan 2022 to ensure that spatial data not affected by gdal library #
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

# Set Mapview options to use data CRS rather than OSM projections
mapviewOptions(native.crs = TRUE, fgb = FALSE)

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# download CID data using the Cycle Infra Lnd package
crossings = get_cid_lines(type = "crossing") # n = 1687

# Convert CRS so matches ONS boundary data CRS
crossings = st_transform(crossings, crs=27700) 
st_crs(crossings) # PROJCRS["OSGB 1936 / British National Grid",

###################################
# Check completeness of variables #
###################################

unique(crossings$FEATURE_ID) # 1687 unique variables
unique(crossings$BOROUGH) # 33 Boroughs plus a NA group
unique(crossings$SVDATE) # 257 unique survey dates, all of which are valid date

# the below all have just true and false
unique(crossings$CRS_SIGNAL)
unique(crossings$CRS_CYGAP)
unique(crossings$CRS_LEVEL)
unique(crossings$CRS_PEDEST)
unique(crossings$CRS_SEGREG)

# examine URL data
count_photo1 =  crossings %>%
  st_drop_geometry() %>%
  count(PHOTO1_URL) # 31 have no asset photo 1
count_photo2 =  crossings %>%
  st_drop_geometry() %>%
  count(PHOTO2_URL) # 32 have no asset photo 2

###############################################
# Tidy up variables and data - except BOROUGH #
###############################################

# Convert certain columns to factors
## Create list of columns to factor
f_v = c("CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", "CRS_LEVEL")

## Factor columns 
# (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_crossings = crossings %>%
  mutate_at(f_v, as.factor)

##########################################################################
# Create dataset where each observation is a single crossing             #
# (as 265 crossing observations actually contain more than one crossing) #
##########################################################################

# Examine original 1687 obs
unique(st_geometry_type(f_crossings)) # Multilinestring
sum(st_length(f_crossings)) # = 19907.02 [m]

# convert multilinestrings to linestrings
crossings_ls = f_crossings %>%
  st_cast("LINESTRING")
unique(st_geometry_type(crossings_ls)) # Linestring, 1987 observations
sum(st_length(crossings_ls)) # = 19907.02 [m] # check length is the same

crossings_ls %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID) %>%
  summarise(number_LS = n()) %>%
  group_by(number_LS) %>%
  summarise(obs = n())
#   number_LS   obs
# 1         1  1422  ie 1422 observations just contained 1 crossing
# 2         2   238  238 observations contain 2 crossings
# 3         3    19  19 contain 3 crossings
# 4         4     8  8 contain 4 crossings
# TOTAL SUM = 1987

# Create new IDs for these crossings so each crossing can be identified
# Group by FEATURE_ID then add a cumulative sum to each grouped observation (will be 1, 2. 3 or 4)
crossings_ls_corrected = crossings_ls %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID) %>%
  mutate(count = cumsum(n)) %>%
  select(-n)

## Create new FEATURE_ID with the cumulative number so each observation has a unique ID
crossings_ls_corrected$FEATURE_ID_crossings =
  paste0(crossings_ls_corrected$FEATURE_ID, "_", crossings_ls_corrected$count)

## Check that the correct number should have 1 or 2 appended to the FEATURE_ID
crossings_ls_corrected %>%
  st_drop_geometry() %>%
  group_by(count) %>%
  count()
# count     n
# 1     1  1687  1687 crossings labelled as 1 (1422 + 238 + 19 + 8)
# 2     2   265  265 have 2 (238 + 19 + 8)
# 3     3    27  27 have 3 (19 + 8)
# 4     4     8  8 have 4 





###################
# Tidy up BOROUGH #
###################

anyNA(crossings_ls_corrected$BOROUGH) # = TRUE, n = 29

# Create dataset for those with a Borough coded 
crossings_borough = crossings_ls_corrected %>%
  filter(!is.na(BOROUGH)) # 1958 observations (ie 1987 original obs - 29 NAs) 

crossings_borough_split = st_intersection(lon_lad_2020, crossings_borough) # n = 1967 ie 9 extra

anyNA(crossings_borough_split$BOROUGH) # FALSE  # ONS data
anyNA(crossings_borough_split$BOROUGH.1) # FALSE # CID data

# Create df of crossings where the ONS borough disagrees with the CID borough
multi_feature_id = crossings_borough_split %>%
  st_drop_geometry() %>%
  group_by(FEATURE_ID_crossings) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs == 2) %>%  # number is only 1 or 2
  ungroup() # n = 9
#   FEATURE_ID_crossings num_obs
# 1 RWG055875_1                2
# 2 RWG107384_1                2
# 3 RWG108304_1                2
# 4 RWG135327_1                2
# 5 RWG135346_1                2
# 6 RWG153062_1                2
# 7 RWG154478_1                2
# 8 RWG154502_1                2
# 9 RWG273925_2                2

# check to see if any of these may have been part of a multicrossing to start with
# (important as post split some might be entirely in a new borough but not picked up
# because current code only identifies where new segments have been created by split)
multi_crossings_check = as.data.frame(pull(multi_feature_id, FEATURE_ID_crossings))
colnames(multi_crossings_check) = "FEATURE_ID_crossings"
multi_crossings_check$FEATURE_ID = sub("_.*", "", multi_crossings_check$FEATURE_ID_crossings)
multi_crossings_check_df = left_join(multi_crossings_check, crossings_ls_corrected, by = "FEATURE_ID")

# the above code identifies these two observations as originally having more than one crossing 
# so some of their crossings may also be affected by incorrect Borough even if not split.                                      
#RWG108304
#RWG273925

# Check the location of these multicrossing original observations
RWG108304_check = crossings_ls_corrected %>%
  filter(FEATURE_ID =="RWG108304") 
mapview(RWG108304_check, zcol = "FEATURE_ID_crossings") + mapview(lon_lad_2020, alpha.regions = 0)
# RWG108304_1 city
# RWG108304_2 Tower Hamlets

RWG273925_check = crossings_ls_corrected %>%
  filter(FEATURE_ID =="RWG273925")
mapview(RWG273925_check, zcol = "FEATURE_ID_crossings") + mapview(lon_lad_2020, alpha.regions = 0)
# RWG273925_1  Ealing
# RWG273925_2  H&F
# RWG273925_3  EAling
# RWG273925_4 defo in H&F



# create list of FEATURE_IDs that have 2 segments
multi_feature_id_list = pull(multi_feature_id, FEATURE_ID_crossings)

# create dataset containing the observations with these FEATURE_IDs
new_segments = crossings_borough_split %>%
  filter(FEATURE_ID_crossings %in% multi_feature_id_list) # n = 18, the 9 original ones 
# plus the additional 9 created when they were split

new_segment_match_pre = new_segments %>%
  st_drop_geometry() %>%
  select(c("FEATURE_ID_crossings", "BOROUGH", "BOROUGH.1")) %>%
  mutate(match = ifelse(BOROUGH == BOROUGH.1, TRUE, FALSE)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1"))  # Yes - this what we see

# Identify the longest segments of the 7 and check that ONS Borough matches CID Borough
new_segments_length = new_segments %>%
  mutate(length = st_length(geometry)) %>%
  group_by(FEATURE_ID_crossings) %>%
  mutate(total_length = sum(length)) %>%
  ungroup() %>%
  mutate(proportion = drop_units(length/total_length*100)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1")) %>%
  select(c("FEATURE_ID", "FEATURE_ID_crossings", "ONS", "CID", "length", "total_length", "proportion")) %>%
  filter(!FEATURE_ID %in% c("RWG273925", "RWG108304")) %>%  # these two excluded as we know they need to be managed differently
  group_by(FEATURE_ID) %>%
  slice(which.max(proportion))  # Yes they match in all 7

# Examine the two previous multicrossing observations where 
# Detailed inspection using mapview - comparing ESRI World Imagery as to where 
# the crossings are compared to the road (see commented out code below)
# Only FEATURE_IDs where the Boroughs needs changing are:
# 1) RWG108304 
# currently both segments coded as City of London in crossings_ls_corrected
# so need to recode RWG108304_2
# RWG108304_1 city
# RWG108304_2 Tower Hamlets
crossings_ls_corrected$BOROUGH = replace(crossings_ls_corrected$BOROUGH, 
                                 which(crossings_ls_corrected$FEATURE_ID_crossings == "RWG108304_2"), 
                                 values = "Tower Hamlets")
# RWG108304_check = crossings_ls_corrected %>%
#   filter(FEATURE_ID =="RWG108304") # check correctly recoded

# 2) RWG273925_2 
# currently all 4 segments coded as Ealing.  Two need changing to H&F # RWG273925_2 and RWG273925_4
crossings_ls_corrected$BOROUGH = replace(crossings_ls_corrected$BOROUGH, 
                                         which(crossings_ls_corrected$FEATURE_ID_crossings == "RWG273925_2"), 
                                         values = "Hammersmith & Fulham")
crossings_ls_corrected$BOROUGH = replace(crossings_ls_corrected$BOROUGH, 
                                         which(crossings_ls_corrected$FEATURE_ID_crossings == "RWG273925_4"), 
                                         values = "Hammersmith & Fulham")
# RWG273925_check = crossings_ls_corrected %>%
#   filter(FEATURE_ID =="RWG273925") # check recoded correctly


#######################
# Correct Borough NAs #
#######################
# Correct NAs as per correct_borough_NAs working
#  1) Create dataset of 29 crossings that have no Borough
crossings_borough_NA = crossings_ls_corrected %>%
  filter(is.na(BOROUGH)) 
# all are single crossings but two were originally a multicrossing observation RWG199197_1 & RWG199197_2
 
#  2) Use st_intersection to produce two observations for the crossings
crossings_borough_NA_i = st_intersection(lon_lad_2020, crossings_borough_NA)
# 49 observations, geometry column is from the crossings dataset but split by ONS borough boundaries
# BOROUGH is the column from the lon_lad_2020 dataset and represents the Borough that the crossing is in,
# whereas BOROUGH.1 is the BOROUGH from the crossing dataset (all NAs)
 
mapview(crossings_borough_NA_i, zcol = "BOROUGH") + mapview(lon_lad_2020, alpha.regions = 0.05, zcol = "BOROUGH")
# #this map shows the crossings coloured by the BOROUGH they are in
 
# # 3) Add columns for length of each observation and proportion of total length
crossings_borough_NA_i_length = crossings_borough_NA_i %>%
  mutate(length = st_length(geometry)) %>%
  group_by(FEATURE_ID_crossings) %>%
  mutate(total_length = sum(length)) %>%
  ungroup() %>%
  mutate(proportion = drop_units(length/total_length*100)) %>%
  rename(c("ONS" = "BOROUGH", "CID" = "BOROUGH.1")) %>%
  select(c("FEATURE_ID", "FEATURE_ID_crossings", "ONS", "CID", "length", "total_length", "proportion"))

# # 4) Identify those observations with >= 60 or 40-60 proportion of length in Borough 
crossings_borough_NA_i_60 = crossings_borough_NA_i_length %>%
  filter(proportion >= 60) # obtain those where prop >60 n= 26
crossings_borough_NA_i_4060 = crossings_borough_NA_i_length %>%
  filter(proportion < 60 & proportion >= 40) # obtain those where prop is 40-60, n= 6
crossings_borough_NA_i_4060_list = unique(pull(crossings_borough_NA_i_4060, FEATURE_ID_crossings)) # get list of these 40-60

# 5) Create new dataset where I want to add the Borough based on them having >=60% of the crossing in that borough
# (borough column now is the ONS one ie the correct one)
crossings_borough_NA_changed = crossings_borough_NA_i %>%
  filter(!FEATURE_ID_crossings %in% crossings_borough_NA_i_4060_list) %>%  # drop observations with less than 
  mutate(length = st_length(geometry)) %>%
  group_by(FEATURE_ID_crossings) %>%
  mutate(total_length = sum(length)) %>%
  ungroup() %>%
  mutate(proportion = drop_units(length/total_length*100)) %>%
  group_by(FEATURE_ID_crossings) %>%
  filter(proportion >= 60) %>%
  select(-BOROUGH.1) # n = 26

# need to pull in original geometry for the 17 observations that have proportion <100% 
# as otherwise length and geometry will only be for the split section
crossings_borough_NA_changed_need_geom = crossings_borough_NA_changed %>%
  filter(proportion <100) # get df of the 17
print(crossings_borough_NA_changed_need_geom$FEATURE_ID_crossings)
# [1] "RWG199184_1" "RWG150947_1" "RWG025510_1" "RWG055869_1"
# [5] "RWG184365_1" "RWG293100_1" "RWG025515_1" "RWG108004_1"
# [9] "RWG108005_1" "RWG003325_1" "RWG244675_1" "RWG049417_1"
# [13] "RWG273946_1" "RWG065992_1" "RWG003326_1" "RWG187249_1"
# [17] "RWG042145_1"  n = 17
sum(st_length(crossings_borough_NA_changed_need_geom)) # 194.2266 [m]
sum(crossings_borough_NA_changed_need_geom$length) # 194.2266 [m]
sum(crossings_borough_NA_changed_need_geom$total_length) #257.4651 [m]
# 257.4651 - 194.2266 # 63.2385  ie the 63m that is missing when compare original 
# total width of crossings 19907 with the new total width if I dont pull in the original
# geometry for these 17 observations (19844)

crossings_borough_NA_changed_need_geom = st_drop_geometry(crossings_borough_NA_changed_need_geom) # drop geometry so can join
crossings_borough_NA_changed_need_geom = inner_join(crossings_ls_corrected 
                                                    %>% select(geometry, FEATURE_ID_crossings), 
                                                    crossings_borough_NA_changed_need_geom, 
                                                    by = "FEATURE_ID_crossings") # join to original geometry 
sum(st_length(crossings_borough_NA_changed_need_geom)) # 257.4651 [m]  now have correct length

# Amend dfs so can join 
crossings_borough_NA_changed_need_geom = crossings_borough_NA_changed_need_geom %>%
  select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", 
           "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL", "geometry", "count", 
           "FEATURE_ID_crossings")) 

crossings_borough_NA_changed_dontneed_geom = crossings_borough_NA_changed %>%
  filter(proportion == 100) %>%
  select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", 
           "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL", "geometry", "count", 
           "FEATURE_ID_crossings")) # get the 9 that had the correct geometry

# join these dataframes together  
crossings_borough_NA_changed_correct_geom = rbind(crossings_borough_NA_changed_dontneed_geom,
                                                  crossings_borough_NA_changed_need_geom)
 
# 6) Create new dataset where the 3 Crossings that have between 40 and 60% in each borough 
# are each created as their own crossing
crossings_borough_NA_split = crossings_borough_NA_i %>%
  filter(FEATURE_ID_crossings %in% crossings_borough_NA_i_4060_list) %>%
  mutate(n = 1) %>%
  group_by(FEATURE_ID_crossings) %>%
  mutate(count2 = cumsum(n)) %>%
  select(-n) %>%
  select(-BOROUGH.1)

## Create new FEATURE_ID with the cumulative number so each observation has a unique ID
crossings_borough_NA_split$FEATURE_ID_crossings =
  paste0(crossings_borough_NA_split$FEATURE_ID, "_", crossings_borough_NA_split$count2)
  
# Amend df so can join 
crossings_borough_NA_split = crossings_borough_NA_split %>%
  select(c("FEATURE_ID","SVDATE","CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", 
         "CRS_LEVEL", "BOROUGH", "PHOTO1_URL", "PHOTO2_URL", "geometry", "count", 
         "FEATURE_ID_crossings")) 

# 7) Join datasets of corrected NAs
crossings_borough_NA_corrected = rbind(crossings_borough_NA_changed_correct_geom, 
                                       crossings_borough_NA_split)
# n = 32 the 26 corrected plus the 3+3 split ones

# 8) Add observations with correct Borough to main crossing dataset (crossing_ls_corrected)
# 8a) Drop 29 observations with no Boroughs frist
crossings_ls_corrected = crossings_ls_corrected %>%
  filter(!is.na(BOROUGH)) # 1958 observations ie  the 1987 - 29 NAs
anyNA(crossings_ls_corrected$BOROUGH) # = FALSE ie all dropped


# 8b) join corrected observations to the f_crossings
crossings_ls_corrected = rbind(crossings_ls_corrected, crossings_borough_NA_corrected)
anyNA(crossings_ls_corrected$BOROUGH) # = FALSE
# n = 1990 ie 1958 + 32

# Tidy up dataframe before saving
crossings_ls_corrected = crossings_ls_corrected %>%
  select(-c("count", "FEATURE_ID")) %>%
  rename(FEATURE_ID = FEATURE_ID_crossings) %>% # use new FEATURE_ID
  select(FEATURE_ID, everything())  # move column to beginning
  

sum(st_length(crossings_ls_corrected)) # check have the correct length(width) YES

######################
# SAVE CLEAN DATASET #
######################
#saveRDS(crossings_ls_corrected, file = "data/cleansed_crossings_24_01_2022")








