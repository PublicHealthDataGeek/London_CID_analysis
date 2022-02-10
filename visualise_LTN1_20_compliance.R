###################################################################################
#              Visualise degree of compliance with LTN 1/20                       #                
#                                                                                 #
# This code takes 2019 osm speed limit data and examines whether the degree of    #
# separation of cycling is appropriate for the speed based on LTN 1/20 guidance.  #
# This is then visualised as spatial distribution, Borough facets and Inner/Outer #
# London level.                                                                   #
#                                                                                 #
# OSM data is tidied and then spatially joined to Cycle lane data.  Where cycles  #
# cant be joined to speed liit data these are then visualised to see if there is  #
# an obvious OSM speed limit nearby - e.g. all surrounding roads are 20mph and    #
# with no indication that the cycle lane road is any different.                   #
#                                                                                 #
# Compliance determined by logic.                                                 #
#                                                                                 #
# Code rerun on 26/01/2022 to check works and to ensure correct spatial location  #
# of any spatial objects following the issue with the gdal libraries and linux    #
# in Dec 2021.  Code works fine and same results obtained. New versions of spatial#
# datasets saved.                                                                 #
###################################################################################

# Background
# Order of Protection from motor traffic on highways (DFT guidance pg 33)
# Fully kerbed > stepped > light segregation > Mandatory/Advisory
# FK/S/LS suitable for most people at 20/30 mph only FK suitable for 40mph+
# M/A only suitable for most poepl on 20mph roads with motor vehicle flow of <5000


#Load packages
library(osmextract)
library(tidyverse)
library(mapview)
library(tmap)
library(sf)
library(RColorBrewer)
library(leafsync) # for syncing mapview maps
library(viridis)


# Package options
mapviewOptions(native.crs = TRUE, fgb = FALSE)



################################################################################
#                   Import and data cleanse OSM speed limit data               #
################################################################################

# Load 2019 OSM dataset
gl_pbf19 = "data/greater-london-190101.osm.pbf"
gl_osm_lines19 = oe_read(gl_pbf19, quiet = FALSE, 
                         extra_tags = c("maxspeed", "width", "maxwidth", "lanes")) # Simple feature collection with 313409 features and 10 fields, CRS WGS84

# Change CRS to match ONS and CID
gl_osm_lines19 = st_transform(gl_osm_lines19, crs=27700) # PROJCRS["OSGB 1936 / British National Grid", n = 313409

# Limit OS data to inside London Boundary
# import May 2020 ONS LA boundary data
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial object for all 33 boroughs
london_union_bfe = st_union(lon_lad_2020_bfe) 

# Limit road links to within the Outer London Boundary
lon_osm_lines19 = st_intersection(gl_osm_lines19, london_union_bfe) # n = 310848

names(lon_osm_lines19)
# 1] "osm_id"     "name"       "highway"    "waterway"   "aerialway"  "barrier"    "man_made"  
# [8] "maxspeed"   "width"      "maxwidth"   "lanes"      "z_order"    "other_tags" "geometry"   

# Keep observations that are highway types that would be relevant to onroad cycle lanes
unique(lon_osm_lines19$highway)
# [1] "primary"         "residential"     "trunk"           "footway"         "service"        
# [6] "unclassified"    "tertiary"        "secondary"       "motorway_link"   "cycleway"       
# [11] NA                "motorway"        "tertiary_link"   "secondary_link"  "construction"   
# [16] "bridleway"       "trunk_link"      "pedestrian"      "primary_link"    "path"           
# [21] "living_street"   "steps"           "track"           "unsurfaced"      "raceway"        
# [26] "proposed"        "road"            "no"              "corridor"        "crossing"       
# [31] "escalator"       "elevator"        "stepping_stones" "disused"         "bus_stop"

# https://wiki.openstreetmap.org/wiki/Key:highway
# highway=* distinguishes roads by function and importance rather by their physical characteristic and legal classification.

### Make decisions about which values from highway to keep
# [1] "primary"      KEEP      "residential"   KEEP  "trunk"         KEEP  "trunk_link"     KEEP
# [5] "footway"      DROP      "service"       KEEP  "unclassified"  KEEP  "tertiary"       KEEP       
# [9] "secondary"    KEEP      "motorway_link" DROP  "cycleway"      DROP        NA         ??      
# [13] "motorway"    DROP      "tertiary_link" KEEP  "bridleway"     DROP  "secondary_link" KEEP
# [17] "pedestrian"  FALSE     "primary_link"  KEEP   "path"         DROP  "living_street"  KEEP
# [21] "steps"       DROP      "track"                "construction" DROP  "proposed"       DROP       
# [25] "raceway"     DROP      "road" ????            "no"                  "corridor"      DROP 
# [29] "escalator"   DROP      "elevator"     DROP        "cy"      DROP    "stepping_stones" DROP
# [33] "disused"     DROP      "crossing"     DROP        "access"  DROP

highways_to_keep = c("primary", "residential", "trunk", "trunk_link", "service", "unclassified", "tertiary",
                     "secondary", "tertiary_link", "secondary_link", "primary_link", "living_street")

osm_relevent_highways = lon_osm_lines19 %>%
  filter(highway %in% highways_to_keep) # n = 155920


# Examine maxspeed variable
max_speed_count = osm_relevent_highways %>%
  st_drop_geometry() %>%
  group_by(maxspeed) %>%
  summarise(count = n())

max_speed_count %>% print(n = Inf)
# maxspeed count
# <chr>    <int>
# 1 10           6
# 2 10 mph     368
# 3 10mph        6
# 4 12 mph      11
# 5 15           3
# 6 15 mph     115
# 7 16.09        1
# 8 20          13
# 9 20 mph   44134
# 10 25 mph       2
# 11 30          40
# 12 30 mph   22714
# 13 30 mpj       2
# 14 30 mpoh      1
# 15 4 mph        7
# 16 40           2
# 17 40 mph    2259
# 18 4mph         1
# 19 5            8
# 20 5 mph      635
# 21 50          10
# 22 50 mph    1097
# 23 5mph         2
# 24 60 mph      96
# 25 64           1
# 26 7            4
# 27 70 mph     122
# 28 national     1
# 29 signals     17
# 30 variable     1
# 31 NA       84241

# drop observations with no speed limit data
osm_speed_limits = osm_relevent_highways %>%
  filter(!is.na(maxspeed)) # n = 71679

# drop observations with signals or variable in the maxspeed
osm_speed_limits = osm_speed_limits %>%
  filter(maxspeed != "variable") %>%
  filter(maxspeed != "signals") %>%
  filter(maxspeed != "national") # n = 71660

# Remove mph and convert to integer 
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed, " mph"), `[`,1) # works byt still have 10mph so run again
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed, "mph"), `[`,1)
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed_num, " mpj"), `[`,1)
osm_speed_limits$maxspeed_num = sapply(str_split(osm_speed_limits$maxspeed_num, " mpoh"), `[`,1)
osm_speed_limits$maxspeed_num = as.integer(osm_speed_limits$maxspeed_num)

# Create speed limit groups
osm_speed_limits = osm_speed_limits %>%
  mutate(speed_limit = cut(maxspeed_num,
                           breaks = seq(0, 70, by = 10),
                           labels = c("10mph", "20mph", "30mph", "40mph", "50mph", "60mph", "70mph"))) 
tidy_speed_limit = osm_speed_limits %>%
  st_drop_geometry() %>%
  group_by(speed_limit) %>%
  summarise(count = n())

# speed_limit count
# <fct>       <int>
# 1 10mph        1037
# 2 20mph       44277
# 3 30mph       22759
# 4 40mph        2261
# 5 50mph        1107
# 6 60mph          96
# 7 70mph         123

mapview(osm_speed_limits, zcol = "speed_limit")

# Select OSM variables to keep 
osm_speed_limits_tidy = osm_speed_limits %>%
  select(c("osm_id", "name", "highway", "maxspeed_num", "speed_limit", "geometry", "lanes"))

#################
# Save OSM data #
#################

#saveRDS(osm_speed_limits_tidy, file = "data/lon_osm_speed_limits_tidy_26_01_2022.Rds")

osm_speed_limits_tidy = readRDS(file = "data/lon_osm_speed_limits_tidy_26_01_2022.Rds")

############################################
# Join speed limit data to Cycle lane data #
############################################

# Import cleansed onroad cycle lanes dataframe that has segregation information coded (created in VISUALISE CYCLELANES.r)
cycle_lanes = readRDS(file = "data/cleansed_onroad_cyclelanes_segregation_26_01_2022.Rds")

# collapse segregation into these categories that match Figure 4.1 in LTN 1/20
# Convert factored numbers to relevant labels
cycle_lanes = cycle_lanes %>%
  select(c(FEATURE_ID, BOROUGH, geometry, length_m, type, Highest_separation)) %>%
  mutate(Highest_separation = fct_collapse(Highest_separation, 
                                           "Stepped/part segregation" = c("Stepped", "Part segregation"),
                                           "Mandatory/Advisory cycle lane" = c("Mandatory cycle lane", "Advisory cycle lane")))
cycle_lanes %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(count = n())

# Highest_separation            `n()`
# <fct>                         <int>
# 1 Segregated                     1371   -> Any speed limit, any traffic flow
# 2 Stepped/part segregation        354   -> max 30moh, any traffic flow
# 3 Mandatory/Advisory cycle lane  8868   -> max20mph and traffic flow under 5000 
# 4 No separation                  3372   -> max 20mp and traffic flow under 2500

# Import tidied london OSM speed limit dataset
osm_speed_limits_tidy = readRDS(file = "data/lon_osm_speed_limits_tidy.Rds")

# Refactor speed limit
osm_speed_limits_tidy$speed_limit = fct_collapse(osm_speed_limits_tidy$speed_limit, 
               "20mph" = c("10mph", "20mph"))
# Recode road name from NA to Unknown 
sum(is.na(osm_speed_limits_tidy$name)) # n = 6452
osm_speed_limits_tidy$name[is.na(osm_speed_limits_tidy$name)] = "Unknown"
sum(is.na(osm_speed_limits_tidy$name)) # n = 0

# import May 2020 ONS LA boundary data
lon_lad_2020_bfe = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")


nrow(cycle_lanes) # n = 13965
nrow(osm_speed_limits_tidy) # n = 71600

# Obtain london speed limits and tidy df
london_speed_limits = st_intersection(osm_speed_limits_tidy, lon_lad_2020_bfe) %>%
  select(c("osm_id", "name", "highway", "maxspeed_num", "speed_limit", "BOROUGH")) # n = 73225

# View data
map_cl =  mapview(cycle_lanes, zcol = "Highest_separation")
map_sl = mapview(london_speed_limits, zcol = "speed_limit", color = brewer.pal(9, "YlOrRd"))
leafsync::sync(map_cl, map_sl)  # initial visualisation suggests many cycle lanes are in 20/30mph areas

# Put buffer around osm speed limit lines (allows for two way traffic on single carriageway plus OSM error)
sl_buffer = st_buffer(london_speed_limits, dist = 9.65) 
#mapview(sl_buffer)

# Use an spatial join where the cycle lane has to be entirely within the speed limit buffer
intersects_within = st_join(cycle_lanes, sl_buffer, join = sf::st_within) # n = 15455
nrow(intersects_within) / nrow(cycle_lanes) # 1.1 matches per CID cycleway

# How many of these new observations are not fully within an osm_id (ieNA) and therefore dont have a speed limit?
summary(is.na(intersects_within$osm_id)) # 5122 TRUE (ie osm_id is NA) so 10333 cycle lanes are totally within an osm sl and have a speed limit

# Examine these observations that are not fully within an osm buffer
sl_touching_cl = sl_buffer[cycle_lanes, ]  # n = 15806, returns all speed limit buffers that touch cycle lanes
mapview(sl_touching_cl) + cycle_lanes
# some are where side roads touch a main road
# some are where osm users have covered parts of the same road but in small overlapping chunks.  

# Get dataframe of unmatched cycle lanes
cl_unmatched_within = cycle_lanes  %>%
  filter(FEATURE_ID %in% intersects_within$FEATURE_ID[is.na(intersects_within$speed_limit)]) # n= 5122

# Obtain all speed limit buffers that touch cycle lanes that dont have a speed limit
sl_touching_cl_unmatched_within = sl_touching_cl[cl_unmatched_within, ] 
#nrow(sl_touching_cl_unmatched_within) # 8527
#mapview(sl_touching_cl_unmatched_within) + cl_unmatched_within

# Group this spatial data by speed limit
grouped_sl = sl_touching_cl_unmatched_within %>% 
  group_by(speed_limit) %>% 
  summarise()

# Pull out unmatched cycle lanes that have a speed limit using spatial subsetting
unmatched_cl_sl = cl_unmatched_within[grouped_sl, ] # n = 2963

# Spatially join unmatched cycle lanes to osm speed limit data
cl_sl_joined = st_join(unmatched_cl_sl, grouped_sl, join = sf::st_within) 
nrow(cl_sl_joined) # n = 2967
table(cl_sl_joined$speed_limit) # now there are 1968 further CID obs that got speed limits, so 999 are missing


# Now need to put these speed limits into the data 
# a) Create dataframe of the intersects_within where osm_id (and speed limit are missing)
missing = intersects_within %>%
  filter(is.na(osm_id)) # n = 5122

# b) Simplify df prior to join
cl_sl_joined_simplified = cl_sl_joined %>% 
  st_drop_geometry() %>%
  select(c(FEATURE_ID, speed_limit))

# c) Join the speed limit data to the cycle lanes that are missing that data
missing_joined = left_join(missing, cl_sl_joined_simplified, by = "FEATURE_ID")
summary(is.na(missing_joined$speed_limit.y))  # Now 1968 NA FALSE, still have 3158 as TRUE NA

# Create new dataframe where we have all known speed limits joined to the cycle lanes
# a) Simplify missing_joined df
missing_joined = missing_joined %>%
  select(c(FEATURE_ID, BOROUGH.x, geometry, type, Highest_separation, speed_limit.y)) %>%
  rename(BOROUGH = BOROUGH.x) %>%
  rename(speed_limit = speed_limit.y)
nrow(missing_joined) # 5126

# b) Simplify original df (intersects_within)
names(intersects_within)
intersects_within = intersects_within %>%
  select(c(FEATURE_ID, BOROUGH.x, geometry, type, Highest_separation, speed_limit)) %>%
  rename(BOROUGH = BOROUGH.x)

# c) Drop the cycle lanes from intersects_within that have speed limit NA
summary(is.na(intersects_within$speed_limit))  #5122 NAs
missing_intersects_within = intersects_within %>%
  filter(!is.na(speed_limit)) # drop the NAs
nrow(missing_intersects_within) # n = 10333

# d) Bind the two dataframes together
manip_sl = rbind(missing_intersects_within, missing_joined)
nrow(manip_sl) # n = 15459
summary(is.na(manip_sl$speed_limit)) # now 3158 NA

# Convert speed limits to numeric 
manip_sl  = manip_sl %>%
  ungroup() %>%
  mutate(numeric_speed_limit = case_when(speed_limit == "20mph" ~ "20",
                                         speed_limit == "30mph" ~ "30",
                                         speed_limit == "40mph" ~ "40",
                                         speed_limit == "50mph" ~ "50",
                                         speed_limit == "60mph" ~ "60",
                                         speed_limit == "70mph" ~ "70",
                                         speed_limit == "NA" ~ "Unknown - no OSM data",
                                         TRUE ~ "Unknown")) 

# What are the speed limits we have associated with the cycle lanes?
manip_sl %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit) 

# numeric_speed_limit     n
# <chr>               <int>
# 1 20                   5766
# 2 30                   6363
# 3 40                    162
# 4 50                     10
# 5 Unknown              3158


# Do appropriateness test to help narrow the Unknowns
test_appropriateness = manip_sl %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",  
                                           TRUE ~ "FALSE"))


# How many unknowns do we now have that need managing? 
test_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 2738 unknown

# tidy up environment
rm(cl_sl_joined, cl_unmatched_within, grouped_sl, intersects_within, london_speed_limits, 
   missing, missing_intersects_within, missing_joined, osm_speed_limits_tidy, sl_buffer, sl_touching_cl,
   sl_touching_cl_unmatched_within, unmatched_cl_sl)

# Save RDS files
saveRDS(test_appropriateness, file = "data/test_appropriateness_26_01_2022.Rds")
saveRDS(london_speed_limits, file = "data/london_speed_limits_26_01_2022.Rds")


###############################################################################
# Use visual analysis to associate OSM speed limits with cycle lanes that do  #
# not have a speed limit from the geographical joins                              #
###############################################################################

# Import datafiles
test_appropriateness = readRDS(file = "data/test_appropriateness.Rds")
london_speed_limits = readRDS(file = "data/london_speed_limits.Rds")

# 1) Barking & Dagenham
Barking = test_appropriateness %>%
  filter(BOROUGH == "Barking & Dagenham")
Bark_sl = london_speed_limits %>%
  filter(BOROUGH == "Barking & Dagenham")

# How many unknowns do we now have that need managing? 
Barking %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 338 unknown

# Examine Barking data to see if can match more speed limits to cycle lanes
map_Bark_approp = mapview(Barking, zcol = "seg_appro_speed_limit") 
map_Bark_seg = mapview(Barking, zcol = "Highest_separation")
map_Bark_sl = mapview(Bark_sl, zcol = "speed_limit")
leafsync::sync(map_Bark, map_Bark_seg, map_Bark_sl, ncol = 2)

barking_30 = c("RWG155040", "RWG155005", "RWG155891", "RWG155888", "RWG154959", 
               "RWG184587", "RWG155758", "RWG184939", "RWG221137", "RWG221141",
               "RWG999512", "RWG184535", "RWG184536", "RWG155751", "RWG155748", 
               "RWG155455")
barking_20 = "RWG184613"

# create df of unknown barking
barking_unknown = Barking %>%
  filter(seg_appro_speed_limit == "Unknown") # n = 338
  
# Update speed limits based on visual comparison
barking_unknown$numeric_speed_limit = replace(barking_unknown$numeric_speed_limit, 
                                              which(barking_unknown$FEATURE_ID %in% barking_20),
                                              values = 20)
barking_unknown$numeric_speed_limit = replace(barking_unknown$numeric_speed_limit, 
                                              which(barking_unknown$FEATURE_ID %in% barking_30),
                                              values = 30)
barking_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      1
# 2 30                     16
# 3 Unknown               321

# Rerun appropriateness test
Barking_appropriateness = barking_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",  
                                           TRUE ~ "FALSE"))

# Rerun count
Barking_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 321 unknown

# Create df of known barking
barking_known = Barking %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 103

# Create final Barking df
Barking_final = rbind(barking_known, Barking_appropriateness)
nrow(Barking_final) # n = 441

# Measure lengths of True, False and Unknown
Barking_summary = Barking_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Barking_total_length = sum(length)) %>%
  mutate(Barking_percentage = Barking_total_length/sum(Barking_total_length)*100)
#seg_appro_speed_limit Barking_total_length Barking_percentage
# <chr>                                  [m]                [1]
# 1 FALSE                               11154.              22.5 
# 2 TRUE                                 2860.               5.76
# 3 Unknown                             35612.              71.8  

# compare with original - increased the known by 8.3% (4.1km)
# Barking_unadjusted_summary = Barking %>%
#   mutate(length = st_length(geometry)) %>%
#   st_drop_geometry() %>%
#   group_by(seg_appro_speed_limit) %>%
#   summarise(Barking_total_length = sum(length)) %>%
#   mutate(Barking_percentage = Barking_total_length/sum(Barking_total_length)*100)
# seg_appro_speed_limit Barking_total_length Barking_percentage
# <chr>                                  [m]                [1]
# 1 FALSE                                7038.              14.2 
# 2 TRUE                                 2831.               5.70
# 3 Unknown                             39757.              80.1 

# Tidy up df
rm(Barking, Bark_sl, Barking_appropriateness, barking_change, Barking_changed, barking_known, Barking_unadjusted_summary,
            barking_unknown, map_Bark, map_Bark_approp, map_Bark_seg, map_Bark_sl, barking_20, barking_30)


# 2) Barnet
Barnet = test_appropriateness %>%
  filter(BOROUGH == "Barnet")
nrow(Barnet) # 101
Barn_sl = london_speed_limits %>%
  filter(BOROUGH == "Barnet")

# How many unknowns do we now have that need managing? 
Barnet %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 7 unknown

# Examine Barnet data to see if can match more speed limits to cycle lanes
map_Barn_approp = mapview(Barnet, zcol = "seg_appro_speed_limit") 
map_Barn_seg = mapview(Barnet, zcol = "Highest_separation")
map_Barn_sl = mapview(Barn_sl, zcol = "speed_limit")
leafsync::sync(map_Barn_approp, map_Barn_seg, map_Barn_sl, ncol = 2)

barnet_30 = c("RWG279372", "RWG274108", "RWG279191", "RWG274089")

# create df of unknown barnet
barnet_unknown = Barnet %>%
  filter(seg_appro_speed_limit == "Unknown") # n = 7

# Update speed limits based on visual comparison
barnet_unknown$numeric_speed_limit = replace(barnet_unknown$numeric_speed_limit, 
                                              which(barnet_unknown$FEATURE_ID %in% barnet_30),
                                              values = 30)
barnet_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 30                      4
# 2 Unknown                 3

# Rerun appropriateness test
Barnet_appropriateness = barnet_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",  
                                           TRUE ~ "FALSE"))

# Rerun count
Barnet_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 4 now False, 3 unknown

# Create df of known barking
barnet_known = Barnet %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 94

# Create final Barnet df
Barnet_final = rbind(barnet_known, Barnet_appropriateness)
nrow(Barnet_final) # n = 101

# Measure lengths of True, False and Unknown
Barnet_summary = Barnet_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Barnet_total_length = sum(length)) %>%
  mutate(Barnet_percentage = Barnet_total_length/sum(Barnet_total_length)*100)

# seg_appro_speed_limit Barnet_total_length Barnet_percentage
# <chr>                                 [m]               [1]
# 1 FALSE                               5673.             84.7 
# 2 TRUE                                 403.              6.02
# 3 Unknown                              625.              9.32

# compare with original - increased the known by 10% (0.7km)
# Barnet_unadjusted_summary = Barnet %>%
#   mutate(length = st_length(geometry)) %>%
#   st_drop_geometry() %>%
#   group_by(seg_appro_speed_limit) %>%
#   summarise(Barnet_total_length = sum(length)) %>%
#   mutate(Barnet_percentage = Barnet_total_length/sum(Barnet_total_length)*100)
# # seg_appro_speed_limit Barnet_total_length Barnet_percentage
# # <chr>                                 [m]               [1]
# # 1 FALSE                               5003.             74.7 
# # 2 TRUE                                 403.              6.02
# # 3 Unknown                             1295.             19.3 

# Tidy up df
rm(Barnet, Barn_sl, Barnet_appropriateness, barnet_change, Barnet_changed, barnet_known, Barnet_unadjusted_summary,
   barnet_unknown, map_Barn_approp, map_Barn_seg, map_Barn_sl, barnet_30)


# 3) Bexley
Bexley= test_appropriateness %>%
  filter(BOROUGH == "Bexley")
nrow(Bexley) # 192
Bex_sl = london_speed_limits %>%
  filter(BOROUGH == "Bexley")

# How many unknowns do we now have that need managing? 
Bexley %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 77 unknown

# Examine Bexley data to see if can match more speed limits to cycle lanes
map_Bex_approp = mapview(Bexley, zcol = "seg_appro_speed_limit") 
map_Bex_seg = mapview(Bexley, zcol = "Highest_separation")
map_Bex_sl = mapview(Bex_sl, zcol = "speed_limit")
leafsync::sync(map_Bex_approp, map_Bex_seg, map_Bex_sl, ncol = 2)

bexley_30 = c("RWG184560", "RWG184701")

# create df of unknown bex
bexley_unknown = Bexley %>%
  filter(seg_appro_speed_limit == "Unknown") # n = 77

# Update speed limits based on visual comparison
bexley_unknown$numeric_speed_limit = replace(bexley_unknown$numeric_speed_limit, 
                                             which(bexley_unknown$FEATURE_ID %in% bexley_30),
                                             values = 30)
bexley_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 30                      2
# 2 Unknown                75

# Rerun appropriateness test
Bexley_appropriateness = bexley_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",  
                                           TRUE ~ "FALSE"))
# Rerun count
Bexley_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # false = 2

# Create df of known bexley
bexley_known = Bexley %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 115

# Create final Bexley df
Bexley_final = rbind(bexley_known, Bexley_appropriateness)
nrow(Bexley_final) # n = 192

# # Measure lengths of True, False and Unknown
Bexley_summary = Bexley_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Bexley_total_length = sum(length)) %>%
  mutate(Bexley_percentage = Bexley_total_length/sum(Bexley_total_length)*100)
# seg_appro_speed_limit Bexley_total_length Bexley_percentage
# <chr>                                 [m]               [1]
# 1 FALSE                               5161.              36.8
# 2 TRUE                                1667.              11.9
# 3 Unknown                             7190.              51.3
# 
# # compare with original - increased the known by 5% (0.7km)
# Bexley_unadjusted_summary = Bexley %>%
#   mutate(length = st_length(geometry)) %>%
#   st_drop_geometry() %>%
#   group_by(seg_appro_speed_limit) %>%
#   summarise(Bexley_total_length = sum(length)) %>%
#   mutate(Bexley_percentage = Bexley_total_length/sum(Bexley_total_length)*100)
# seg_appro_speed_limit Bexley_total_length Bexley_percentage
# <chr>                                 [m]               [1]
# 1 FALSE                               4483.              32.0
# 2 TRUE                                1667.              11.9
# 3 Unknown                             7868.              56.1

# # Tidy up df
rm(Bexley, Bex_sl, Bexley_appropriateness, bexley_change, bexley_known, Bexley_unadjusted_summary,
   bexley_unknown, map_Bex_approp, map_Bex_seg, map_Bex_sl, bexley_30)


# 4) Brent
Brent = test_appropriateness %>%
  filter(BOROUGH == "Brent")
nrow(Brent) #229
Brent_sl = london_speed_limits %>%
  filter(BOROUGH == "Brent")

# How many unknowns do we now have that need managing?
Brent %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 63 unknown

# Examine Brent data to see if can match more speed limits to cycle lanes
map_Brent_approp = mapview(Brent, zcol = "seg_appro_speed_limit")
map_Brent_seg = mapview(Brent, zcol = "Highest_separation")
map_Brent_sl = mapview(Brent_sl, zcol = "speed_limit")
leafsync::sync(map_Brent_approp, map_Brent_seg, map_Brent_sl, ncol = 2)

brent_30 = c("RWG274969", "RWG274390", "RWG274388", "RWG274387", "RWG274385", 
             "RWG274383", "RWG274381")
brent_20 = c("RWG275526", "RWG274835")

# create df of unknown brent
brent_unknown = Brent %>%
  filter(seg_appro_speed_limit == "Unknown") # n = 63
#readODS::write_ods(brent_unknown, path = "/home/bananafan/Downloads/Brent.ods")

# Update speed limits based on visual comparison
brent_unknown$numeric_speed_limit = replace(brent_unknown$numeric_speed_limit,
                                             which(brent_unknown$FEATURE_ID %in% brent_30),
                                             values = 30)
brent_unknown$numeric_speed_limit = replace(brent_unknown$numeric_speed_limit,
                                            which(brent_unknown$FEATURE_ID %in% brent_20),
                                            values = 20)
brent_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)

# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      2
# 2 30                      7
# 3 Unknown                54

# Rerun appropriateness test
Brent_appropriateness = brent_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Brent_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # false = 8, true 1

# Create df of known 
brent_known = Brent %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 166
# Create final df
Brent_final = rbind(brent_known, Brent_appropriateness)
nrow(Brent_final) # n = 229

# # Measure lengths of True, False and Unknown
Brent_summary = Brent_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Brent_total_length = sum(length)) %>%
  mutate(Brent_percentage = Brent_total_length/sum(Brent_total_length)*100)

# seg_appro_speed_limit Brent_total_length Brent_percentage
# <chr>                                [m]              [1]
# 1 FALSE                              7683.             55.7
# 2 TRUE                               1686.             12.2
# 3 Unknown                            4414.             32.0

# # Tidy up df
rm(Brent, Brent_sl, Brent_appropriateness, brent_known, brent_unknown, 
   map_Brent_approp, map_Brent_seg, map_Brent_sl, brent_30, brent_20)


# # 5) Bromley
Bromley= test_appropriateness %>%
  filter(BOROUGH == "Bromley")
nrow(Bromley) #208
Bromley_sl = london_speed_limits %>%
  filter(BOROUGH == "Bromley")

# How many unknowns do we now have that need managing?
Bromley%>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 48 unknown

# Examine Bromley data to see if can match more speed limits to cycle lanes
map_Bromley_approp = mapview(Bromley, zcol = "seg_appro_speed_limit")
map_Bromley_seg = mapview(Bromley, zcol = "Highest_separation")
map_Bromley_sl = mapview(Bromley_sl, zcol = "speed_limit")
leafsync::sync(map_Bromley_approp, map_Bromley_seg, map_Bromley_sl, ncol = 2)

Bromley_40 = c("RWG198250", "RWG198254")
Bromley_30 = c("RWG198184", "RWG204734", "RWG204797", "RWG291806", "RWG999347")
Bromley_20 = c("RWG204829", "RWG204850", "RWG198069", "RWG198108", "RWG198109")

# create df of unknown Bromley
Bromley_unknown = Bromley %>%
  filter(seg_appro_speed_limit == "Unknown") # n = 48

# Update speed limits based on visual comparison
Bromley_unknown$numeric_speed_limit = replace(Bromley_unknown$numeric_speed_limit,
                                              which(Bromley_unknown$FEATURE_ID %in% Bromley_40),
                                              values = 40)
Bromley_unknown$numeric_speed_limit = replace(Bromley_unknown$numeric_speed_limit,
                                            which(Bromley_unknown$FEATURE_ID %in% Bromley_30),
                                            values = 30)
Bromley_unknown$numeric_speed_limit = replace(Bromley_unknown$numeric_speed_limit,
                                            which(Bromley_unknown$FEATURE_ID %in% Bromley_20),
                                            values = 20)
Bromley_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      5
# 2 30                      5
# 3 40                      2
# 4 Unknown                36

# Rerun appropriateness test
Bromley_appropriateness = Bromley_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Bromley_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # false = 9, true 3

# Create df of known
Bromley_known =Bromley %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 160

# Create final df
Bromley_final = rbind(Bromley_known, Bromley_appropriateness)
nrow(Bromley_final) # n = 208

# # Measure lengths of True, False and Unknown
Bromley_summary = Bromley_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Bromley_total_length = sum(length)) %>%
  mutate(Bromley_percentage = Bromley_total_length/sum(Bromley_total_length)*100)
# seg_appro_speed_limit Bromley_total_length Bromley_percentage
# <chr>                                  [m]                [1]
# 1 FALSE                               14867.              69.2 
# 2 TRUE                                 2068.               9.63
# 3 Unknown                              4552.              21.2 


# # Tidy up df
rm(Bromley, Bromley_sl, Bromley_appropriateness, Bromley_known, Bromley_unknown,
   map_Bromley_approp, map_Bromley_seg, map_Bromley_sl, Bromley_40, Bromley_30, Bromley_20)

# # 6) Camden
Camden = test_appropriateness %>%
  filter(BOROUGH == "Camden")
nrow(Camden) # 834
Camden_sl = london_speed_limits %>%
  filter(BOROUGH == "Camden")

# How many unknowns do we now have that need managing?
Camden %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 26 unknown

# # Examine Camden data to see if can match more speed limits to cycle lanes
map_Camden_approp = mapview(Camden, zcol = "seg_appro_speed_limit")
map_Camden_seg = mapview(Camden, zcol = "Highest_separation")
map_Camden_sl = mapview(Camden_sl, zcol = "speed_limit")
leafsync::sync(map_Camden_approp, map_Camden_seg, map_Camden_sl, ncol = 2)


Camden_30 = c("RWG000062", "RWG000176", "RWG038467", "RWG038468", "RWG089413")
Camden_20 = c("RWG000072", "RWG000079", "RWG030401", "RWG038394", "RWG066070", 
              "RWG066078", "RWG108339", "RWG108340", "RWG152070", "RWG152089", 
              "RWG152133", "RWG152230")
 
# create df of unknown Camden
Camden_unknown = Camden %>%
  filter(seg_appro_speed_limit == "Unknown") # n = 26

# # Update speed limits based on visual comparison
Camden_unknown$numeric_speed_limit = replace(Camden_unknown$numeric_speed_limit,
                                              which(Camden_unknown$FEATURE_ID %in% Camden_30),
                                              values = 30)
Camden_unknown$numeric_speed_limit = replace(Camden_unknown$numeric_speed_limit,
                                              which(Camden_unknown$FEATURE_ID %in% Camden_20),
                                              values = 20)
Camden_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                     12
# 2 30                      5
# 3 Unknown                 9

# Rerun appropriateness test
Camden_appropriateness = Camden_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Camden_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit) 

# Create df of known
Camden_known = Camden %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 808

# Create final df
Camden_final = rbind(Camden_known, Camden_appropriateness)
nrow(Camden_final) # n = 834

# # Measure lengths of True, False and Unknown
Camden_summary = Camden_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Camden_total_length = sum(length)) %>%
  mutate(Camden_percentage = Camden_total_length/sum(Camden_total_length)*100)
# seg_appro_speed_limit Camden_total_length Camden_percentage
# <chr>                                 [m]               [1]
# 1 FALSE                              19771.             53.8 
# 2 TRUE                               16017.             43.6 
# 3 Unknown                              944.              2.57

# # Tidy up df
rm(Camden, Camden_sl, Camden_appropriateness, Camden_known, Camden_unknown,
   map_Camden_approp, map_Camden_seg, map_Camden_sl, Camden_30, Camden_20)

# # # 7) Croydon
Croydon = test_appropriateness %>%
  filter(BOROUGH == "Croydon")
nrow(Croydon) # 758
Croydon_sl = london_speed_limits %>%
  filter(BOROUGH == "Croydon")

# How many unknowns do we now have that need managing?
Croydon %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 50

# # Examine Croydon data to see if can match more speed limits to cycle lanes
map_Croydon_approp = mapview(Croydon, zcol = "seg_appro_speed_limit")
map_Croydon_seg = mapview(Croydon, zcol = "Highest_separation")
map_Croydon_sl = mapview(Croydon_sl, zcol = "speed_limit")
leafsync::sync(map_Croydon_approp, map_Croydon_seg, map_Croydon_sl, ncol = 2)

Croydon_50 = c("RWG219729")
Croydon_40 = c("RWG219726")
Croydon_30 = c("RWG218430", "RWG218433", "RWG218507", "RWG218852", "RWG218971", 
               "RWG218972", "RWG219220", "RWG219410", "RWG219486", "RWG219640", 
               "RWG219718", "RWG219777", "RWG292679")
Croydon_20 = c("RWG218916", "RWG218945", "RWG218946", "RWG218997", "RWG219460")

# # create df of unknown Croydon
Croydon_unknown = Croydon %>%
  filter(seg_appro_speed_limit == "Unknown")

# # Update speed limits based on visual comparison
Croydon_unknown$numeric_speed_limit = replace(Croydon_unknown$numeric_speed_limit,
                                              which(Croydon_unknown$FEATURE_ID %in% Croydon_50),
                                              values = 50)
Croydon_unknown$numeric_speed_limit = replace(Croydon_unknown$numeric_speed_limit,
                                              which(Croydon_unknown$FEATURE_ID %in% Croydon_40),
                                              values = 40)
Croydon_unknown$numeric_speed_limit = replace(Croydon_unknown$numeric_speed_limit,
                                             which(Croydon_unknown$FEATURE_ID %in% Croydon_30),
                                             values = 30)
Croydon_unknown$numeric_speed_limit = replace(Croydon_unknown$numeric_speed_limit,
                                             which(Croydon_unknown$FEATURE_ID %in% Croydon_20),
                                             values = 20)
Croydon_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      5
# 2 30                     13
# 3 40                      1
# 4 50                      1
# 5 Unknown                30



# Rerun appropriateness test
Croydon_appropriateness = Croydon_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Croydon_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Croydon_known = Croydon %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 708

# Create final df
Croydon_final = rbind(Croydon_known, Croydon_appropriateness)
nrow(Croydon_final) # n = 758

# # Measure lengths of True, False and Unknown
Croydon_summary = Croydon_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Croydon_total_length = sum(length)) %>%
  mutate(Croydon_percentage = Croydon_total_length/sum(Croydon_total_length)*100)
# seg_appro_speed_l… Croydon_total_len… Croydon_percent…
# <chr>                             [m]              [1]
# 1 FALSE                          51665.            87.5 
# 2 TRUE                            4551.             7.70
# 3 Unknown                         2862.             4.84

# # Tidy up df
rm(Croydon, Croydon_sl, Croydon_appropriateness, Croydon_known, Croydon_unknown,
   map_Croydon_approp, map_Croydon_seg, map_Croydon_sl, Croydon_50, Croydon_40, Croydon_30, Croydon_20)

# # # 8) City of London
City = test_appropriateness %>%
  filter(BOROUGH == "City of London")
nrow(City) # 366
City_sl = london_speed_limits %>%
  filter(BOROUGH == "City of London")

# How many unknowns do we now have that need managing?
City %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 10

# # # Examine City data to see if can match more speed limits to cycle lanes
map_City_approp = mapview(City, zcol = "seg_appro_speed_limit")
map_City_seg = mapview(City, zcol = "Highest_separation")
map_City_sl = mapview(City_sl, zcol = "speed_limit")
leafsync::sync(map_City_approp, map_City_seg, map_City_sl, ncol = 2)


City_30 = c("RWG066228")
City_20 = c("RWG066539", "RWG108343", "RWG108367", "RWG066720","RWG066398", 
            "RWG066125", "RWG108346", "RWG066836")

# create df of unknown City
City_unknown = City %>%
  filter(seg_appro_speed_limit == "Unknown")
 
# Update speed limits based on visual comparison
City_unknown$numeric_speed_limit = replace(City_unknown$numeric_speed_limit,
                                              which(City_unknown$FEATURE_ID %in% City_30),
                                              values = 30)
City_unknown$numeric_speed_limit = replace(City_unknown$numeric_speed_limit,
                                              which(City_unknown$FEATURE_ID %in% City_20),
                                              values = 20)
City_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      8
# 2 30                      1
# 3 Unknown                 1

# Rerun appropriateness test
City_appropriateness = City_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
City_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
City_known = City %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 356
# Create final df
City_final = rbind(City_known, City_appropriateness)
nrow(City_final) # n = 366

# # Measure lengths of True, False and Unknown
City_summary = City_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(City_total_length = sum(length)) %>%
  mutate(City_percentage = City_total_length/sum(City_total_length)*100)
# seg_appro_speed_limit City_total_length City_percentage
# <chr>                               [m]             [1]
# 1 FALSE                             9241.          42.8  
# 2 TRUE                             12215.          56.6  
# 3 Unknown                            128.           0.592

# Tidy up df
rm(City, City_sl, City_appropriateness, City_known, City_unknown,
   map_City_approp, map_City_seg, map_City_sl, City_30, City_20)

# 9) Ealing
Ealing = test_appropriateness %>%
  filter(BOROUGH == "Ealing")
nrow(Ealing) # 719
Ealing_sl = london_speed_limits %>%
  filter(BOROUGH == "Ealing")

# How many unknowns do we now have that need managing?
Ealing %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 105 unknowns

# # # Examine Ealing data to see if can match more speed limits to cycle lanes
map_Ealing_approp = mapview(Ealing, zcol = "seg_appro_speed_limit")
map_Ealing_seg = mapview(Ealing, zcol = "Highest_separation")
map_Ealing_sl = mapview(Ealing_sl, zcol = "speed_limit")
leafsync::sync(map_Ealing_approp, map_Ealing_seg, map_Ealing_sl, ncol = 2)

 
Ealing_30 = c("RWG233615", "RWG237434", "RWG237616", "RWG237772")
Ealing_20 = c("RWG237752 _ 1", "RWG237757 _ 1", "RWG275122", "RWG275125", 
              "RWG275212", "RWG275213", "RWG275393", "RWG275406", "RWG275412",
              "RWG275416")

 # create df of unknown Ealing
Ealing_unknown = Ealing %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Ealing_unknown$numeric_speed_limit = replace(Ealing_unknown$numeric_speed_limit,
                                           which(Ealing_unknown$FEATURE_ID %in% Ealing_30),
                                           values = 30)
Ealing_unknown$numeric_speed_limit = replace(Ealing_unknown$numeric_speed_limit,
                                           which(Ealing_unknown$FEATURE_ID %in% Ealing_20),
                                           values = 20)
Ealing_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                     10
# 2 30                      4
# 3 Unknown                91

# Rerun appropriateness test
Ealing_appropriateness = Ealing_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Ealing_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Ealing_known = Ealing %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 614
# Create final df
Ealing_final = rbind(Ealing_known, Ealing_appropriateness)
nrow(Ealing_final) # n = 719

# # Measure lengths of True, False and Unknown
Ealing_summary = Ealing_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Ealing_total_length = sum(length)) %>%
  mutate(Ealing_percentage = Ealing_total_length/sum(Ealing_total_length)*100)
# seg_appro_speed_limit Ealing_total_length Ealing_percentage
# <chr>                                 [m]               [1]
# 1 FALSE                              21016.              53.3
# 2 TRUE                               12181.              30.9
# 3 Unknown                             6228.              15.8

# Tidy up df
rm(Ealing, Ealing_sl, Ealing_appropriateness, Ealing_known, Ealing_unknown,
   map_Ealing_approp, map_Ealing_seg, map_Ealing_sl, Ealing_30, Ealing_20)

# 10) Enfield
Enfield = test_appropriateness %>%
  filter(BOROUGH == "Enfield")
nrow(Enfield) # 312
Enfield_sl = london_speed_limits %>%
  filter(BOROUGH == "Enfield")

# How many unknowns do we now have that need managing?
Enfield %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 143 unknowns

# # # Examine Enfield data to see if can match more speed limits to cycle lanes
map_Enfield_approp = mapview(Enfield, zcol = "seg_appro_speed_limit")
map_Enfield_seg = mapview(Enfield, zcol = "Highest_separation")
map_Enfield_sl = mapview(Enfield_sl, zcol = "speed_limit")
leafsync::sync(map_Enfield_approp, map_Enfield_seg, map_Enfield_sl, ncol = 2)


Enfield_40 = c("RWG187318", "RWG276311")
Enfield_30 = c("RWG149138", "RWG149140", "RWG149378", "RWG149380", "RWG149461", 
              "RWG149593", "RWG149632", "RWG187372", "RWG187429", "RWG276312")

# create df of unknown Enfield
Enfield_unknown = Enfield %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Enfield_unknown$numeric_speed_limit = replace(Enfield_unknown$numeric_speed_limit,
                                             which(Enfield_unknown$FEATURE_ID %in% Enfield_30),
                                             values = 30)
Enfield_unknown$numeric_speed_limit = replace(Enfield_unknown$numeric_speed_limit,
                                             which(Enfield_unknown$FEATURE_ID %in% Enfield_40),
                                             values = 40)
Enfield_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# # <chr>               <int>
# # 1 30                     10
# # 2 40                      2
# # 3 Unknown               131

# Rerun appropriateness test
Enfield_appropriateness = Enfield_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Enfield_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Enfield_known = Enfield %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 169

# Create final df
Enfield_final = rbind(Enfield_known, Enfield_appropriateness)
nrow(Enfield_final) # n = 312

# # Measure lengths of True, False and Unknown
Enfield_summary = Enfield_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Enfield_total_length = sum(length)) %>%
  mutate(Enfield_percentage = Enfield_total_length/sum(Enfield_total_length)*100)
# seg_appro_speed_limit Enfield_total_length Enfield_percentage
# <chr>                                  [m]                [1]
# 1 FALSE                                9688.              56.4 
# 2 TRUE                                 1582.               9.21
# 3 Unknown                              5910.              34.4 

# Tidy up df
rm(Enfield, Enfield_sl, Enfield_appropriateness, Enfield_known, Enfield_unknown,
   map_Enfield_approp, map_Enfield_seg, map_Enfield_sl, Enfield_30, Enfield_40)

# 11) Greenwich
Greenwich = test_appropriateness %>%
  filter(BOROUGH == "Greenwich")
nrow(Greenwich) # 421
Greenwich_sl = london_speed_limits %>%
  filter(BOROUGH == "Greenwich")

# How many unknowns do we now have that need managing?
Greenwich %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 65 unknowns

# # # Examine Greenwich data to see if can match more speed limits to cycle lanes
map_Greenwich_approp = mapview(Greenwich, zcol = "seg_appro_speed_limit")
map_Greenwich_seg = mapview(Greenwich, zcol = "Highest_separation")
map_Greenwich_sl = mapview(Greenwich_sl, zcol = "speed_limit")
leafsync::sync(map_Greenwich_approp, map_Greenwich_seg, map_Greenwich_sl, ncol = 2)


Greenwich_20 = c("RWG184850")
Greenwich_30 = c("RWG184727", "RWG184910", "RWG184997", "RWG185029", "RWG277282", 
               "RWG290288", "RWG291033")

# create df of unknown Greenwich
Greenwich_unknown = Greenwich %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Greenwich_unknown$numeric_speed_limit = replace(Greenwich_unknown$numeric_speed_limit,
                                              which(Greenwich_unknown$FEATURE_ID %in% Greenwich_30),
                                              values = 30)
Greenwich_unknown$numeric_speed_limit = replace(Greenwich_unknown$numeric_speed_limit,
                                              which(Greenwich_unknown$FEATURE_ID %in% Greenwich_20),
                                              values = 20)
Greenwich_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      1
# 2 30                      7
# 3 Unknown                57

# Rerun appropriateness test
Greenwich_appropriateness = Greenwich_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Greenwich_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Greenwich_known = Greenwich %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 356

# Create final df
Greenwich_final = rbind(Greenwich_known, Greenwich_appropriateness)
nrow(Greenwich_final) # n = 421

# # Measure lengths of True, False and Unknown
Greenwich_summary = Greenwich_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Greenwich_total_length = sum(length)) %>%
  mutate(Greenwich_percentage = Greenwich_total_length/sum(Greenwich_total_length)*100)
# seg_appro_speed_limit Greenwich_total_length Greenwich_percentage
# <chr>                                    [m]                  [1]
# 1 FALSE                                 21181.                 65.4
# 2 TRUE                                   3762.                 11.6
# 3 Unknown                                7449.                 23.0

# Tidy up df
rm(Greenwich, Greenwich_sl, Greenwich_appropriateness, Greenwich_known, Greenwich_unknown,
   map_Greenwich_approp, map_Greenwich_seg, map_Greenwich_sl, Greenwich_30, Greenwich_20)

# 12) Hackney
Hackney = test_appropriateness %>%
  filter(BOROUGH == "Hackney")
nrow(Hackney) # 492
Hackney_sl = london_speed_limits %>%
  filter(BOROUGH == "Hackney")

# How many unknowns do we now have that need managing?
Hackney %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 20 unknowns

# # # Examine Hackney data to see if can match more speed limits to cycle lanes
map_Hackney_approp = mapview(Hackney, zcol = "seg_appro_speed_limit")
map_Hackney_seg = mapview(Hackney, zcol = "Highest_separation")
map_Hackney_sl = mapview(Hackney_sl, zcol = "speed_limit")
leafsync::sync(map_Hackney_approp, map_Hackney_seg, map_Hackney_sl, ncol = 2)


Hackney_30 = c("RWG097791 _ 1")
Hackney_20 = c("RWG008911", "RWG008930 _ 1", "RWG008969", "RWG025616", "RWG042276", 
               "RWG042365", "RWG073499", "RWG149667 _ 1", "RWG151467", "RWG151592",
               "RWG999763")

# create df of unknown Hackney
Hackney_unknown = Hackney %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Hackney_unknown$numeric_speed_limit = replace(Hackney_unknown$numeric_speed_limit,
                                                which(Hackney_unknown$FEATURE_ID %in% Hackney_30),
                                                values = 30)
Hackney_unknown$numeric_speed_limit = replace(Hackney_unknown$numeric_speed_limit,
                                                which(Hackney_unknown$FEATURE_ID %in% Hackney_20),
                                                values = 20)
Hackney_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                     11
# 2 30                      1
# 3 Unknown                 8

# Rerun appropriateness test
Hackney_appropriateness = Hackney_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Hackney_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Hackney_known = Hackney %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 472

# Create final df
Hackney_final = rbind(Hackney_known, Hackney_appropriateness)
nrow(Hackney_final) # n = 492

# # Measure lengths of True, False and Unknown
Hackney_summary = Hackney_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Hackney_total_length = sum(length)) %>%
  mutate(Hackney_percentage = Hackney_total_length/sum(Hackney_total_length)*100)
# seg_appro_speed_limit Hackney_total_length Hackney_percentage
# <chr>                                  [m]                [1]
# 1 FALSE                               24238.               72.6
# 2 TRUE                                 4677.               14.0
# 3 Unknown                              4452.               13.3

# Tidy up df
rm(Hackney, Hackney_sl, Hackney_appropriateness, Hackney_known, Hackney_unknown,
   map_Hackney_approp, map_Hackney_seg, map_Hackney_sl, Hackney_30, Hackney_20)

# 13) Hammersmith & Fulham
Ham = test_appropriateness %>%
  filter(BOROUGH == "Hammersmith & Fulham")
nrow(Ham) # 506
Ham_sl = london_speed_limits %>%
  filter(BOROUGH == "Hammersmith & Fulham")

# How many unknowns do we now have that need managing?
Ham %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 13 unknowns

# # # Examine Ham data to see if can match more speed limits to cycle lanes
map_Ham_approp = mapview(Ham, zcol = "seg_appro_speed_limit")
map_Ham_seg = mapview(Ham, zcol = "Highest_separation")
map_Ham_sl = mapview(Ham_sl, zcol = "speed_limit")
leafsync::sync(map_Ham_approp, map_Ham_seg, map_Ham_sl, ncol = 2)

Ham_30 = c("RWG117899")
Ham_20 = c("RWG018063", "RWG018067", "RWG042785", "RWG042818", "RWG042575",
           "RWG042839", "RWG042697")
          
# create df of unknown Ham
Ham_unknown = Ham %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Ham_unknown$numeric_speed_limit = replace(Ham_unknown$numeric_speed_limit,
                                              which(Ham_unknown$FEATURE_ID %in% Ham_30),
                                              values = 30)
Ham_unknown$numeric_speed_limit = replace(Ham_unknown$numeric_speed_limit,
                                              which(Ham_unknown$FEATURE_ID %in% Ham_20),
                                              values = 20)
Ham_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      7
# 2 30                      1
# 3 Unknown                 5

# Rerun appropriateness test
Ham_appropriateness = Ham_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Ham_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Ham_known = Ham %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 493

# Create final df
Ham_final = rbind(Ham_known, Ham_appropriateness)
nrow(Ham_final) # n = 506

# # Measure lengths of True, False and Unknown
Ham_summary = Ham_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Ham_total_length = sum(length)) %>%
  mutate(Ham_percentage = Ham_total_length/sum(Ham_total_length)*100)
# seg_appro_speed_limit Ham_total_length Ham_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           10772.          33.2 
# 2 TRUE                            21088.          65.1 
# 3 Unknown                           550.           1.70

# Tidy up df
rm(Ham, Ham_sl, Ham_appropriateness, Ham_known, Ham_unknown,
   map_Ham_approp, map_Ham_seg, map_Ham_sl, Ham_30, Ham_20)

#14) Haringey
Harin = test_appropriateness %>%
  filter(BOROUGH == "Haringey")
nrow(Harin) # 360
Harin_sl = london_speed_limits %>%
  filter(BOROUGH == "Haringey")

# How many unknowns do we now have that need managing?
Harin %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 28 unknowns

# # # Examine Harin data to see if can match more speed limits to cycle lanes
map_Harin_approp = mapview(Harin, zcol = "seg_appro_speed_limit")
map_Harin_seg = mapview(Harin, zcol = "Highest_separation")
map_Harin_sl = mapview(Harin_sl, zcol = "speed_limit")
leafsync::sync(map_Harin_approp, map_Harin_seg, map_Harin_sl, ncol = 2)

Harin_30 = c("RWG279264 _ 1", "RWG279265", "RWG279266", "RWG279268","RWG279269",
             "RWG279289", "RWG279346", "RWG279347", "RWG279349")
Harin_20 = c("RWG279220", "RWG279111", "RWG279145", "RWG279132", "RWG279104",
             "RWG279330", "RWG279329", "RWG279123")

# create df of unknown Harin
Harin_unknown = Harin %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Harin_unknown$numeric_speed_limit = replace(Harin_unknown$numeric_speed_limit,
                                          which(Harin_unknown$FEATURE_ID %in% Harin_30),
                                          values = 30)
Harin_unknown$numeric_speed_limit = replace(Harin_unknown$numeric_speed_limit,
                                          which(Harin_unknown$FEATURE_ID %in% Harin_20),
                                          values = 20)
Harin_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      8
# 2 30                      9
# 3 Unknown                11

# Rerun appropriateness test
Harin_appropriateness = Harin_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Harin_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Harin_known = Harin %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 332

# Create final df
Harin_final = rbind(Harin_known, Harin_appropriateness)
nrow(Harin_final) # n = 360

# # Measure lengths of True, False and Unknown
Harin_summary = Harin_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Harin_total_length = sum(length)) %>%
  mutate(Harin_percentage = Harin_total_length/sum(Harin_total_length)*100)
# seg_appro_speed_limit Harin_total_length Harin_percentage
# <chr>                                [m]              [1]
# 1 FALSE                             17964.            76.4 
# 2 TRUE                               4740.            20.2 
# 3 Unknown                             798.             3.39

# Tidy up df
rm(Harin, Harin_sl, Harin_appropriateness, Harin_known, Harin_unknown,
   map_Harin_approp, map_Harin_seg, map_Harin_sl, Harin_30, Harin_20)


#15) Harrow
Harrow = test_appropriateness %>%
  filter(BOROUGH == "Harrow")
nrow(Harrow) # 364
Harrow_sl = london_speed_limits %>%
  filter(BOROUGH == "Harrow")

# How many unknowns do we now have that need managing?
Harrow %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 170 unknowns

# # # Examine Harrow data to see if can match more speed limits to cycle lanes
map_Harrow_approp = mapview(Harrow, zcol = "seg_appro_speed_limit")
map_Harrow_seg = mapview(Harrow, zcol = "Highest_separation")
map_Harrow_sl = mapview(Harrow_sl, zcol = "speed_limit")
leafsync::sync(map_Harrow_approp, map_Harrow_seg, map_Harrow_sl, ncol = 2)

Harrow_30 = c("RWG274972", "RWG274232", "RWG274140", "RWG274001","RWG273969",
             "RWG274058", "RWG274124", "RWG274208", "RWG274204")
Harrow_20 = c("RWG274747")

# create df of unknown Harrow
Harrow_unknown = Harrow %>%
  filter(seg_appro_speed_limit == "Unknown")

# Update speed limits based on visual comparison
Harrow_unknown$numeric_speed_limit = replace(Harrow_unknown$numeric_speed_limit,
                                            which(Harrow_unknown$FEATURE_ID %in% Harrow_30),
                                            values = 30)
Harrow_unknown$numeric_speed_limit = replace(Harrow_unknown$numeric_speed_limit,
                                            which(Harrow_unknown$FEATURE_ID %in% Harrow_20),
                                            values = 20)
Harrow_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
# 1 20                      1
# 2 30                      9
# 3 Unknown               160

# Rerun appropriateness test
Harrow_appropriateness = Harrow_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Harrow_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Harrow_known = Harrow %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 194

# Create final df
Harrow_final = rbind(Harrow_known, Harrow_appropriateness)
nrow(Harrow_final) # n = 364

# # Measure lengths of True, False and Unknown
Harrow_summary = Harrow_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Harrow_total_length = sum(length)) %>%
  mutate(Harrow_percentage = Harrow_total_length/sum(Harrow_total_length)*100)
# seg_appro_speed_limit Harrow_total_length Harrow_percentage
# <chr>                                 [m]               [1]
# 1 FALSE                              16657.             51.8 
# 2 TRUE                                 373.              1.16
# 3 Unknown                            15133.             47.1 

# Tidy up df
rm(Harrow, Harrow_sl, Harrow_appropriateness, Harrow_known, Harrow_unknown,
   map_Harrow_approp, map_Harrow_seg, map_Harrow_sl, Harrow_30, Harrow_20)

#16) Havering
Hav = test_appropriateness %>%
  filter(BOROUGH == "Havering")
nrow(Hav) # 435
Hav_sl = london_speed_limits %>%
  filter(BOROUGH == "Havering")

# How many unknowns do we now have that need managing?
Hav %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 297 unknowns

# # # Examine Hav data to see if can match more speed limits to cycle lanes
map_Hav_approp = mapview(Hav, zcol = "seg_appro_speed_limit")
map_Hav_seg = mapview(Hav, zcol = "Highest_separation")
map_Hav_sl = mapview(Hav_sl, zcol = "speed_limit")
leafsync::sync(map_Hav_approp, map_Hav_seg, map_Hav_sl, ncol = 2)

Hav_unknown = Hav %>%
  filter(seg_appro_speed_limit == "Unknown")

Hav_30 = c("RWG122882", "RWG154676", "RWG154778")

# Update speed limits based on visual comparison
Hav_unknown$numeric_speed_limit = replace(Hav_unknown$numeric_speed_limit,
                                             which(Hav_unknown$FEATURE_ID %in% Hav_30),
                                             values = 30)
Hav_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 30                      3
# 2 Unknown               294

# Rerun appropriateness test
Hav_appropriateness = Hav_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Hav_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Hav_known = Hav %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 138

# Create final df
Hav_final = rbind(Hav_known, Hav_appropriateness)
nrow(Hav_final) # n = 435

# # Measure lengths of True, False and Unknown
Hav_summary = Hav_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Hav_total_length = sum(length)) %>%
  mutate(Hav_percentage = Hav_total_length/sum(Hav_total_length)*100)
# seg_appro_speed_limit Hav_total_length Hav_percentage
# <chr>                              [m]            [1]
# 1 FALSE                            6550.          25.8 
# 2 TRUE                             1139.           4.48
# 3 Unknown                         17727.          69.7 

# Tidy up df
rm(Hav, Hav_sl, Hav_appropriateness, Hav_known, Hav_unknown,
   map_Hav_approp, map_Hav_seg, map_Hav_sl, Hav_30)

#17) Hillingdon
Hill = test_appropriateness %>%
  filter(BOROUGH == "Hillingdon")
nrow(Hill) # 275
Hill_sl = london_speed_limits %>%
  filter(BOROUGH == "Hillingdon")

# How many unknowns do we now Hille that need managing?
Hill %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 107 unknowns

# # # Examine Hill data to see if can match more speed limits to cycle lanes
map_Hill_approp = mapview(Hill, zcol = "seg_appro_speed_limit")
map_Hill_seg = mapview(Hill, zcol = "Highest_separation")
map_Hill_sl = mapview(Hill_sl, zcol = "speed_limit")
leafsync::sync(map_Hill_approp, map_Hill_seg, map_Hill_sl, ncol = 2)

Hill_unknown = Hill %>%
  filter(seg_appro_speed_limit == "Unknown")
Hill_50 = c("RWG234109")
Hill_40 = c("RWG237143")
Hill_30 = c("RWG233605", "RWG237319", "RWG237748", "RWG237756")
Hill_20 = c("RWG236740")

# Update speed limits based on visual comparison
Hill_unknown$numeric_speed_limit = replace(Hill_unknown$numeric_speed_limit,
                                           which(Hill_unknown$FEATURE_ID %in% Hill_50),
                                           values = 50)
Hill_unknown$numeric_speed_limit = replace(Hill_unknown$numeric_speed_limit,
                                           which(Hill_unknown$FEATURE_ID %in% Hill_40),
                                           values = 40)
Hill_unknown$numeric_speed_limit = replace(Hill_unknown$numeric_speed_limit,
                                          which(Hill_unknown$FEATURE_ID %in% Hill_30),
                                          values = 30)
Hill_unknown$numeric_speed_limit = replace(Hill_unknown$numeric_speed_limit,
                                           which(Hill_unknown$FEATURE_ID %in% Hill_20),
                                           values = 20)
Hill_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      1
# 2 30                      4
# 3 40                      1
# 4 50                      1
# 5 Unknown               100

# Rerun appropriateness test
Hill_appropriateness = Hill_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Hill_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Hill_known = Hill %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 168

# Create final df
Hill_final = rbind(Hill_known, Hill_appropriateness)
nrow(Hill_final) # n = 175

# # Measure lengths of True, False and Unknown
Hill_summary = Hill_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Hill_total_length = sum(length)) %>%
  mutate(Hill_percentage = Hill_total_length/sum(Hill_total_length)*100)
# seg_appro_speed_limit Hill_total_length Hill_percentage
# <chr>                               [m]             [1]
# 1 FALSE                            12185.           57.6 
# 2 TRUE                               639.            3.02
# 3 Unknown                           8329.           39.4 

# Tidy up df
rm(Hill, Hill_sl, Hill_appropriateness, Hill_known, Hill_unknown,
   map_Hill_approp, map_Hill_seg, map_Hill_sl, Hill_50, Hill_40, Hill_30, Hill_20)

#18) Hounslow
Houn = test_appropriateness %>%
  filter(BOROUGH == "Hounslow")
nrow(Houn) # 543
Houn_sl = london_speed_limits %>%
  filter(BOROUGH == "Hounslow")

# How many unknowns do we now Houne that need managing?
Houn %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 168 unknowns

# # # Examine Houn data to see if can match more speed limits to cycle lanes
map_Houn_approp = mapview(Houn, zcol = "seg_appro_speed_limit")
map_Houn_seg = mapview(Houn, zcol = "Highest_separation")
map_Houn_sl = mapview(Houn_sl, zcol = "speed_limit")
leafsync::sync(map_Houn_approp, map_Houn_seg, map_Houn_sl, ncol = 2)

Houn_unknown = Houn %>%
  filter(seg_appro_speed_limit == "Unknown")

Houn_30 = c("RWG233482", "RWG233483", "RWG233600", "RWG233606", "RWG233776",
            "RWG233787", "RWG233850", "RWG233937", "RWG233942", "RWG233944",
            "RWG233952", "RWG233966", "RWG233968", "RWG233972", "RWG233973",
            "RWG234125", "RWG234230", "RWG234231", "RWG234232", "RWG234233",
            "RWG234279", "RWG234318", "RWG234328", "RWG234345")
Houn_20 = c("RWG233999", "RWG234003", "RWG234006", "RWG234019", "RWG234062",
            "RWG234342")

# Update speed limits based on visual comparison

Houn_unknown$numeric_speed_limit = replace(Houn_unknown$numeric_speed_limit,
                                           which(Houn_unknown$FEATURE_ID %in% Houn_30),
                                           values = 30)
Houn_unknown$numeric_speed_limit = replace(Houn_unknown$numeric_speed_limit,
                                           which(Houn_unknown$FEATURE_ID %in% Houn_20),
                                           values = 20)
Houn_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      6
# 2 30                     24
# 3 Unknown               138

# Rerun appropriateness test
Houn_appropriateness = Houn_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Houn_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Houn_known = Houn %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 375

# Create final df
Houn_final = rbind(Houn_known, Houn_appropriateness)
nrow(Houn_final) # n = 543

# # Measure lengths of True, False and Unknown
Houn_summary = Houn_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Houn_total_length = sum(length)) %>%
  mutate(Houn_percentage = Houn_total_length/sum(Houn_total_length)*100)
# seg_appro_speed_limit Houn_total_length Houn_percentage
# <chr>                               [m]             [1]
# 1 FALSE                            20651.            61.2
# 2 TRUE                              4763.            14.1
# 3 Unknown                           8343.            24.7

# Tidy up df
rm(Houn, Houn_sl, Houn_appropriateness, Houn_known, Houn_unknown,
   map_Houn_approp, map_Houn_seg, map_Houn_sl, Houn_30, Houn_20)

#19) Islington
Isl = test_appropriateness %>%
  filter(BOROUGH == "Islington")
nrow(Isl) # 576
Isl_sl = london_speed_limits %>%
  filter(BOROUGH == "Islington")

# How many unknowns do we now Isle that need managing?
Isl %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 17 unknowns

# # # Examine Isl data to see if can match more speed limits to cycle lanes
map_Isl_approp = mapview(Isl, zcol = "seg_appro_speed_limit")
map_Isl_seg = mapview(Isl, zcol = "Highest_separation")
map_Isl_sl = mapview(Isl_sl, zcol = "speed_limit")
leafsync::sync(map_Isl_approp, map_Isl_seg, map_Isl_sl, ncol = 2)

Isl_unknown = Isl %>%
  filter(seg_appro_speed_limit == "Unknown")

Isl_30 = c("RWG030441")
Isl_20 = c("RWG003621", "RWG003616", "RWG003617", "RWG030494", "RWG003595",
            "RWG003596", "RWG151066", "RWG151067", "RWG066480", "RWG066822",
           "RWG066388")

# Update speed limits based on visual comparison

Isl_unknown$numeric_speed_limit = replace(Isl_unknown$numeric_speed_limit,
                                           which(Isl_unknown$FEATURE_ID %in% Isl_30),
                                           values = 30)
Isl_unknown$numeric_speed_limit = replace(Isl_unknown$numeric_speed_limit,
                                           which(Isl_unknown$FEATURE_ID %in% Isl_20),
                                           values = 20)
Isl_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                     11
# 2 30                      1
# 3 Unknown                 5

# Rerun appropriateness test
Isl_appropriateness = Isl_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Isl_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Isl_known = Isl %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 559

# Create final df
Isl_final = rbind(Isl_known, Isl_appropriateness)
nrow(Isl_final) # n = 576

# # Measure lengths of True, False and Unknown
Isl_summary = Isl_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Isl_total_length = sum(length)) %>%
  mutate(Isl_percentage = Isl_total_length/sum(Isl_total_length)*100)
# seg_appro_speed_limit Isl_total_length Isl_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           17599.         56.5  
# 2 TRUE                            13384.         43.0  
# 3 Unknown                           174.          0.559

# Tidy up df
rm(Isl, Isl_sl, Isl_appropriateness, Isl_known, Isl_unknown,
   map_Isl_approp, map_Isl_seg, map_Isl_sl, Isl_30, Isl_20)

#20) Kensington & Chelsea
Ken = test_appropriateness %>%
  filter(BOROUGH == "Kensington & Chelsea")
nrow(Ken) # 190
Ken_sl = london_speed_limits %>%
  filter(BOROUGH == "Kensington & Chelsea")

# How many unknowns do we now Kene that need managing?
Ken %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 78 unknowns

# # # Examine Ken data to see if can match more speed limits to cycle lanes
map_Ken_approp = mapview(Ken, zcol = "seg_appro_speed_limit")
map_Ken_seg = mapview(Ken, zcol = "Highest_separation")
map_Ken_sl = mapview(Ken_sl, zcol = "speed_limit")
leafsync::sync(map_Ken_approp, map_Ken_seg, map_Ken_sl, ncol = 2)

Ken_unknown = Ken %>%
  filter(seg_appro_speed_limit == "Unknown")

Ken_30 = c("RWG018030 _ 1", "RWG018037", "RWG018123", "RWG018124", "RWG018132",
           "RWG018133", "RWG018134", "RWG042606 _ 2", "RWG056156")

# Update speed limits based on visual comparison

Ken_unknown$numeric_speed_limit = replace(Ken_unknown$numeric_speed_limit,
                                          which(Ken_unknown$FEATURE_ID %in% Ken_30),
                                          values = 30)
Ken_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 30                      9
# 2 Unknown                69

# Rerun appropriateness test
Ken_appropriateness = Ken_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Ken_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Ken_known = Ken %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 112

# Create final df
Ken_final = rbind(Ken_known, Ken_appropriateness)
nrow(Ken_final) # n = 190

# # Measure lengths of True, False and Unknown
Ken_summary = Ken_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Ken_total_length = sum(length)) %>%
  mutate(Ken_percentage = Ken_total_length/sum(Ken_total_length)*100)
# seg_appro_speed_limit Ken_total_length Ken_percentage
# <chr>                              [m]            [1]
# 1 FALSE                            5630.          44.8 
# 2 TRUE                              504.           4.02
# 3 Unknown                          6423.          51.2 

# Tidy up df
rm(Ken, Ken_sl, Ken_appropriateness, Ken_known, Ken_unknown,
   map_Ken_approp, map_Ken_seg, map_Ken_sl, Ken_30)

# 21) Kingston upon Thames
King = test_appropriateness %>%
  filter(BOROUGH == "Kingston upon Thames")
nrow(King) # 466
King_sl = london_speed_limits %>%
  filter(BOROUGH == "Kingston upon Thames")

# How many unknowns do we now Kinge that need managing?
King %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 126 unknowns

# # # Examine King data to see if can match more speed limits to cycle lanes
map_King_approp = mapview(King, zcol = "seg_appro_speed_limit")
map_King_seg = mapview(King, zcol = "Highest_separation")
map_King_sl = mapview(King_sl, zcol = "speed_limit")
leafsync::sync(map_King_approp, map_King_seg, map_King_sl, ncol = 2)

King_unknown = King %>%
  filter(seg_appro_speed_limit == "Unknown")

King_30 = c("RWG124075", "RWG124054", "RWG123748", "RWG124182")
King_20 = c("RWG206656", "RWG206676", "RWG124037")

# Update speed limits based on visual comparison
King_unknown$numeric_speed_limit = replace(King_unknown$numeric_speed_limit,
                                          which(King_unknown$FEATURE_ID %in% King_30),
                                          values = 30)
King_unknown$numeric_speed_limit = replace(King_unknown$numeric_speed_limit,
                                           which(King_unknown$FEATURE_ID %in% King_20),
                                           values = 20)
King_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# 1 20                      3
# 2 30                      4
# 3 Unknown               119

# Rerun appropriateness test
King_appropriateness = King_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
King_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
King_known = King %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 340

# Create final df
King_final = rbind(King_known, King_appropriateness)
nrow(King_final) # n = 190

# # Measure lengths of True, False and Unknown
King_summary = King_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(King_total_length = sum(length)) %>%
  mutate(King_percentage = King_total_length/sum(King_total_length)*100)
# seg_appro_speed_limit King_total_length King_percentage
# <chr>                               [m]             [1]
# 1 FALSE                            17644.            59.8
# 2 TRUE                              3377.            11.4
# 3 Unknown                           8475.            28.7

# Tidy up df
rm(King, King_sl, King_appropriateness, King_known, King_unknown,
   map_King_approp, map_King_seg, map_King_sl, King_30, King_20)

# 22) Lewbeth
Lam = test_appropriateness %>%
  filter(BOROUGH == "Lambeth")
nrow(Lam) # 920
Lam_sl = london_speed_limits %>%
  filter(BOROUGH == "Lambeth")

# How many unknowns do we now Lame that need managing?
Lam %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 47 unknowns

# # # Examine Lam data to see if can match more speed limits to cycle lanes
map_Lam_approp = mapview(Lam, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Lam_seg = mapview(Lam, zcol = "Highest_separation")
map_Lam_sl = mapview(Lam_sl, zcol = "speed_limit")
leafsync::sync(map_Lam_approp, map_Lam_seg, map_Lam_sl, ncol = 2)

Lam_unknown = Lam %>%
  filter(seg_appro_speed_limit == "Unknown")

Lam_30 = c("RWG056389", "RWG056388", "RWG056346", "RWG056317", "RWG056353",
           "RWG056357", "RWG056201", "RWG056202", "RWG056168", "RWG056251",
           "RWG056250", "RWG102198", "RWG102173", "RWG081936", "RWG081930")
Lam_20 = c("RWG153704 _ 1", "RWG153705", "RWG135121", "RWG056008", "RWG055935",
           "RWG056354", "RWG153731", "RWG056210", "RWG056298 _ 1", "RWG055907",
           "RWG135186", "RWG056329", "RWG102198", "RWG102400", "RWG107412",
           "RWG107409", "RWG107704")

# Update speed limits based on visual comparison
Lam_unknown$numeric_speed_limit = replace(Lam_unknown$numeric_speed_limit,
                                           which(Lam_unknown$FEATURE_ID %in% Lam_30),
                                           values = 30)
Lam_unknown$numeric_speed_limit = replace(Lam_unknown$numeric_speed_limit,
                                           which(Lam_unknown$FEATURE_ID %in% Lam_20),
                                           values = 20)
Lam_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
#numeric_speed_limit     n
# <chr>               <int>
#   1 20                     17
# 2 30                     14
# 3 Unknown                16
# NB one 30 missing but cant figure out why - wont substantially affect results

# Rerun appropriateness test
Lam_appropriateness = Lam_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Lam_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Lam_known = Lam %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 873

# Create final df
Lam_final = rbind(Lam_known, Lam_appropriateness)
nrow(Lam_final) # n = 920

# # Measure lengths of True, False and Unknown
Lam_summary = Lam_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Lam_total_length = sum(length)) %>%
  mutate(Lam_percentage = Lam_total_length/sum(Lam_total_length)*100)
#seg_appro_speed_limit Lam_total_length Lam_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           33851.          67.2 
# 2 TRUE                            14556.          28.9 
# 3 Unknown                          1947.           3.87

# Tidy up df
rm(Lam, Lam_sl, Lam_appropriateness, Lam_known, Lam_unknown,
   map_Lam_approp, map_Lam_seg, map_Lam_sl, Lam_30, Lam_20)

# 23) Lewisham
Lew = test_appropriateness %>%
  filter(BOROUGH == "Lewisham")
nrow(Lew) # 390
Lew_sl = london_speed_limits %>%
  filter(BOROUGH == "Lewisham")

# How many unknowns do we now Lewe that need managing?
Lew %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 17 unknowns

# # # Examine Lew data to see if can match more speed limits to cycle lanes
map_Lew_approp = mapview(Lew, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Lew_seg = mapview(Lew, zcol = "Highest_separation")
map_Lew_sl = mapview(Lew_sl, zcol = "speed_limit")
leafsync::sync(map_Lew_approp, map_Lew_seg, map_Lew_sl, ncol = 2)

Lew_unknown = Lew %>%
  filter(seg_appro_speed_limit == "Unknown")

Lew_30 = c("RWG198332","RWG198334","RWG198313","RWG198340","RWG198323", 
           "RWG198397","RWG198398","RWG198310","RWG204825","RWG198421")
Lew_20 = c("RWG198322 _ 1","RWG198183","RWG198755","RWG198129","RWG204822")

# Update speed limits based on visual comparison
Lew_unknown$numeric_speed_limit = replace(Lew_unknown$numeric_speed_limit,
                                          which(Lew_unknown$FEATURE_ID %in% Lew_30),
                                          values = 30)
Lew_unknown$numeric_speed_limit = replace(Lew_unknown$numeric_speed_limit,
                                          which(Lew_unknown$FEATURE_ID %in% Lew_20),
                                          values = 20)
Lew_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      5
# 2 30                     10
# 3 Unknown                 2

# Rerun appropriateness test
Lew_appropriateness = Lew_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Lew_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Lew_known = Lew %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 373

# Create final df
Lew_final = rbind(Lew_known, Lew_appropriateness)
nrow(Lew_final) # n = 390

# # Measure lengths of True, False and Unknown
Lew_summary = Lew_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Lew_total_length = sum(length)) %>%
  mutate(Lew_percentage = Lew_total_length/sum(Lew_total_length)*100)
# seg_appro_speed_limit Lew_total_length Lew_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           17822.           64.1
# 2 TRUE                             5231.           18.8
# 3 Unknown                          4752.           17.1

# Tidy up df
rm(Lew, Lew_sl, Lew_appropriateness, Lew_known, Lew_unknown,
   map_Lew_approp, map_Lew_seg, map_Lew_sl, Lew_30, Lew_20)

# 24) Merton
Mer = test_appropriateness %>%
  filter(BOROUGH == "Merton")
nrow(Mer) # 372
Mer_sl = london_speed_limits %>%
  filter(BOROUGH == "Merton")

# How many unknowns do we now Mere that need managing?
Mer %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 76 unknowns

# # # Examine Mer data to see if can match more speed limits to cycle lanes
map_Mer_approp = mapview(Mer, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Mer_seg = mapview(Mer, zcol = "Highest_separation")
map_Mer_sl = mapview(Mer_sl, zcol = "speed_limit")
leafsync::sync(map_Mer_approp, map_Mer_seg, map_Mer_sl, ncol = 2)

Mer_unknown = Mer %>%
  filter(seg_appro_speed_limit == "Unknown")

Mer_30 = c("RWG124129","RWG124130","RWG124420","RWG218755","RWG218858",
           "RWG218688")

# Update speed limits based on visual comparison
Mer_unknown$numeric_speed_limit = replace(Mer_unknown$numeric_speed_limit,
                                          which(Mer_unknown$FEATURE_ID %in% Mer_30),
                                          values = 30)
Mer_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# <chr>               <int>
#   1 30                      6
# 2 Unknown                70

# Rerun appropriateness test
Mer_appropriateness = Mer_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Mer_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Mer_known = Mer %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 296

# Create final df
Mer_final = rbind(Mer_known, Mer_appropriateness)
nrow(Mer_final) # n = 372

# # Measure lengths of True, False and Unknown
Mer_summary = Mer_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Mer_total_length = sum(length)) %>%
  mutate(Mer_percentage = Mer_total_length/sum(Mer_total_length)*100)
# seg_appro_speed_limit Mer_total_length Mer_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           15732.          73.9 
# 2 TRUE                             1392.           6.54
# 3 Unknown                          4151.          19.5 

# Tidy up df
rm(Mer, Mer_sl, Mer_appropriateness, Mer_known, Mer_unknown,
   map_Mer_approp, map_Mer_seg, map_Mer_sl, Mer_30)

# 25) Newham
New = test_appropriateness %>%
  filter(BOROUGH == "Newham")
nrow(New) # 576
New_sl = london_speed_limits %>%
  filter(BOROUGH == "Newham")

# How many unknowns do we now Newe that need managing?
New %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 47 unknowns

# # # Examine New data to see if can match more speed limits to cycle lanes
map_New_approp = mapview(New, zcol = "seg_appro_speed_limit", legend = FALSE)
map_New_seg = mapview(New, zcol = "Highest_separation")
map_New_sl = mapview(New_sl, zcol = "speed_limit")
leafsync::sync(map_New_approp, map_New_seg, map_New_sl, ncol = 2)

New_unknown = New %>%
  filter(seg_appro_speed_limit == "Unknown")

New_30 = c("RWG154882","RWG155020","RWG155288","RWG155494","RWG155496",
           "RWG155611","RWG155664","RWG156241","RWG156248","RWG156255",
           "RWG156256","RWG184786","RWG184787","RWG290281")
New_20 = c("RWG155773","RWG155779","RWG155790")

# Update speed limits based on visual comparison
New_unknown$numeric_speed_limit = replace(New_unknown$numeric_speed_limit,
                                          which(New_unknown$FEATURE_ID %in% New_30),
                                          values = 30)
New_unknown$numeric_speed_limit = replace(New_unknown$numeric_speed_limit,
                                          which(New_unknown$FEATURE_ID %in% New_20),
                                          values = 20)
New_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      3
# 2 30                     14
# 3 Unknown                30

# Rerun appropriateness test
New_appropriateness = New_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
New_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
New_known = New %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 529

# Create final df
New_final = rbind(New_known, New_appropriateness)
nrow(New_final) # n = 576

# # Measure lengths of True, False and Unknown
New_summary = New_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(New_total_length = sum(length)) %>%
  mutate(New_percentage = New_total_length/sum(New_total_length)*100)
# seg_appro_speed_limit New_total_length New_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           22961.          71.9 
# 2 TRUE                             6015.          18.8 
# 3 Unknown                          2970.           9.30

# Tidy up df
rm(New, New_sl, New_appropriateness, New_known, New_unknown,
   map_New_approp, map_New_seg, map_New_sl, New_30)

# 26) Redbridge
Red = test_appropriateness %>%
  filter(BOROUGH == "Redbridge")
nrow(Red) # 476
Red_sl = london_speed_limits %>%
  filter(BOROUGH == "Redbridge")

# How many unknowns do we now Rede that need managing?
Red %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 115 unknowns

# # # Examine Red data to see if can match more speed limits to cycle lanes
map_Red_approp = mapview(Red, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Red_seg = mapview(Red, zcol = "Highest_separation")
map_Red_sl = mapview(Red_sl, zcol = "speed_limit")
leafsync::sync(map_Red_approp, map_Red_seg, map_Red_sl, ncol = 2)

Red_unknown = Red %>%
  filter(seg_appro_speed_limit == "Unknown")

Red_30 = c("RWG156073","RWG154801","RWG155259","RWG155201","RWG155101",
           "RWG155100","RWG154877")

# Update speed limits based on visual comparison
Red_unknown$numeric_speed_limit = replace(Red_unknown$numeric_speed_limit,
                                          which(Red_unknown$FEATURE_ID %in% Red_30),
                                          values = 30)

Red_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 30                      7
# 2 Unknown               108

# Rerun appropriateness test
Red_appropriateness = Red_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Red_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Red_known = Red %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 361

# Create final df
Red_final = rbind(Red_known, Red_appropriateness)
nrow(Red_final) # n = 476

# # Measure lengths of True, False and Unknown
Red_summary = Red_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Red_total_length = sum(length)) %>%
  mutate(Red_percentage = Red_total_length/sum(Red_total_length)*100)
# 1 FALSE                           14927.          60.6 
# 2 TRUE                              569.           2.31
# 3 Unknown                          9144.          37.1

# Tidy up df
rm(Red, Red_sl, Red_appropriateness, Red_known, Red_unknown,
   map_Red_approp, map_Red_seg, map_Red_sl, Red_30)

# 27) Richmond upon Thames
Ric = test_appropriateness %>%
  filter(BOROUGH == "Richmond upon Thames")
nrow(Ric) # 259
Ric_sl = london_speed_limits %>%
  filter(BOROUGH == "Richmond upon Thames")

# How many unknowns do we now Rice that need managing?
Ric %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 107 unknowns

# # # Examine Ric data to see if can match more speed limits to cycle lanes
map_Ric_approp = mapview(Ric, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Ric_seg = mapview(Ric, zcol = "Highest_separation")
map_Ric_sl = mapview(Ric_sl, zcol = "speed_limit")
leafsync::sync(map_Ric_approp, map_Ric_seg, map_Ric_sl, ncol = 2)

Ric_unknown = Ric %>%
  filter(seg_appro_speed_limit == "Unknown")

Ric_30 = c("RWG123580", "RWG123816", "RWG123557", "RWG234304", "RWG234300",
           "RWG234113","RWG234186")

# Update speed limits based on visual comparison
Ric_unknown$numeric_speed_limit = replace(Ric_unknown$numeric_speed_limit,
                                          which(Ric_unknown$FEATURE_ID %in% Ric_30),
                                          values = 30)

Ric_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 30                      7
# 2 Unknown               100

# Rerun appropriateness test
Ric_appropriateness = Ric_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Ric_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Ric_known = Ric %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 142

# Create final df
Ric_final = rbind(Ric_known, Ric_appropriateness)
nrow(Ric_final) # n = 249

# # Measure lengths of True, False and Unknown
Ric_summary = Ric_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Ric_total_length = sum(length)) %>%
  mutate(Ric_percentage = Ric_total_length/sum(Ric_total_length)*100)
# seg_appro_speed_limit Ric_total_length Ric_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           15130.          61.5 
# 2 TRUE                             1693.           6.88
# 3 Unknown                          7775.          31.6 

# Tidy up df
rm(Ric, Ric_sl, Ric_appropriateness, Ric_known, Ric_unknown,
   map_Ric_approp, map_Ric_seg, map_Ric_sl, Ric_30)

# 28) Southwark
Sou = test_appropriateness %>%
  filter(BOROUGH == "Southwark")
nrow(Sou) # 909
Sou_sl = london_speed_limits %>%
  filter(BOROUGH == "Southwark")

# How many unknowns do we now Soue that need managing?
Sou %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 46 unknowns

# # # Examine Sou data to see if can match more speed limits to cycle lanes
map_Sou_approp = mapview(Sou, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Sou_seg = mapview(Sou, zcol = "Highest_separation")
map_Sou_sl = mapview(Sou_sl, zcol = "speed_limit")
leafsync::sync(map_Sou_approp, map_Sou_seg, map_Sou_sl, ncol = 2)

Sou_unknown = Sou %>%
  filter(seg_appro_speed_limit == "Unknown")

Sou_30 = c("RWG081925","RWG082030", "RWG108043","RWG108016","RWG082408",
           "RWG082684", "RWG135971", "RWG135742 _ 1", "RWG082216", "RWG082218",
           "RWG082335", "RWG135735", "RWG107442")
Sou_20 = c("RWG082686","RWG082359","RWG082212","RWG107477","RWG999683",
           "RWG107411")

# Update speed limits based on visual comparison
Sou_unknown$numeric_speed_limit = replace(Sou_unknown$numeric_speed_limit,
                                          which(Sou_unknown$FEATURE_ID %in% Sou_30),
                                          values = 30)
Sou_unknown$numeric_speed_limit = replace(Sou_unknown$numeric_speed_limit,
                                          which(Sou_unknown$FEATURE_ID %in% Sou_20),
                                          values = 20)

Sou_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      6
# 2 30                     13
# 3 Unknown                27

# Rerun appropriateness test
Sou_appropriateness = Sou_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Sou_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Sou_known = Sou %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 863

# Create final df
Sou_final = rbind(Sou_known, Sou_appropriateness)
nrow(Sou_final) # n = 909

# # Measure lengths of True, False and Unknown
Sou_summary = Sou_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Sou_total_length = sum(length)) %>%
  mutate(Sou_percentage = Sou_total_length/sum(Sou_total_length)*100)
# seg_appro_speed_limit Sou_total_length Sou_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           25164.          59.9 
# 2 TRUE                            14704.          35.0 
# 3 Unknown                          2146.           5.11

# Tidy up df
rm(Sou, Sou_sl, Sou_appropriateness, Sou_known, Sou_unknown,
   map_Sou_approp, map_Sou_seg, map_Sou_sl, Sou_30)

# 29) Sutton    
Sut = test_appropriateness %>%
  filter(BOROUGH == "Sutton")
nrow(Sut) # 115
Sut_sl = london_speed_limits %>%
  filter(BOROUGH == "Sutton")

# How many unknowns do we now Sute that need managing?
Sut %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 55 unknowns

# # # Examine Sut data to see if can match more speed limits to cycle lanes
map_Sut_approp = mapview(Sut, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Sut_seg = mapview(Sut, zcol = "Highest_separation")
map_Sut_sl = mapview(Sut_sl, zcol = "speed_limit")
leafsync::sync(map_Sut_approp, map_Sut_seg, map_Sut_sl, ncol = 2)

# unable to identify any unknown road speeds
Sut_final = Sut

# # Measure lengths of True, False and Unknown
Sut_summary = Sut_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Sut_total_length = sum(length)) %>%
  mutate(Sut_percentage = Sut_total_length/sum(Sut_total_length)*100)
# seg_appro_speed_limit Sut_total_length Sut_percentage
# <chr>                              [m]            [1]
# 1 FALSE                            2150.          36.6 
# 2 TRUE                              352.           5.99
# 3 Unknown                          3366.          57.4 

# Tidy up df
rm(Sut, Sut_sl, Sut_unknown,
   map_Sut_approp, map_Sut_seg, map_Sut_sl)

# 30) Tower Hamlets
Tow = test_appropriateness %>%
  filter(BOROUGH == "Tower Hamlets")
nrow(Tow) # 755
Tow_sl = london_speed_limits %>%
  filter(BOROUGH == "Tower Hamlets")

# How many unknowns do we now Towe that need managing?
Tow %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 29 unknowns

# # # Examine Tow data to see if can match more speed limits to cycle lanes
map_Tow_approp = mapview(Tow, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Tow_seg = mapview(Tow, zcol = "Highest_separation")
map_Tow_sl = mapview(Tow_sl, zcol = "speed_limit")
leafsync::sync(map_Tow_approp, map_Tow_seg, map_Tow_sl, ncol = 2)

Tow_unknown = Tow %>%
  filter(seg_appro_speed_limit == "Unknown")

Tow_30 = c("RWG108906","RWG152810","RWG152811","RWG109345","RWG073630",
           "RWG153237","RWG073440","RWG073396","RWG073342","RWG073277",
           "RWG097810","RWG073402","RWG073404","RWG073405","RWG073515",
           "RWG073606","RWG073611","RWG073243")

# Update speed limits based on visual comparison
Tow_unknown$numeric_speed_limit = replace(Tow_unknown$numeric_speed_limit,
                                          which(Tow_unknown$FEATURE_ID %in% Tow_30),
                                          values = 30)
Tow_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 30                     18
# 2 Unknown                11

# Rerun appropriateness test
Tow_appropriateness = Tow_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Tow_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Tow_known = Tow %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 726

# Create final df
Tow_final = rbind(Tow_known, Tow_appropriateness)
nrow(Tow_final) # n = 755

# # Measure lengths of True, False and Unknown
Tow_summary = Tow_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Tow_total_length = sum(length)) %>%
  mutate(Tow_percentage = Tow_total_length/sum(Tow_total_length)*100)
# seg_appro_speed_limit Tow_total_length Tow_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           14669.          49.1 
# 2 TRUE                            14709.          49.2 
# 3 Unknown                           512.           1.71

# Tidy up df
rm(Tow, Tow_sl, Tow_appropriateness, Tow_known, Tow_unknown,
   map_Tow_approp, map_Tow_seg, map_Tow_sl, Tow_30)

# 31) Waltham Forest
Wal = test_appropriateness %>%
  filter(BOROUGH == "Waltham Forest")
nrow(Wal) # 568
Wal_sl = london_speed_limits %>%
  filter(BOROUGH == "Waltham Forest")

# How many unknowns do we now Wale that need managing?
Wal %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 68 unknowns

# # # Examine Wal data to see if can match more speed limits to cycle lanes
map_Wal_approp = mapview(Wal, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Wal_seg = mapview(Wal, zcol = "Highest_separation")
map_Wal_sl = mapview(Wal_sl, zcol = "speed_limit")
leafsync::sync(map_Wal_approp, map_Wal_seg, map_Wal_sl, ncol = 2)

Wal_unknown = Wal %>%
  filter(seg_appro_speed_limit == "Unknown")

Wal_40 = c("RWG149577")
Wal_30 = c("RWG149522","RWG149524","RWG149643","RWG149490","RWG149489",
           "RWG149596","RWG187335","RWG149310","RWG149335","RWG187613")
Wal_20 = c("RWG149638","RWG149291","RWG187538","RWG187646")

# Update speed limits based on visual comparison
Wal_unknown$numeric_speed_limit = replace(Wal_unknown$numeric_speed_limit,
                                          which(Wal_unknown$FEATURE_ID %in% Wal_40),
                                          values = 40)
Wal_unknown$numeric_speed_limit = replace(Wal_unknown$numeric_speed_limit,
                                          which(Wal_unknown$FEATURE_ID %in% Wal_30),
                                          values = 30)
Wal_unknown$numeric_speed_limit = replace(Wal_unknown$numeric_speed_limit,
                                          which(Wal_unknown$FEATURE_ID %in% Wal_20),
                                          values = 20)

Wal_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      4
# 2 30                     10
# 3 40                      1
# 4 Unknown                53

# Rerun appropriateness test
Wal_appropriateness = Wal_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Wal_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Wal_known = Wal %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 500

# Create final df
Wal_final = rbind(Wal_known, Wal_appropriateness)
nrow(Wal_final) # n = 568

# # Measure lengths of True, False and Unknown
Wal_summary = Wal_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Wal_total_length = sum(length)) %>%
  mutate(Wal_percentage = Wal_total_length/sum(Wal_total_length)*100)
# seg_appro_speed_limit Wal_total_length Wal_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           26581.           59.4
# 2 TRUE                            11414.           25.5
# 3 Unknown                          6746.           15.1

# Tidy up df
rm(Wal, Wal_sl, Wal_appropriateness, Wal_known, Wal_unknown,
   map_Wal_approp, map_Wal_seg, map_Wal_sl, Wal_30)

# 32) Wandsworth
Wan = test_appropriateness %>%
  filter(BOROUGH == "Wandsworth")
nrow(Wan) # 773
Wan_sl = london_speed_limits %>%
  filter(BOROUGH == "Wandsworth")

# How many unknowns do we now Wane that need managing?
Wan %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 88 unknowns

# # # Examine Wan data to see if can match more speed limits to cycle lanes
map_Wan_approp = mapview(Wan, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Wan_seg = mapview(Wan, zcol = "Highest_separation")
map_Wan_sl = mapview(Wan_sl, zcol = "speed_limit")
leafsync::sync(map_Wan_approp, map_Wan_seg, map_Wan_sl, ncol = 2)

Wan_unknown = Wan %>%
  filter(seg_appro_speed_limit == "Unknown")

Wan_30 = c("RWG089232","RWG089292","RWG109729","RWG109993","RWG109994",
           "RWG109905","RWG109812","RWG171205","RWG110008","RWG999775",
           "RWG089287","RWG055975","RWG056247")
Wan_20 = c("RWG171219","RWG171218","RWG109837","RWG089218 _ 2")

# Update speed limits based on visual comparison
Wan_unknown$numeric_speed_limit = replace(Wan_unknown$numeric_speed_limit,
                                          which(Wan_unknown$FEATURE_ID %in% Wan_30),
                                          values = 30)
Wan_unknown$numeric_speed_limit = replace(Wan_unknown$numeric_speed_limit,
                                          which(Wan_unknown$FEATURE_ID %in% Wan_20),
                                          values = 20)
Wan_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      4
# 2 30                     13
# 3 Unknown                71

# Rerun appropriateness test
Wan_appropriateness = Wan_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Wan_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Wan_known = Wan %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 685

# Create final df
Wan_final = rbind(Wan_known, Wan_appropriateness)
nrow(Wan_final) # n = 773

# # Measure lengths of True, False and Unknown
Wan_summary = Wan_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Wan_total_length = sum(length)) %>%
  mutate(Wan_percentage = Wan_total_length/sum(Wan_total_length)*100)
# <chr>                              [m]            [1]
# 1 FALSE                           27511.          79.2 
# 2 TRUE                             2979.           8.58
# 3 Unknown                          4249.          12.2 

# Tidy up df
rm(Wan, Wan_sl, Wan_appropriateness, Wan_known, Wan_unknown,
   map_Wan_approp, map_Wan_seg, map_Wan_sl, Wan_30)

# 33) Westminster
Wes = test_appropriateness %>%
  filter(BOROUGH == "Westminster")
nrow(Wes) # 568
Wes_sl = london_speed_limits %>%
  filter(BOROUGH == "Westminster")

# How many unknowns do we now Wese that need managing?
Wes %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)  # 87 unknowns

# # # Examine Wes data to see if can match more speed limits to cycle lanes
map_Wes_approp = mapview(Wes, zcol = "seg_appro_speed_limit", legend = FALSE)
map_Wes_seg = mapview(Wes, zcol = "Highest_separation")
map_Wes_sl = mapview(Wes_sl, zcol = "speed_limit")
leafsync::sync(map_Wes_approp, map_Wes_seg, map_Wes_sl, ncol = 2)

Wes_unknown = Wes %>%
  filter(seg_appro_speed_limit == "Unknown")

Wes_30 = c("RWG066041","RWG066812","RWG066543","RWG066306","RWG152187",
           "RWG152190","RWG152188","RWG152191","RWG056109","RWG135108",
           "RWG056161")
Wes_20 = c("RWG056298 _ 2")

# Update speed limits based on visual comparison
Wes_unknown$numeric_speed_limit = replace(Wes_unknown$numeric_speed_limit,
                                          which(Wes_unknown$FEATURE_ID %in% Wes_30),
                                          values = 30)
Wes_unknown$numeric_speed_limit = replace(Wes_unknown$numeric_speed_limit,
                                          which(Wes_unknown$FEATURE_ID %in% Wes_20),
                                          values = 20)
Wes_unknown %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit)
# numeric_speed_limit     n
# <chr>               <int>
#   1 20                      1
# 2 30                     11
# 3 Unknown                75

# Rerun appropriateness test
Wes_appropriateness = Wes_unknown %>%
  mutate(seg_appro_speed_limit = case_when((numeric_speed_limit == 50) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 40) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit == 30) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Segregated") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Stepped/part segregation") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "Mandatory/Advisory cycle lane") ~ "TRUE",
                                           (numeric_speed_limit <= 20) & (Highest_separation == "No separation") ~ "FALSE",
                                           (numeric_speed_limit == "Unknown") & (Highest_separation == "Segregated") ~ "TRUE",  # this is ok as any speed is ok for seg
                                           (numeric_speed_limit == "Unknown") & (Highest_separation != "Segregated") ~ "Unknown",
                                           TRUE ~ "FALSE"))
# Rerun count
Wes_appropriateness %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit)

# Create df of known
Wes_known = Wes %>%
  filter(seg_appro_speed_limit != "Unknown") # n = 481

# Create final df
Wes_final = rbind(Wes_known, Wes_appropriateness)
nrow(Wes_final) # n = 568

# # Measure lengths of True, False and Unknown
Wes_summary = Wes_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(Wes_total_length = sum(length)) %>%
  mutate(Wes_percentage = Wes_total_length/sum(Wes_total_length)*100)
# seg_appro_speed_limit Wes_total_length Wes_percentage
# <chr>                              [m]            [1]
# 1 FALSE                           22323.          64.0 
# 2 TRUE                             9146.          26.2 
# 3 Unknown                          3404.           9.76

# Tidy up df
rm(Wes, Wes_sl, Wes_appropriateness, Wes_known, Wes_unknown,
   map_Wes_approp, map_Wes_seg, map_Wes_sl, Wes_30, Wes_20)

#####################################################################
# Create London cycle lane compliance dataframes for visualisations #
#####################################################################
# first15_final = rbind(Barking_final, Barnet_final, Bexley_final, Brent_final, 
#                 Bromley_final, Camden_final, Croydon_final, City_final,
#                 Ealing_final, Enfield_final, Greenwich_final, Hackney_final,
#                 Ham_final, Harin_final, Harrow_final)
# 
# first15_summary = plyr::join_all(list(Barking_summary, Barnet_summary, Bexley_summary, Brent_summary, 
#                         Bromley_summary, Camden_summary, Croydon_summary, City_summary,
#                         Ealing_summary, Enfield_summary, Greenwich_summary, Hackney_summary,
#                         Ham_summary, Harin_summary, Harrow_summary), by = "seg_appro_speed_limit", type = "left")
# 
# saveRDS(first15_final, file = "data/first15_final.Rds")
# saveRDS(first15_summary, file = "data/first15_summary.Rds")

# london_compliance_final = rbind(first15_final, Hav_final, Hill_final, Houn_final, 
#                                 Isl_final, Ken_final, King_final, Lam_final, Lew_final,
#                                 Mer_final, New_final, Red_final, Ric_final, Sou_final,
#                                 Sut_final, Tow_final, Wal_final, Wan_final, Wes_final)
# nrow(london_compliance_final) # 15459
# 
# london_compliance_summary_final = plyr::join_all(list(first15_summary, Hav_summary, Hill_summary, Houn_summary, 
#                                 Isl_summary, Ken_summary, King_summary, Lam_summary, Lew_summary,
#                                 Mer_summary, New_summary, Red_summary, Ric_summary, Sou_summary,
#                                 Sut_summary, Tow_summary, Wal_summary, Wan_summary, Wes_summary),
#                                 by = "seg_appro_speed_limit", type = "left")
# 
# london_compliance_summary_final= t(london_compliance_summary_final)
# london_compliance_summary_final = london_compliance_summary_final %>%
#   janitor::row_to_names(row_number = 1)
#saveRDS(london_compliance_final, file = "data/london_compliance_final_26_01_2022.Rds")
# saveRDS(london_compliance_summary_final, file = "data/london_compliance_summary_final.Rds")

#  Check how many now coded
# a) speed limit
london_compliance_final %>%
  st_drop_geometry() %>%
  count(numeric_speed_limit) 
# numeric_speed_limit     n
# <chr>               <int>
#  1 20                   5898
# 2 30                   6625
# 3 40                    169
# 4 50                     12
# 5 Unknown              2755  # prior to this had 3158 unknown in test_approrpiateness df

# b) compliance
compli_count = london_compliance_final %>%
  st_drop_geometry() %>%
  count(seg_appro_speed_limit) %>%
  mutate(total_count = sum(n)) %>%
  mutate(percentage = round(n/total_count*100))
# seg_appro_speed_limit     n total_count percentage
# <chr>                 <int>       <int>      <dbl>
# 1 FALSE                  7632       15459         49
# 2 TRUE                   5492       15459         36
# 3 Unknown                2335       15459         15  #Prior to this visual adding of speed limit had 2738 unknown


london_compliance_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(length = sum(length)) %>%
  mutate(total_length = sum(length)) %>%
  mutate(percentage = round(length/total_length*100))
# seg_appro_speed_limit  length total_length percentage
# <chr>                     [m]          [m]        [1]
# 1 FALSE                 565811.      959726.         59
# 2 TRUE                  196436.      959726.         20
# 3 Unknown               197478.      959726.         21

# Compliance by type and seg
compliance_by_type = london_compliance_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(type, seg_appro_speed_limit) %>%
  summarise(total_length_a = sum(length)) %>%
  group_by(type) %>%
  mutate(total_type_length = sum(total_length_a)) %>%
  mutate(type_percentage = units::drop_units(total_length_a/total_type_length*100))

compliance_by_seg = london_compliance_final %>%
  mutate(length = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(type, seg_appro_speed_limit) %>%
  summarise(total_length_b = sum(length)) %>%
  group_by(seg_appro_speed_limit) %>%
  mutate(total_seg_length = sum(total_length_b)) %>%
  mutate(seg_percentage = units::drop_units(total_length_b/total_seg_length*100))

compliance_table = left_join(compliance_by_type, compliance_by_seg)
# type     seg_appro_speed_l… total_length_a total_type_leng… type_percentage total_length_b total_seg_length seg_percentage
# <fct>    <chr>                         [m]              [m]           <dbl>            [m]              [m]          <dbl>
# 1 Rest     FALSE                     286541.          606465.          47.2          286541.          565811.         50.6  
# 2 Rest     TRUE                      164625.          606465.          27.1          164625.          196436.         83.8  
# 3 Rest     Unknown                   155299.          606465.          25.6          155299.          197478.         78.6  
# 4 Shared   FALSE                     212869.          239393.          88.9          212869.          565811.         37.6  
# 5 Shared   TRUE                        1251.          239393.           0.523          1251.          196436.          0.637
# 6 Shared   Unknown                    25274.          239393.          10.6           25274.          197478.         12.8  
# 7 Contraf… FALSE                      66401.          113868.          58.3           66401.          565811.         11.7  
# 8 Contraf… TRUE                       30561.          113868.          26.8           30561.          196436.         15.6  
# 9 Contraf… Unknown                    16906.          113868.          14.8           16906.          197478.          8.56 


#######################################################################
# Load and manipulate Local Authority spatial data for visualisations #
#######################################################################

# import Dec 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_Dec_2020_BFC.Rds")

# simply borough shapes
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs

# Add three letter acronym column
lon_lad_2020_c2c$b_acronym = fct_recode(lon_lad_2020_c2c$BOROUGH, 
                                        "K&C" = "Kensington & Chelsea",
                                        "B&D" = "Barking & Dagenham",
                                        "H&F" = "Hammersmith & Fulham",
                                        "Kin" = "Kingston upon Thames",
                                        "Ric" = "Richmond upon Thames",
                                        "City" = "City of London",
                                        "Wal" = "Waltham Forest",
                                        "Cro" = "Croydon",
                                        "Bro" = "Bromley",
                                        "Hou" = "Hounslow",
                                        "Eal" = "Ealing",
                                        "Hav" = "Havering",
                                        "Hil" = "Hillingdon",
                                        "Hrw" = "Harrow",
                                        "Bre" = "Brent",
                                        "Bar" = "Barnet",
                                        "Lam" = "Lambeth",
                                        "Sou" = "Southwark", 
                                        "Lew" = "Lewisham",
                                        "Gre" = "Greenwich",
                                        "Bex" = "Bexley",
                                        "Enf" = "Enfield",
                                        "Red" = "Redbridge",
                                        "Sut" = "Sutton",
                                        "Mer" = "Merton",
                                        "Wan" = "Wandsworth",
                                        "Wes" = "Westminster",
                                        "Cam" = "Camden",
                                        "Tow" = "Tower Hamlets",
                                        "Isl" = "Islington",
                                        "Hac" = "Hackney",
                                        "Har" = "Haringey",
                                        "New" = "Newham")

# Select variables of interest
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "b_acronym", "geometry"))

# Import River thames data for specifying coord_sf 
riverthames = st_read("map_data/riverthames.shp")
riverthames_simplify = rmapshaper::ms_simplify(riverthames)

###########################################################################################
# Import and manipulate london_squared dataset to enable spatial arrangement of barcharts #
###########################################################################################

# source data  = "https://github.com/aftertheflood/londonsquared/blob/master/site/data/grid.csv"

# Import london squared dataset
london_squared = read.table(file = "data/londonsquared.txt", header = TRUE, sep = ",")

# Rename columns
london_squared$fY <-london_squared$y
london_squared$fX <-london_squared$x

# Helper function for rescaling
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

# Manipulate london_squared layout for use in ggplot2 facet_grid().
max_y <- max(london_squared$fY)
min_y <- min(london_squared$fY)

london_squared <- london_squared %>% 
  mutate(fY=map_scale(fY, min_y, max_y, max_y, min_y))
rm(min_y,max_y)

# Relabel values in london_squared so can join to Boroughs
london_squared$BOROUGH <- london_squared$name
london_squared$BOROUGH = as.factor(london_squared$BOROUGH) 
london_squared$BOROUGH = fct_recode(london_squared$BOROUGH, 
                                    "Kensington & Chelsea" = "Kensington and Chelsea", 
                                    "Barking & Dagenham" = "Barking and Dagenham",
                                    "Hammersmith & Fulham" = "Hammersmith and Fulham")                            

london_squared$b_acronym = fct_recode(london_squared$BOROUGH, 
                                      "K&C" = "Kensington & Chelsea",
                                      "B&D" = "Barking & Dagenham",
                                      "H&F" = "Hammersmith & Fulham",
                                      "Kin" = "Kingston upon Thames",
                                      "Ric" = "Richmond upon Thames",
                                      "City" = "City of London",
                                      "Wal" = "Waltham Forest",
                                      "Cro" = "Croydon",
                                      "Bro" = "Bromley",
                                      "Hou" = "Hounslow",
                                      "Eal" = "Ealing",
                                      "Hav" = "Havering",
                                      "Hil" = "Hillingdon",
                                      "Hrw" = "Harrow",
                                      "Bre" = "Brent",
                                      "Bar" = "Barnet",
                                      "Lam" = "Lambeth",
                                      "Sou" = "Southwark", 
                                      "Lew" = "Lewisham",
                                      "Gre" = "Greenwich",
                                      "Bex" = "Bexley",
                                      "Enf" = "Enfield",
                                      "Red" = "Redbridge",
                                      "Sut" = "Sutton",
                                      "Mer" = "Merton",
                                      "Wan" = "Wandsworth",
                                      "Wes" = "Westminster",
                                      "Cam" = "Camden",
                                      "Tow" = "Tower Hamlets",
                                      "Isl" = "Islington",
                                      "Hac" = "Hackney",
                                      "Har" = "Haringey",
                                      "New" = "Newham")


# simplify dataset for joining
london_squared_tidy = london_squared %>%
  select(c("BOROUGH", "b_acronym", "fY", "fX"))

##################################################
# Reload and manipulate dataframe of cycle lanes #
##################################################

london_compliance_final = readRDS(file = "data/london_compliance_final.Rds")

# Reorder and rename factors
london_compliance_final$seg_appro_speed_limit = as.factor(london_compliance_final$seg_appro_speed_limit)
london_compliance_final$seg_appro_speed_limit = fct_relevel(london_compliance_final$seg_appro_speed_limit, c("Unknown", "FALSE", "TRUE"))
london_compliance_final$seg_appro_speed_limit = fct_recode(london_compliance_final$seg_appro_speed_limit, 
                                                             "Compliant" = "TRUE", "Not compliant" = "FALSE")

london_compliance_final %>%
  st_drop_geometry() %>%
  group_by(seg_appro_speed_limit) %>%
  summarise(count = n())


# Create variable for length by compliance by Borough
borough_separation_length = london_compliance_final  %>%
  mutate(length_m = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(BOROUGH, seg_appro_speed_limit) %>%
  summarise(total_length = sum(length_m))

# # Create variable for total length by Borough of all on road cycle lanes - may be needed for Results section
total_borough_separation_length = london_compliance_final %>%
  mutate(length_m = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length = sum(length_m))

# join total borough lengths to borough_separation_length
borough_separation_length = left_join(borough_separation_length, total_borough_separation_length)

# Join london_squared and borough area to cycle lane lengths
borough_separation_length_spatial = left_join(borough_separation_length, london_squared_tidy, by = "BOROUGH") 

# remove units to allow plotting
borough_separation_length_spatial = units::drop_units(borough_separation_length_spatial)


###########################################################
# Create table for results/appendix of borough compliance #
###########################################################
borough_compliance_table = borough_separation_length %>%
  arrange(seg_appro_speed_limit, desc(total_length)) %>%
  mutate(total_length_rounded = round(units::set_units(total_length, "km"), digits = 1)) %>%
  mutate(percentage = total_length/total_borough_length*100)

# Create table where each row is Borough level data by compliance
wider_borough_compliance_table = borough_compliance_table %>%
  select(-c("total_borough_length")) %>%
  units::drop_units() %>%
  pivot_wider(names_from = "seg_appro_speed_limit", values_from = c("total_length_rounded", "percentage", "total_length")) 

names(wider_borough_compliance_table)[3] = "total_length_rounded_Not_compliant"
names(wider_borough_compliance_table)[6] = "percentage_Not_compliant"
names(wider_borough_compliance_table)[9] = "total_length_Not_compliant"

# # Calculations for totals
#  sum(wider_borough_compliance_table$total_length_Compliant) # 196436.3
# sum(wider_borough_compliance_table$total_length_Not_compliant) # 565811.3
# sum(wider_borough_compliance_table$total_length_Unknown) # 197478
#  total_length = sum(wider_borough_compliance_table$total_length_Compliant) +
#    sum(wider_borough_compliance_table$total_length_Not_compliant) +
#    sum(wider_borough_compliance_table$total_length_Unknown)
# total_length # 959725.7
# per_compliant = sum(wider_borough_compliance_table$total_length_Compliant)/total_length *100 # 20.46797
# per_not_compliant = sum(wider_borough_compliance_table$total_length_Not_compliant)/total_length *100 # 58.95552
# per_unknown = sum(wider_borough_compliance_table$total_length_Unknown)/total_length *100 # 20.57651

wider_borough_compliance_table = wider_borough_compliance_table %>%
  mutate(percentage_Compliant = round(percentage_Compliant)) %>%
  mutate(percentage_Not_compliant = round(percentage_Not_compliant)) %>%
  mutate(percentage_Unknown = round(percentage_Unknown))

# Create results table
# Below creates new columns that we use to order the bars and label as Inner/Outer
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

wider_borough_compliance_table = wider_borough_compliance_table %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London"))
wider_borough_compliance_table$Compliant = paste(wider_borough_compliance_table$percentage_Compliant, "(", 
                                                  wider_borough_compliance_table$total_length_rounded_Compliant, ")")
wider_borough_compliance_table$Non_compliant = paste(wider_borough_compliance_table$percentage_Not_compliant, "(", 
                                                  wider_borough_compliance_table$total_length_rounded_Not_compliant, ")")
wider_borough_compliance_table$Unknown = paste(wider_borough_compliance_table$percentage_Unknown, "(", 
                                                     wider_borough_compliance_table$total_length_rounded_Unknown, ")")
# finalise table
final_borough_compliance_table = wider_borough_compliance_table %>%
  select(c(London, BOROUGH, Compliant, Non_compliant, Unknown))

# Save table
#write_csv2(final_borough_compliance_table, file = "output/summary_stats/summary_stats_borough_compliance_table.csv")

#####################################################
# Create maps of cycle lanes coloured by compliance #
#####################################################

# Map of lines  - overall
lines = ggplot()+
  geom_sf(data = lon_lad_2020_c2c, fill = "#F7F4F4",  colour = "black", alpha = 0.3, size = 0.06) +
  geom_sf(data = london_compliance_final, aes(fill = seg_appro_speed_limit, color = seg_appro_speed_limit),
          size = 0.9, alpha = 0.85, show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE, direction = -1) +  #this makes compliant purple and non compliant green
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

# Map of lines - facetted by type
london_compliance_final = london_compliance_final %>%
  mutate(type = fct_relevel(type, c("Shared", "Contraflow", "Rest")))

type_lines = ggplot()+
  geom_sf(data = lon_lad_2020_c2c, fill = "#F7F4F4",  colour = "black", alpha = 0.3, size = 0.06) +
  geom_sf(data = london_compliance_final, aes(fill = seg_appro_speed_limit, color = seg_appro_speed_limit),
          size = 0.9, alpha = 0.85, show.legend = TRUE) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  facet_wrap(~type) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, -0.1, 0.1, -0.1), "cm"),
        strip.background = element_rect(color = "white"),
        legend.position = "bottom", legend.direction = "vertical",
        legend.title = element_blank(),
        legend.key.size = unit(0.25, "cm")) +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)
ggsave("output/summary_stats/clt_compliance_type_lines.tiff", 
       plot = type_lines, dpi = 300, 
       width = 170, height = 80, units = "mm") 

# Create spatially positioned borough bar charts
map_fixed = borough_separation_length_spatial %>%
  ungroup() %>%
  mutate(total_length2 = total_length/max(total_length)) %>%  # This just scales the bars
  ggplot() +
  geom_rect(data=.%>% filter(seg_appro_speed_limit == "Unknown"), xmin = 0.5, xmax = 1.5, ymin = -0.2, ymax = 1.5, fill = "#cdcdcd") +
  geom_bar(aes(x = -seg_appro_speed_limit, y = total_length2, fill = seg_appro_speed_limit), stat = "identity", show.legend = TRUE) +
  geom_text(data=.%>% filter(seg_appro_speed_limit == "Unknown"), x = 1, y = 0.6, aes(label = b_acronym), size = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, labels = c("Compliance unknown",  "Not compliant", "Compliant")) +
  facet_grid(-fY ~ fX) +   # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(-0.2, "lines"),
        legend.position = "bottom", 
        aspect.ratio = 1) + # fixes aspect ratio so size of bars/shape remains the one I want
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = "") 

#######################################################
#  Potential compliance by Borough - ie drop unknowns #
#######################################################

# Create table of separation by length
borough_separation_table = borough_separation_length %>%
  arrange(seg_appro_speed_limit, desc(total_length)) %>%
  mutate(total_length_rounded = round(units::set_units(total_length, "km"), digits = 1))

# Create table where each row is Borough level data by separation
wider_borough_separation_table = borough_separation_table %>%
  select(-c("total_borough_length", "total_length_rounded")) %>%
  units::drop_units() %>%
  pivot_wider(names_from = "seg_appro_speed_limit", values_from = "total_length")
 
# Create variable for total length by Borough of only cycle lanes where we have a speed limit
total_borough_length_potential_compliance = borough_separation_length %>%
  filter(seg_appro_speed_limit != "Unknown") %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length_potential_compliance = sum(total_length))
 
# Join this total length to wider_borough_separation_table and drop 'unknown' column
known_sl_compliance = left_join(wider_borough_separation_table,total_borough_length_potential_compliance) %>%
  select(-c(Unknown)) %>%
  mutate(percentage_compliant = units::drop_units(Compliant/total_borough_length_potential_compliance*100)) %>%
  mutate(percentage_notcompliant = 100 - percentage_compliant)
# Table of compliance for cycle lanes with known speed limits
# BOROUGH   Compliant `Not compliant` total_borough_len… percentage_comp…
# <fct>         <dbl>           <dbl>                [m]            <dbl>
# 1 Hammersm…    21088.          10772.             31861.            66.2 
# 2 Camden       16017.          19771.             35788.            44.8 
# 3 Tower Ha…    14709.          14669.             29378.            50.1 
# 4 Southwark    14704.          25164.             39867.            36.9 
# 5 Lambeth      14556.          33851.             48407.            30.1 
# 6 Islington    13384.          17599.             30983.            43.2 
# 7 City of …    12215.           9241.             21456.            56.9 
# 8 Ealing       12181.          21016.             33197.            36.7 
# 9 Waltham …    11414.          26581.             37995.            30.0 
# 10 Westmins…     9146.          22323.             31469.            29.1 
# 11 Newham        6015.          22961.             28976.            20.8 
# 12 Lewisham      5231.          17822.             23053.            22.7 
# 13 Hounslow      4763.          20651.             25414.            18.7 
# 14 Haringey      4740.          17964.             22703.            20.9 
# 15 Hackney       4677.          24238.             28915.            16.2 
# 16 Croydon       4551.          51665.             56216.             8.10
# 17 Greenwich     3762.          21181.             24944.            15.1 
# 18 Kingston…     3377.          17644.             21021.            16.1 
# 19 Wandswor…     2979.          27511.             30490.             9.77
# 20 Barking …     2860.          11154.             14014.            20.4 
# 21 Bromley       2068.          14867.             16936.            12.2 
# 22 Richmond…     1693.          15130.             16823.            10.1 
# 23 Brent         1686.           7683.              9369.            18.0 
# 24 Bexley        1667.           5161.              6828.            24.4 
# 25 Enfield       1582.           9688.             11271.            14.0 
# 26 Merton        1392.          15732.             17124.             8.13
# 27 Havering      1139.           6550.              7689.            14.8 
# 28 Hillingd…      639.          12185.             12824.             4.98
# 29 Redbridge      569.          14927.             15496.             3.67
# 30 Kensingt…      504.           5630.              6134.             8.22
# 31 Barnet         403.           5673.              6077.             6.64
# 32 Harrow         373.          16657.             17030.             2.19
# 33 Sutton         352.           2150.              2502.            14.1 

# save this data
#readODS::write_ods(known_sl_compliance, "data/known_sl_london_compliance.ods")

# Manipulate data frame so can now visualise compliance where sl known
## Order Borough based on greatest percentage of compliance
borough_order = known_sl_compliance %>%
  arrange(desc(percentage_compliant)) %>%
  mutate(BOROUGH = as.character(BOROUGH))
B_order = pull(borough_order, BOROUGH)

# Below creates new columns that we use to order the bars and label as Inner/Outer
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

known_sl_compliance_order = known_sl_compliance %>%
  mutate(variable_order = factor(BOROUGH,
                                 levels = c("Hammersmith & Fulham", "City of London", "Tower Hamlets", "Camden", "Islington",
                                            "Southwark", "Ealing", "Lambeth", "Waltham Forest", "Westminster", "Bexley",
                                            "Lewisham", "Haringey", "Newham" ,"Barking & Dagenham", "Hounslow", "Brent", "Hackney",
                                            "Kingston upon Thames", "Greenwich", "Havering", "Sutton", "Enfield", "Bromley",
                                            "Richmond upon Thames", "Wandsworth", "Kensington & Chelsea", "Merton", "Croydon",
                                            "Barnet", "Hillingdon", "Redbridge", "Harrow"))) %>%
  mutate(variable_order = fct_rev(variable_order)) %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) %>%
  mutate(London = fct_rev(London))

known_sl_compliance_vis = known_sl_compliance_order %>%
  pivot_longer(cols = c("percentage_compliant","percentage_notcompliant"),
               names_to = "Compliance", 
               values_to = "Percentage") %>%
  mutate(Compliance = factor(Compliance, levels = c("percentage_notcompliant", "percentage_compliant"))) # gets in right order so compliance will be on left in bar chart

# Obtain mean % compliance 
compliance_mean = known_sl_compliance_vis %>%
  group_by(Compliance) %>%
  filter(Compliance == "percentage_compliant") %>%
  summarise(mean = mean(Percentage))
compliance_mean # 22.2%

# Create bar chart of compliance faceted by Inner/Outer
comp_vis = known_sl_compliance_vis %>%
  mutate(London = fct_rev(London)) %>%
  ggplot() +
  geom_bar(aes(x = Percentage, y = variable_order, fill = Compliance), stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#21908C", "#440154"), labels = c("Not compliant", "Compliant")) +
  labs(x = "% length") +
  scale_x_continuous(breaks = c(0, 50, 100)) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(), # removes y axis lines
        axis.text = element_text(size = 9, colour = "grey25"),
        axis.title.x = element_text(size = 9),
        strip.text.y = element_text(size = 9, colour = "grey25"),
        panel.spacing.x = unit(1, "lines")) +
  geom_vline(data = compliance_mean, aes(xintercept = mean), linetype = "solid") +
  facet_grid(scales = "free_y", space = "free_y", rows = vars(London), switch = "y") 



############################################# 
# Create visualisation for paper (figure 6) #
#############################################

# had to alter text sizes so that when combined look ok
#Option1) cowplot
maps = plot_grid(lines, map_fixed, 
                 ncol = 1, nrow = 2)
fig_6 = plot_grid(maps, comp_vis,
          ncol = 2, rel_widths = c(2,1.2))
ggsave("output/summary_stats/Figure_6.tiff", 
       plot = fig_6, dpi = 300, width = 190, height = 160, units = "mm", bg = "white")


# Option 2) - makes lots of white space around maps
library(patchwork)
ltn_comp_plot = ((lines/map_fixed)|comp_vis) + plot_layout(widths = c(4,1), )
ggsave("output/summary_stats/ltn_compliance.tiff", 
       plot = ltn_comp_plot, dpi = 300, width = 190, height = 160, units = "mm")




#############################################################################################
#############################################################################################

#############################################################################################
#  Potential compliance by Borough for nonshared cycle lanes  plus - ie drop unknowns       #
#############################################################################################

# Create variable for length by compliance by Borough for those non-shared cycle lanes
borough_separation_length_nonshared = london_compliance_final  %>%
  mutate(length_m = st_length(geometry)) %>%
  st_drop_geometry() %>%
  filter(type != "Shared") %>%
  group_by(BOROUGH, seg_appro_speed_limit) %>%
  summarise(total_length = sum(length_m))

# # Create variable for total length by Borough of all on road cycle lanes - may be needed for Results section
total_borough_separation_length_nonshared = london_compliance_final %>%
  mutate(length_m = st_length(geometry)) %>%
  st_drop_geometry() %>%
  filter(type != "Shared") %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length = sum(length_m))

# join total borough lengths to borough_separation_length
borough_separation_length_nonshared = left_join(borough_separation_length_nonshared, 
                                                total_borough_separation_length_nonshared)

# Create table of separation by length
borough_separation_table_nonshared = borough_separation_length_nonshared %>%
  arrange(seg_appro_speed_limit, desc(total_length)) %>%
  mutate(total_length_rounded = round(units::set_units(total_length, "km"), digits = 1))

# Create table where each row is Borough level data by separation
wider_borough_separation_table_nonshared = borough_separation_table_nonshared %>%
  select(-c("total_borough_length", "total_length_rounded")) %>%
  units::drop_units() %>%
  pivot_wider(names_from = "seg_appro_speed_limit", values_from = "total_length")

# Create variable for total length by Borough of only cycle lanes where we have a speed limit
total_borough_length_potential_compliance_nonshared = borough_separation_length_nonshared %>%
  filter(seg_appro_speed_limit != "Unknown") %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length_potential_compliance = sum(total_length))

# Join this total length to wider_borough_separation_table and drop 'unknown' column
known_sl_compliance_nonshared = left_join(wider_borough_separation_table_nonshared,total_borough_length_potential_compliance_nonshared) %>%
  select(-c(Unknown)) %>%
  mutate(percentage_compliant = units::drop_units(Compliant/total_borough_length_potential_compliance*100)) %>%
  mutate(percentage_notcompliant = 100 - percentage_compliant)
# Table of compliance for cycle lanes with known speed limits
# BOROUGH         Compliant `Not compliant` total_borough_length_potentia… percentage_compli… percentage_notcomp…
# <fct>               <dbl>           <dbl>                            [m]              <dbl>               <dbl>
# 1 Hammersmith & …    21030.           1461.                         22491.              93.5                 6.49
# 2 Camden             16011.           5903.                         21914.              73.1                26.9 
# 3 Lambeth            14556.          11232.                         25788.              56.4                43.6 
# 4 Tower Hamlets      14511.           4496.                         19007.              76.3                23.7 
# 5 Southwark          14201.           7984.                         22185.              64.0                36.0 
# 6 Islington          13384.           4402.                         17785.              75.3                24.7 
# 7 City of London     12215.           6676.                         18891.              64.7                35.3 
# 8 Ealing             12181.          12454.                         24635.              49.4                50.6 
# 9 Waltham Forest     11414.          23855.                         35269.              32.4                67.6 
# 10 Westminster         9146.          10229.                         19375.              47.2                52.8 
# 11 Newham              6015.          17683.                         23698.              25.4                74.6 
# 12 Lewisham            5231.           5086.                         10316.              50.7                49.3 
# 13 Hounslow            4763.          15804.                         20567.              23.2                76.8 
# 14 Haringey            4740.           8448.                         13187.              35.9                64.1 
# 15 Hackney             4635.          11024.                         15659.              29.6                70.4 
# 16 Croydon             4551.          46608.                         51159.               8.90               91.1 
# 17 Greenwich           3596.          15389.                         18985.              18.9                81.1 
# 18 Kingston upon …     3377.          15530.                         18907.              17.9                82.1 
# 19 Wandsworth          2918.          14937.                         17855.              16.3                83.7 
# 20 Barking & Dage…     2860.           9269.                         12129.              23.6                76.4 
# 21 Bromley             2068.          12642.                         14710.              14.1                85.9 
# 22 Richmond upon …     1693.          11874.                         13567.              12.5                87.5 
# 23 Brent               1686.           2714.                          4399.              38.3                61.7 
# 24 Bexley              1667.           5033.                          6701.              24.9                75.1 
# 25 Enfield             1582.           5256.                          6839.              23.1                76.9 
# 26 Merton              1392.          11365.                         12757.              10.9                89.1 
# 27 Havering            1038.           5921.                          6959.              14.9                85.1 
# 28 Redbridge            569.          14808.                         15377.               3.70               96.3 
# 29 Hillingdon           525.           8747.                          9271.               5.66               94.3 
# 30 Kensington & C…      504.           5423.                          5927.               8.51               91.5 
# 31 Barnet               403.           3778.                          4181.               9.65               90.4 
# 32 Harrow               373.          15090.                         15463.               2.41               97.6 
# 33 Sutton               352.           1823.                          2175.              16.2                83.8

# Manipulate data frame so can now visualise compliance where sl known
## Order Borough based on greatest percentage of compliance
borough_order_ns = known_sl_compliance_nonshared %>%
  arrange(desc(percentage_compliant)) %>%
  mutate(BOROUGH = as.character(BOROUGH))
B_order_ns = pull(borough_order_ns, BOROUGH)

# Below creates new columnsthat we use to order the bars and label as Inner/Outer
Inner = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith & Fulham", 
          "Islington", "Kensington & Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
          "Tower Hamlets", "Wandsworth", "Westminster") 

known_sl_compliance_ns_order = known_sl_compliance_nonshared %>%
  mutate(variable_order = factor(BOROUGH,
                                 levels = c("Hammersmith & Fulham", "Tower Hamlets", "Islington", "Camden", "City of London", 
                                            "Southwark", "Lambeth",  "Lewisham", "Ealing", "Westminster", "Brent", "Haringey", 
                                            "Waltham Forest", "Hackney", "Newham" , "Bexley", "Barking & Dagenham", 
                                            "Hounslow", "Enfield", "Greenwich", "Kingston upon Thames", "Wandsworth",
                                            "Sutton", "Havering", "Bromley", "Richmond upon Thames", "Merton", 
                                            "Barnet", "Croydon", "Kensington & Chelsea", "Hillingdon", "Redbridge", "Harrow"))) %>%                                           
  mutate(variable_order = fct_rev(variable_order)) %>%
  mutate(London = ifelse(BOROUGH %in% Inner, "Inner London", "Outer London")) %>%
  mutate(London = fct_rev(London))

known_sl_compliance_ns_vis = known_sl_compliance_ns_order %>%
  pivot_longer(cols = c("percentage_compliant","percentage_notcompliant"),
               names_to = "Compliance", 
               values_to = "Percentage") %>%
  mutate(Compliance = factor(Compliance, levels = c("percentage_notcompliant", "percentage_compliant"))) # gets in right order so compliance will be on left in bar chart

# Obtain mean % compliance 
compliance_ns_mean = known_sl_compliance_ns_vis %>%
  group_by(Compliance) %>%
  filter(Compliance == "percentage_compliant") %>%
  summarise(mean = mean(Percentage))
compliance_ns_mean # 32.3%


# Create bar chart of compliance faceted by Inner/Outer
figureA3 = known_sl_compliance_ns_vis %>%
  mutate(London = fct_rev(London)) %>%
  ggplot() +
  geom_bar(aes(x = Percentage, y = variable_order, fill = Compliance), stat = "identity", show.legend = TRUE) +
  scale_fill_manual(values = c("#21908C", "#440154"), labels = c("Not compliant", "Compliant"), guide = guide_legend(reverse = TRUE)) +
  labs(x = "% length") +
  scale_x_continuous(breaks = c(0, 50, 100)) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(), # removes y axis lines
        axis.text = element_text(size = 9, colour = "grey25"),
        axis.title.x = element_text(size = 9),
        strip.text.y = element_text(size = 9, colour = "grey25"),
        panel.spacing.x = unit(1, "lines"),
        legend.position = "bottom", legend.title = element_blank()) +
  geom_vline(data = compliance_ns_mean, aes(xintercept = mean), linetype = "solid") +
  facet_grid(scales = "free_y", space = "free_y", rows = vars(London), switch = "y") # saved as 525 x900 when legend excluded (600x9000 with legend)

# Save visualisation
figA3 = figureA3 + plot_spacer() +plot_layout(width = c(1, 0.1)) # have to add spacer as otherwise includes grey squares (!?!)
ggsave("output/summary_stats/fa3_ltn_compliance_exc_shared.tiff", 
       plot = figA3, dpi = 300, width = 90, height = 160, units = "mm")






