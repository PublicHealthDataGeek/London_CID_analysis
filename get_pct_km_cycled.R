#################################################################################
# Get PCT cycling data                                                          #
#                                                                               #
# This code obtains the estimated amount of commuting cycling on route network  #
# segments in London using the Census 2011 commuting data.                     #
# It is upscaled from metre to kilometre.                                       #
# There is also Borough level estimates where the amount of cycling on the      #
# segments that fall within each London Borough is aggregated.                  #
#                                                                               #
# Created 19/3/20                                                               #
#################################################################################


#load packages
library(tidyverse)
library(pct)
library(sf)
library(mapview)
mapviewOptions(native.crs = TRUE, legend = FALSE) # set mapview options so that matches crs

# import May 2020 ONS LA boundary data full extent of realmn
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create Outer London Spatial object for boundary
out_lon_union = st_union(lon_lad_2020)

# Get route network for London from PCT
lon_rnet = pct::get_pct_rnet(region = "london") # n = 74624

# Change CRS to ONS CRS
st_crs(lon_rnet) # QGS 84, ePSG 4326
lon_rnet = st_transform(lon_rnet, crs=27700) 
# st_crs(rnet) # PROJCRS["OSGB 1936 / British National Grid",

names(lon_rnet)
# [1] "local_id"       "bicycle"        "govtarget_slc" 
# [4] "govnearmkt_slc" "gendereq_slc"   "dutch_slc"     
# [7] "ebike_slc"      "geometry" 

# Visualise pct data with London Borough boundaries -> some PCT data is outside London Boroughs
mapview(lon_rnet, color = "red") + mapview(lon_lad_2020, zcol = "BOROUGH")


#  Split route network by London Boroughs boundaries into segments
lon_rnet_intersection = st_intersection(lon_lad_2020, lon_rnet) # n = 71494 
mapview(lon_rnet_intersection, color = "red") + mapview(lon_lad_2020, zcol = "BOROUGH") # now data is confined to London Boroughs
# names(lon_rnet_intersection)
# [1] "BOROUGH"        "objectid"       "lad20cd"        "lad20nmw"      
# [5] "bng_e"          "bng_n"          "long"           "lat"           
# [9] "st_areasha"     "st_lengths"     "local_id"       "bicycle"       
# [13] "govtarget_slc"  "govnearmkt_slc" "gendereq_slc"   "dutch_slc"     
# [17] "ebike_slc"      "segment_length" "geometry"      


###################################################################################
# Manage segments that cross Borough boundaries in order to assign the correct    #
# amount of cycling along each segment to the Borough in which the segment exists #
###################################################################################

# Find newly created segments that cross Borough boundaries by identifying 
# local_id with more than one observation
# (1 observation with 1 local_id means it doesnt cross a Borough boundary)
lon_rnet_intersection %>%
  st_drop_geometry() %>%
  group_by(local_id) %>%
  summarise(num_obs = n())%>%
  group_by(num_obs) %>%
  filter(num_obs >= 2) %>%
  summarise(num_groups = n())

# num_obs num_groups
#  2       1600  ie 1600 have two segments
#  3         11 have 3 segments

# Create list of local_ids that have 2 or more segments
multi_feature_id_list = lon_rnet_intersection %>%
  st_drop_geometry() %>%
  group_by(local_id) %>%
  summarise(num_obs = n()) %>%
  group_by(num_obs) %>%
  filter(num_obs >= 2) %>%
  ungroup() %>%
  pull(local_id)  # n = 1611

# Create dataset containing the observations with these local_ids
new_segments = lon_rnet_intersection %>%
  filter(local_id %in% multi_feature_id_list) # n = 3233
# 1600 + 1600 (these segmented into two) 
# 11 + 11 + 11 (these segmented into 3

# Visualise these new segments (coloured by Borough) - can see they are all occurring cross boundary
mapview(new_segments, zcol = "BOROUGH") + 
  mapview(lon_lad_2020, alpha.regions = 0.1, zcol = "BOROUGH", legend = FALSE, lwd = 1)

# Update segment length by inserting new segment length into that column
new_segments$segment_length = as.numeric(sf::st_length(new_segments))
sum(new_segments$segment_length) # = 437975.4m (n = 3233) - ie matches that of the original segments with these local_ids 
# (see below for validation code)

# # # # Validate this against  the total length of these rnet before intersection
# only_lon_rnet = st_intersection(lon_rnet, out_lon_union) # Limit PCT data to Outer London Borough Boundaries, n = 69872 
# x = only_lon_rnet %>%
#   filter(local_id %in% multi_feature_id_list) # limit to those obs that cross boundaries, n = 1611
# x$segment_length = as.numeric(sf::st_length(x))
# sum(x$segment_length) # total length of the new segments should be 437975.4

# Calculate metres cycled per working day for the new segments
new_segments$m_cycled_per_working_day = new_segments$segment_length * new_segments$bicycle


####################################################
# Create df of segments that dont cross boundaries #
####################################################

# Create dataset containing the observations that dont have multiple id's
old_segments = lon_rnet_intersection %>%
  filter(!local_id %in% multi_feature_id_list) # n = 68261 (68261 +3233 = 71494)

# Calculate segment length
old_segments$segment_length = as.numeric(sf::st_length(old_segments))

# Calculate metres cycled per working day for the new segments
old_segments$m_cycled_per_working_day = old_segments$segment_length * old_segments$bicycle 


#########################################################
# Create new final dataframe and get Borough level data #
#########################################################

# Join old_segments to new_segments to get full London Borough dataset 
lon_rnet_pct_cycling_data = rbind(old_segments, new_segments)
#sum(lon_rnet_pct_cycling_data$segment_length)
#[1] 10369759

# Obtain kms cycled for commuting per year (200 = number of working days)
lon_rnet_pct_cycling_data$km_cycled_for_commuting_per_year_estimated = lon_rnet_pct_cycling_data$m_cycled_per_working_day * 
  2 * 200 / # estimate of trips days per year, morning and afternoon
  1000 # to get km

# Create dataset of Boroughs and estimated cycling through the borough
Borough_commuting = lon_rnet_pct_cycling_data %>%
  st_drop_geometry() %>%
  select(c(BOROUGH, km_cycled_for_commuting_per_year_estimated)) %>%
  group_by(BOROUGH) %>%
  summarise(total_km_cycled_for_commuting_per_year_estimated = sum(km_cycled_for_commuting_per_year_estimated))

############
# Save RDS #
############
# saveRDS(Borough_commuting, file = "data/Borough_commuting.rds")
# saveRDS(lon_rnet_pct_cycling_data, "data/lon_rnet_pct_cycling.rds")





####################################################################################
# Methodological comparison - results obtained using st_centroid v st_intersection #
####################################################################################


### Compare with st_centroid
# only_lon_rnet = st_intersection(lon_rnet, out_lon_union) # Limit PCT data to Outer London Borough Boundaries
cen_test = only_lon_rnet
cen_test$segment_length = as.numeric(sf::st_length(cen_test)) # calculate segment lengths
cen_test$m_cycled_per_working_day = cen_test$segment_length * cen_test$bicycle # calculate m_cycled
cen_test = st_centroid(cen_test) # convert geometry to centroid

sum(cen_test$segment_length) # 10369759 matches the sum of the lon_rnet_pct_cycling_data 

cycle_m_per_borough = aggregate(cen_test["m_cycled_per_working_day"], lon_lad_2020, FUN = sum) # aggregate to borough (borough names are dropped)

lon_lad_2020$km_cycled_for_commuting_per_year_estimated = cycle_m_per_borough$m_cycled_per_working_day * 
  2 * 200 / # estimate of trips days per year, morning and afternoon
  1000 # to get km  ( and put in lon_lad_2020 so linked back to actual boroughs)

lon_lad_2020 = lon_lad_2020 %>%
  st_drop_geometry() %>%
  select(c("BOROUGH", "km_cycled_for_commuting_per_year_estimated")) %>%
  rename("CENTROIDkm_cycled_for_commuting_per_year_estimated" = "km_cycled_for_commuting_per_year_estimated")

lon_lad_2020 = lon_lad_2020 %>%
  rename("CENTROIDkm_cycled_for_commuting_per_year_estimated" = "km_cycled_for_commuting_per_year_estimated")

comparison = left_join(lon_lad_2020, Borough_commuting)
comparison = comparison %>%
  rename("INTERSECTIONkm_cycled_for_commuting_per_year_estimated" = "total_km_cycled_for_commuting_per_year_estimated")
comparison$difference = round(comparison$total_km_cycled_for_commuting_per_year_estimated - 
  comparison$CENTROIDkm_cycled_for_commuting_per_year_estimated)
sum(comparison$INTERSECTIONkm_cycled_for_commuting_per_year_estimated) # = 166777094
sum(comparison$CENTROIDkm_cycled_for_commuting_per_year_estimated) # = 166777094

sum(comparison$difference)
comparison$per_difference = round(comparison$difference/comparison$total_km_cycled_for_commuting_per_year_estimated *100)

ggplot(comparison) +
  geom_col(aes(x = BOROUGH, y = reorder(per_difference))) +
  lab

centroid_intersection_comparision_plot = ggplot(comparison) +
  geom_col(aes(reorder(BOROUGH, per_difference), per_difference)) +
  coord_flip() +
  labs(y = "Percentage difference", x = "Borough", title = "Percentage difference in kilometres cycled per year when calculated by st_centroid and st_intersection", 
       subtitle =  "Negative percentage means that st_centroid overestimated when compared to st_intersection")
ggsave(centroid_intersection_comparision_plot, filename = "centroid_intersection_comparision_plot.png",
       path = "output/", height = 6.14, width = 10, units = "in")

########################################################################
# Code development - managing new segments - using a smaller dataset #
########################################################################

# Developing code for the new segments on a smaller dataset
test_ids = c(74621, 74611, 40899) # these local_ids have 2 or 3 segments
test_df = new_segments %>%
  filter(local_id %in% test_ids) # n = 7

# need to create new segment_length
test_df$segment_length = as.numeric(sf::st_length(test_df))

# calculate m cycled per working day
test_df$m_cycled_per_working_day = round(test_df$segment_length * test_df$bicycle)



