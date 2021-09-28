####################################################################################
#                            Visualise cycle lanes 
# 
# This code aims to visualise on road cycle lanes by the degree of separation
# from other road users.  
#
# Initial code sorts out the assets that have more than one label for separation
# e.g. jointly labelled as being segregated and stepped.  
# Also adds type so can see difference between shared, contraflow and other cycle lanes
# Visualises the lines on a borough map overall and then facetted by type.  


#Load packages
library(tidyverse)
library(mapview)
library(tmap)
library(sf)
library(tmaptools) # for palette explorer 
library(viridis)

# Package options
mapviewOptions(native.crs = TRUE)
tmap_design_mode(design.mode = FALSE)


#######################################################################
# Load and manipulate Local Authority spatial data for visualisations #
#######################################################################

# import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")

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

# Convert borough area into km^2 from m^2 
lon_lad_2020_c2c$Shape__Are = units::set_units(lon_lad_2020_c2c$Shape__Are, m^2)
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  mutate(Borough_Area_km2 = (units::set_units(lon_lad_2020_c2c$Shape__Are, km^2)))# change area units to km^2 from m^2

# Select variables of interest
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "b_acronym", "Borough_Area_km2", "geometry"))

# Create new dataset just containing Borough_Area
lon_area = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "Borough_Area_km2"))

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



##############################################################################
# Import cycle lane data and manipulate to enable analysis and visualisation #
##############################################################################

# Import Cycle Lanes and Tracks dataset 
c_cyclelanetrack = readRDS(file = "data/cleansed_cycle_lane_track")
# n = 25315

# Details of variables:
# CLT_CARR   on/off
# CLT_SEGREG lanes or tracks - TRUE = physical separation of cyclist from other users using continuous/near continuous kerb, upstand or planted verge.
# CLT_STEPP - onroad only, cycle lane/track is at a level between the carriageway and footway 
# CLT_PARSEG - on and off, on - involves objects eg wants, off is a visual feature eg white line
# TRUE = additional forms of delineation NB as often used by Mand/Adv cycle lanes can have this set to TRUE and mand/advi set to TRUE
# CLT_SHARED - on and off, on - TRUE = shared with bus lane, off - TRUE = shared with other users eg pedestrians
# CLT_MANDAT - onroad only, TRUE = mandatory cycle lane
# CLT_ADVIS - onroad only, TRUE = advisory cycle lane
# NB onroad can only have MAND TRUE, ADVIS TRUE or both FALSE

# Order of Protection from motor traffic on highways (DFT guidance pg 33)
# Fully kerbed > stepped > light segregation > Mandatory/Advisory
# FK/S/LS suitable for most people at 20/30 mph only FK suitable for 40mph+
# M/A only suitable for most poepl on 20mph roads with motor vehicle flow of <5000

# Correspond in CID to:
# CLT_SEGREG > CLT_STEPP > CLT_PARSEG > CLT_MANDAT > CLT_ADVIS
#  NB seems to be little difference in CID between SEGREG and STEPP - majority of 
# stepped are also labelled as segreg and only 5 are labelled as just stepped and they
# look very similar to those that are segreg in the photos

# Limit CID to on road infrastructure only 
on_road = c_cyclelanetrack %>%
  filter(CLT_CARR == TRUE) # n = 13965

# Create dataframe so can obtain a df summary of number of observations
# on_road_drop = on_road %>%
#   st_drop_geometry() %>%
#   select(-c("length_m", "length_km"))
#view(dfSummary(on_road_drop))

# seg = 1371
# stepped = 94
# part seg = 349
# manda = 1854
# advi = 7273
# shared = 2845
# NB total is 13965 - what are the rest of the on road cycle lanes????


# Create new variable that divides observations into shared, contraflow and rest as these seem to have different patterns of segregation
on_road = on_road %>%
  mutate(type = case_when(CLT_SHARED == TRUE ~ "Shared",
                          CLT_CONTRA == TRUE ~ "Contraflow",
                          TRUE ~ "Rest"))
on_road$type = factor(on_road$type, levels = c("Rest", "Shared", "Contraflow"))

on_road %>%
  st_drop_geometry() %>%
  group_by(type) %>%
  summarise(count = n(), sum_length = sum(length_km))
# type       count sum_length
# <fct>      <int>       [km]
# 1 Rest        9685       595.
# 2 Shared      2845       236.
# 3 Contraflow  1435       112.


# Convert Factors to numeric
# e.g.  CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
on_road_numeric = on_road %>%
  mutate(CLT_SEGREG_NUMERIC = as.numeric(on_road$CLT_SEGREG)) %>%
  mutate(CLT_STEPP_NUMERIC = as.numeric(on_road$CLT_STEPP))  %>%
  mutate(CLT_PARSEG_NUMERIC = as.numeric(on_road$CLT_PARSEG))  %>%
  mutate(CLT_MANDAT_NUMERIC = as.numeric(on_road$CLT_MANDAT))  %>%
  mutate(CLT_ADVIS_NUMERIC = as.numeric(on_road$CLT_ADVIS)) %>%
  mutate(CLT_SHARED_NUMERIC = as.numeric(on_road$CLT_SHARED)) %>%
  mutate(CLT_CONTRA_NUMERIC = as.numeric(on_road$CLT_CONTRA)) %>%
  mutate(CLT_PARKR_NUMERIC = as.numeric(on_road$CLT_PARKR))
  # converts all False to 1 and True to 2

# Convert 1(false) to 0 and 2(true) to 1
on_road_numeric$CLT_SEGREG_NUMERIC = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_STEPP_NUMERIC = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_PARSEG_NUMERIC = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_MANDAT_NUMERIC = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_ADVIS_NUMERIC = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 0, 1)

# Check now gives the count that I expect
sum(on_road_numeric$CLT_SEGREG_NUMERIC) # n = 1371
sum(on_road_numeric$CLT_STEPP_NUMERIC) # n = 94
sum(on_road_numeric$CLT_PARSEG_NUMERIC) # n = 349
sum(on_road_numeric$CLT_MANDAT_NUMERIC) # n = 1854
sum(on_road_numeric$CLT_ADVIS_NUMERIC) # n = 7273

# Recode to give weighted value with segregated weighted highest and advisory cycle lane weighted lowest
on_road_numeric$CLT_SEGREG_weight = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 10000, 0)
on_road_numeric$CLT_STEPP_weight = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 1000, 0)
on_road_numeric$CLT_PARSEG_weight = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 100, 0)
on_road_numeric$CLT_MANDAT_weight = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 10, 0)
on_road_numeric$CLT_ADVIS_weight = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 1, 0)

# Create new column with the sum of the weights for the 5 classes of separation
on_road_numeric = on_road_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(CLT_SEGREG_weight:CLT_ADVIS_weight)))

#unique(on_road_numeric$weight_5)
# 1    10   100 10000   110     0   101 11000  1001 10010  1000 10001

# on_road_numeric %>%
#   st_drop_geometry() %>%
#   group_by(weight_5) %>%
#   summarise(count = n())

#       weight_5 count
#           <dbl> <int>
# 1            0  3372  # none of the 5 categories - might be shared or contraf
# 2            1  7196  advisory cycle lane only
# 3           10  1672  mand cycle lane only
# 4          100   100  part segregated only
# 5          101    73  part seg + advisory
# 6          110   176  part seg + mand
# 7         1000     3  stepped only
# 8         1001     2  stepped + advisory 
# 9        10000  1274  segregated only
# 10       10001     2  segregated + advisory
# 11       10010     6  segregated + mandatory
# 12       11000    89  segregated + stepped


##  Create factored column where labelled by the 'highest' degree of separation
# Factor weight_5
on_road_factor = on_road_numeric %>%
  mutate(Highest_separation = factor(weight_5))
rm(on_road_numeric) # remove this dataframe to avoid confusion

# Convert factored numbers to relevant labels
on_road_factor = on_road_factor %>%
  mutate(Highest_separation = fct_collapse(Highest_separation, 
           "Full segregation" = c("10000","10001","10010", "11000"),
           "Stepped" = c("1000", "1001"),
           "Part segregation" = c("100", "101", "110"),
           "Mandatory cycle lane" = c("10"),
           "Advisory cycle lane" = c("1"),
           "No separation" = c("0")))

# Relevel order of factors in the degree of separation
on_road_factor = on_road_factor %>%
  mutate(Highest_separation = fct_relevel(Highest_separation, 
                                           c("Full segregation", "Stepped", "Part segregation",
                                             "Mandatory cycle lane", "Advisory cycle lane",
                                             "No separation")))
# # Check to see works ok
table = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(count = n(), sum_length = sum(length_km)) %>%
  mutate(proportion = round((sum_length/sum(sum_length) *100), digit = 1))
sum(table$sum_length) # 944.0157 [km]

# Highest_separation   count sum_length proportion
# <fct>                <int>       [km]        [1]
# 1 Full segregation      1371     39.2          4.1
# 2 Stepped                  5      0.713        0.1
# 3 Part segregation       349     15.7          1.7
# 4 Mandatory cycle lane  1672     85.3          9  
# 5 Advisory cycle lane   7196    487.          51.6
# 6 No separation         3372    316.          33.5


# Create datasets by type
contra = on_road_factor %>%
  filter(type == "Contraflow") # n = 1435
shared = on_road_factor %>%
  filter(type == "Shared") # n = 2845
rest = on_road_factor %>%
  filter(type == "Rest") # n= 9685

# create df of degrees of separation by type
rest_sep = rest %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(sum_r_length = sum(length_km)) %>%
  mutate(proportion_r = round((sum_r_length/sum(sum_r_length) *100), digit = 1))
contra_sep = contra %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(sum_c_length = sum(length_km)) %>%
  mutate(proportion_c = round((sum_c_length/sum(sum_c_length) *100), digit = 1))
shared_sep = shared %>%
  st_drop_geometry() %>%
  group_by(Highest_separation) %>%
  summarise(sum_s_length = sum(length_km)) %>%
  mutate(proportion_s = round((sum_s_length/sum(sum_s_length) *100), digit = 1))
summary_high_sep = left_join(rest_sep, contra_sep) %>%
  left_join(shared_sep) %>%
  units::drop_units()
summary_high_sep[is.na(summary_high_sep)] <- 0

total_contra_length = round(sum(summary_high_sep$sum_c_length), digit = 1)
total_shared_length = round(sum(summary_high_sep$sum_s_length), digit = 1)

# Highest_separation   rest_count sum_rest_length_km contra_count sum_contra_length_km shared_count sum_shared_length_km
# <fct>                     <int>              <dbl>        <int>                <dbl>        <int>                <dbl>
# 1 Segregated                  976             33.2            393                 5.75            2                0.268
# 2 Stepped                       5              0.713            0                 0               0                0    
# 3 Part-segregated             273             12.2             72                 3.25            4                0.288
# 4 Mandatory cycle lane       1501             74.4            165                 9.93            6                0.924
# 5 Advisory cycle lane        6877            464.             283                20.8            36                2.48 
# 6 No separation                53             11.0            522                72.7          2797              232.   

rm(rest, rest_sep, shared, shared_sep, contra, contra_sep) # remove redundant objects

# Save onroad cycle lane dataset for use with LTN 1/20 analysis
#saveRDS(on_road_factor, file = "data/cleansed_onroad_cyclelanes_segregation.Rds")

##########################################################################
# Create maps of cycle lanes coloured by degrees of separation #
##########################################################################
on_road_factor = readRDS(file = "data/cleansed_onroad_cyclelanes_segregation.Rds")

# Separation map
lines = ggplot()+
  geom_sf(data = lon_lad_2020_c2c, fill = "#F7F4F4",  colour = "black", alpha = 0.3, size = 0.06) +
  geom_sf(data = on_road_factor, aes(fill = Highest_separation, color = Highest_separation),
          size = 0.9, alpha = 0.85, show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE, direction = 1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

# tm_shape(lon_lad_2020_c2c) +
#   tm_polygons(col = "gray98", border.col = "gray70") +
#   tm_shape(on_road_factor) +
#   tm_lines("Highest_separation",
#            palette = "viridis",
#            lwd = 2.5, 
#            alpha = 0.75, legend.col.show = FALSE) +
#   tm_layout(frame = FALSE)

# Plot facetted by type
on_road_factor = on_road_factor %>%
  mutate(type = fct_relevel(type, c("Shared", "Contraflow", "Rest")))
type_lines = ggplot()+
  geom_sf(data = lon_lad_2020_c2c, fill = "#F7F4F4",  colour = "black", alpha = 0.3, size = 0.06) +
  geom_sf(data = on_road_factor, aes(fill = Highest_separation, color = Highest_separation),
          size = 0.9, alpha = 0.85, show.legend = TRUE) +
  scale_color_viridis(discrete = TRUE, direction = 1) +
  facet_wrap(~type) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, -0.1, 0.1, -0.1), "cm"),
        strip.background = element_rect(color = "white"),
        legend.position = "bottom", legend.direction = "vertical",
        legend.title = element_blank(),
        legend.key.size = unit(0.25, "cm")) +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)


####################################################################
# Generating borough level data on lengths by degree of separation #
# and visualisations                                               #
#                                                                  #
####################################################################

# Create variable for length by Highest Separation by Borough
borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH, Highest_separation) %>%
  summarise(total_length = sum(length_m))

# # Create variable for total length by Borough of all on road cycle lanes - may be needed for Results section
total_borough_separation_length = on_road_factor %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(total_borough_length = sum(length_m))# max = Croydon 58331.287 [m], min = Sutton 5832.761 [m]

# join total borough lengths to borough_separation_length
borough_separation_length = left_join(borough_separation_length, total_borough_separation_length)

# Reorder factors so Segregated is 'highest' value
borough_separation_length_reorder <- borough_separation_length # reorder so that Segregated appears at top
borough_separation_length_reorder$Highest_separation = fct_rev(borough_separation_length_reorder$Highest_separation)

# Join london_squared and borough area to cycle lane lengths
borough_separation_length_spatial = left_join(borough_separation_length_reorder, london_squared_tidy, by = "BOROUGH") %>%
  left_join(lon_area, by = "BOROUGH")

# Create new variables that divide length by area
borough_separation_length_spatial = borough_separation_length_spatial %>%
  mutate(tl_km = units::set_units(total_length, "km"),
         tlperkm2 = tl_km/Borough_Area_km2,
         tbl_km = units::set_units(total_borough_length, "km"),
         tblperkm2 = tbl_km/Borough_Area_km2)

# remove units to allow plotting
borough_separation_length_spatial = units::drop_units(borough_separation_length_spatial)


## need to recreate maps with larger fonts
library(cowplot)
map3 = borough_separation_length_spatial %>%
  ungroup() %>%
  mutate(total_length2 = total_length/max(total_length)) %>%  # This just scales the bars
  ggplot() +
  geom_rect(data=.%>% filter(Highest_separation == "No separation"), xmin = 0.5, xmax = 1.5, ymin = -0.2, ymax = 1.5, fill = "#cdcdcd") +
  geom_bar(aes(x = -Highest_separation, y = total_length2, fill = Highest_separation), stat = "identity", show.legend = FALSE) +
  geom_text(data=.%>% filter(Highest_separation == "No separation"), x = 1, y = 0.7, aes(label = b_acronym), size = 3) + #text size specified for cowplot
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(-0.2, "lines"))

# Create degree of separation by Borough - standardised by Borough area - saved as 500x370
map4 = borough_separation_length_spatial %>%
  ungroup() %>%
  mutate(total_length2 = tlperkm2/max(tlperkm2)) %>%  # This just scales the bars
  ggplot() +
  geom_rect(data=.%>% filter(Highest_separation == "No separation"), xmin = 0.5, xmax = 1.5, ymin = -0.2, ymax = 2.4, fill = "#cdcdcd") +
  geom_bar(aes(x = -Highest_separation, y = total_length2, fill = Highest_separation), stat = "identity", show.legend = FALSE) +
  geom_text(data=.%>% filter(Highest_separation == "No separation"), x = 1, y = 1.2, aes(label = b_acronym), size = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(-0.2, "lines"),
        plot.margin = unit(c(0,0,0,0.4), "cm"))

# # Obtain legend for use in paper
legend_cowplot = borough_separation_length_spatial %>%
  ungroup() %>%
  mutate(total_length2 = total_length/max(total_length)) %>%
  ggplot() +
  geom_rect(data=.%>% filter(Highest_separation == "No separation"), xmin = 0.5, xmax = 1.5, ymin = -0.2, ymax = 1.5, fill = "#cdcdcd") +
  geom_bar(aes(x = -Highest_separation, y = total_length2, fill = Highest_separation), stat = "identity") +
  geom_text(data=.%>% filter(Highest_separation == "No separation"), x = 1, y = 0.7, aes(label = b_acronym)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE), name = "") +
  facet_grid(-fY ~ fX) +  # need to do -fY to get correct orientation with enfield top row and sutton bottom row
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(-0.2, "lines"))

# extract and convert to ggplot (so can save as jpeg)
legend_cp = ggpubr::get_legend(legend_cowplot)
legend_cp = ggpubr::as_ggplot(legend_cp)

plot_grid(lines, map3, legend_cp, map4,
          ncol = 2, nrow = 2)
fig_5 = plot_grid(lines, map3, legend_cp, map4,
                  ncol = 2, nrow = 2)



