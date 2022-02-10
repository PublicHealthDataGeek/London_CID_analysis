################################################################################
# Create maps of Safety assets
#
# This script creates maps of all the safety assets plus an orientation map and 
# a separate legend 
# 

# Code rerun on 24/01/2022 to incorporate changes requested by R1 - namely to 
# remove motorways from map. 

# install packages
library(tidyverse)
library(sf)
library(ggspatial) # get north arrow and bar


# color options
# https://www.pagetutor.com/common/bgcolors1536.png


# Load asset datasets - these datasets were originally downloaded from TFL 25th February 2021
# New version of the datasets were created on 24/01/2022 due to issue with gdal libraries 
# affecting spatial data - NB no difference between data from 24/01/2022 and 25/02/2021
c_asl = readRDS(file = "data/cleansed_asl_24_01_2022")
c_crossings = readRDS(file = "data/cleansed_crossings_24_01_2022")
c_cyclelanetrack = readRDS(file = "data/cleansed_cycle_lane_track_24_01_2022")
c_signals = readRDS(file = "data/cleansed_signals_24_01_2022")
c_trafficcalming = readRDS(file = "data/cleansed_trafficcalming_24_01_2022")

# convert certain assets to point data
c_asl_point = st_centroid(c_asl)
c_crossings_point = st_centroid(c_crossings)
c_signals_point = st_centroid(c_signals)
c_trafficcalming = st_centroid(c_trafficcalming)


# Create context for map

# 1) Borough boundaries and labelling
boroughs <- st_read("map_data/London_Borough_Excluding_MHW.shp")
boroughs = st_transform(boroughs, 27700)
borough_areas <- rmapshaper::ms_simplify(boroughs, keep=0.015) #Simplify boroughs
borough_areas = rename(boroughs, BOROUGH = NAME)
borough_areas$b_acronym = fct_recode(borough_areas$BOROUGH, 
                                      "K&C" = "Kensington and Chelsea",
                                      "B&D" = "Barking and Dagenham",
                                      "H&F" = "Hammersmith and Fulham",
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


# Create Inner London Borough list
inn_lon_B_list = c("City of London", "Camden", "Greenwich", "Hackney", "Hammersmith and Fulham", 
                   "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", "Newham", "Southwark",  
                   "Tower Hamlets", "Wandsworth", "Westminster")
# Create inner London spatial object for boundary
inn_lon_union = borough_areas %>%
  filter(BOROUGH %in% inn_lon_B_list) %>%
  st_union()
# Create Outer London Spatial object for boundary
out_lon_union = borough_areas %>%
  filter(!BOROUGH %in% inn_lon_B_list) %>%
  st_union()

# 2) River thames
riverthames = st_read("map_data/riverthames.shp")
st_crs(riverthames)
riverthames_simplify = rmapshaper::ms_simplify(riverthames)

# # 3) Motorways
# motorways <- st_read("map_data/motorways_outer.json") %>% 
#   st_transform(crs=27700) 
# box_new = c(xmin = 498745.5, ymin = 149044.6, xmax = 564000.0, ymax = 205391.0)
# motorways = st_crop(motorways, box_new)
# #box_orig = c(xmin = 498745.5, ymin = 149044.6, xmax = 569602.4, ymax = 205391.0)

# 4) Create map so can copy and paste legend
legend_plot = 
  ggplot()+
  geom_sf(data = out_lon_union, aes(colour = "myline1"), fill="white", show.legend = "line") +
  #geom_sf(data = motorways, aes(colour = "myline3"), size = 0.14, show.legend = "line") +
  geom_sf(data = inn_lon_union, aes(colour = "myline2"), show.legend = "line") +
  geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15)+
  geom_sf(data = riverthames_simplify, aes(colour = "myline4"), fill="#99CCEE",  show.legend = "line")+
  geom_sf_label(data = borough_areas, aes(label = b_acronym), label.padding = unit(0.15, "lines")) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial"),
        legend.title = element_blank(),
        legend.text = element_text(size = 7)) + 
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  ggtitle("a) Geographical features of London *") +
  xlab(label = NULL) +
  ylab(label = NULL) +
  scale_colour_manual(values = c(myline1 = "black", myline2 = "#991100", myline4 = "#99CCEE"),
                      labels = c("Outer London boundary", "Inner London boundary", "River Thames")) +
  # scale_colour_manual(values = c(myline3 = "#b77107", myline1 = "black", myline2 = "#991100", myline4 = "#99CCEE"),
  #                     labels = c("Motorways", "Outer London boundary", "Inner London boundary", "River Thames")) +
  annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
                   text_cex = 0.65, line_width = 0.5, line_col = "#222222",
                   text_family = "Arial") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1.3, "cm"), width = unit(1.3, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 0.5, 
                                                                line_col = "#222222",
                                                                fill = c("white", "Gray83"),
                                                                text_size = 8, text_family = "Arial"))

# extract and convert to ggplot (so can save as jpeg)
legend = ggpubr::get_legend(legend_plot)
legend = ggpubr::as_ggplot(legend)
                  
        
                
# 5) Create 6 panel maps in black 
# Create Orientation map
b0 = ggplot()+
  geom_sf(data = out_lon_union, colour = "black", fill="white") +
 # geom_sf(data = motorways, colour = "#b77107", size = 0.14) +
  geom_sf(data = inn_lon_union, colour = "#991100") +
  geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15) +
  geom_sf(data = riverthames_simplify, colour = "#99CCEE", fill="#99CCEE") +
  geom_sf_label(data = borough_areas, aes(label = b_acronym), label.padding = unit(0.15, "lines"),
                label.size = 0.1, size = 2) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
  ggtitle("Key") +
  xlab(label = NULL) +
  ylab(label = NULL) +
  annotation_scale(location = "bl", width_hint = 0.3, bar_cols = c("Gray83", "white"),
                   text_cex = 0.5, line_width = 0.5, line_col = "#222222",
                   text_family = "Arial") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(0.65, "cm"), width = unit(0.65, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 0.5,
                                                                line_col = "#222222",
                                                                fill = c("white", "Gray83"),
                                                                text_size = 6, text_family = "Arial"))

b1 = ggplot()+
#  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_asl_point, colour = alpha("black", 0.2), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  ggtitle("ASL") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

b2 = ggplot()+
#  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_crossings_point, colour = alpha("black", 0.2), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  ggtitle("Cycle crossings") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

b3 = ggplot()+
#  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack %>% filter(CLT_CARR == TRUE), colour = alpha("black", 0.3)) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  ggtitle("On-road cycle lanes") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

b4 = ggplot()+
#  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_cyclelanetrack %>% filter(CLT_CARR == FALSE), colour = alpha("black", 0.2)) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  ggtitle("Off-road cycle tracks") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

b5 = ggplot()+
#  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#444444", alpha=0.3, size=0.05)+
  geom_sf(data=riverthames_simplify, fill="#99CCEE",  colour="#99CCEE") +
  geom_sf(data = c_trafficcalming, colour = alpha("black", 0.05), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  ggtitle("Physical traffic calming") +
  coord_sf(crs=st_crs(riverthames_simplify), datum=NA)

b6 = ggplot()+
#  geom_sf(data = motorways, fill = "#EEEEEE",  colour = "#EEEEEE") +
  geom_sf(data = borough_areas, fill = "#d4d4d4",  colour = "#444444", alpha = 0.3, size = 0.05) +
  geom_sf(data = riverthames_simplify, fill = "#99CCEE",  colour = "#99CCEE") +
  geom_sf(data = c_signals_point, colour = alpha("black", 0.2), size = 0.1) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        text = element_text(family = "Arial")) +
  ggtitle("Cycle signals") +
  coord_sf(crs = st_crs(riverthames_simplify), datum = NA)

# Arranging plots
# 1) Cowplot

# cowplot::plot_grid(b1, b2, b6,
#                    b3, b4, b5, 
#                    b0, NULL, NULL)
# saved size 1100x960 from within rstudio plot
# But have issues with grey between the plots when open it as a tiff

# 2) Patchwork
library(patchwork)
all_assets = b1 + b2 + b6 + b3 + b4 + b5 + b0 + legend + plot_layout((ncol = 3))  # YES

ggsave("output/summary_stats/JTG_sub2/all_assets.jpeg", plot = all_assets, dpi = 300, 
       width = 190, height = 170, units = "mm", bg = "white") # -> 426kb
# 170 seems to be right height when motorways removed
ggsave("output/summary_stats/JTG_sub2/all_assets_final.tiff", plot = all_assets, dpi = 300,
      width = 190, height = 170, units = "mm", bg = "white") # ->13.5MB


# Size of 190/190 is correct size for when motorways included.
#ggsave("output/summary_stats/JTG_sub2/all_assets_final.tiff", plot = all_assets, dpi = 300, 
#       width = 190, height = 190, units = "mm", bg = "white") # ->15.1MB
# NB EPS doesnt show the CID assets - just the lines
# ggsave("output/summary_stats/JTG_sub2/all_assets_final.jpeg", plot = all_assets, dpi = 300, 
#        width = 190, height = 190, units = "mm", bg = "white") # -> 431.4kb


### b0 for original plot with motorways in. 
# b0 = ggplot()+
#   geom_sf(data = out_lon_union, colour = "black", fill="white") +
#   # geom_sf(data = motorways, colour = "#b77107", size = 0.14) +
#   geom_sf(data = inn_lon_union, colour = "#991100") +
#   geom_sf(data = borough_areas, fill="#d4d4d4",  colour="black", alpha=0.3, size=0.15) +
#   geom_sf(data = riverthames_simplify, colour = "#99CCEE", fill="#99CCEE") +
#   geom_sf_label(data = borough_areas, aes(label = b_acronym), label.padding = unit(0.15, "lines"),
#                 label.size = 0.1, size = 2) +
#   theme_classic() +
#   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
#         text = element_text(family = "Arial")) +
#   coord_sf(crs=st_crs(riverthames_simplify), datum=NA) +
#   ggtitle("Key") +
#   xlab(label = NULL) +
#   ylab(label = NULL) +
#   annotation_scale(location = "br", width_hint = 0.3, bar_cols = c("Gray83", "white"),
#                   line_col = "#222222", line_width = 0.5,
#                    text_family = "Arial", text_cex = 0.55) +
#   annotation_north_arrow(location = "tr", which_north = "true",
#                          height = unit(0.8, "cm"), width = unit(0.8, "cm"),
#                          style = north_arrow_fancy_orienteering(line_width = 0.5,
#                                                                 line_col = "#222222",
#                                                                 fill = c("white", "Gray83"),
#                                                                 text_size = 6, text_family = "Arial"))
