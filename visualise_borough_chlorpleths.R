##########################################################################################
#           Visualising borough counts/lengths by chloropleth and bar chart              #
#                                                                                        # 
# Code development started on 7/4/21                                                     #
#                                                                                        #
# This code generates chlorpleths with bar charts showing the count of assets by Borough #
##########################################################################################


# Load packages
library(tidyverse)
library(sf)
library(cowplot)
library(ggpubr)
#library(patchwork)


################################
# Load and manipulate datasets #
################################

# 1) Local Authority spatial data

# import May 2020 ONS LA boundary data clipped to coastline (used so that River Thames appears)
lon_lad_2020_c2c = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFC.Rds")

# simply borough shapes
lon_lad_2020_c2c <- rmapshaper::ms_simplify(lon_lad_2020_c2c, keep=0.015) #Simplify boroughs

# Create new variable that labels the Boroughs by number (matches the overall map)
lon_lad_2020_c2c$Borough_number = fct_recode(lon_lad_2020_c2c$BOROUGH, 
                                      "7" = "Kensington & Chelsea",
                                      "32" = "Barking & Dagenham",
                                      "8" = "Hammersmith & Fulham",
                                      "25" = "Kingston upon Thames",
                                      "24" = "Richmond upon Thames",
                                      "1" = "City of London",
                                      "15" = "Waltham Forest",
                                      "28" = "Croydon",
                                      "29" = "Bromley",
                                      "23" = "Hounslow",
                                      "20" = "Ealing",
                                      "31" = "Havering",
                                      "22" = "Hillingdon",
                                      "21" = "Harrow",
                                      "19" = "Brent",
                                      "18" = "Barnet",
                                      "10" = "Lambeth",
                                      "11" = "Southwark", 
                                      "12" = "Lewisham",
                                      "13" = "Greenwich",
                                      "30" = "Bexley",
                                      "17" = "Enfield",
                                      "33" = "Redbridge",
                                      "27" = "Sutton",
                                      "26" = "Merton",
                                      "9" = "Wandsworth",
                                      "6" = "Westminster",
                                      "5" = "Camden",
                                      "2" = "Tower Hamlets",
                                      "4" = "Islington",
                                      "3" = "Hackney",
                                      "16" = "Haringey",
                                      "14" = "Newham")

# Convert borough area into km^2 from m^2 
lon_lad_2020_c2c$Shape__Are = units::set_units(lon_lad_2020_c2c$Shape__Are, m^2)
lon_lad_2020_c2c = lon_lad_2020_c2c %>%
  mutate(Borough_Area_km2 = (units::set_units(lon_lad_2020_c2c$Shape__Are, km^2)))# change area units to km^2 from m^2


# Select variables of interest
lon_lad_2020_c2c_reduced = lon_lad_2020_c2c %>%
  select(c("BOROUGH", "Borough_number", "Borough_Area_km2", "geometry"))


# 2) CID data

# Import CID borough counts 
CID_count = readRDS(file = "data/CID_count_by_borough")
CID_length = readRDS(file = "data/CID_length_by_borough_on_off")


# Create new column (SignalsNA) where 0 are changed to NA - Signals is the only dataset where some Boroughs have 0 assets
CID_count$SignalsNA = CID_count$Signals
CID_count$SignalsNA[CID_count$SignalsNA == 0] = NA

# keep safety related assets
CID_count_safety = CID_count %>%
  select(c("BOROUGH", "ASL", "Crossings", "Signals", "SignalsNA", "TrafficCalming"))
CID_length_safety = CID_length %>%
  select(c("BOROUGH", "clt_total_length_km", "length_km_offroad", "length_km_onroad"))


# 3) Population estimates
# ONS Mid year population estimates 2013-2019 
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
              "/home/bananafan/Downloads/ONS_pop_estimates")

ons_pop_estimates = readxl::read_excel("/home/bananafan/Downloads/ONS_pop_estimates", 
                                       sheet = "MYE 5", skip = 3) # skip top few excel rows that arent relevent
lon_pop_estimates_2019 = ons_pop_estimates %>%
  filter(Geography1 == "London Borough") %>% # select London Boroughs
  select(c("Name", "Estimated Population mid-2019")) %>% # keep 2019 data only
  rename("BOROUGH" = "Name") %>% # rename to match CID
  rename("Population" = "Estimated Population mid-2019") %>%
  mutate(Population_100000 = Population / 100000)

# rename values to match those names used in CID
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Kensington and Chelsea"] <- "Kensington & Chelsea"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Barking and Dagenham"] <- "Barking & Dagenham"
lon_pop_estimates_2019$BOROUGH[lon_pop_estimates_2019$BOROUGH == "Hammersmith and Fulham"] <- "Hammersmith & Fulham"

# 4) PCT data
# The code for obtaining this data is in: get_pct_km_cycled.R file
pct_borough_commuting = readRDS(file = "data/Borough_commuting.rds") %>%
  mutate(total_mil_km_cycled_for_commuting_per_year_estimated = total_km_cycled_for_commuting_per_year_estimated / 1000000) 


# Join datasets together
CID_counts = left_join(CID_count_safety, CID_length_safety) 
denominators = left_join(lon_lad_2020_c2c_reduced, lon_pop_estimates_2019) %>%
  left_join(pct_borough_commuting)
chloropleth_dataset = left_join(denominators, CID_counts)


# Create variables with dropped units (ggplot doesnt like units)
chloropleth_dataset$Borough_Area_km2_no_units = round(units::drop_units(chloropleth_dataset$Borough_Area_km2), digits = 2)

# produce counts by area, per 100000 head population and per million km cycle commuted
chloropleth_dataset <- chloropleth_dataset %>%
  mutate(across(.cols = c("ASL", "Crossings", "clt_total_length_km", "length_km_offroad", 
                          "length_km_onroad", "Signals", "SignalsNA", "TrafficCalming"),
                .fns = ~.x/Borough_Area_km2_no_units,
                .names = "{.col}_by_area")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "clt_total_length_km", "length_km_offroad",
                          "length_km_onroad", "Signals", "SignalsNA", "TrafficCalming"),
                .fns = ~.x/Population_100000,
                .names = "{.col}_per_100000pop")) %>%
  mutate(across(.cols = c("ASL", "Crossings", "clt_total_length_km", "length_km_offroad", 
                          "length_km_onroad", "Signals","SignalsNA", "TrafficCalming"),
                .fns = ~.x/total_mil_km_cycled_for_commuting_per_year_estimated,
                .names = "{.col}_per_mil_km_cycled"))

# Create variables with dropped units (ggplot doesnt like units)
chloropleth_dataset$clt_raw_numeric = round(units::drop_units(chloropleth_dataset$clt_total_length_km), digits = 2)
chloropleth_dataset$on_clt_raw_numeric = round(units::drop_units(chloropleth_dataset$length_km_onroad), digits = 2)
chloropleth_dataset$off_clt_raw_numeric = round(units::drop_units(chloropleth_dataset$length_km_offroad), digits = 2)

chloropleth_dataset$clt_area_numeric = round(units::drop_units(chloropleth_dataset$clt_total_length_km_by_area), digits = 2)
chloropleth_dataset$on_clt_area_numeric = round(units::drop_units(chloropleth_dataset$length_km_onroad_by_area), digits = 2)
chloropleth_dataset$off_clt_area_numeric = round(units::drop_units(chloropleth_dataset$length_km_offroad_by_area), digits = 2)

chloropleth_dataset$clt_pop_numeric = round(units::drop_units(chloropleth_dataset$clt_total_length_km_per_100000pop), digits = 2)
chloropleth_dataset$on_clt_pop_numeric = round(units::drop_units(chloropleth_dataset$length_km_onroad_per_100000pop), digits = 2)
chloropleth_dataset$off_clt_pop_numeric = round(units::drop_units(chloropleth_dataset$length_km_offroad_per_100000pop), digits = 2)

chloropleth_dataset$clt_pct_numeric = round(units::drop_units(chloropleth_dataset$clt_total_length_km_per_mil_km_cycled), digits = 2)
chloropleth_dataset$on_clt_pct_numeric = round(units::drop_units(chloropleth_dataset$length_km_onroad_per_mil_km_cycled), digits = 2)
chloropleth_dataset$off_clt_pct_numeric = round(units::drop_units(chloropleth_dataset$length_km_offroad_per_mil_km_cycled), digits = 2)

# Create df of just City of London data as this is an outlier and often needs to be handled differently in the visualisations
city_chloropleth_dataset = chloropleth_dataset %>%
   filter(BOROUGH == "City of London")

# Create df without City of London data for some of the visualisations
drop_city = chloropleth_dataset %>%
  filter(BOROUGH != "City of London") 

###############################################################################
#                             Reference data maps                             #
###############################################################################

# 1) Boroughs areas
area_chloro = ggplot(chloropleth_dataset, 
                     aes(fill = Borough_Area_km2_no_units)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# plot bar chart
area_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Borough_Area_km2_no_units),
                                           y = Borough_Area_km2_no_units, fill = Borough_Area_km2_no_units)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  # adds borders to bars
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 160), expand = c(0,0),
                     breaks = c(0, 120)) +
  geom_hline(aes(yintercept = mean(Borough_Area_km2_no_units)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Borough_Area_km2_no_units)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
one_two = plot_grid(area_chloro, area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))
title_1_2 = ggdraw() +
  draw_label(bquote("Area"~(km^2)), size = 10)

# 2) Raw population
pop_chloro = ggplot(chloropleth_dataset, 
                     aes(fill = Population)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
# plot bar chart
pop_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -Population_100000),
                                          y = Population_100000, fill = Population_100000)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +  
  coord_flip() +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), breaks = c(0,2)) +  
  geom_hline(aes(yintercept = mean(Population_100000)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Population_100000)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
       
# Create cowplot of both plots
one_three = plot_grid(pop_chloro, pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))
title_1_3 = ggdraw() +
  draw_label("Population (100,000)", size = 10)

# 3) PCT cycling chloropleth 
pct_chloro = ggplot(chloropleth_dataset, 
       aes(fill = total_mil_km_cycled_for_commuting_per_year_estimated)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
pct_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -total_mil_km_cycled_for_commuting_per_year_estimated),
                                y = total_mil_km_cycled_for_commuting_per_year_estimated,
                                fill = total_mil_km_cycled_for_commuting_per_year_estimated)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 25), expand = c(0,0), breaks = c(0, 12.5), labels = c("0", "12.5")) +
  geom_hline(aes(yintercept = mean(total_mil_km_cycled_for_commuting_per_year_estimated)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(total_mil_km_cycled_for_commuting_per_year_estimated)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
one_four = plot_grid(pct_chloro, pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))
title_1_4 = ggdraw() + draw_label(bquote("Commuting cycling (annual million" ~"km)"^c), size = 10)
side_label_ref = ggdraw() +  
  draw_label("Reference data \n", x = 0.3, angle = 90, size = 10)  #created with cowplot


###############################################################################
#                                   ASL                                       # 
###############################################################################

######################
# Raw count - Orange #
######################

# create chloropleth
asl_raw_chloro = ggplot(chloropleth_dataset, 
                    aes(fill = ASL)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart
asl_raw_bar = ggplot(chloropleth_dataset, aes(x = reorder(Borough_number, -ASL),
                                          y = ASL, fill = ASL)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 350), expand = c(0,0), breaks = c(0, 200)) +
  geom_hline(aes(yintercept = mean(ASL)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(ASL)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots plut side label and title

two_one = plot_grid(asl_raw_chloro, asl_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55), ncol = 2)
title_2_1 = ggdraw() +
  draw_label(bquote("Raw" ~data^b), size = 10)



#######################################
# Standardised to Borough Area -Blues #
#######################################

asl_area_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = ASL_by_area), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = ASL_by_area), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

#Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(ASL_by_area)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

asl_area_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_by_area),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = ASL_by_area, fill = ASL_by_area),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 45), expand = c(0,0), breaks = c(0, 20)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(ASL_by_area)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(ASL_by_area)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots and title
two_two = plot_grid(asl_area_chloro, asl_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))
title_2_2 = ggdraw() +
  draw_label(bquote("Raw data per area"~(km^2)), size = 10)



########################################
# Standardised to Population  (greens) #
########################################

# Create chloropleth
asl_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = ASL_per_100000pop), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = ASL_per_100000pop), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(ASL_per_100000pop)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

asl_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = ASL_per_100000pop),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = ASL_per_100000pop, fill = ASL_per_100000pop),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1300), expand = c(0,0), breaks = c(0, 700)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(ASL_per_100000pop)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(ASL_per_100000pop)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot 
two_three = plot_grid(asl_pop_chloro, asl_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))
title_2_3 = ggdraw() +
  draw_label("Raw data per 100,000 Population", size = 10)


###############################################################
# Standardised to PCT data(km cycled through borough) - Reds  #
###############################################################

# # create chloropleth
asl_pct_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = ASL_per_mil_km_cycled)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
asl_pct_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -ASL_per_mil_km_cycled), y = ASL_per_mil_km_cycled, 
                         fill = ASL_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 140), expand = c(0,0), breaks = c(0, 70)) +
  geom_hline(aes(yintercept = mean(ASL_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(ASL_per_mil_km_cycled)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
two_four = plot_grid(asl_pct_chloro, asl_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))
title_2_4 = ggdraw() +
  draw_label(bquote("Raw data per commuting cycling"^c), size = 10)

##################################
# Create final row visualisation #
##################################
side_label_asl = ggdraw() +  
  draw_label("ASL \n", x = 0.3, angle = 90, size = 10)




###############################################################################
#                                   Crossings                                 # 
###############################################################################

######################
# Raw count - Orange #
######################
cross_raw_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = Crossings)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

cross_raw_bar = ggplot(chloropleth_dataset, 
                       aes(x = reorder(Borough_number, -Crossings), y = Crossings, 
                           fill = Crossings)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), expand = c(0,0), breaks = c(0, 100)) +
  geom_hline(aes(yintercept = mean(Crossings)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Crossings)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots plus side label 
three_one = plot_grid(cross_raw_chloro, cross_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55), ncol = 2)


#######################################
# Standardised to Borough Area -Blues #
#######################################

# create chloropleth
cross_area_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = Crossings_by_area), show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create bar chart
cross_area_bar = ggplot(chloropleth_dataset, 
                        aes(x = reorder(Borough_number, -Crossings_by_area), y = Crossings_by_area, 
                            fill = Crossings_by_area)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 6), expand = c(0,0), breaks = c(0, 3)) +
  geom_hline(aes(yintercept = mean(Crossings_by_area)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Crossings_by_area)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
three_two = plot_grid(cross_area_chloro, cross_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


########################################
# Standardised to Population  (greens) #
########################################

# create chloropleth
cross_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = Crossings_per_100000pop), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = Crossings_per_100000pop), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart
drop_city_reorder = drop_city %>%
  arrange(desc(Crossings_per_100000pop)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

cross_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = Crossings_per_100000pop),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = Crossings_per_100000pop, fill = Crossings_per_100000pop),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 180), expand = c(0,0), breaks = c(0, 100)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(Crossings_per_100000pop)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(Crossings_per_100000pop)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
three_three = plot_grid(cross_pop_chloro, cross_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

###############################################################
# Standardised to PCT data(km cycled through borough) - Reds  #
###############################################################

# create chloropleth
cross_pct_chloro = ggplot(chloropleth_dataset, 
                          aes(fill = Crossings_per_mil_km_cycled)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
cross_pct_bar = ggplot(chloropleth_dataset, 
                       aes(x = reorder(Borough_number, -Crossings_per_mil_km_cycled), y = Crossings_per_mil_km_cycled, 
                           fill = Crossings_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 110), expand = c(0,0), breaks = c(0, 50)) +
  geom_hline(aes(yintercept = mean(Crossings_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Crossings_per_mil_km_cycled)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
three_four = plot_grid(cross_pct_chloro, cross_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))



##################################
# Create final row visualisation #
##################################
side_label_cross = ggdraw() +  
  draw_label("Crossings \n", x = 0.3, angle = 90, size = 10)




###############################################################################
#                       On road Cycle lanes and tracks                        # 
###############################################################################


######################
# Raw count - Orange #
######################

# create chloropleth - on road clt
clt_on_raw_chloro = ggplot(chloropleth_dataset, 
                           aes(fill = on_clt_raw_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart - on road clt
clt_on_raw_bar = ggplot(chloropleth_dataset, 
                        aes(x = reorder(Borough_number, -on_clt_raw_numeric), y = on_clt_raw_numeric, 
                            fill = on_clt_raw_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 60), expand = c(0,0), breaks = c(0, 30)) +
  geom_hline(aes(yintercept = mean(on_clt_raw_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(on_clt_raw_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
four_one = plot_grid(clt_on_raw_chloro, clt_on_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


#######################################
# Standardised to Borough Area -Blues #
#######################################

# create chloropleth - on road clt
on_clt_area_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = on_clt_area_numeric), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = on_clt_area_numeric), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart - on_road clt
drop_city_reorder = drop_city %>%
  arrange(desc(on_clt_area_numeric)) %>%
  mutate(Borough_number = (row_number() + 1))

on_clt_area_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = on_clt_area_numeric),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = on_clt_area_numeric, fill = on_clt_area_numeric),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 8), expand = c(0,0), breaks = c(0, 4)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(on_clt_area_numeric)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(on_clt_area_numeric)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
four_two = plot_grid(on_clt_area_chloro, on_clt_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

########################################
# Standardised to Population  (greens) #
########################################
# Create chloropleth - on road only clt
on_clt_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = on_clt_pop_numeric), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = on_clt_pop_numeric), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart - on road only clt
drop_city_reorder = drop_city %>%
  arrange(desc(on_clt_pop_numeric)) %>%
  mutate(Borough_number = (row_number() + 1))  # this ensures that there is 'spare' bar space for the city one to drop into when plotted

on_clt_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = on_clt_pop_numeric),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = on_clt_pop_numeric, fill = on_clt_pop_numeric),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 220), expand = c(0,0), breaks = c(0, 150)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(on_clt_pop_numeric)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(on_clt_pop_numeric)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots - on road only clt
four_three = plot_grid(on_clt_pop_chloro, on_clt_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


###############################################################
# Standardised to PCT data(km cycled through borough) - Reds  #
###############################################################

# create chloropleth - on clt
on_clt_pct_chloro = ggplot(chloropleth_dataset, 
                           aes(fill = on_clt_pct_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart - on clt
on_clt_pct_bar = ggplot(chloropleth_dataset, 
                        aes(x = reorder(Borough_number, -on_clt_pct_numeric), y = on_clt_pct_numeric, 
                            fill = on_clt_pct_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 80), expand = c(0,0), breaks = c(0, 40)) +
  geom_hline(aes(yintercept = mean(on_clt_pct_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(on_clt_pct_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots - on clt
four_four = plot_grid(on_clt_pct_chloro, on_clt_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


##################################
# Create final row visualisation #
##################################

side_label_onclt = ggdraw() +  
  draw_label("On-road \n cycle lanes", x = 0.3, angle = 90, size = 10)


###############################################################################
#                       Off road cycle tracks                                 # 
###############################################################################

######################
# Raw count - Orange #
######################

# create chloropleth - off road clt
clt_off_raw_chloro = ggplot(chloropleth_dataset, 
                            aes(fill = off_clt_raw_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart - off road clt
clt_off_raw_bar = ggplot(chloropleth_dataset, 
                         aes(x = reorder(Borough_number, -off_clt_raw_numeric), y = off_clt_raw_numeric, 
                             fill = off_clt_raw_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 120), expand = c(0,0), breaks = c(0, 60)) +
  geom_hline(aes(yintercept = mean(off_clt_raw_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(off_clt_raw_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
five_one = plot_grid(clt_off_raw_chloro, clt_off_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


#######################################
# Standardised to Borough Area -Blues #
#######################################

# create chloropleth - off road clt
off_clt_area_chloro = ggplot(chloropleth_dataset, 
                             aes(fill = off_clt_area_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create Bar chart - all clt
off_clt_area_bar = ggplot(chloropleth_dataset, 
                          aes(x = reorder(Borough_number, -off_clt_area_numeric), y = off_clt_area_numeric, 
                              fill = off_clt_area_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 4), expand = c(0,0), breaks = c(0, 2)) +
  geom_hline(aes(yintercept = mean(off_clt_area_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(off_clt_area_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
five_two = plot_grid(off_clt_area_chloro, off_clt_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

########################################
# Standardised to Population  (greens) #
########################################

# Create chloropleth - off road only clt
off_clt_pop_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = off_clt_pop_numeric), show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create barchart - off road only clt
off_clt_pop_bar = ggplot(chloropleth_dataset, 
                         aes(x = reorder(Borough_number, -off_clt_pop_numeric), y = off_clt_pop_numeric, 
                             fill = off_clt_pop_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1, show.legend = F) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 60), expand = c(0,0), breaks = c(0, 30)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(off_clt_pop_numeric)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(off_clt_pop_numeric)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots - off road only clt
five_three = plot_grid(off_clt_pop_chloro, off_clt_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))



###############################################################
# Standardised to PCT data(km cycled through borough) - Reds  #
###############################################################

# create chloropleth - off clt
off_clt_pct_chloro = ggplot(chloropleth_dataset, 
                            aes(fill = off_clt_pct_numeric)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "transparent", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart - on clt
off_clt_pct_bar = ggplot(chloropleth_dataset, 
                         aes(x = reorder(Borough_number, -off_clt_pct_numeric), y = off_clt_pct_numeric, 
                             fill = off_clt_pct_numeric)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 165), expand = c(0,0), breaks = c(0, 80)) +
  geom_hline(aes(yintercept = mean(off_clt_pct_numeric)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(off_clt_pct_numeric)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots - on clt
five_four = plot_grid(off_clt_pct_chloro, off_clt_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


##################################
# Create final row visualisation #
##################################
side_label_offclt = ggdraw() +  
  draw_label("Off-road \n cycle tracks", x = 0.3, angle = 90, size = 10)



###############################################################################
#                                Signals                                      # 
###############################################################################

######################
# Raw count - Orange #
######################

# # create chloropleth - use SignalsNA as the polygon as this then colours the NAs white)
signals_raw_chloro = ggplot(chloropleth_dataset, 
                            aes(fill = SignalsNA)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar
signals_raw_bar = ggplot(chloropleth_dataset, 
                         aes(x = reorder(Borough_number, -Signals), y = Signals, 
                             fill = SignalsNA)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0), breaks = c(0, 75)) +
  geom_hline(aes(yintercept = mean(Signals)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Signals)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
six_one = plot_grid(signals_raw_chloro, signals_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


#######################################
# Standardised to Borough Area -Blues #
#######################################

# create chloropleth - 
sig_area_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = SignalsNA_by_area), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = SignalsNA_by_area), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
drop_city_reorder = drop_city %>%
  arrange(desc(Signals_by_area)) %>%
  mutate(Borough_number = (row_number() + 1))

sig_area_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = Signals_by_area),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = Signals_by_area, fill = Signals_by_area),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0,21), expand = c(0,0), breaks = c(0, 10)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(Signals_by_area)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(Signals_by_area)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
six_two = plot_grid(sig_area_chloro, sig_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

########################################
# Standardised to Population  (greens) #
########################################

# create chloropleth - 
sig_pop_chloro = ggplot() + 
  geom_sf(data = drop_city, aes(fill = SignalsNA_per_100000pop), show.legend = F) +
  geom_sf(data = city_chloropleth_dataset, aes(fill = Signals_per_100000pop), fill = "black") +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
drop_city_reorder = drop_city %>%
  arrange(desc(Signals_per_100000pop)) %>%
  mutate(Borough_number = (row_number() + 1))

sig_pop_bar = ggplot() +
  geom_bar(data = city_chloropleth_dataset, aes(x = Borough_number, y = Signals_per_100000pop),
           stat = "identity", color = "black", size = 0.1, fill = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_bar(data = drop_city_reorder, aes(x = Borough_number, y = Signals_per_100000pop, fill = Signals_per_100000pop),
           stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 600), expand = c(0,0), breaks = c(0, 300)) +
  geom_hline(data = chloropleth_dataset, aes(yintercept = mean(Signals_per_100000pop)),
             linetype = "solid") +
  geom_hline(data = chloropleth_dataset, aes(yintercept = median(Signals_per_100000pop)),
             linetype = "dashed") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
six_three = plot_grid(sig_pop_chloro, sig_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


###############################################################
# Standardised to PCT data(km cycled through borough) - Reds  #
###############################################################

# create chloropleth - 
sig_pct_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = SignalsNA_per_mil_km_cycled), show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
sig_pct_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -Signals_per_mil_km_cycled), y = Signals_per_mil_km_cycled, 
                         fill =Signals_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 15), expand = c(0,0), breaks = c(0, 7)) +
  geom_hline(aes(yintercept = mean(Signals_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(Signals_per_mil_km_cycled)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
six_four = plot_grid(sig_pct_chloro, sig_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))



##################################
# Create final row visualisation #
##################################
side_label_sig = ggdraw() +  
  draw_label("Signals \n", x = 0.3, angle = 90, size = 10)




###############################################################################
#                               Traffic calming                               # 
###############################################################################

######################
# Raw count - Orange #
######################

# create chloropleth
tc_raw_chloro = ggplot(chloropleth_dataset, 
                       aes(fill = TrafficCalming)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "YlOrBr", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# create bar chart
tc_raw_bar = ggplot(chloropleth_dataset, 
                    aes(x = reorder(Borough_number, -TrafficCalming), y = TrafficCalming, 
                        fill = TrafficCalming)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 3800), expand = c(0,0), breaks = c(0, 3000)) +
  geom_hline(aes(yintercept = mean(TrafficCalming)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
seven_one = plot_grid(tc_raw_chloro, tc_raw_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))

#######################################
# Standardised to Borough Area -Blues #
#######################################

tc_area_chloro = ggplot(chloropleth_dataset, 
                        aes(fill = TrafficCalming_by_area)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Blues", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
tc_area_bar = ggplot(chloropleth_dataset, 
                     aes(x = reorder(Borough_number, -TrafficCalming_by_area), y = TrafficCalming_by_area, 
                         fill = TrafficCalming_by_area)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 160), expand = c(0,0), breaks = c(0, 75)) +
  geom_hline(aes(yintercept = mean(TrafficCalming_by_area)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming_by_area)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
seven_two = plot_grid(tc_area_chloro, tc_area_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


########################################
# Standardised to Population  (greens) #
########################################

# create chloropleth
tc_pop_chloro = ggplot(chloropleth_dataset, 
                       aes(fill = TrafficCalming_per_100000pop)) + 
  geom_sf(show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# # create Bar chart
tc_pop_bar = ggplot(chloropleth_dataset, 
                    aes(x = reorder(Borough_number, -TrafficCalming_per_100000pop), y = TrafficCalming_per_100000pop, 
                        fill = TrafficCalming_per_100000pop)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 2000), expand = c(0,0), breaks = c(0, 1500)) +
  geom_hline(aes(yintercept = mean(TrafficCalming_per_100000pop)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming_per_100000pop)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
seven_three = plot_grid(tc_pop_chloro, tc_pop_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


###############################################################
# Standardised to PCT data(km cycled through borough) - Reds  #
###############################################################

# # create chloropleth
tc_pct_chloro = ggplot() + 
  geom_sf(data = chloropleth_dataset, aes(fill = TrafficCalming_per_mil_km_cycled), show.legend = F) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       na.value = "light grey", direction = 1) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create bar chart
tc_pct_bar = ggplot(chloropleth_dataset, 
                    aes(x = reorder(Borough_number, -TrafficCalming_per_mil_km_cycled), y = TrafficCalming_per_mil_km_cycled, 
                        fill = TrafficCalming_per_mil_km_cycled)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 3200), expand = c(0,0), breaks = c(0, 1500)) +
  geom_hline(aes(yintercept = mean(TrafficCalming_per_mil_km_cycled)),
             linetype = "solid") +
  geom_hline(aes(yintercept = median(TrafficCalming_per_mil_km_cycled)),
             linetype = "dashed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 8, colour = "grey25"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Create cowplot of both plots
seven_four = plot_grid(tc_pct_chloro, tc_pct_bar, rel_widths = c(1, 0.5), scale = c(1, 0.55))


##################################
# Create final row visualisation #
##################################
side_label_tc = ggdraw() +  
  draw_label("Traffic calming \n", x = 0.3, angle = 90, size = 10)


##################################
# Final visualisation - figure 4 #
##################################


# This approach to figure 4 creation using cowplot makes all the chloropleths the same size
# full_fig4 = plot_grid(NULL, NULL, title_1_2, title_1_3, title_1_4,
#                      side_label_ref, NULL, one_two, one_three, one_four,
#                      NULL, title_2_1, title_2_2, title_2_3, title_2_4,
#                      side_label_asl, two_one, two_two, two_three, two_four,
#                      side_label_cross, three_one, three_two, three_three, three_four,
#                      side_label_onclt, four_one, four_two, four_three, four_four,
#                      side_label_offclt, five_one, five_two, five_three, five_four,
#                      side_label_sig, six_one, six_two, six_three, six_four, 
#                      side_label_tc, seven_one, seven_two, seven_three, seven_four,
#                      ncol = 5, rel_heights = c(0.2, 1, 0.2, 1, 1, 1, 1, 1, 1),
#                      rel_widths = c(1, 4, 4, 4, 4))



# As full figure 4 is too big and plus we want the fig over two pages the below code works.
fig4_left = plot_grid(NULL, NULL, title_1_2, 
                      side_label_ref, NULL, one_two,  
                      NULL, title_2_1, title_2_2, 
                      side_label_asl, two_one, two_two, 
                      side_label_cross, three_one, three_two, 
                      side_label_onclt, four_one, four_two, 
                      side_label_offclt, five_one, five_two, 
                      side_label_sig, six_one, six_two, 
                      side_label_tc, seven_one, seven_two, 
                      ncol = 3, rel_heights = c(0.2, 1, 0.2, 1, 1, 1, 1, 1, 1),
                      rel_widths = c(1, 4, 4))

fig4_right = plot_grid(title_1_3, title_1_4,
                      one_three, one_four,
                      title_2_3, title_2_4,
                      two_three, two_four,
                      three_three, three_four,
                      four_three, four_four,
                      five_three, five_four,
                      six_three, six_four, 
                      seven_three, seven_four,
                      ncol = 2, rel_heights = c(0.2, 1, 0.2, 1, 1, 1, 1, 1, 1),
                      rel_widths = c(1, 1))

