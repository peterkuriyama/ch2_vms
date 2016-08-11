###VMS Stuff
#--------------------------------------------------------------------------------
#To Do
#Expand tow footprints to get a sense of overall effort rather than set/up points
#Remove points with only 
#NE ALSO
#Temporal Distribution of effort different before and after?
#Wher and how are they catching species in certain areas?
#Shift in effort for each vessel, directional shift maybe?
#Track median of distribution or something 
#k means clustering


#--------------------------------------------------------------------------------
#Start
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

source('R/dist_funcs.r')

#--------------------------------------------------------------------------------
#Load expanded West Coast Data
load('output/wc_data_expanded_tows.Rdata')

#Add in ratio of apounds to hpounds
#ratio should be between 0.6-1.1 for acceptable rows, Lee and Sampson
wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds

# hist(subset(wc_data, ha_ratio < 2)$ha_ratio, breaks = 30) #histogram looks fairly normal
# length(which(wc_data$ha_ratio <= 1.2 & 
#   wc_data$ha_ratio >= 0.6)) / nrow(wc_data) #45% of tows seem to 

#Seems to be some duplicated rows, given lat and long
wc_data %>% group_by(drvid, trip_id, haul_id, lat, long) %>% filter(row_number() == 1) %>%
  as.data.frame -> wc_data_unique

#considered set lat and set long to be "lat" "long"

#--------------------------------------------------------------------------------
#Set Map stuff
world_map <- map_data("world")
wc_map <- states_map[states_map$region %in% c('USA', 'Canada'), ]
wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')
