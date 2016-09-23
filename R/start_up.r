
#--------------------------------------------------------------------------------
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

#--------------------------------------------------------------------------------
#Load and Format Data

#Load expanded West Coast Data
load('output/wc_data_expanded_tows.Rdata')

# wc_data_expanded <- wc_data
wc_data_orig <- wc_data

####################################################
# #Load Port data and rename
port_codes <- read.csv("data/port_codes.csv", stringsAsFactors = FALSE)
port_codes <- plyr::rename(port_codes, c('Pcid' = 'text_id', 'Agid' = 'state',
  'Agency' = 'number_id', 'Port.Agency.Description' = 'description'))

# #identify missing ports
# #Add in ports that I know are missing
added_ports <- data.frame(text_id = c("", "", "", "", "", "", "", "", "", "", "", ""),
                   state = c("O", "O", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W"),
                   number_id = c("02", "46", "WES", "ORE", "BEL", "N.B", "SEA", "BLA",
                                 "P.A", "ILW", "ANA", "CAL"),
                   description = c("ASTORIA", "dont know", "WESTPORT", "dont know ORE", 
                                   "BELLINGHAM", "NORTH BEND", "SEATTLE", "BLAINE", 
                                   "PORT ANGELES", "ILWACO", "ANACORTES", "dont know CAL"))
port_codes <- rbind(port_codes, added_ports)
port_codes$state_port <- paste(port_codes$state, port_codes$number_id)

#Add dport and rport codes
wc_data$state_dport <- paste(wc_data$agid, wc_data$dport)
wc_data$state_rport <- paste(wc_data$agid, wc_data$rport)

##rport 
test <- data.frame("state_port" = paste(wc_data$agid, wc_data$rport))
test$state_port <- as.character(test$state_port)
thing <- inner_join(x = test, y = port_codes[, c('state_port', 'description')], by = 'state_port')
wc_data$rport_desc <- thing$description

##dport
test <- data.frame("state_port" = paste(wc_data$agid, wc_data$dport))
test$state_port <- as.character(test$state_port)
thing <- inner_join(test, port_codes[, c('state_port', 'description')], by = 'state_port')
wc_data$dport_desc <- thing$description


####################################################
#Add in ratio of apounds to hpounds
#ratio should be between 0.6-1.1 for acceptable rows, Lee and Sampson
wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds

#Save some columns of original data
# wc_data_orig <- wc_data %>% select(trip_id, ddate, agid, rdate, drvid, dyear, townum,
#   set_lat, set_long, up_lat, up_long, depth1, target, species, rport_desc,
#   dport_desc, d_portgrp, arid_psmfc, duration, net_type, ha_ratio, hpounds, apounds)


#--------------------------------------------------------------------------------
#Set Map stuff
world_map <- map_data("world")
# states_map <- map_data("state")
# wc_map <- states_map[states_map$region %in% c('USA', 'Canada'), ]

wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')








# ###VMS Stuff
# #--------------------------------------------------------------------------------
# #To Do
# #Expand tow footprints to get a sense of overall effort rather than set/up points
# #Remove points with only 
# #NE ALSO
# #Temporal Distribution of effort different before and after?
# #Wher and how are they catching species in certain areas?
# #Shift in effort for each vessel, directional shift maybe?
# #Track median of distribution or something 
# #k means clustering


# #--------------------------------------------------------------------------------
# #Start
# library(ggplot2)
# library(plyr)
# library(dplyr)
# library(lubridate)
# library(reshape2)

# # source('R/dist_funcs.r')

# #--------------------------------------------------------------------------------
# #Load expanded West Coast Data
# load('output/wc_data_expanded_tows.Rdata')

# #Add in ratio of apounds to hpounds
# #ratio should be between 0.6-1.1 for acceptable rows, Lee and Sampson
# wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds
# wc_data <- subset(wc_data, ha_ratio >= 0.6 & ha_ratio <= 1.1)
# # hist(subset(wc_data, ha_ratio < 2)$ha_ratio, breaks = 30) #histogram looks fairly normal
# # length(which(wc_data$ha_ratio <= 1.2 & 
# #   wc_data$ha_ratio >= 0.6)) / nrow(wc_data) #45% of tows seem to 

# #Seems to be some duplicated rows, given lat and long
# # wc_data %>% group_by(drvid, trip_id, haul_id, lat, long) %>% filter(row_number() == 1) %>%
# #   as.data.frame -> wc_data_unique

# #considered set lat and set long to be "lat" "long"

# #--------------------------------------------------------------------------------
# #Set Map stuff
# world_map <- map_data("world")
# wc_map <- states_map[states_map$region %in% c('USA', 'Canada'), ]
# wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
#     map_id = region), fill = 'gray') + 
#     geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')
