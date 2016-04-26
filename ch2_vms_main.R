###VMS Stuff
setwd('/Users/peterkuriyama/School/Research/ch2_vms')

library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)

funcs <- list.files('R')
sapply(funcs, FUN = function(x) source(paste0('R/', x)))

#Takes 45 seconds to run
wc_data <- ch2_load_and_format()

#Convert lat/lon degrees to radians
wc_data$set_lat_r <- deg2rad(wc_data$set_lat)
wc_data$set_long_r <- deg2rad(wc_data$set_long)
wc_data$up_lat_r <- deg2rad(wc_data$up_lat)
wc_data$up_long_r <- deg2rad(wc_data$up_long)

wc_data %>% rowwise() %>% 
  mutate(dist_slc_km = gcd_slc(set_long_r, set_lat_r, up_long_r, up_lat_r),
         dist_hf_km = gcd_hf(set_long_r, set_lat_r, up_long_r, up_lat_r)) %>%
  as.data.frame -> wc_data

wc_data$dist_slc_mi <- wc_data$dist_slc_km * 0.621371
wc_data$mph <- wc_data$dist_slc_mi / (wc_data$duration_min / 60)

too_fast_inds <- which(wc_data$mph > 5)
too_fast <- wc_data[too_fast_inds, ]

#Remove tows that are too fast
wc_data <- wc_data[-too_fast_inds, ]

#--------------------------------------------------------------------------------
wc_data %>% distinct(drvid, tow_month, tow_day, tow_year, mph) %>%
  group_by(tow_year) %>% summarise(tot_min = sum(duration_min, na.rm = TRUE)) -> sum_effort

plot()


ttt <- subset(tow_time_unq, drvid == '1037785')
plot(ttt$tow_year, ttt$tot_min, type = 'b', pch = 19, ylim = c(0, 70000))

plot(tow_time_unq$tow_year, tow_time_u)



#Evaluate Time spent trawling
wc_data %>% group_by(drvid, tow_year) %>% summarise(tot_min = sum(duration_min, 
  na.rm = TRUE)) -> tow_time

ttt <- subset(tow_time, drvid == 1037785)
plot(ttt$tow_year, ttt$tot_min)

ggplot(tow_time, aes(x = tow_year, y = tot_min, group = drvid)) + geom_line(aes(colour = drvid)) 






#Start HERE
#--------------------------------------------------------------------------------


#convert longitudes to be in latitude units (Branch et al. 2005)
which(is.na(wc_data$set_long))
test_set_long <- wc_data$set_long * cos((wc_data$set_lat * pi) / 180)
test_up_long <- wc_data$up_long * cos((wc_data$up_lat * pi) / 180)


hist(log(wc_data$duration_min))




#----------------------------------------
#calculate miles per hour for towing and filter out ones that are too high
#First convert lon units with lat unit conversion from eq. 1 of Branch et al 2005
temp$set_long




#----------------------------------------
#Plot Maps
states_map <- map_data("state")
wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington'), ]

#Map with points for the highest 
ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-125.5, -123),
  ylim = c(41, 46)) + geom_segment(data = temp, aes(x = -set_long, xend = -up_long,
    y = set_lat, yend = up_lat, group = tow_month, colour = tow_month), 
  arrow = arrow(length = unit(0.1, 'cm'))) + facet_wrap(~ tow_year, ncol = 5) 

#Looks like there are some really long tows









temp %>% group_by(ryear) %>% summarise(nstate = length(unique(agid))) %>%




















# #----------------------------------------
# #Scraps with old wc_data which only goes to 2012
# #----------------------------------------
# # wc_data <- ch2_load_data()

# load('data/wc_data.Rdata')

# #rename unq column as tow
# wc.data <- plyr::rename(wc.data, c('unq' = 'tow', 'noncon_vid' = 'vesselid',
#   'columns..agid' = 'state'))
# wc_data <- wc.data #rename with underscore

# #find the rows and remove them from wc_data
# #Filter out:
# #sturgeon
# #cucumber
# #prawn
# #whiting
# #dogfish
# #grenadier
# #squid
# #wrymouth
# wc_data <- wc_data[-grep(paste("sturgeon", 'cucumber', 'prawn', 'whiting', 'dogfish', 'grenadier',
#   'squid', "wrymouth", 'halibut', sep='|'), wc_data$target.desc), ]

# #Filter rows where tow_date is missing
# wc_data <- wc_data[-which(nchar(wc_data$tow_date) == 0), ]

# #Parse out tow dates
# wc_data$tow_month <- substr(wc_data$tow_date, 4, 6)
# wc_data$tow_day <- substr(wc_data$tow_date, 1, 2)
# wc_data$tow_year <- substr(wc_data$tow_date, 8, 9)

# wc_data$state_port <- paste(wc_data$state, wc_data$dport)
# #----------------------------------------

# wc_data %>% group_by(vesselid) %>% summarise(nstate = length(unique(state))) %>% as.data.frame

# #Merge Port Codes with wc data

# #Load Port data and rename
# port_codes <- read.csv("data/port_codes.csv", stringsAsFactors = FALSE)
# port_codes <- plyr::rename(port_codes, c('Pcid' = 'text_id', 'Agid' = 'state',
#   'Agency' = 'number_id', 'Port.Agency.Description' = 'description'))

# #identify missing ports
# #Add in ports that I know are missing
# added_ports <- data.frame(text_id = c("", "", "", "", "", "", "", "", "", "", "", ""),
#                    state = c("O", "O", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W"),
#                    number_id = c("02", "46", "WES", "ORE", "BEL", "N.B", "SEA", "BLA",
#                                  "P.A", "ILW", "ANA", "CAL"),
#                    description = c("ASTORIA", "dont know", "WESTPORT", "dont know ORE", 
#                                    "BELLINGHAM", "NORTH BEND", "SEATTLE", "BLAINE", 
#                                    "PORT ANGELES", "ILWACO", "ANACORTES", "dont know CAL"))
# port_codes <- rbind(port_codes, added_ports)
# port_codes$state_port <- paste(port_codes$state, port_codes$number_id)

# unq_port_codes <- unique(wc_data$state_port)

# wc_data_merged <- merge(wc_data, port_codes[, c('state_port', 'description')], by = 'state_port')
# wc_data <- wc_data_merged

# #----------------------------------------
# #Load Vessel data
# permits <- read.csv('data/permits.csv', stringsAsFactors = FALSE)
# trawl_permits <- subset(permits, TrawlGear == 'Yes')

# #pull first character of state columns
# trawl_permits$PermitOwnerState <- substr(trawl_permits$PermitOwnerState, 1, 1)
# trawl_permits$VesselOwnerState <- substr(trawl_permits$VesselOwnerState, 1, 1)

# trawl_permits[which(trawl_permits$PermitOwnerState != trawl_permits$VesselOwnerState), 
# c("PermitOwnerState", "VesselOwnerState")]

# trawl_permits$state_length <- paste(trawl_permits$)

# wc_data %>% group_by(description) %>% summarise(nvess = length(unique(vesselid))) %>% 
#   as.data.frame



# #Merge port codes 
# unique(wc_data$dport)

# head(port_codes)



# aa <- unique(wc_data$state_port)
# aa[which(aa %in% unique(port_codes$state_port) == FALSE)]


# bb <- wc_data[which(wc_data$state_port == 'O 46'), ]
# max(bb$hpounds, na.rm = TRUE)
# bb[which(bb$hpounds == 15000), ]


# #----------------------------------------
# #Match port codes to vessels
# #then match with vessel accounts in each year




# #Merge the data
# wc_data_merged <- merge(wc_data, port_codes[, c('Port.Agency.Description', 'state_port')],
#   all = TRUE, by = "state_port")




# #Find which dports are characters
# #Westport

# head(subset(wc_data, dport == "WES"))


# dim(subset(wc_data, dport == "ORE"))

# dim(subset(wc_data, dport == "BEL"))#Bellingham

# dim(subset(wc_data, dport == "SEA"))#Seattle
# head(subset(wc_data, dport == "SEA"))

# dim(subset(wc_data, dport == "BLA"))#Blaine
# head(subset(wc_data, dport == "BLA"))

# dim(subset(wc_data, dport == "N.B"))#Neah Bay
# head(subset(wc_data, dport == "N.B"))

# head(subset(wc_data, dport == "ILW")) #ilwaco

# head(subset(wc_data, dport == "ANA")) #anacortes

# dim(subset(wc_data, dport == "CAL")) #Centralia?
# head(subset(wc_data, dport == "CAL"))
# wc_data$dport == "BEL"




# #Select one vessel and see where it goes and how that changes through time
# #Number of rows per vessel and years
# # wc_data %>% group_by(vesselid) %>% summarise(min_year = min(year), max_year = max(year),
# #   nrows = length(year)) %>% filter(max_year >= 2012) %>% arrange(desc(nrows))

# #----------------------------------------
# #Group by port...


# #----------------------------------------
# #Look at H4033, the vessel with most rows
# highest <- subset(wc_data, vesselid == 'H4033')

# #Try mapping
# #Load map data
# states_map <- map_data("state")
# wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington'), ]

# #Map with points for the highest 
# ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-127, -123),
#   ylim = c(40, 49)) + geom_point(data = highest, aes(x = mid_long, y = mid_lat), colour = 'red') + 
#   facet_wrap(~ year)

# #Look at map in one particular year
# ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-127, -123),
#   ylim = c(40, 49)) + geom_point(data = subset(highest, tow_year == '08'), 
#   aes(x = mid_long, y = mid_lat), colour = 'red') + 
#   facet_wrap(~ tow_month)



# #look at histogram of tow durations during the year
# highest %>% group_by(year) %>% 

# ggplot(data = highest, aes(duration)) + geom_histogram() + facet_wrap(~ year) + 




# ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-127, -123),
#   ylim = c(40, 49)) + geom_point(data = highest, aes(x = mid_long, y = mid_lat), colour = 'red') + 
#   facet_wrap(~ year)






# ggplot(highest, aes(x = mid_long, y = mid_lat)) + geom_point() + facet_wrap(~ year)


# #



# #Check
# # wc_data %>% group_by(target.desc) %>% summarise(nrowz = length(year)) %>% arrange(desc(nrowz)) %>% 
# #   as.data.frame

# # ggplot(wc_data, aes(x = length, y = depth1)) + geom_point()

# #Plot histograms of depth by year
# ggplot(wc_data, aes(depth1)) + geom_bar() + facet_wrap(~ year)

# #Plot heatmaps of location choice by year
# ggplot(wc_data, aes(x = mid_long, y = mid_lat)) + 
#   geom_bin2d(binwidth = (c(.15, .15))) + facet_wrap(~ year)








# #SCRAPPS


# hist(wc_data$mid_lat)






# #Filter CHLB, 
# #pacific whiting

# wc.data[is.na(wc.data$target), 'state.target']

# wc.data %>% filter(target == '') -> zz

# zz %>% group_by(spcode.desc) %>% summarise(ntows = length(hpounds)) %>% arrange(desc(ntows)) %>% 
#   as.data.frame
# unique(zz$spcode.desc)

# unique(zz$columns..agid)



# wc.data %>% filter(target != "CHLB") %>% group_by(target.desc) %>% mutate(ntrips = length(trip), 
#   perc_trips = ntrips / sum(ntrips)) %>%
#   arrange(desc(ntrips)) 





#  as.data.frame %>% 



# unique(wc.data$net_type)

# subset(wc.data, net_type == 'D')

# unique(wc.data$target.desc)
# #Filter logbook data to remove NAs and nongroundfish species

# (wc.data[is.na(wc.data$target.desc), ])

# nrow(wc.data) - nrow(wc.data[is.na(wc.data$target.desc), ])


# nas <- subset(wc.data, target.desc == NA)




# load('data/nw_vms.Rdata')










# unique(wc.data$target.desc)

# nw.vms1 <- load_and_process_data()

# dat <- nw.vms1


# ggplot(dat, aes(x = rounded.lon, y = rounded.lat)) + geom_point() + facet_wrap(~ year)


# dat %>% group_by(vessel_name, year) %>% summarise()

# #Look at n trips per vessel

# nw.vms1 %>% group_by(vessel_name) %>% 










# #Reclassify
# nw.vms$speed <- as.numeric(nw.vms$speed)
# nw.vms$year <- as.numeric(nw.vms$year)
# nw.vms$month <- as.numeric(nw.vms$month)

# #Remove values with NA for speed
# nw.vms <- nw.vms[is.na(nw.vms$speed) == FALSE, ]
# #Remove values with 0 for speed
# nw.vms <- subset(nw.vms, speed != 0)

# #Assumed trawl speed is between 2 and 4, this is arbitrary
# nw.vms <- subset(nw.vms, speed >= 2 & speed <= 4)
# nw.vms$rounded.lat <- round(nw.vms$lat, digits = 2)
# nw.vms$rounded.lon <- round(nw.vms$lon, digits = 2)

# #Filter out nonsensical values
# nw.vms <- subset(nw.vms, lon < 0)

# #
# states_map <- map_data('world')
# # wc <- subset(states_map, region %in% c('california', 'oregon', 'washington'))

# ggplot(states_map, aes(x = long, y = lat)) + geom_polygon()


# ggplot(nw.vms, aes(x = lon, y = lat)) + stat_bin2d(bins = 100) + facet_wrap(~ year)


# subset(nw.vms)

# temp <- nw.vms %>% group_by(month, year) %>% summarise(nvess = length(unique(vessel_name)), 
#   npoints = length(lat))






# ggplot(temp, aes(x = month, y = nvess)) + geom_point() + facet_wrap(~ year)

# ggplot(temp, aes(x = month, y = npoints)) + geom_point() + facet_wrap(~ year)





# ggplot(nw.vms aes(x = ))



# #
# first.50 <- head(nw.vms, n = 50)
# first.50[is.na(first.50$speed) == FALSE, ]
