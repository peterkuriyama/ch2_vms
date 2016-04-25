###VMS Stuff
setwd('/Users/peterkuriyama/School/Research/ch2_vms')

library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)

#--------------------------------------------------------------------------------
#Load Data through 2015
load("data/LBKDATA_Barnett_Logbook_Data_2002_2014_2015-11-24.Rda")

wc_data <- LBK.out
names(wc_data) <- tolower(names(wc_data))

#Remove certain columns 
to_remove <- c("block_or", "latlong_type", "ch_lat", "ch_long", "up_area", 
      "up_arid_psmfc", "up_block", "up_block_or", "up_ch_lat", "up_ch_long",
      "depth_type2", "ps_grnd_code", "catchsource", 
      "ftid6", "ticket_date", "ftsource", 
      "tripwarning", "towwarning", "catchwarning", "rel")
wc_data <- wc_data[, which(names(wc_data) %in% to_remove == FALSE)]

#Convert factor columns to character
factor_cols <- which(sapply(wc_data, FUN = is.factor))

for(fff in 1:length(factor_cols)){
  ind <- factor_cols[fff]
  # print(ind)
  wc_data[, ind] <- as.character(wc_data[, ind])
  # wc_data[, fff] <- as
}

#Parse towdate
wc_data$tow_day <- substr(wc_data$towdate, 1, 2)
wc_data$tow_month <- substr(wc_data$towdate, 4, 6)
wc_data$tow_year <- substr(wc_data$towdate, 8, 12)

#--------------------------------------------------------------------------------
# #Filter out set_times that are NA
nas <- apply(wc_data[, c('set_long', 'set_lat', 'up_long', 'up_lat', 'set_time')],
  FUN = function(x) which(is.na(x)), MARGIN = 2)
nas <- unlist(nas)
names(nas) <- NULL

head(wc_data[nas, c('set_long', 'set_lat', 'up_long', 'up_lat', 'set_time')])

nas <- c(which(is.na(wc_data$set_time)),
         which(is.na(wc_data$set_long)))

# removals <- rbind(removals, c('set_times are NA', length(nas), 
#   removals$nrow_end, removals$nrow_end - length(nas)))
wc_data <- wc_data[-nas, ]

#Filter out hake tows
wc_data %>% group_by(haul_id) %>% summarise(nspecies = length(unique(spc.name)),
  hake = ifelse("PACIFIC WHITING" %in% unique(common.name), 1, 0)) -> cccc
hake_tow_ids <- subset(cccc, hake == 1)
hake_tows <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id)

#Remove hake rows
wc_data <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id == FALSE)

#Filter out certain data
#Filter out set_times with nchar == 1
# removals <- data.frame('desc' = 1, 'nremoved' = 1, 'nrow_start' = nrow(wc_data), 'nrow_end' = 1)
# removals$desc <- 'rows that have nchar(set_time) == 1'
# removals$nremoved = length(which(nchar(wc_data$set_time) == 1))
# wc_data <- wc_data[-which(nchar(wc_data$set_time) == 1), ]
# removals$nrow_end <- nrow(wc_data)

# #Filter out set_times with nchar == 2
# rem2 <- which(nchar(wc_data$set_time) == 2)
# wc_data[rem2, c('set_time', 'up_time', 'duration')]

#--------------------------------------------------------------------------------
##Calculate tow durations in minutes and seconds

##assign dates and times
wc_data <- plyr::rename(wc_data, c('towdate' = 'set_date'))
wc_data$set_date <- dmy(wc_data$set_date)
wc_data$up_date <- wc_data$set_date

wc_data[which(wc_data$set_time >= wc_data$up_time), 'up_date'] <- wc_data[which(wc_data$set_time >= 
  wc_data$up_time), 'up_date'] + days(1)

wc_data$set_time <- as.character(wc_data$set_time)
wc_data$up_time <- as.character(wc_data$up_time)

#Paste colon between hour and minutes
wc_data$up_time <- paste(substr(wc_data$up_time, nchar(wc_data$up_time) - 3, nchar(wc_data$up_time) - 2),
                       substr(wc_data$up_time, nchar(wc_data$up_time) - 1, nchar(wc_data$up_time)), sep = ':')
wc_data[which(nchar(wc_data$up_time) == 3), 'up_time'] <- paste0('0', 
  wc_data[which(nchar(wc_data$up_time) == 3), 'up_time'])
wc_data$set_time <- paste(substr(wc_data$set_time, nchar(wc_data$set_time) - 3, nchar(wc_data$set_time) - 2),
                       substr(wc_data$set_time, nchar(wc_data$set_time) - 1, nchar(wc_data$set_time)), sep = ':')
wc_data[which(nchar(wc_data$set_time) == 3), 'set_time'] <- paste0('0', 
  wc_data[which(nchar(wc_data$set_time) == 3), 'set_time'])

#convert to year month date, hour minute formats
wc_data$set_date_full <- ymd_hm(paste(wc_data$set_date, wc_data$set_time))
wc_data$up_date_full <- ymd_hm(paste(wc_data$up_date, wc_data$up_time))

#Calculate durations in minutes and hours
wc_data$duration_min <- interval(wc_data$set_date_full, wc_data$up_date_full) / dminutes(1)
wc_data$duration_hour <- interval(wc_data$set_date_full, wc_data$up_date_full) / dhours(1)


#--------------------------------------------------------------------------------
#Calculate distances between start and end points (assuming linearity)

#Add direction
wc_data$tow_direction <- paste0(ifelse(wc_data$set_lat >= wc_data$up_lat, 'S', 'N'),
                                ifelse(wc_data$set_long >= wc_data$up_long, 'E', 'W'))

#--------------------------------------------------------------------------------
#Start HERE
#--------------------------------------------------------------------------------

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

gcd.vif <- function(long1, lat1, long2, lat2) {
 
  # WGS-84 ellipsoid parameters
  a <- 6378137         # length of major axis of the ellipsoid (radius at equator)
  b <- 6356752.314245  # ength of minor axis of the ellipsoid (radius at the poles)
  f <- 1/298.257223563 # flattening of the ellipsoid
 
  L <- long2-long1 # difference in longitude
  U1 <- atan((1-f) * tan(lat1)) # reduced latitude
  U2 <- atan((1-f) * tan(lat2)) # reduced latitude
  sinU1 <- sin(U1)
  cosU1 <- cos(U1)
  sinU2 <- sin(U2)
  cosU2 <- cos(U2)
 
  cosSqAlpha <- NULL
  sinSigma <- NULL
  cosSigma <- NULL
  cos2SigmaM <- NULL
  sigma <- NULL
 
  lambda <- L
  lambdaP <- 0
  iterLimit <- 100
  while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
    sinLambda <- sin(lambda)
    cosLambda <- cos(lambda)
    sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                      (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
    if (sinSigma==0) return(0)  # Co-incident points
    cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
    sigma <- atan2(sinSigma, cosSigma)
    sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
    cosSqAlpha <- 1 - sinAlpha*sinAlpha
    cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
    if (is.na(cos2SigmaM)) cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
    C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
    lambdaP <- lambda
    lambda <- L + (1-C) * f * sinAlpha *
              (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    iterLimit <- iterLimit - 1
  }
  if (iterLimit==0) return(NA)  # formula failed to converge
  uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
  A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
  B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
  deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                                      B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
  s <- b*A*(sigma-deltaSigma) / 1000
 
  return(s) # Distance in km
}

deg2rad <- function(deg) return(deg*pi/180)

check1 <- wc_data[1, c('set_lat', 'set_long', 'up_lat', 'up_long')]
check <- sapply(check1, FUN = deg2rad)

gcd.slc(check[2], -check[1], check[4], -check[3])
gcd.vif(check[2], -check[1], check[4], -check[3])
gcd.hf(check[2], -check[1], check[4], -check[3])


check[1]




deg2hms(47.7517, sep = ':')


apply(check, FUN = deg2hms, MARGIN = 1)

lon2 <- -check$up_long
lon1 <- -check$set_long
lat2 <- check$up_lat
lat1 <- check$set_lat

dlon <- lon2 - lon1
dlat <- lat2 - lat1

aa <- (sin(dlat / 2)) ^ 2 + cos(lat1) * cos(lat2) * (sin(dlon / 2)) ^ 2
cc <- 2 * atan2(sqrt(aa), sqrt(1 - aa))
3961 * cc

#4.744 miles, 7.633 km
4.744 / 1.83
4.744 / (110 / 60)



#Haversine Distance Formula
diff_long <- wc_data$up_long - wc_data$set_long
diff_lat <- wc_data$up_lat - wc_data$set_lat
aa <- (sin(diff_lat / 2)) ^ 2 + cos(-wc_data$set_long) * cos(-wc_data$up_long) * 
  (sin(diff_long / 2)) ^ 2
cc <- 2 * atan2(sqrt(aa), sqrt(1 - aa))
dd <- 


a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
d = R * c (where R is the radius of the Earth)





#convert longitudes to be in latitude units (Branch et al. 2005)
which(is.na(wc_data$set_long))
test_set_long <- wc_data$set_long * cos((wc_data$set_lat * pi) / 180)
test_up_long <- wc_data$up_long * cos((wc_data$up_lat * pi) / 180)


hist(log(wc_data$duration_min))




# #Training data set
# train <- head(wc_data, n = 50)
# train <- train[, c('towdate','set_time', 'up_time')]
# train <- plyr::rename(train, c('towdate' = 'set_date'))
# train$set_date <- dmy(train$set_date)
# train$up_date <- train$set_date

# # train[which(train$set_time >= train$up_time), ]

# train[which(train$set_time >= train$up_time), 'up_date'] <- train[which(train$set_time >= train$up_time), 'up_date'] + days(1)

# train$set_time <- as.character(train$set_time)
# train$up_time <- as.character(train$up_time)

# train$up_time <- paste(substr(train$up_time, nchar(train$up_time) - 3, nchar(train$up_time) - 2),
#                        substr(train$up_time, nchar(train$up_time) - 1, nchar(train$up_time)), sep = ':')

# train[which(nchar(train$up_time) == 3), 'up_time'] <- paste0('0', 
#   train[which(nchar(train$up_time) == 3), 'up_time'])

# train$set_time <- paste(substr(train$set_time, nchar(train$set_time) - 3, nchar(train$set_time) - 2),
#                        substr(train$set_time, nchar(train$set_time) - 1, nchar(train$set_time)), sep = ':')

# train[which(nchar(train$set_time) == 3), 'set_time'] <- paste0('0', 
#   train[which(nchar(train$set_time) == 3), 'set_time'])

# train$set_date_full <- ymd_hm(paste(train$set_date, train$set_time))
# train$up_date_full <- ymd_hm(paste(train$up_date, train$up_time))

# train$duration_min <- interval(train$set_date_full, train$up_date_full) / dminutes(1)
# train$duration_hour <- interval(train$set_date_full, train$up_date_full) / dhours(1)



# write.csv(train, row.names = FALSE, file = '/Users/peterkuriyama/Desktop/check.csv')


# train$up_time <- (paste(substr(train$up_time, nchar(train$up_time) - 3, nchar(train$up_time) - 2),
#           substr(train$up_time, nchar(train$up_time) - 1, nchar(train$up_time)), sep = ':'))









nchar(train$set_time)
nchar(train$up_time)

nchar(train$set_time)
nchar(train$set_time) - 1
nchar(train$set_time) - 1

train[which(train$set_time >= train$up_time), 'up_date'] + days(1)

train[which(train$set_time >= train$up_time])
dmy(train$towdate)
train[which(train$set_time >= train$up_time), ]



train$set_hour <- substr(train$set_time, nchar(train$set_time) - 3, nchar(train$set_time) - 2)
train$set_minute <- substr(train$set_time, nchar(train$set_time) - 1, nchar(train$set_time))
train$up_hour <- substr(train$up_time, nchar(train$up_time) - 3, nchar(train$up_time) - 2)
train$up_minute <- substr(train$up_time, nchar(train$up_time) - 1, nchar(train$up_time))
train$set_time2 <- paste(train$set_hour, train$set_minute, sep = ':')
train$up_time2 <- paste(train$up_hour, train$up_minute, sep = ':')

time(train$set_time2)

train$set_time <- hms(train$set_time)
train$up_time <- as.character(train$up_time)



nchar(train$set_time)
nchar(train$up_time)



train <- train[, c('set_time', 'up_time')]





xx[] <- lapply(train, FUN = function(x) if(nchar(x[1]) == 3) paste0('0', x[1]))

if(nchar(train$set_time) == 3) paste0('0', train$set_teime)

nchar(train$set_time)

paste(substr(train$set_time, 1, 2), substr(train$set_time, 3, 4), sep = ":")

train$dur <- duration(train$up_time - train$set_time, 'minutes')

interval(start = train$up_time, end = train$set_time)


temp <- subset(wc_data, drvid == "213184")

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
