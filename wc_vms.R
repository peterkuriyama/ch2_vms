#Fishing Styles, per Boonstra and Hentati-Sundberg


#VMS Analysis

load("data/nw_vms.Rdata")

#Add in unadjusted latitude and longitude points
# nw.vms <- head(nw.vms)
nw.vms$latitude <- gsub("\\.", "\\,", nw.vms$latitude)
nw.vms$latitude <- gsub("\\\260", ".", nw.vms$latitude)
nw.vms$latitude <- gsub("\\,", "", nw.vms$latitude)

nw.vms$longitude <- gsub("\\.", "\\,", nw.vms$longitude)
nw.vms$longitude <- gsub("\\\260", ".", nw.vms$longitude)
nw.vms$longitude <- gsub("\\,", "", nw.vms$longitude)

nw.vms$longitude <- as.numeric(nw.vms$longitude)
nw.vms$latitude <- as.numeric(nw.vms$latitude)

#Filter out VMS data that are too large
outlier_ind <- which(nw.vms$longitude > 0 | nw.vms$longitude < -140)

length(outlier_ind) / nrow(nw.vms) * 100 #17% of data is in alaska
outliers <- nw.vms[outlier_ind, ]
nw.vms <- nw.vms[-outlier_ind, ]

#Some of the outliers are fished on shorebased IFQ
ifqs <- subset(outliers, outliers %in% unique(outliers$sector_desc[c(2, 3, 6)]))




outliers <- (nw.vms[which(nw.vms$longitude > 0), ])


nrow(outliers) / nrow(nw.vms) * 100


#Filter out VMS data from Alaska
ak_index <- which(nw.vms$longitude < -140)



hist(nw.vms$longitude, breaks = 30)
hist(nw.vms[ak_index, 'longitude'])
length(ak_index)


ak_vms <- 
nw.vms[which(nw.vms$longitude < -130)]

ak




states_map <- map_data("state")
wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington'), ]


temp <- subset(nw.vms, year == "2012" & month == "02")




temp$latitude <- as.numeric(temp$latitude)
temp$longitude <- as.numeric(temp$longitude)

ggplot() + geom_map(data = wc_map, map = wc_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = wc_map, aes(x = long, y = lat), fill = NA, color = 'gray') + 
    # coord_cartesian(xlim = c(-125, -117.3)) + 
    geom_point(data = temp, aes(x = longitude, y = latitude))

