#Fishing Styles, per Boonstra and Hentati-Sundberg

#--------------------------------------------------------------------------------
#Format West Coast Map
world_map <- map_data("world")
wc_map <- states_map[states_map$region %in% c('USA', 'Canada'), ]
wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')

#--------------------------------------------------------------------------------
#VMS Analysis
load("data/nw_vms.Rdata")

#--------------------------------------------------------------------------------
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

#--------------------------------------------------------------------------------
#Filter our outliers
#Filter out VMS data that are too far away from the US West Coast
outlier_ind <- which(nw.vms$longitude > -115 | nw.vms$longitude < -130)

# length(outlier_ind) / nrow(nw.vms) * 100 #17% of data is in alaska

outliers <- nw.vms[outlier_ind, ]
nw.vms <- nw.vms[-outlier_ind, ]

##Outliers
# wc_map + geom_point(data = outliers, aes(x = lon, y = lat)) 

##IFQS
#Some of the outliers are fished on shorebased IFQ
# ifqs <- outliers[which(outliers$sector_desc %in% unique(outliers$sector_desc)[c(2, 3, 6)]), ]

# png(width = 12.9, height = 7.9, res = 200, file = 'figs/outlier_vms.png', units = 'in')
# print(wc_map + scale_x_continuous(limits = c(-180, -117)) + 
#       scale_y_continuous(limits = c(32, 60)) + 
#       geom_point(data = ifqs, aes(x = lon, y = lat)) + facet_grid(~ sector_desc))
# dev.off()

# wc_map + scale_x_continuous(limits = c(-180, -117)) + 
#       scale_y_continuous(limits = c(32, 60)) + 
#       geom_point(data = ifqs, aes(x = lon, y = lat)) + facet_grid(~ sector_desc)

#OK to remove the outliers
#--------------------------------------------------------------------------------
#Filter and Change the names of the objects
#Note that nw_vms is now filtered to remove outliers, nw.vms still has outliers      
nw_vms_unfilt <- nw.vms
nw_vms <- nw_vms_unfilt[-outlier_ind, ]

#Drop lat and lon columns because they're wrong
nw_vms$lat <- NULL
nw_vms$lon <- NULL
nw_vms$speed <- as.numeric(nw_vms$speed)

#Also make sure to get rows with tow speed between 2 and 6

#--------------------------------------------------------------------------------
#Axes to look at:
#Speed, sector, month, year, 

nw_vms %>% group_by(vessel_name, year) %>% summarise(nsector = length(unique(sector_desc))) %>%
  as.data.frame -> vess_sector

vess_sector %>% filter(nsector == 2)

#Pretty much every vessel fished in one sector with the exception of a couple
#grumpy j, isl
grump <- nw_vms %>% filter(vessel_name == 'Grumpy J' & speed != 0)

# to_plot <- grump %>% filter(speed != 0 & sector_desc == unique(grump$sector_desc)[2])

wc_map + scale_x_continuous(limits = range(grump$longitude)) + 
  scale_y_continuous(limits = range(grump$latitude)) + 
  geom_point(data = grump, aes(x = longitude, y = latitude)) +
  facet_grid(month ~ year)

#--------------------------------------------------------------------------------
#Calculate centroid of distributions for each vessel to quantify

#Calculate this by year
nw_vms %>% group_by(year, vessel_name, sector_desc) %>% summarise(lon_mean = mean(longitude, na.rm = TRUE),
  lat_mean = mean(latitude, na.rm = TRUE), lon_med = median(longitude, na.rm = TRUE), 
  lat_med = median(latitude, na.rm = TRUE)) %>% as.data.frame -> nw_vms_centroid
nw_vms_centroid$year <- as.numeric(nw_vms_centroid$year)

#Fit linear model to find the characterize average changes in effort by sector and vessel
nw_vms_centroid %>% 
  group_by(sector_desc, vessel_name) %>% do({
    mod_lon <- lm(lon_mean ~ year, data = .)
    slope_lon <- coef(mod_lon)[2]
    names(slope_lon) <- NULL

    mod_lat <- lm(lat_mean ~ year, data = .)
    slope_lat <- mod_lat$coefficients[2]
    names(slope_lat) <- NULL
    data.frame(., slope_lon, slope_lat)
  }) %>% as.data.frame -> slopes


#Evaluate how to incorporate the slopes into maps 
slopes[, c("slope_lon", "slope_lat")] %>% distinct() -> ss
plot(ss$slope_lon, ss$slope_lat, pch = 19, col = "#4D4D4D20")
abline(v = 0)
abline(h = 0)
hist(ss$slope_lon, breaks = 30)
hist(ss$slope_lat, breaks = 30)
hist(unique(slopes$slope_lon))



#Create plot that has raw points and the averaged points
alex <- subset(nw_vms, vessel_name == 'Alex (Faria)' & speed)
temp <- subset(slopes, vessel_name == "Alex (Faria)")

wc_map + geom_point(data = temp, aes(x = lon_mean, y = lat_mean, colour = year)) +
  geom_path(data = temp, aes(x = lon_mean, y = lat_mean), arrow = arrow()) + 
  scale_x_continuous(lim = c(-125, -122)) + scale_y_continuous(lim = c(39, 42))




hist(subset(grump, month == '09' & year == 2014)$latitude, density = TRUE)

grump %>% group_by(month, year) %>% summarise(lon_center = mean(longitude, na.rm = TRUE), 
  lat_center = mean(latitude, na.rm = TRUE)) %>% as.data.frame -> temp 

wc_map + geom_point(data = temp, aes(x = lon_center, y = lat_center, colour = year)) +
  facet_wrap(~ month) + geom_path(data = temp, aes(x = lon_center, y = lat_center), arrow = arrow()) + 
  scale_x_continuous(lim = c(-127, -124)) + scale_y_continuous(lim = c(40, 50))

#Latitude
ggplot(temp, aes(x = year, y = lat_center, colour = month)) + geom_point() + 
  geom_line(aes(x = year, y = lat_center, group = month))

ggplot(temp, aes(x = month, y = lat_center, colour = year)) + geom_point() + 
  geom_line(aes(x = month, y = lat_center, group = year))

#Longitude
ggplot(temp, aes(x = year, y = lon_center, colour = month)) + geom_point() + 
  geom_line(aes(x = year, y = lon_center, group = month))

ggplot(temp, aes(x = month, y = lon_center, colour = year)) + geom_point() + 
  geom_line(aes(x = month, y = lon_center, group = year))

#Fit linera model to latitude changes over time

grump %>% filter(month == '09' & year == 2014) %>%
  summarise(lat_center = mean(latitude, na.rm = TRUE))



#--------------------------------------------------------------------------------
#Look at paths of specific vessels
speeds <- subset(nw_vms, speed >= 2 & speed <= 6)

vess <- unique(speeds$vessel_name)

col2rgb(c('grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10', 'black'),
  alpha = TRUE)

grays <- c(rgb(t(col2rgb('grey60')), maxColorValue = 255),
      rgb(t(col2rgb('grey50')), maxColorValue = 255),
      rgb(t(col2rgb('grey40')), maxColorValue = 255),
      rgb(t(col2rgb('grey30')), maxColorValue = 255),
      rgb(t(col2rgb('grey20')), maxColorValue = 255),
      rgb(t(col2rgb('grey10')), maxColorValue = 255),
      rgb(t(col2rgb('black')), maxColorValue = 255))

grays <- sapply(grays, FUN =  function(x) paste(x, '66', sep = ''))


pdf(width = 15, height = 9.5, units = 'in', file = 'figs/test.png')
for(ii in 1:2){
  temp <- subset(speeds, vessel_name == vess[ii])
  xlims_temp <- range(temp$longitude)
  xlims <- c(floor(xlims_temp[1]), ceiling(xlims_temp[2]))
  ylims_temp <- range(temp$latitude)
  ylims <- c(floor(ylims_temp[1]), ceiling(ylims_temp[2]))


  
    wc_map + scale_x_continuous(limits = xlims) + 
    scale_y_continuous(limits = ylims) + 
      geom_point(data = belle, aes(x = longitude, y = latitude, group = year,
        color = year)) + 
      facet_wrap(~ month) + scale_colour_manual(limits = seq(2008, 2014), 
        values = grays) + 
      theme_bw()


      scale_colour_gradient(low = 'white', high = 'red')
}


scale_colour_manual(limits = c(6, 8, 4), breaks = c(8, 4, 6),
  values = c("grey50", "grey80", "black"))

belle <- subset(speeds, vessel_name == 'Belle (Faria)')
wc_map + scale_x_continuous(limits = c(-126, -123)) + scale_y_continuous(limits = c(46, 49)) + 
  geom_point(data = belle, aes(x = longitude, y = latitude, group = year,
    color = year)) + 
  facet_wrap(~ month)



holy_bull <- subset(nw_vms, vessel_name == 'Holy Bull')
wc_map + scale_x_continuous(limits = c(-125, -123)) + scale_y_continuous(limits = c(40, 42)) + 
  geom_point(data = holy_bull, aes(x = longitude, y = latitude, group = year,
    color = year)) + 
  facet_wrap(~ month)



hist(nw_vms$latitude)
hist(nw_vms$longitude)
range(nw_vms$longitude)

check <- nw_vms[which(nw_vms$longitude < -130), ]

wc_map + scale_x_continuous(limits = c(-140, -117)) + 
      scale_y_continuous(limits = c(32, 60)) + 
      geom_point(data = check, aes(x = longitude, y = latitude))





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

