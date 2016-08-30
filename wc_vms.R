#Fishing Styles, per Boonstra and Hentati-Sundberg
setwd('/Users/peterkuriyama/School/Research/ch2_vms')

source("ch2_vms_main.R")
#--------------------------------------------------------------------------------
#Format West Coast Map

#--------------------------------------------------------------------------------
#VMS Analysis
load("data/nw_vms.Rdata")

#--------------------------------------------------------------------------------
#Add in unadjusted latitude and longitude points
# wc.vms <- head(wc.vms)
wc.vms <- nw.vms
rm(nw.vms)
wc.vms$latitude <- gsub("\\.", "\\,", wc.vms$latitude)
wc.vms$latitude <- gsub("\\\260", ".", wc.vms$latitude)
wc.vms$latitude <- gsub("\\,", "", wc.vms$latitude)

wc.vms$longitude <- gsub("\\.", "\\,", wc.vms$longitude)
wc.vms$longitude <- gsub("\\\260", ".", wc.vms$longitude)
wc.vms$longitude <- gsub("\\,", "", wc.vms$longitude)

wc.vms$longitude <- as.numeric(wc.vms$longitude)
wc.vms$latitude <- as.numeric(wc.vms$latitude)

#--------------------------------------------------------------------------------
#Filter our outliers
#Filter out VMS data that are too far away from the US West Coast
outlier_ind <- which(wc.vms$longitude > -115 | wc.vms$longitude < -130)
# length(outlier_ind) / nrow(wc.vms) * 100 #17% of data is in alaska

outliers <- wc.vms[outlier_ind, ]
wc.vms <- wc.vms[-outlier_ind, ]

#OK to remove the outliers
#--------------------------------------------------------------------------------
#Filter and Change the names of the objects
#Note that wc_vms is now filtered to remove outliers, wc.vms still has outliers      
wc_vms_unfilt <- wc.vms
wc_vms <- wc_vms_unfilt[-outlier_ind, ]

#Drop lat and lon columns because they're wrong
wc_vms$lat <- NULL
wc_vms$lon <- NULL
wc_vms$speed <- as.numeric(wc_vms$speed)

#Also make sure to get rows with tow speed between 2 and 6
sum(is.na(wc_vms$speed)) / nrow(wc_vms)
wc_vms %>% filter(speed >= 2 & speed <= 6) -> wc_vms_fish #Names to have values that 
  #are associated with actual tows. 

#Look at proportion of tows in each sector
unique(wc_vms_fish$sector_desc)
wc_vms_fish %>% group_by(sector_desc) %>% summarize(perc = length(latitude) / 
  nrow(wc_vms_fish))

#Look at proportion of tows in each vessel
wc_vms_fish %>% group_by(vessel_name) %>% summarize(perc = length(latitude) / 
  nrow(wc_vms_fish)) %>% ggplot() + geom_histogram(aes(x = perc)) + theme_bw()

#--------------------------------------------------------------------------------
#Look at trend in number of vessels overall
wc_vms_fish %>% group_by(year) %>% summarize(nvess = length(unique(vessel_name)))

#What was proportion of vessel VMS Coverage?
#Is there any way to combine the vessel locations to see if they match with the other records?

#Read in vessel records from 2011 - 2014
vess11 <- read.csv('data/vessels2011.csv', stringsAsFactors = FALSE)
vess12 <- read.csv('data/vessels2012.csv', stringsAsFactors = FALSE)
vess13 <- read.csv('data/vessels2013.csv', stringsAsFactors = FALSE)
vess14 <- read.csv('data/vessels2014.csv', stringsAsFactors = FALSE)

wc_vess <- rbind(vess11, vess12, vess13, vess14)
names(wc_vess) <- tolower(names(wc_vess))
wc_vess$vessel <- tolower(wc_vess$vessel)

wc_vess %>% group_by(quota.year) %>% summarize(nvess = length(unique(vessel)))
#So there are like 100 allocated vessels

wc_vess %>% group_by(vessel, quota.year) %>% summarize(tc = sum(qp.balance)) %>% 
  filter(tc <= 0) %>% as.data.frame %>% group_by(quota.year) %>% 
  summarize(nvess = length(unique(vessel)))
  
#So there should be records from about 130 vessels and we only have records 
  #for about 50

#--------------------------------------------------------------------------------
#Bin the data, maybe bin by sector also

#Add in number of tows and number of sectors for each vessel and year combination
wc_vms_fish %>% group_by(vessel_name, year) %>% mutate(ntows = length(speed)) %>% 
  group_by(vessel_name) %>% mutate(nsect = length(unique(sector_desc))) %>% 
  as.data.frame %>% arrange(desc(ntows)) -> wc_vms_fish
   
#Only Grumpy J, Island Enterprise, and Lisa Melinda fished in two sectors

#------------------------------------------------------------
#Visualize individual vessel bins

#Look at Pacific Future for example
vess <- subset(wc_vms_fish, vessel_name == 'Pacific Future')

#Remove NA locations,
xlims <- range(vess$longitude, na.rm = TRUE)
xlims <- c(floor(xlims[1]), ceiling(xlims[2]))

ylims <- range(vess$latitude, na.rm = TRUE)
ylims <- c(floor(ylims[1]), ceiling(ylims[2]))

wc_map + scale_x_continuous(limits = xlims) + scale_y_continuous(limits = ylims) + 
  geom_point(data = vess, aes(x = longitude, y = latitude, colour = month)) + 
  facet_wrap(~ year) 

###Write function to process binned data
#Bin by number of tows
#Bin by number of start points, mid points, end points

#Function to bin VMS points by year
wc_vms_fish$year <- as.numeric(wc_vms_fish$year)

bin_by_year <- function(data = wc_vms_fish, bw = c(0.0909, 0.11)){
  
  bh <- ggplot(data, aes(x = longitude, y = latitude, group = year)) +
    stat_bin2d(binwidth = bw)

  binned <- ggplot_build(bh)$data[[1]]
  binned$unq <- paste(binned$xbin, binned$ybin)
  binned$id <- 1:nrow(binned)

  yrz <- data.frame(group = unique(binned$group), 
                    year = unique(data$year)[order(unique(data$year))])
  binned <- inner_join(binned, yrz, by = 'group')  

  return(binned)
}

#Bin the data by year
binned <- bin_by_year(data = wc_vms_fish)

#Find number of tows in each year
wc_vms_fish %>% group_by(year) %>% summarize(ntows = length(year))

#filter to have years 2008, 2009, 2010 as before and 2011, 2012, 2013 as after
#names for before and after
binned <- binned %>% filter(year >= 2008 & year <= 2013)
binned$when <- "before"
binned[binned$year >= 2011, 'when'] <- 'after'

#Fill in zeroes for areas that don't have tows in all years
binned %>% group_by(unq) %>% mutate(nyear = length(unique(year))) %>%  
  filter(nyear == 6) %>% as.data.frame -> high_sites

binned %>% group_by(unq) %>% mutate(nyear = length(unique(year))) %>%  
  filter(nyear < 6) %>% as.data.frame -> low_sites

#Low sites are sites that don't have 6 years of data
exp_sites <- vector('list', length(unique(low_sites$unq)))

yrz <- 2008:2013

for(ii in 1:length(unique(low_sites$unq))){
  if(ii %% 100 == 0) print(ii)
  zz <- subset(low_sites, unq == unique(low_sites$unq)[ii])
  missing <- which(yrz %in% zz$year == FALSE)
  to_add <- zz[rep(which(zz$year %in% yrz)[1], 
      length(missing)), ]
  to_add$year <- yrz[missing]
  to_add$count <- 0
  to_add$density <- 0

  out <- rbind(zz, to_add)
  out <- out[order(out$year), ]
  exp_sites[[ii]] <- out
}

aa <- ldply(exp_sites)




binned %>% filter(unq == '100 26')

binned %>% group_by(unq) %>% do({
  if(length(unique(year)) < 6) print('poop')
})



if(length(unique(year)) < 6) print('poop')



binned %>% distinct(unq, when, avg_count) -> aa

dcast(aa, unq ~ when)

dcast(binned, year ~ when)


#Calculate average number of tows before and after

binned %>% group_by(unq, when) %>% mutate(avg_count = mean(count)) %>% 
  as.data.frame -> binned

binned


binned %>% group_by(year) %>% summarize(ntows = length(year)) 


#look at trends in VMS points in each site.
ggplot(binned) + g(aes(x = year, y = count, group = unq)) + theme_bw()


#Function to plot the tiles faceted by year
plot_bin2d <- function(input){

  browser()
  xlims <- range(input$x)
  xlims <- c(floor(xlims[1]), ceiling(xlims[2]))

  ylims <- range(input$y)
  ylims <- c(floor(ylims[1]), ceiling(ylims[2]))

  wc_map + geom_tile(data = input, aes(x = x, y = y, fill = count)) + 
    scale_fill_gradient2(low = 'blue', high = 'red') + 
    scale_x_continuous(limits = xlims) + scale_y_continuous(limits = ylims) + 
    facet_wrap(~ year)

  print('done')
}



plot_bin2d(input = binned)

wc_plot + geom_tile(data = subset(diff_plot_ba, legal == 'yes'), aes(x = x,
  y = y, fill = diff)) + scale_fill_gradient2(low = 'blue', high = 'red') + 
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_rect(fill = 'white'))      



bh <- ggplot(wc_data_unique, aes(x = -long, y = lat, group = tow_year)) + 
  stat_bin2d(binwidth = c(.0909, .11))





#Quick look at these
wc_vms_fish
wc_vms_fishing %>% summarize()


#--------------------------------------------------------------------------------
#Axes to look at:
#Speed, sector, month, year, 

wc_vms %>% group_by(vessel_name, year) %>% summarise(nsector = length(unique(sector_desc))) %>%
  as.data.frame -> vess_sector

vess_sector %>% filter(nsector > 1)


#Look at vessels south of morro bay
wc_vms %>% filter(latitude < 36 & latitude > 34.5) -> cen_cal

#Find most active vessels
cen_cal %>% group_by(vessel_name) %>% mutate(nyears = length(unique(year))) %>% 
  as.data.frame -> cen_cal

wc_map + ggplot() + geom_point(aes(x = longitude, y = latitude, colour = vessel_name)) + 
  facet_wrap(~ year)


cen_cal %>% group_by()


unique(south$vessel_name)


#Pretty much every vessel fished in one sector with the exception of a couple


#grumpy j, isl
grump <- wc_vms %>% filter(vessel_name == 'Grumpy J' & speed != 0)

# to_plot <- grump %>% filter(speed != 0 & sector_desc == unique(grump$sector_desc)[2])




wc_map + scale_x_continuous(limits = range(grump$longitude)) + 
  scale_y_continuous(limits = range(grump$latitude)) + 
  geom_point(data = grump, aes(x = longitude, y = latitude)) 
+
#   facet_grid(month ~ year)

#--------------------------------------------------------------------------------
#Calculate centroid of distributions for each vessel to quantify

#Calculate this by year
wc_vms %>% group_by(year, vessel_name, sector_desc) %>% summarise(lon_mean = mean(longitude, na.rm = TRUE),
  lat_mean = mean(latitude, na.rm = TRUE), lon_med = median(longitude, na.rm = TRUE), 
  lat_med = median(latitude, na.rm = TRUE)) %>% as.data.frame -> wc_vms_centroid
wc_vms_centroid$year <- as.numeric(wc_vms_centroid$year)

#Fit linear model to find the characterize average changes in effort by sector and vessel
wc_vms_centroid %>% 
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
alex <- subset(wc_vms, vessel_name == 'Alex (Faria)' & speed)
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
speeds <- subset(wc_vms, speed >= 2 & speed <= 6)

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



holy_bull <- subset(wc_vms, vessel_name == 'Holy Bull')
wc_map + scale_x_continuous(limits = c(-125, -123)) + scale_y_continuous(limits = c(40, 42)) + 
  geom_point(data = holy_bull, aes(x = longitude, y = latitude, group = year,
    color = year)) + 
  facet_wrap(~ month)


#--------------------------------------------------------------------------------
##SCRAPS

# hist(wc_vms$latitude)
# hist(wc_vms$longitude)
# range(wc_vms$longitude)

# check <- wc_vms[which(wc_vms$longitude < -130), ]

# wc_map + scale_x_continuous(limits = c(-140, -117)) + 
#       scale_y_continuous(limits = c(32, 60)) + 
#       geom_point(data = check, aes(x = longitude, y = latitude))





# outliers <- (wc.vms[which(wc.vms$longitude > 0), ])


# nrow(outliers) / nrow(wc.vms) * 100


# #Filter out VMS data from Alaska
# ak_index <- which(wc.vms$longitude < -140)



# hist(wc.vms$longitude, breaks = 30)
# hist(wc.vms[ak_index, 'longitude'])
# length(ak_index)


# ak_vms <- 
# wc.vms[which(wc.vms$longitude < -130)]

# ak




# states_map <- map_data("state")
# wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington'), ]


# temp <- subset(wc.vms, year == "2012" & month == "02")




# temp$latitude <- as.numeric(temp$latitude)
# temp$longitude <- as.numeric(temp$longitude)

# ggplot() + geom_map(data = wc_map, map = wc_map, aes(x = long, y = lat, 
#     map_id = region), fill = 'gray') + 
#     geom_polygon(data = wc_map, aes(x = long, y = lat), fill = NA, color = 'gray') + 
#     # coord_cartesian(xlim = c(-125, -117.3)) + 
#     geom_point(data = temp, aes(x = longitude, y = latitude))

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
