###VMS Stuff
#--------------------------------------------------------------------------------
#To Do
#Expand tow footprints to get a sense of overall effort rather than set/up points
#Remove points with only 
#NE ALSO

#--------------------------------------------------------------------------------
#Start
setwd('/Users/peterkuriyama/School/Research/ch2_vms')

library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

source('R/dist_funcs.r')

#--------------------------------------------------------------------------------
#Load expanded West Coast Data
load('output/wc_data_expanded_tows.Rdata')

#Seems to be some duplicated rows, given lat and long
wc_data %>% group_by(drvid, trip_id, haul_id, lat, long) %>% filter(row_number() == 1) %>%
  as.data.frame -> wc_data_unique


#--------------------------------------------------------------------------------
#Source Functions to plot things
funcs <- list.files('R')

source('R/ch2_load_and_format.r')

#Load Functions
for(ii in 1:length(funcs)){
  source(paste0("R/", funcs[ii]))
}

sapply(funcs, FUN = function(x) source(paste0('R/', x)))

#Takes 45 seconds to run
# wc_data <- ch2_load_and_format()
#load wc_data that has already been run
# load('output/wc_data.Rdata')

#--------------------------------------------------------------------------------
#Plot distribution of effort in each area

#bin values I think to 10km by 10km cells
bh <- ggplot(wc_data_unique, aes(x = -long, y = lat, group = tow_year)) + 
  stat_bin2d(binwidth = c(.0909, .11))

binned <- ggplot_build(bh)$data[[1]]
binned$unq <- paste(binned$xbin, binned$ybin)
binned$id <- 1:nrow(binned)

binned %>% rowwise() %>% (function(x) rep(x$id, x$value)) ->
  ids

#expanded binned data frame
#repeats
binned_reps <- binned[ids, ]

#Use states_map to assign states to each coordinate
binned_reps$state <- 1
binned_reps[which(binned_reps$ymax <= 42.02073), 'state'] <- 'california'
binned_reps[which(binned_reps$ymax <= 46.22623 & 
                  binned_reps$ymin >= 42.00354), 'state'] <- 'oregon'

binned_reps[which(binned_reps$ymin >= 45.5329622623), 'state'] <- 'washington'

#evaluate changes in spatial effort changes in specific states
cal <- subset(binned_reps, state == 'california') 
ggplot(cal, aes(x = group, y = value, group = unq)) + geom_line()

ore <- subset(binned_reps, state == 'oregon')
ggplot(ore, aes(x = group, y = value, group = unq)) + geom_line()

wa <- subset(binned_reps, state == 'washington')
ggplot(wa, aes(x = group, y = value, group = unq)) + geom_line()

ggplot(binned_reps, aes(factor(unq))) + geom_bar() + facet_wrap(~ group)

##Fit linear regression to each unique area over time
#Then can group plots by areas with increased / decreased effort

#add number of years to binned reps
binned_reps %>% group_by(unq) %>% mutate(nyears = length(unique(group))) %>%
  as.data.frame -> binned_reps

#Try this with a handful of unq values
test <- binned_reps[which(binned_reps$unq %in% unique(binned_reps$unq)[1:10]), ]

test %>% filter(nyears > 1) %>% distinct %>% 
  group_by(unq) %>%
  do({
    mod <- lm(count ~ group, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
  }) %>% as.data.frame -> test


#Now apply this code to the whole data set
binned_reps %>% filter(nyears > 1) %>% distinct %>% 
  group_by(unq) %>%
  do({
    mod <- lm(count ~ group, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
  }) %>% as.data.frame -> binned_reps

#--------------------------------------------------------------------------------
hist(binned_reps$slope, breaks = 50)
binned_reps$unq1 <- paste(binned_reps$unq, binned_reps$group)

#Plots of places with positive slopes in effort
binned_reps %>% filter(slope > 0) %>% ggplot(., aes(x = group, y = count,
  group = unq)) + 
  facet_wrap(~ state) + geom_line()

#plot places that had fishing in all six years
binned_reps %>% filter(nyears == 6) %>% ggplot(aes(x = group, y = count, 
  group = unq)) + facet_wrap(~ state) + geom_line()

#polygon plots
which(duplicated(binned_reps$unq1))

binned_reps %>% filter(nyears == 6) -> all_six
ggplot(all_six, aes(x = x, y = y, fill = density)) + geom_tile() + 
  facet_wrap(~ group) + scale_fill_gradient2(low = 'blue', high = 'red')

#what proportions had increases/decreases in trawl activity?
#Add column to specify increasing/decreasing/constant
binned_reps$trend <- 'constant'
binned_reps[which(binned_reps$slope > 0), 'trend'] <- 'increasing'
binned_reps[which(binned_reps$slope < 0), 'trend'] <- 'decreasing'
#20% had positive slopes
#78% had negative slopes
#1% had slopes of zero

#Facet by trend
binned_reps %>% filter(nyears == 6) %>% 
  ggplot(aes(x = group, y = count, group = unq, colour = slope)) + geom_line() + 
  theme_bw() + facet_wrap(~ trend + state) + scale_fill_gradient2(low = 'blue', high = 'red')

#Try visualizing this a different way
binned_reps %>% filter(nyears == 6 & state == 'washington') %>% 
  ggplot(aes(x = group, y = count, group = unq, colour = slope)) + geom_line() + 
  theme_bw()





#--------------------------------------------------------------------------------

#Find locations that had the lowest decreases in effort
big_declines <- binned_reps[which(binned_reps$slope < -20), ]



#--------------------------------------------------------------------------------
#Create plots of aggregated fleet effort
source('R/aggregated_effort.r')

#--------------------------------------------------------------------------------
#Estimate kernel densities for each year

#Smoothed Densities, takes the most time to plot
# dens <- ggplot(wc_data, aes(x = -set_long, y = set_lat, group = tow_year)) + 
#   stat_density2d(aes(fill = ..level..), geom = 'polygon') 

# dens + facet_wrap(~ tow_year)

#--------------------------------------------------------------------------------
#Expand tow points between start and end points
#specify distance between expansion points

expanded <- vector('list', length = nrow(wc_data))

for(ii in 1:nrow(wc_data)){
# for(ii in 1:100){
  # if(sum(is.na(wc_data[ii, c('up_lat', 'up_long', 
  #   'set_lat', 'set_long')])) != 4) next
  tt <- wc_data[ii, c('up_lat', 'set_lat', 'up_long', 'set_long', 'dist_slc_km')]
  
  if(is.na(tt$dist_slc_km)) next
  if(sum(is.na(tt[, c('up_lat', 'up_long', 'set_lat', 'set_long')])) > 0) next
  
  expanded[[ii]] <- expand_tow_track(tt)
  if(ii %% 10000 == 0) print(ii)
# if(ii %% 10 == 0) print(ii)
}

#check to see that expanded tows are within the ranges of each
#row 5
expanded[[5]]
wc_data[5, c('set_lat', 'set_long', 'up_lat', 'up_long')]

#row 300000
expanded[[300000]]
wc_data[300000, c('set_lat', 'set_long', 'up_lat', 'up_long')]

#row 500000
expanded[[200000]]
wc_data[200000, c('set_lat', 'set_long', 'up_lat', 'up_long')]

names(expanded) <- 1:length(expanded)
expanded_df <- plyr::ldply(expanded)

#Expand wc data and add tow points
expanded_df$.id <- as.integer(expanded_df$.id)
idea <- wc_data[expanded_df$.id, ]

idea$lat <- expanded_df$lat
idea$long <- expanded_df$long

#This is the expanded data now...
wc_data <- idea
save(wc_data, file = 'output/wc_data_expanded_tows.Rdata')

#--------------------------------------------------------------------------------

#Try to calculate this in a different way


# extract data from ggplot
binned <- ggplot_build(bh)$data[[1]]

# #create year column in binned
yrz <- data.frame(group = 1:6, year = as.character(2008:2013))
yrz$year <- as.character(yrz$year)
binned <- merge(binned, yrz, by = 'group')

##### 
count_vess <- function(zz){
  wc_data_unique %>% filter(tow_year == zz$year & -long >= zz$xmin &
                       -long <= zz$xmax & lat >= zz$ymin & lat <= zz$ymax) %>% 
    summarise(nvess = length(unique(drvid))) -> out
  return(out$nvess)
}

#
nvess <- rep(999, nrow(binned))

strt <- Sys.time()
#Takes like a minute
for(jj in 1:nrow(binned)){
  #loop over rows in binned
  zz <- binned[jj, ]
  nvess[jj] <- count_vess(binned[jj, ]) #store number of vessels
  
  if(jj %% 500 == 0) cat('index= ', jj, 'time= ', Sys.time() - strt, '\n')
}


#----------------------------------------------------------
#Add the counts to binned
binned$nvess <- nvess
binned$legal <- 'no'
binned[which(binned$nvess >= 3), 'legal'] <- 'yes'
binned %>% group_by(group) %>% mutate(tot_tows = sum(count), perc = count / tot_tows) %>%
  as.data.frame -> binned

#Average before and after
binned$when <- sapply(binned$year, FUN = function(x) ifelse(x >= 2011, 'After', 'Before'))
binned %>% group_by(x, y, xbin, ybin, when) %>% mutate(avg_perc = mean(perc)) %>% 
  as.data.frame -> binned_avg_when
binned_avg_when %>% group_by(x, y, xbin, ybin, when) %>% filter(row_number() == 1) %>% 
  as.data.frame -> binned_avg_when

diff_plot <- reshape2::dcast(binned_avg_when, x + y + xbin + ybin ~ when, value.var = 'avg_perc')
diff_plot$diff <- diff_plot$After - diff_plot$Before

#Merge in diff with binned
diff_plot <- merge(diff_plot, binned[, c('xbin', 'ybin', 'legal')], by = c('xbin', 'ybin'))

binned$when <- factor(binned$when, levels = c('Before', 'After'))

nyears_fished <- binned %>% group_by(x, y) %>% 
  summarise(nyrs_fished = length(is.na(perc)))
nyears_fished$area_id <- 1:nrow(nyears_fished)

binned <- merge(nyears_fished, binned, by = c('x', 'y'), all = FALSE)
binned %>% group_by(x, y, when) %>% mutate(avg_perc = mean(perc, na.rm = TRUE)) %>%
  as.data.frame -> binned
binned$when <- as.factor(binned$when)
reshape2::dcast(binned_avg_when, x + y + xbin + ybin ~ when, value.var = "avg_perc")

#----------------------------------------------------------
#Plot the legal spatial changes on maps
legals <- binned %>% filter(legal == 'yes')

#calculate summary statistics
by_year <- reshape2::dcast(binned, x + y + xbin + ybin ~ year, value.var = 'avg_perc')
by_when <- reshape2::dcast(binned_avg_when, xbin + ybin ~ when, value.var = 'avg_perc')

by_when$status <- 1

by_when[which(is.na(by_when$Before)), 'status'] <- 'new_area'
by_when[which(is.na(by_when$After)), 'status'] <- 'old_area'
by_when[which(by_when$After > by_when$Before), 'status'] <- 'increased'
by_when[which(by_when$After < by_when$Before), 'status'] <- 'decreased'

#Merge statuses into binned data frame
names(by_when)[1:2] <- c('x', 'y')
binned <- merge(binned, by_when[, c('x', 'y', 'status')], by = c('x', 'y'))

#---------------------------------------------------------
#All the Maps
wc_plot <- ggplot() + geom_map(data = wc_map, map = wc_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = wc_map, aes(x = long, y = lat), fill = NA, color = 'gray') + 
    coord_cartesian(xlim = c(-125, -117.3)) 

blanks <- theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank())      )      
    #   , axis.title.x = element_blank(), axis.title.y = element_blank(),
    #   axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
    #   axis.text = element_blank())
    
    # + theme(panel.border = element_blank(),
    #   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #   rect = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
    #   axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
    #   axis.text = element_blank())

#Plot differences instead    
reshape2::dcast(binned, x + y + xbin + ybin ~ when, value.var = 'avg_perc')

#Plot old area figures
png(width = 3.9, height = 7, units = 'in', res = 200,
  file = 'figs/west_coast.png')
wc_plot 
dev.off()


##Diff plot with all values
subset(diff_plot, legal == 'yes')

#Remove NAs
diff_plot_ba <- diff_plot[which(is.na(diff_plot$diff) == FALSE), ]

subset(diff_plot_ba, legal == 'yes')

png(width = 5, height = 7, units = 'in', res = 200, file = 'figs/diff.png')
wc_plot + geom_tile(data = subset(diff_plot_ba, legal == 'yes'), aes(x = x,
  y = y, fill = diff)) + scale_fill_gradient2(low = 'blue', high = 'red') + 
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_rect(fill = 'white'))      
dev.off()

png(width = 4, height = 7, units = 'in', res = 200, file = 'figs/wc_diff.png')
ggplot() + geom_tile(data = legals, aes(x = xbin, y = ybin, fill = avg_perc)) + 
  facet_wrap(~ when) + theme_bw() + scale_fill_gradient(high = 'red', low = 'white') + 
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_rect(fill = 'gray'))      

dev.off()

wc_plot + geom_tile(data = subset(legals, status == 'old_area'),
  aes(x = x, y = y, fill = avg_perc)) + facet_wrap(~ when) + 
  scale_fill_gradient(high = 'red', low = 'white') + blanks

binned %>% filter(status == 'old_area', legal == 'yes') %>% ggplot(aes(x = xbin,
  y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
  scale_fill_gradient(high = 'red', low = 'white')




#Can't show any of the new areas
binned %>% filter(status == 'new_area', legal == 'no') %>% ggplot(aes(x = xbin,
  y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
scale_fill_gradient(high = 'red', low = 'white')

#Areas that Increased
binned %>% filter(status == 'increased', legal == 'yes') %>% ggplot(aes(x = xbin,
  y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
  scale_fill_gradient(high = 'red', low = 'white')

binned %>% filter(status == 'decreased', legal == 'yes') %>% ggplot(aes(x = xbin,
  y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
  scale_fill_gradient(high = 'red', low = 'white')


ggplot(binned, aes(x = xbin, y = ybin))


by_when %>% group_by(status) %>% summarise(n_stats = n(), 
  perc = round(n_stats / 1182 * 100, 2))
# 34% of locations decreased
# 33% of locations had increased percentage of average effort
# 8% of locations were new
# 25% of locations were not revisited




#Find which sites were visited before and after catch shares
sites <- data.frame(before = rowSums(is.na(by_year[, c('2008', '2009', '2010')]) == FALSE), 
                    after = rowSums(is.na(by_year[, c('2011', '2012', '2013')]) == FALSE))
sites$x <- by_year$x
sites$y <- by_year$y
sites$status <- 'poop'

sites[which(sites$before & sites$after != 0), 'status'] <- ''

apply(sites, MAR = 1, FUN = function(x) (which(x == 0)))

sites <- cbind(rowSums(is.na(by_year[, c('2008', '2009', '2010')]) == FALSE),
               rowSums(is.na(by_year[, c('2011', '2012', '2013')]) == FALSE))



#--------------------------------------------------------------------------------
#Estimate percentage of effort in each area annually
####Binned number of sets in each area

# bin <- ggplot(wc_data, aes(x = -long, y = lat, group = tow_year)) + 
#   stat_bin2d(binwidth = c(.0909, .11))
# 
# binned <- ggplot_build(bin)$data[[1]]
# 
# #create year column in binned
# yrz <- data.frame(group = 1:6, year = as.character(2008:2013))
# yrz$year <- as.character(yrz$year)
# binned <- merge(binned, yrz, by = 'group')
# 
# #Calculate number of unique vessels in each bin...
# nvess <- vector(length = nrow(binned))
# start_time <- Sys.time()
# 
# for(ii in 1:nrow(binned)){
# # for(ii in 1:20){
#   rrow <- binned[ii,  ]
# 
#   temp <- subset(wc_data, tow_year == rrow$year)
#   # wc_data %>% filter(tow_year == rrow$year) -> xx
#   
#   keeps <- which(-temp$long >= rrow$xmin & -temp$long <= rrow$xmax &
#              temp$lat >= rrow$ymin & temp$lat <= rrow$ymax)
#   # if(length(keeps) == 0) next
#   if(ii %% 5 == 0) cat('time=',Sys.time() - start_time, 'ii=',ii, '\n')
#     nvess[ii] <- length(unique(temp[keeps, 'drvid']))
# }
# 
# save(nvess, file = 'output/nvess_in_each_bin.Rdata')
# load(file = 'output/nvess_in_each_bin.Rdata')






# 
strt <- Sys.time()
for(jj in 1:nrow(bbn)){
  zz <- bbn[jj, ]
  nnn[jj] <- count_vess(bbn[jj, ])
  wc_data %>% filter(tow_year == zz$year & -long >= zz$xmin &
    -long <= zz$xmax & lat >= zz$ymin & lat <= zz$ymax) %>% 
    summarise(nvess = length(unique(drvid))) -> out
  nnn[jj] <- out$nvess
  if(jj %% 200 == 0) print(jj)
}
edn <- Sys.time()
edn - strt

bbn$nvess <- nnn

bbn$legal <- 'no'
bbn[which(bbn$nvess >= 3), 'legal'] <- 'yes'

bbn %>% group_by(group) %>% mutate(tot_tows = sum(count), perc = count / tot_tows) %>%
  as.data.frame -> bbn

#Average before and after
bbn$when <- sapply(bbn$year, FUN = function(x) ifelse(x >= 2011, 'after', 'before'))
bbn %>% group_by(x, y, when) %>% summarise(avg_perc = mean(perc)) -> bbn_avg_when
bbn$when <- factor(bbn$when, levels = c('before', 'after'))

nyears_fished <- bbn %>% group_by(x, y) %>% 
  summarise(nyrs_fished = length(is.na(perc)))
nyears_fished$area_id <- 1:nrow(nyears_fished)

bbn <- merge(nyears_fished, bbn, by = c('x', 'y'), all = FALSE)
bbn %>% group_by(x, y, when) %>% mutate(avg_perc = mean(perc, na.rm = TRUE)) %>%
  as.data.frame -> bbn
bbn$when <- as.factor(bbn$when)
cc <- dcast(bbn, x + y ~ when, value.var = 'avg_perc')




#Plot percentages spatially
bbn %>% filter(nyrs_fished == 6 & legal == 'yes') %>% ggplot(aes(x = xbin, y = y)) + 
  geom_tile(aes(fill = perc)) + facet_wrap(~ year)

#Plot percentage changes by area
bbn %>% filter(nyrs_fished == 6 & legal == 'yes') %>% 
  ggplot(aes(x = year, y = perc, group = area_id)) + geom_line()
  
  geom_tile(aes(fill = perc)) + facet_wrap(~ year)




#
change_by_year <- dcast(bbn[, c('x', 'y', 'perc', 'year')], x + y ~ year, value.var = 'perc')


#Calculate number of locations with increases, decreases new locations 
# or gone locations by BEFORE or AFTER
change_period <- dcast(bbn_when, x + y ~ when, value.var = "avg_perc")
change_period$status <- 1
change_period[which(is.na(change_period$before)), 'status'] <- 'new_area'
change_period[which(is.na(change_period$after)), 'status'] <- 'old_area'
change_period[which(change_period$after > change_period$before), 'status'] <- 'increased'
change_period[which(change_period$after < change_period$before), 'status'] <- 'decreased'




bbn %>% filter(year == 2008) %>% ggplot(aes(x = xbin, y = ybin)) + 
  geom_tile(aes(fill = count)) + facet_wrap(~ legal)







#copy over count column and make rows with less than 3 vessels
binned$nvess <- nvess
binned$can_plot <- 'no'
binned[which(binned$nvess >= 3), 'can_plot'] <- 'yes'


binned %>% group_by(group) %>% mutate(tot_tows = sum(count), perc = count / tot_tows) %>%
  as.data.frame -> binned

#add years to binned
# binned <- merge(data.frame(group = 1:6, year = 2008:2013), binned, all = TRUE)

#Plot Stylized figures without lat and long
ggplot(binned, aes(x = xbin, y = y, group = can_plot)) + 
  geom_tile(aes(fill = perc)) + facet_wrap(~ can_plot)

#Plot on maps
wc_plot + geom_tile(data = binned, aes(x = x, y = y, fill = perc)) + 
  facet_wrap(~ year)




#plots of areas with highest percentage of effort
pdf(width = 12, height = 10, file = 'figs/effort_by_year.pdf')
ggplot(binned, aes(x = x, y = y)) + 
  geom_tile(aes(fill = perc)) + facet_wrap(~ year) + theme_bw()
dev.off()

#Average before and after
binned$when <- sapply(binned$year, FUN = function(x) ifelse(x >= 2011, 'after', 'before'))
binned %>% group_by(x, y, when) %>% summarise(avg_perc = mean(perc)) -> ba_binned
# ba_binned$when <- as.factor(ba_binned$when, levels = c('before', 'after'))
ba_binned$when <- factor(ba_binned$when, levels = c('before', 'after'))

#Averaged before and after plots 
pdf(width = 7, height = 7, file = 'figs/avg_effort_bef_aft.pdf')
ggplot(ba_binned, aes(x = x, y = y)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
  theme_bw()
dev.off()

#Calculate number of locations with increases, decreases new locations or gone locations
sum_table <- dcast(ba_binned, x + y ~ when, value.var = "avg_perc")
sum_table$status <- 1
sum_table[which(is.na(sum_table$before)), 'status'] <- 'new_area'
sum_table[which(is.na(sum_table$after)), 'status'] <- 'old_area'
sum_table[which(sum_table$after > sum_table$before), 'status'] <- 'increased'
sum_table[which(sum_table$after < sum_table$before), 'status'] <- 'decreased'



sum_table %>% group_by(status) %>% summarise(n_stats = n(), perc = round(n_stats / 448 * 100, 2))
# 40% of locations decreased
# 30% of locations had increased percentage of average effort
# 7% of locations were new
# 24% of locations were not revisited

sum_table$x <- round(sum_table$x, digits = 4)
long_vals <- unique(sum_table$x)[order(unique(sum_table$x), decreasing = TRUE)]

ids <- data.frame(x = long_vals, id = 1:length(unique(sum_table$x)))
ids$id <- -ids$id

sum_table <- merge(sum_table, ids, by = 'x')

#Make plots of each category
plot_by_status <- function(stat1){
  y_range <- range(sum_table$y)
  x_range <- range(sum_table$id)

  inc <- subset(sum_table, status == stat1)
  inc$status <- NULL
  xx <- melt(inc, id = c('x', 'y', 'id'), variable.name = 'when')

  # xx$x <- round(xx$x, digits = 4)
  # ids <- data.frame(x = unique(xx$x), id = 1:length(unique(xx$x)))
  # ids$id <- -ids$id

  # xx <- merge(xx, ids, by = 'x')
  
  pp <- ggplot(xx, aes(x = id, y = y)) + geom_tile(aes(fill = value)) + facet_wrap(~ when) +   
          scale_fill_gradient(low = 'white', high = 'red') + ylim(y_range[1], y_range[2]) + 
          xlim(x_range[1], x_range[2])

  #Increased locations
  # filename <- paste0('figs/', stat1, '.pdf')
  # pdf(width = 12, height = 10, file = filename)
  # pp <- wc_plot + geom_tile(data = xx, aes(x = x, y = y, fill = value)) + facet_wrap(~ when) + theme_bw() + 
  #   scale_fill_gradient(low = 'white', high = 'red')

   # ggplot(xx, aes(x = x, y = y)) + geom_tile(aes(fill = value)) + 
   #  facet_wrap(~ when) + theme_bw()
  print(pp)
  # dev.off()
}

plot_by_status(stat1 = 'old_area')
plot_by_status(stat1 = 'new_area')
plot_by_status(stat1 = 'increased')
plot_by_status(stat1 = 'decreased')


#Start HERE
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#Map vessel average paths

#Correct wc_data that has year 2022 to 2002
wc_data[which(wc_data$tow_year == 2022), 'tow_year'] <- 2002

#Remove duplicated tow rows, 
wc_data %>% distinct(haul_id) -> unq_tows

#Add in column indicating before/after catch share implementation
unq_tows$when <- ifelse(unq_tows$tow_year <= 2010, 'bef', 'aft')

#add incolumn indicating if number of years is balanced before/after
unq_tows$balanced <- FALSE
unq_tows[unq_tows$tow_year >= 2007, 'balanced'] <- TRUE

#Calculate annual average locations for all vessels
unq_tows %>% group_by(drvid, tow_year) %>%
  summarise(avg_set_lat = mean(set_lat, na.rm = T),
            avg_set_long = mean(set_long, na.rm = T), avg_up_lat = mean(up_lat, na.rm = T), 
            avg_up_long = mean(up_long, na.rm = T), ntows = n()) -> avg_loc

#For loop to create vessel maps
ids <- unique(unq_tows$drvid)

pdf(width = 7, height = 7,
  file = 'figs/vess_tows.pdf')

for(ii in 1:length(ids)){
  unq_tows %>% filter(drvid == ids[ii]) -> unq1
  unq1$when <- ifelse(unq1$tow_year <= 2010, 'bef', 'aft')
  unq1$when <- factor(unq1$when, levels = c("bef", 'aft')) #so that order of plots is bef aft

  # unq1_bef <- unq1 %>% filter(tow_year <= 2010 & tow_year >= 2008)
  # unq1_aft <- unq1 %>% filter(tow_year > 2010)

  avg_loc %>% filter(drvid == '1037785') -> avg1

  long_range <- range(c(-unq1$set_long, -unq1$up_long), na.rm = TRUE)
  long_range[1] <- long_range[1] - .5
  long_range[2] <- long_range[2] + .5

  lat_range <- range(unq1$set_lat, unq1$up_lat, na.rm =TRUE)
  lat_range[1] <- lat_range[1] - .5
  lat_range[2] <- lat_range[2] + .5
  
  if(length(unique(unq1$when)) == 1){
    xx <- wc_plot + coord_map(xlim = long_range, ylim = lat_range) + 
                  geom_segment(data = unq1, 
                    aes(x = -set_long, y = set_lat, xend = -up_long, 
                    yend = up_lat, colour = tow_year), 
                    arrow = arrow(length = unit(.03, 'npc'))) + 
                  labs(title = paste(unique(unq1$drvid), as.character(unique(unq1$when))))  
  }
  if(length(unique(unq1$when)) == 2){
    xx <- wc_plot + coord_map(xlim = long_range, ylim = lat_range) + 
                      geom_segment(data = unq1 %>% filter(balanced == TRUE), 
                        aes(x = -set_long, y = set_lat, xend = -up_long, 
                        yend = up_lat, colour = tow_year), 
                        arrow = arrow(length = unit(.03, 'npc'))) + facet_wrap(~ when) + 
                      labs(title = unique(unq1$drvid))  
  }
  print(xx)
  print(ii)
}

dev.off()



          # geom_segment(data = unq1_aft, aes(x = -set_long, y = set_lat, xend = -up_long, 
          #   yend = up_lat, colour = 'blue'), arrow = arrow(length = unit(.04, 'npc')))
            

    

  filter(drvid == '1037785') %>% ggplot(aes(x = -avg_set_long,
    y = avg_set_lat)) + geom_point(aes(colour = ifelse(tow_year >= 2011, 'red', 'blue')))


wc_data %>% filter(drvid == '1037785') -> xx
xx %>% group_by(tow_year) %>% summarise(length(unique(haul_id)))
unique(xx$haul_id)




tt <- subset()

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



# binned$unq <- paste(binned$x, binned$y)

# binned$perc <- binned$perc * 100
# binned$perc <- round(binned$perc, digits = 4 )

# #Cutoffs
# #data must have values from before and after catch shares
# #Fit linear model and look at slopes of 

# binned <- merge(data.frame(group = 1:12, year = 2002:2013),
#   binned, all = TRUE)

# #before, after binned
# ba_binned <- subset(binned, year >= 2008)
# ba_binned$when <- binned$year

# ba_binned$when <- sapply(ba_binned$year, 
#   FUN = function(x) ifelse(x >= 2011, 'after', 'before'))

# ba_binned %>% group_by(unq) %>% summarise(min_year = min(year), max_year = max(year))

# nunq <- unique(ba_binned$unq)
# keeps <- rep(999, length(nunq))

# for(zz in 1:length(nunq)){
#   temp <- subset(ba_binned, unq == nunq[zz])
#   if(length(unique(temp$when)) == 2) keeps[zz] <- 1
# }

# full <- nunq[which(keeps == 1)]
# outs <- vector(length = length(full), 'list')

# for(ff in 1:length(full)){
#   temp <- subset(ba_binned, unq == full[ff])
#   outs[[ff]] <- lm(perc ~ year, data = temp)
# }


# #slopes
# slopes <- sapply(outs, FUN = function(x) coef(x)[2])
# names(slopes) <- NULL

# pos <- subset(ba_binned, unq %in% full[which(slopes > 0)])
# plot(pos$year, pos$perc, type = 'n')
# punq <- unique(pos$unq)

# for(pp in 1:length(punq)){
#   temp <- subset(pos, unq == punq[pp])
#   lines(temp$year, temp$perc, col = 'gray')
# }


# ba_binned[which(slopes > 0)]

# pos <- subset(ba_binned)which(slopes > 0)


# coef(outs[[1]])


# zz <- 
# length(unique(temp$when)) == 2


# plot(binned$group, binned$perc, type = 'n')
# nunq <- unique(binned$unq)

# for(ii in 1:length(nunq)){
#   temp <- subset(binned, unq == nunq[ii])

#   lines(temp$group, temp$perc, col = "#80808050")
# }


# rgb(0, 1, 0)
# rgb(190, 190, 190, alpha = .9, maxColorVal = 250)

# ggplot(binned, aes(x = group, y = perc))

# binned$group <- as.factor(binned$group)

# ggplot(binned, aes(x = group, y = std_count)) + geom_boxplot()

# #look at individual lines
# ggplot(binned, aes(x = group, y = std_count, group = unq)) + 
#   geom_line(colour = 'gray') + theme_bw()
  

#   geom_line(col = rgb(red = 190, green = 190, blue =  190, alpha = .1))


# ggplot(binned, aes(x = group, y = count, group = unq)) + geom_boxplot() 


# binned %>% group_by(group) %>% summarise(tot_tows = sum(count))


##Number of vessels and minutes per year
# wc_data %>% distinct(drvid, tow_month, tow_day, tow_year, mph) %>%
#   group_by(tow_year) %>% 
#   summarise(tot_min = sum(duration_min, na.rm = TRUE), 
#             nvess = length(unique(drvid))) -> sum_effort
# sum_effort <- subset(sum_effort, tow_year >= 2002 & tow_year < 2015)

# #2014 is really low with only 40 vessels, check that this is right
# #load 2014 data
# vess14 <- read.csv('data/vessels2014.csv', stringsAsFactors = FALSE)
# catch14 <- read.csv('data/catch14.csv', stringsAsFactors = FALSE)

# #Format catch14
# names(catch14) <- c('year', 'species', 'sector_lb', 'carryover_lb',
#   'catch', 'remaining')
# catch14$species == unique(vess14$species)

# #Format vess14
# names(vess14) <- c('year', 'vessel', 'owner', 'species', 'balance', 'limit')
# vess14$limit <- as.numeric(gsub(',', '', vess14$limit))
# vess14$balance <- as.numeric(gsub(',', '', vess14$balance))
# vess14$vessel <- tolower(vess14$vessel)
# vess14$owner <- tolower(vess14$owner)

# vess14 %>% group_by(vessel) %>% summarise(tot_bal = sum(balance)) %>% 
#   arrange(desc(tot_bal)) %>% as.data.frame
# vess14 %>% group_by(species) %>% summarise(tot_bal = sum(balance), 
#   tot_lim = sum(limit), rat = tot_bal / tot_lim) %>% as.data.frame -> check

# vv <- data.frame(x = catch14$sector_lb + catch14$carryover_lb , y = catch14$catch)

# catch14[grep('of', catch14$species), 'species']
# catch14$rat <- vv$y / vv$x


# data.frame('species' = check$species, 'ifq_catch' = check$tot_bal, 'bal_catch' = catch14$catch)

# length(unique(vess14$vessel))
