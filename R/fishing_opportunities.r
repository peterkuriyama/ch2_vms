#--------------------------------------------------------------------------------
setwd('/Users/peterkuriyama/School/Research/ch2_vms')
source('R/start_up.r')

#--------------------------------------------------------------------------------
#Look at opportunities for single vessel
#look at single vessel
temp <- subset(wc_data, drvid == "546053")

# temp <- wc_data

temp1 <- temp[, c('trip_id', 'ddate', 'rdate', 'drvid', 'dyear' ,'townum', 'set_lat',
 'set_long', 'up_lat', 'up_long', 'depth1', 'target', 'hpounds', 'apounds', 'species' )]

#Convert longitudes to latitudes
#trans for transformed
temp1$trans_set_long <- temp1$set_long * cos((2 * pi * temp1$set_lat) / 360)
temp1$trans_up_long <- temp1$up_long * cos((2 * pi * temp1$up_lat) / 360)

#calculate euclidean distances and cluster
#Filter unique tows for euclidean stuff
temp1 %>% group_by(trip_id, ddate, drvid, townum) %>% filter(row_number() == 1) %>%
  as.data.frame -> dd


distance <- dist(dd[, c('up_lat', 'set_lat', 'trans_set_long', 'trans_up_long')],
  method = 'euclidean')
cluster.tree <- hclust(distance, method = 'average')

#Cut the clusters based on a cut point

y <- cutree(cluster.tree, h = 0.5)

length(unique(y))

dd$cluster <- y

#Add this
xx <- inner_join(temp1, dd[, c('trip_id', 'ddate', 'rdate', 'drvid', 'townum', 'cluster') ], 
  by = c('trip_id', 'ddate', 'rdate', 'drvid', 'townum'))

#Merge it back with the bigger data set
#What did they catch, how much and where was it

#look at cluster 1, the most common cluster
clus <- subset(xx, cluster == 1)

ggplot() + geom_segment(data = clus, aes(x = -set_long, xend = -up_long,
  y = set_lat, yend = up_lat), 
  arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() + facet_wrap(~ dyear)

#Did 


###Look at all the tow lines for this one vessel



hist(clus$hpounds, )
