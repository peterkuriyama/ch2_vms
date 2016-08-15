# #--------------------------------------------------------------------------------
# setwd('/Users/peterkuriyama/School/Research/ch2_vms')
# source('R/start_up.r')

# #--------------------------------------------------------------------------------
# #Filter west coast data so that ha_ratio is pretty close in agreement
# wc_data <- subset(wc_data, ha_ratio >= 0.6 & ha_ratio <= 1.1)
# #per Lee and Sampson oregon trawl paper

#--------------------------------------------------------------------------------
#Prepare WC data for analysis
wcwc <- wc_data
wcwc <- wcwc[, c('trip_id', 'ddate', 'agid','rdate', 'dport', 'rport','drvid', 'dyear' ,'townum', 'set_lat',
 'set_long', 'up_lat', 'up_long', 'depth1', 'target', 'hpounds', 'apounds', 'species' )]



wcwc$trans_set_long <- wcwc$set_long * cos((2 * pi * wcwc$set_lat) / 360)
wcwc$trans_up_long <- wcwc$up_long * cos((2 * pi * wcwc$up_lat) / 360)

wcwc %>% group_by(trip_id, ddate, drvid, townum, agid) %>% filter(row_number() == 1) %>%
  as.data.frame -> wcwc1

#--------------------------------------------------------------------------------
#Subset and cluster by state

#Subset
or <- subset(wcwc1, agid == 'O')
wa <- subset(wcwc1, agid == 'W')
ca <- subset(wcwc1, agid == 'C')

#Calculate distances
dist_or <- dist(or[, c('up_lat', 'set_lat', 'trans_set_long', 'trans_up_long')],
  method = 'euclidean')

# clust_or <- hclust(distor, method = 'average')
# tree_or <- cutree(clus_or, h = 0.15)

#Analyzed these on the lab mac
load('output/clust_ca.Rdata')
load('output/clust_wa.Rdata')
load('output/clust_or.Rdata')

#These dimensions are off for some reason


#Cut the tree and assign 

tree_ca <- cutree(clust_ca, h = 0.15)



tree_wa <- cutree(clust_wa, h = 0.15)


nrow(or %>% distinct(up_lat, set_lat, trans_set_long, trans_up_long))
length(tree_or)

duplicated(or['trip_id'])



length(tree_or)


#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#Look at opportunities for single vessel
#look at single vessel
# temp <- subset(wc_data, drvid == "546053")



#Convert longitudes to latitudes
#trans for transformed

#calculate euclidean distances and cluster
#Filter unique tows for euclidean stuff
temp1 %>% group_by(trip_id, ddate, drvid, townum) %>% filter(row_number() == 1) %>%
  as.data.frame -> dd

#66,785 unique tows

distance <- dist(dd[, c('up_lat', 'set_lat', 'trans_set_long', 'trans_up_long')],
  method = 'euclidean')






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
