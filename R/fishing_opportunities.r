# #--------------------------------------------------------------------------------
# setwd('/Users/peterkuriyama/School/Research/ch2_vms')
# source('R/start_up.r')

# #--------------------------------------------------------------------------------
# #Filter west coast data so that ha_ratio is pretty close in agreement
wc_data <- subset(wc_data, ha_ratio >= 0.6 & ha_ratio <= 1.1)
# #per Lee and Sampson oregon trawl paper

#--------------------------------------------------------------------------------
#Prepare WC data for analysis
#wc_data_orig is the raw data frame

#Remove anything with a . in the rowname
wc_data <- wc_data[-grep('\\.', rownames(wc_data)), ]

#Add in percentages
# subset(wc_data1, trip_id == '1081113' & townum == 5)

#Calculate tow-level percentages
wc_data %>% group_by(trip_id, townum) %>% mutate(tow_hpounds = sum(hpounds, na.rm = TRUE),
  tow_apounds = sum(apounds, na.rm = TRUE), tow_spp_hperc = hpounds / tow_hpounds, 
  tow_spp_aperc = apounds / tow_apounds) %>%
  as.data.frame -> wc_data

#Select only certain columns of wc_data to make it easier to work with
part_wc_data <- wc_data %>% select(trip_id, ddate, agid, rdate, drvid, dyear, townum,
  set_lat, set_long, up_lat, up_long, depth1, target, tow_spp_hperc, tow_spp_aperc, species, rport_desc,
  dport_desc, d_portgrp, arid_psmfc, duration, net_type, tow_hpounds, tow_apounds, hpounds, 
  apounds)

#Filter out unique tows also for part_wc_data
part_wc_data %>% group_by(trip_id, townum) %>% filter(row_number(townum) == 1) %>%
  as.data.frame -> part_wc_data

length(which(part_wc_data$rport_desc != part_wc_data$dport_desc)) / nrow(part_wc_data)
#3% of tows departed and returned at different ports

#Transform longitudes to latitude space
part_wc_data$trans_set_long <- part_wc_data$set_long * cos((2 * pi * part_wc_data$set_lat) / 360)
part_wc_data$trans_up_long <- part_wc_data$up_long * cos((2 * pi * part_wc_data$up_lat) / 360)

# #Lok at trends in catches/landings
# wc_data %>% group_by(species, dyear) %>% summarize(sum_apounds = sum(apounds), sum_hpounds = sum(hpounds)) %>% 
#   arrange(desc(sum_apounds)) %>% filter(species %in% c('Arrowtooth Flounder', 
#     'Dover Sole', 'Sablefish', 'Petrale Sole')) %>% 
# ggplot() + geom_line(aes(x = dyear, y = sum_apounds)) + facet_wrap(~ species) + theme_bw()

#--------------------------------------------------------------------------------
#Look at aggregated numbers

#Number of tows at each port
  #Find port with most tows
part_wc_data %>% group_by(dport_desc) %>% summarize(ntows = length(trans_up_long), tot_h = 
  sum(hpounds, na.rm = TRUE), tot_a = sum(apounds, na.rm = TRUE), lbs_per_tow = tot_h / ntows) %>% 
  arrange(desc(ntows)) %>% as.data.frame

#Vessel Catch in each year
  #Find Biggest vessels
part_wc_data %>% group_by(drvid, dyear) %>% summarize(tot_h = sum(hpounds, na.rm = TRUE),
  tot_a = sum(apounds, na.rm = TRUE)) %>% as.data.frame -> vess_catch

#--------------------------------------------------------------------------------
#Write a function to subset and cluster by port



sub_clust <- function(port, cut_point, 
       targs = c("Dover Sole", "Sablefish", "Shortspine Thornyhead", "Petrale Sole", 
                 'Longspine Thornyhead', "Lingcod"),
       const = c("Darkblotched Rockfish", "Pacific Ocean Perch", 'Canary Rockfish', 
                 'Bocaccio Rockfish', 'Yelloweye Rockfish', 'Cowcod Rockfish')){
  wc_ast <- subset(part_wc_data, dport_desc == port)
    
  dist_ast <- dist(wc_ast[, c('up_lat', 'set_lat', 'trans_set_long', 'trans_up_long')], 
    method = 'euclidean')
  clust_ast <- hclust(dist_ast, method = 'average')
  # save(clust_ast, file = 'output/clust_ast.Rdata')

  tree_ast <- cutree(clust_ast, h = cut_point)
  # rm(list = c("dist_ast"))

  wc_ast$clust <- tree_ast

  #Find most common clusters
  wc_ast %>% group_by(dyear, clust) %>% summarise(nn = length(clust)) %>% group_by(clust) %>%
    mutate(ntot = sum(nn)) %>% arrange(desc(nn)) %>%
    as.data.frame -> ast_sum
  ast_sum %>% distinct(clust, ntot)

  #Need to merge the clusters back into the original wc_data that has the tow catch compositions  
  #Group all the dplyr functions that add columns on to the ast data frame
  ast <- subset(wc_data, dport_desc == port)

  #Merge with inner join
  ast <- inner_join(ast, wc_ast[, c('trip_id', 'townum', 'clust')], 
    by = c('trip_id', 'townum'))


  #Select certain columns
  ast %>% select(trip_id, ddate, agid, rdate, drvid, dyear, townum,
    set_lat, set_long, up_lat, up_long, depth1, target, species, rport_desc,
    dport_desc, d_portgrp, arid_psmfc, duration, net_type, hpounds, apounds, clust, 
    ha_ratio, tow_spp_hperc, tow_spp_aperc ) -> ast

  #Calculate catch compositions for each trip, vessel, and cluster through time
    #Trip Catch Compositions
  ast %>% group_by(drvid, trip_id, dyear) %>% mutate(trip_hpounds = sum(hpounds),
    trip_apounds = sum(apounds)) %>% group_by(drvid, trip_id, species, dyear) %>%
    mutate(trip_spp_hperc = sum(hpounds) / trip_hpounds, 
           trip_spp_aperc = sum(apounds) / trip_apounds) %>% 
    #Vessel Catch Compositions
    group_by(drvid, dyear) %>% mutate(vess_hpounds = sum(hpounds),
      vess_apounds = sum(apounds)) %>% group_by(drvid, species, dyear) %>%
    mutate(vess_spp_hperc = sum(hpounds) / vess_hpounds, 
           vess_spp_aperc = sum(apounds) / vess_apounds) %>% 
    #Cluster Catch Compositions
    group_by(clust, dyear) %>% mutate(clust_hpounds = sum(hpounds),
      clust_apounds = sum(apounds)) %>% group_by(clust, dyear, species) %>%
      mutate(clust_spp_hperc = sum(hpounds) / clust_hpounds,
             clust_spp_aperc = sum(apounds) / clust_apounds) %>% as.data.frame -> ast
  

  #--------------------------------------------------------------------------------
  #Cluster everything by group
  ast$group <- "other"

  #Classify Species
  # targs <- c("Dover Sole", "Sablefish", "Shortspine Thornyhead", "Petrale Sole", 
  #   'Longspine Thornyhead', "Lingcod" )

  # const <- c("Darkblotched Rockfish", "Pacific Ocean Perch", 'Canary Rockfish', 
  #   'Bocaccio Rockfish', 'Yelloweye Rockfish', 'Cowcod Rockfish')

  #add in category column to data
  ast$group <- 'other'
  ast[which(ast$species %in% targs), 'group'] <- 'targ'
  ast[which(ast$species %in% const), 'group'] <- 'cons'

  #Calculate compositions for each group
  ast %>% group_by(drvid, trip_id, dyear) %>% group_by(drvid, trip_id, group, dyear) %>%
    mutate(trip_group_hperc = sum(hpounds) / trip_hpounds, 
           trip_group_aperc = sum(apounds) / trip_apounds) %>% 
    #Vessel Catch Compositions
    group_by(drvid, dyear) %>% group_by(drvid, group, dyear) %>%
    mutate(vess_group_hperc = sum(hpounds) / vess_hpounds, 
           vess_group_aperc = sum(apounds) / vess_apounds) %>% 
    #Cluster Catch Compositions
    group_by(clust, dyear) %>% group_by(clust, dyear, group) %>%
      mutate(clust_group_hperc = sum(hpounds) / clust_hpounds,
             clust_group_aperc = sum(apounds) / clust_apounds) %>% as.data.frame -> ast

  return(ast)
}

#--------------------------------------------------------------------------------
#Filter and save the plots    

sub_clust_plot <- function(port, cut_point,
       targs = c("Dover Sole", "Sablefish", "Shortspine Thornyhead", "Petrale Sole", 
                 'Longspine Thornyhead', "Lingcod"),
       const = c("Darkblotched Rockfish", "Pacific Ocean Perch", 'Canary Rockfish', 
                 'Bocaccio Rockfish', 'Yelloweye Rockfish', 'Cowcod Rockfish')){
  library(dplyr)
  library(ggplot2)
  ast <- sub_clust(port = port, cut_point = cut_point)
  #Maps
  xlims <- c(floor(range(-c(ast$set_long, ast$up_long))[1]),
             ceiling(range(-c(ast$set_long, ast$up_long))[2]))

  ylims <- c(floor(range(c(ast$set_lat, ast$up_lat))[1]),
             ceiling(range(c(ast$set_lat, ast$up_lat))[2]))

  ###Plot maps in a for loop
  
  filename <- tolower(port)

  if(length(grep(" ", port)) > 0){
    filename <- gsub(" ", "_",tolower(port))
  }
  cutval <- strsplit(as.character(cut_point), '\\.')[[1]][2]
  filename <- paste0('figs/', filename, paste0("_cut", cutval))
  
  pdf(width = 9, height = 7, file = paste0(filename, "_clusters.pdf"))

  for(ii in unique(ast$clust)){
    # cat(ii, length(unique(ast$clust)), '\n')
    cc <- ii
    temp <- ast %>% filter(clust == cc)
    temp$dyear <- as.character(temp$dyear)
    one <- wc_map + geom_segment(data = temp, aes(x = -set_long, 
                xend = -up_long, y = set_lat, 
                yend = up_lat), arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() + 
                ggtitle(paste("cluster = ", cc, ",", "ntows = ", nrow(temp))) + 
                scale_x_continuous(limits = xlims) + 
                scale_y_continuous(limits = ylims) + 
                facet_wrap(~ dyear)
                # scale_colour_manual(values = c("2008" = 'red', 
                #   "2009" = 'blue', "2010" = 'green', "2011" = 'darkgreen', "2012" = 'orange',
                #   "2013" = 'yellow'))
    print(one)            
  }

  dev.off()

  # targs <- c("Dover Sole", "Sablefish", "Shortspine Thornyhead", "Petrale Sole", 
  #   'Longspine Thornyhead', "Lingcod" )

  # const <- c("Darkblotched Rockfish", "Pacific Ocean Perch", 'Canary Rockfish', 
  #   'Bocaccio Rockfish', 'Yelloweye Rockfish', 'Cowcod Rockfish')

  ###Barplots
  pdf(width = 9, height = 7, file = paste0(filename, '_barplots.pdf'))

  for(ii in unique(ast$clust)){
    # cat(ii, length(unique(ast$clust)), '\n')
    cc <- ii
    temp <- ast %>% filter(clust == cc & species %in% c(targs, const)) %>% 
      distinct(dyear, species, clust_spp_hperc)
    if(nrow(temp) == 0) next
    two <- ggplot(data = temp) + geom_bar(stat = 'identity', 
          aes(x = factor(species), y = clust_spp_hperc)) + facet_grid(~ dyear) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          ggtitle(paste("cluster = ", cc))

    print(two)
  }

  dev.off()

}

#--------------------------------------------------------------------------------
#Loop through Ports saving plots in parallel

part_wc_data %>% group_by(dport_desc) %>% summarize(ntows = length(trans_up_long), tot_h = 
  sum(hpounds, na.rm = TRUE), tot_a = sum(apounds, na.rm = TRUE), lbs_per_tow = tot_h / ntows) %>% 
  arrange(desc(ntows)) %>% as.data.frame -> ports

#Try this with lapply for only two things
lapply(ports[c(21, 22), 1], FUN = sub_clust_plot, cut_point = 0.15)

#Plot things in parallel
library(parallel)
cl <- makeCluster(5)

clusterExport(cl, c("sub_clust", "part_wc_data", "wc_data", "wc_map"))
parLapply(cl, ports$dport_desc, sub_clust_plot, cut_point = 0.15)

stopCluster(cl)

sub_clust_plot(port = 'ASTORIA', cut_point = 0.3)
#--------------------------------------------------------------------------------
#Calculate CV in pounds and maybe % compositions 
#Find most selective (lowest CVs) vessels, clusters
#Look into vessel catch compositions
#Variability in trip compositions?

#Define Target and Constraining Species
targs <- c("Dover Sole", "Sablefish", "Shortspine Thornyhead", "Petrale Sole", 
          'Longspine Thornyhead', "Lingcod")
const <- c("Darkblotched Rockfish", "Pacific Ocean Perch", 'Canary Rockfish', 
          'Bocaccio Rockfish', 'Yelloweye Rockfish', 'Cowcod Rockfish')

xx <- sub_clust(port = 'ASTORIA', cut_point = 0.3)

xx %>% group_by(drvid) %>% summarize(nclust = length(unique(clust))) %>% arrange(desc(nclust))

##Calculate CVs by Cluster
#Filter so that only target and constraining species are included
xx %>% group_by(clust, species, dyear) %>% 
  summarize(tow_hperc_cv = sd(tow_spp_hperc) / mean(tow_spp_hperc),
    tow_hpound_cv = sd(hpounds) / mean(hpounds), tow_apound_cv = sd(apounds) / mean(apounds),
    nvals = length(hpounds)) %>% 
  filter(species %in% c(targs, const)) %>% as.data.frame %>% arrange(desc(nvals)) -> clust_cv

#fit linear model and filter clusters that had the highest slopes
clust_cv %>% group_by(clust, species) %>% 
  filter(length(clust) > 1 & is.na(tow_hpound_cv) == FALSE) %>% 
  arrange(dyear) %>% do({
    mod <- lm(tow_hpound_cv ~ dyear, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
  }) %>% as.data.frame -> clust_cv

qs <- quantile(unique(clust_cv$slope), na.rm = TRUE)

#Plot slopes in the 25 percentile, these ones got better
cv_down <- clust_cv %>% filter(slope <= qs[2])

ggplot(dat = cv_down) + 
  geom_point(aes(x = dyear, y = tow_hpound_cv, size = nvals)) + 
  geom_line(aes(x = dyear, y = tow_hpound_cv, group = clust, colour = clust)) + 
  facet_wrap(~ species)

#Slopes above the 75 Percentile
cv_up <- clust_cv %>% filter(slope >= qs[4])

ggplot(dat = cv_up) + 
  geom_point(aes(x = dyear, y = tow_hpound_cv, size = nvals)) + 
  geom_line(aes(x = dyear, y = tow_hpound_cv, group = clust, colour = clust)) + 
  facet_wrap(~ species) + theme_bw()

hist(unique(clust_cv$slope), breaks = 30)
clust_cv[clust_cv$slope > 1, ]

#Find lowest cvs
clust_cv %>% group_by(clust) %>% mutate(avg_cv = mean(tow_hpound_cv, na.rm = TRUE)) %>%
  as.data.frame -> clust_cv
clust_cv %>% filter(avg_cv <= 0.5)

#Look at species-specific slopes in CVs
ggplot(clust_cv) + geom_histogram(aes(x = slope)) + facet_wrap(~ species) + theme_bw() + 
  geom_vline(xintercept = 0, col = 'red')

#Species Specific CV in hpounds
#Add in before or after catch shares as column
clust_cv$when <- 'before'
clust_cv[clust_cv$dyear >= 2011, 'when'] <- 'after'

#Find median values before and after for each species
clust_cv %>% group_by(species, when) %>% mutate(med_cv = median(tow_hpound_cv)) %>%
  as.data.frame -> clust_cv
clust_cv %>% select(species, when, med_cv) %>% distinct() %>% dcast(species ~ when) -> dd
  dd %>% group_by(species) %>% mutate(diff = before - after)

clust_cv$when <- as.factor(clust_cv$when)
levels(clust_cv$when) <- c('before', 'after')

#Plot vertically to visualize the changes
pdf(width = 6, height = 24, file = 'figs/species_hpound_cv.pdf')
print(ggplot(data = clust_cv) + 
  geom_histogram(aes(x = tow_hpound_cv)) + coord_flip() +
  facet_wrap(~ species + when, ncol = 2) + theme_bw() + 
  geom_vline(aes(xintercept = med_cv, col = 'red'))
)
dev.off()  

#--------------------------------------------------------------------------------
#Calculate cvs for each species for individual
wc_data %>% group_by(drvid, species, dyear) %>% 
  summarize(tow_hperc_cv = sd(tow_spp_hperc) / mean(tow_spp_hperc),
    tow_hpound_cv = sd(hpounds) / mean(hpounds), tow_apound_cv = sd(apounds) / mean(apounds),
    nvals = length(hpounds)) %>% 
  filter(species %in% c(targs, const)) %>% as.data.frame %>% arrange(desc(nvals)) -> vess_cv

#fit linear model and filter clusters that had the highest slopes
  #Do this on hpounds
vess_cv %>% group_by(drvid, species) %>% 
  filter(length(drvid) > 1 & is.na(tow_hpound_cv) == FALSE) %>% 
  arrange(dyear) %>% do({
    mod <- lm(tow_hpound_cv ~ dyear, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
  }) %>% as.data.frame -> vess_cv

#find lowest cvs
#lookt at species specific slopes in cvs
#find median values before and after for each species




ggplot(cvs) + geom_point(aes(x = dyear, y = tow_hpound_cv, size = nvals)) + 
  geom_line(aes(x = dyear, y = tow_hpound_cv)) + 
  facet_wrap(~ species)


#Calculate CVs by vessel

%>%
  as.data.frame %>% ggplot() + geom_line(aes(x = dyear, y = tow_cv)) + facet_wrap(~ species)

check$tow_spp_hperc

check$trip_tow <- paste(check$trip_id, check$townum)

#

check %>% filter(species == 'Pacific Ocean Perch') -> pop

#Tows that caught POP
check %>% filter(trip_tow %in% pop$trip_tow) 

xlims <- c(floor(range(-c(check$set_long, check$up_long))[1]),
           ceiling(range(-c(check$set_long, check$up_long))[2]))

ylims <- c(floor(range(c(check$set_lat, check$up_lat))[1]),
           ceiling(range(c(check$set_lat, check$up_lat))[2]))

wc_map + geom_segment(data = xx %>% filter(drvid == '511697'), aes(x = -set_long,
  xend = -up_long, y = set_lat, yend = up_lat), arrow = arrow(length = unit(0.1, 'cm'))) + 
  theme_bw() + facet_wrap(~ dyear) + scale_x_continuous(limits = xlims) + 
  scale_y_continuous(limits = ylims) + facet_wrap(~ dyear)


 geom_segment(data = temp, aes(x = -set_long, 
                xend = -up_long, y = set_lat, 
                yend = up_lat), arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() + 
                ggtitle(paste("cluster = ", cc, ",", "ntows = ", nrow(temp))) + 
                scale_x_continuous(limits = xlims) + 
                scale_y_continuous(limits = ylims) + 
                facet_wrap(~ dyear)


#--------------------------------------------------------------------------------
#TO DO
#Compare the amounts of pounds to records in catch IFQ database






















# xx <- foreach(1:length(ports$dport_desc)) %dopar% {
#   sub_clust_plot(port = ports$dport_desc[ii], cut_point = 0.15)
# }

# for(ii in 1:length(ports$dport_desc)){
#   print(ii)
#   sub_clust_plot(port = ports$dport_desc[ii], cut_point = 0.15)
# }


# Notes about things













ast %>% group_by(trip_id) %>% mutate(tot_hpounds = sum(hpounds),
  tot_apounds = sum(apounds)) %>% as.data.frame -> ast





#Calculate average species catch proportion for each vessel 
  #also count the # of years each vessel is in logbook
ast %>% group_by(drvid, dyear, species) %>% mutate(drvid_selex = mean(h_perc)) %>%
  group_by(drvid) %>% mutate(drv_years = length(unique(dyear))) %>%
  as.data.frame -> ast

#Find Majority caught species for each tow
ast %>% group_by(trip_id, townum) %>% arrange(desc(h_perc)) %>% 
  mutate(most_caught = species[row_number(1)]) %>% as.data.frame -> ast

#Calculate number of tows in each cluster
ast %>% group_by(dyear, species, clust) %>% mutate(ntows = length(h_perc)) %>%
  as.data.frame %>% arrange(desc(ntows)) -> ast

#Need to add this in 

 #Plot the top 10 clusters and see which target and rebuilding species they caught

unique(wc_data$species)

#Calculate catch compositions for each cluster
ast %>% group_by(clust) %>% mutate(clust_hpounds = sum(tot_hpounds, na.rm = TRUE), clust_apounds = 
  sum(tot_apounds, na.rm = TRUE)) %>% group_by(clust, species) %>% 
  mutate(clust_hperc = sum(tot_hpounds) / clust_hpounds, 
         clust_aperc = sum(tot_apounds) / clust_apounds) %>% as.data.frame -> ast



#

#####ASTORIA PLOTS
#Which clusters had the most catch
dat <- ast %>% filter(clust %in% 1:10 & species %in% c(targs, const))

#Plot the tow locations
ggplot(data = dat) + geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat,
  yend = up_lat, colour = clust), arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() 


ast %>% filter(clust %in% 1:10 & species %in% c(targs, const)) %>% 
  distinct(clust, species, clust_hperc) %>% 
  ggplot(.) + geom_bar(stat = 'identity', aes(x = factor(species), y = clust_hperc)) + 
  facet_grid(~ clust) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

pdf(figs = '', units = 'in', height = 7, weight = 7)

ggplot(data = dat) + 




ggplot() + geom_segment(data = ast %>% filter(species == 'Sablefish'), 
  aes(x = -set_long, xend = -up_long,
  y = set_lat, yend = up_lat, colour = h_mostly_targ), 
  arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() + facet_wrap(~ dyear)


ast %>% group_by(clust) %>% summarize(clust_hpounds = unique(clust_hpounds)) %>%
  arrange(desc(clust_hpounds)) %>% filter(row_number(clust) <= 10) %>% select(clust) %>%
  as.vector






ast %>% filter(clust %in% 1:10) %>% distinct(clust, species, clust_hperc) %>% 
  ggplot() + geom_bar(stat = 'identity', aes(x = factor(species), y = clust_hperc)) + 
  facet_grid(~ clust) + theme(axis.text.x = element_text(angle = 90, hjust = 1))




arrange(desc(clust_hpounds)) %>% distinct() %>%
  as.data.frame







#


ast$h_other_perc <- 1 - ast$h_perc
ast$a_other_perc <- 1 - ast$a_perc


#Look at histograms of bycatch in each cluster for each species





ast %>% filter(species == "Dover Sole" & drv_years > 3) %>% ggplot() + geom_line(aes(x = dyear, 
  y = drvid_selex, colour = drvid)) 





#Can ID the most selective clusters
check <- subset(ast, clust == 12)
check$h_mostly_targ <- "yes"
check[which(check$h_other_perc >= 0.5), 'h_mostly_targ'] <- 'no'

  
#Look at track lines for each tow
ggplot() + geom_segment(data = check %>% filter(species == 'Sablefish'), 
  aes(x = -set_long, xend = -up_long,
  y = set_lat, yend = up_lat, colour = h_mostly_targ), 
  arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() + facet_wrap(~ dyear)





ast %>% filter(trip_id = 1095969 & )





#For each species find out the proportion of tows that have low bycatch
ast %>% group_by(dyear, species, clust) %>% summarise(perc_selective = 
  length(which(h_perc >= 0.5)) / length(h_perc), ntows = length(h_perc)) %>% 
  as.data.frame -> selective_ast


#Look at percentage of tows that catch more than 50% of specified species
ggplot(ast) + geom_point(aes(x = clust, 
  y = h_perc, colour = ntows)) + facet_wrap(~ species)

#Big species are arrowtooth, dover, lingcod, POP, longspine, petrale, sablefish, shortspine thornyhead, yellowtail
ast %>% filter(species == "Dover Sole" & ntows > 50) %>% ggplot() + geom_histogram(aes(h_perc)) + 
  facet_wrap(~ clust)

#Look at track lines for each tow 
#Also cluster 6
ggplot() + geom_segment(data = check %>% filter(species == 'Dover Sole' & clust == 12), 
  aes(x = -set_long, xend = -up_long,
  y = set_lat, yend = up_lat, colour = h_mostly_targ), 
  arrow = arrow(length = unit(0.1, 'cm'))) + theme_bw() + facet_wrap(~ dyear)




unique(selective$species)
ggplot(selective) + geom_line(aes(x = dyear, y = perc_selective, colour = species))

#50% of Dover tows catch more than 50%






