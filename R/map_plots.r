
#--------------------------------------------------------------------------------
#How to Plot Maps
states_map <- map_data("state")

###United States Map
png(width = 7.5, height = 4.5, units = 'in', res = 200,
  file = 'us_map.png')
ggplot() + geom_map(data = states_map, map = states_map, aes(x = long, y = lat,
  map_id = region), fill = 'gray') +
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      rect = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
      axis.text = element_blank())
dev.off()

##Northeast Map
ne_map <- subset(states_map, region == 'maine' | region == 'massachusetts')
ne_map <- states_map[states_map$region %in% c('maine', 'massachusetts', 
  'rhode island', 'connecticut', 'new hampshire', 'vermont'), ]
  
png(width = 5.5, height = 4.5, units = 'in', res = 200,
  file = 'ne_map.png')
ggplot() + geom_map(data = ne_map, map = ne_map, aes(x = long, y = lat,
  map_id = region), fill = 'gray') +
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      rect = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
      axis.text = element_blank())
dev.off()

##West Coast Map
wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington',
  'nevada'), ]

png(width = 4.5, height = 7.05, units = 'in', res = 200,
  file = 'wc_map.png')
ggplot() + geom_map(data = wc_map, map = wc_map, aes(x = long, y = lat,
  map_id = region), fill = 'gray') +
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      rect = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
      axis.text = element_blank()) 
dev.off()

#Map with points for the highest 
wc_plot <- ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() 


wc_plot <- ggplot() + geom_map(data = wc_map, map = wc_map, aes(x = long, y = lat, 
  map_id = region), fill = 'gray') + 
  geom_polygon(data = wc_map, aes(x = long, y = lat), fill = NA, color = 'black') + 
  coord_cartesian(xlim = c(-125, -117))
