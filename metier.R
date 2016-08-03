#--------------------------------------------------------------------------------
#Tyranny of the grid methods
#Identify Metiers

#--------------------------------------------------------------------------------
#Load expanded West Coast Data
load('output/wc_data_expanded_tows.Rdata')

#Add in ratio of apounds to hpounds
#ratio should be between 0.6-1.1 for acceptable rows, Lee and Sampson
wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds
hist(subset(wc_data, ha_ratio < 2)$ha_ratio, breaks = 30) #histogram looks fairly normal
length(which(wc_data$ha_ratio <= 1.2 & 
  wc_data$ha_ratio >= 0.6)) / nrow(wc_data) #45% of tows seem to 


#--------------------------------------------------------------------------------
#Fishing Styles, per Boonstra and Hentati-Sundberg




ggplot(all_six, aes(x = x, y = y, fill = density)) + geom_tile() + 
  facet_grid(~ group) + scale_fill_gradient2(low = 'blue', high = 'red')
