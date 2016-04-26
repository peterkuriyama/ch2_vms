ch2_load_and_format <- function(){
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

  #Object to keep track of removed rows
  removals <- wc_data[nas, ]
  
  wc_data <- wc_data[-nas, ]

  #Filter out hake tows
  wc_data %>% group_by(haul_id) %>% summarise(nspecies = length(unique(spc.name)),
    hake = ifelse("PACIFIC WHITING" %in% unique(common.name), 1, 0)) -> cccc
  hake_tow_ids <- subset(cccc, hake == 1)

  hake_tows <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id)
  hakes <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id)
  removals <- rbind(hakes, removals)
  
  #Remove hake rows
  wc_data <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id == FALSE)

  #Remove rows with lat longs == 0
  zeros_inds <- which(wc_data$up_lat == 0 | wc_data$up_long == 0,
                 wc_data$set_lat == 0 | wc_data$set_long == 0)
  zeroes <- wc_data[zeros_inds, ]
  removals <- rbind(removals, zeroes)
  wc_data <- wc_data[-zeros_inds, ]

  #Save removals object
  save(removals, file = 'output/removals.Rdata')  
  #--------------------------------------------------------------------------------
  ##Calculate tow durations in minutes and seconds
  
  #Add direction
  wc_data$tow_direction <- paste0(ifelse(wc_data$set_lat >= wc_data$up_lat, 'S', 'N'),
                                  ifelse(wc_data$set_long >= wc_data$up_long, 'E', 'W'))

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
  return(wc_data)
}