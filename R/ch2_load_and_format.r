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

  # removals <- rbind(removals, c('set_times are NA', length(nas), 
  #   removals$nrow_end, removals$nrow_end - length(nas)))
  wc_data <- wc_data[-nas, ]

  #Filter out hake tows
  wc_data %>% group_by(haul_id) %>% summarise(nspecies = length(unique(spc.name)),
    hake = ifelse("PACIFIC WHITING" %in% unique(common.name), 1, 0)) -> cccc
  hake_tow_ids <- subset(cccc, hake == 1)
  hake_tows <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id)

  #Remove hake rows
  wc_data <- subset(wc_data, haul_id %in% hake_tow_ids$haul_id == FALSE)

  #Filter out certain data
  #Filter out set_times with nchar == 1
  # removals <- data.frame('desc' = 1, 'nremoved' = 1, 'nrow_start' = nrow(wc_data), 'nrow_end' = 1)
  # removals$desc <- 'rows that have nchar(set_time) == 1'
  # removals$nremoved = length(which(nchar(wc_data$set_time) == 1))
  # wc_data <- wc_data[-which(nchar(wc_data$set_time) == 1), ]
  # removals$nrow_end <- nrow(wc_data)

  # #Filter out set_times with nchar == 2
  # rem2 <- which(nchar(wc_data$set_time) == 2)
  # wc_data[rem2, c('set_time', 'up_time', 'duration')]
  }