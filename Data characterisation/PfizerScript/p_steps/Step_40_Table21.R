## Author   : Ivonne Martin (I.Martin@umcutrecht.nl)
## Date     : 24 Jan 2023

## The hazard ratio is only calculated when the number of events in both group (exposed and control are nonzero). 
## If at least one of the group has zero events, the resulting hazard ratio will be NA. 
## Version 30 Aug 2022 change the stop criteria due to negative fupdays
## Version 12 Sep 2022 return to the version of 30 Aug
## version 24 Jan 2023 add Anaphylaxis analysis

#########################################################################################################
## Function to create table 21
#########################################################################################################

fun_tab21 <- function(M_Studycohort, AESI_info, opt){
  
  ##  Similar as table 16 to get the follow-up times 
  
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
  AESI_info <- AESI_info[-which(AESI_info$Event_abbreviation == "I_COVID_COV"),]
  
  ## IM added this
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', "FOURTH_OTHER", 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_OTHER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  if(nrow(AESI_info) == 0) stop('AESI_info is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
  
  ## Copy data
  d <- subset(M_Studycohort, group != 'UNMATCHED')
  rm(M_Studycohort)
  gc()
  
  ## include propensity score
  if (opt == 1){
    propensity_var_file <- readRDS(paste0(populations_dir, "weights.rds"))
    propensity_var <- propensity_var_file %>% select(person_id, id, IPTW_full_cor_trim)
    d <- merge(x = d, y = propensity_var, by = c('person_id','id'), all.x = TRUE)
  } 
  
  ## More data checks
  if(nrow(d) == 0) stop('0 persons with group == EXPOSED or CONTROL')
  if(any(duplicated(d$person_id[d$group == 'Exposed'])))	stop('Duplicated person_id values should not be possible in the exposed population.')
  ## To do: Add more checks, e.g. check if dates are not in the future, or if indeed all *_HIST dates < first_pfizer
  
  ## Set other to NA if before t0
  d$FIRST_OTHER[as.Date(d$FIRST_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$SECOND_OTHER[as.Date(d$SECOND_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$THIRD_OTHER[as.Date(d$THIRD_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$FOURTH_OTHER[as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  
  ## Find earliest date of other vaccination after t0
  
  d$other_aftert0 <- pmin(as.Date(d$FIRST_OTHER, format = '%Y-%m-%d'), as.Date(d$SECOND_OTHER, format = '%Y-%m-%d'), as.Date(d$THIRD_OTHER, format = '%Y-%m-%d'), as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d'), na.rm = T)
  
  ## For unexposed, also include the date of first pfizer vaccin
  d$oth_pfiz_aftert0 <- d$other_aftert0 
  d$oth_pfiz_aftert0[d$group == 'CONTROL'] <- pmin(as.Date(d$other_aftert0[d$group == 'CONTROL'], format = '%Y-%m-%d'),
                                                   as.Date(d$FIRST_PFIZER[d$group == 'CONTROL'], format = '%Y-%m-%d'), na.rm = T)
  
  ## This is to differentiate the computation for table 20 and 21 
  ##   d$cens.sens <- NA
  d$day42aftert0 <- as.Date(d$T0, format = "%Y-%m-%d") + days(42) - 1
  d$day14aftert0 <- as.Date(d$T0, format = "%Y-%m-%d") + days(14) - 1
  
  # create censoring dates due to adherence to Pfizer
  # if SECOND_PFIZER is NA or later than 42 days after T0, sens.sens = 42 days after T0. 
  d$cens.sens[(d$group == "EXPOSED") & (is.na(d$SECOND_PFIZER) | (as.Date(d$SECOND_PFIZER, format = "%Y-%m-%d") > as.Date(d$day42aftert0, format = "%Y-%m-%d")))] <- as.character(as.Date(d$day42aftert0[(d$group == "EXPOSED") & (is.na(d$SECOND_PFIZER) | (as.Date(d$SECOND_PFIZER, format = "%Y-%m-%d") > as.Date(d$day42aftert0, format = "%Y-%m-%d")))], format = "%Y-%m-%d", origin = "1970-01-01"))
  # if SECOND_PFIZER occured within 14 days after T0, cens.sens = the day at second pfizer
  d$cens.sens[(d$group == "EXPOSED") & !is.na(d$SECOND_PFIZER) & (as.Date(d$SECOND_PFIZER, format = "%Y-%m-%d") < as.Date(d$day14aftert0, format = "%Y-%m-%d"))] <- as.character(as.Date(d$SECOND_PFIZER[(d$group == "EXPOSED") & !is.na(d$SECOND_PFIZER) & (as.Date(d$SECOND_PFIZER, format = "%Y-%m-%d") < as.Date(d$day14aftert0, format = "%Y-%m-%d"))], format = "%Y-%m-%d", origin = "1970-01-01"))
  # Other than two conditions above, cens.sens = NA. 
  
  # Make sure the cens.sens in the control group is the same as the cens.sens in the control group : 
  TEMP <- as.data.table(d)[,.(person_id, id, cens.sens)][!is.na(cens.sens),]
  TEMP <- TEMP[, .(cens.sens), by = "id"]
  d <- merge(x = d, y = TEMP, by = "id" , all.x = T)
  d$cens.sens.x <- NULL
  rm(TEMP)
  # The minimum of censoring sensitivity and non-Pfizer vaccine 
  
  d$cens_oth_sens <- pmin(as.Date(d$cens.sens.y, format = "%Y-%m-%d"), as.Date(d$oth_pfiz_aftert0, format = "%Y-%m-%d"),na.rm = TRUE)
  
  ## Per pair, select the earliest date of oth_pfiz_aftert0 (this is when the pair should be censored)
  
  print("Start new code")
  TEMP <- as.data.table(d)[,.(person_id, id, cens_oth_sens)][!is.na(cens_oth_sens),]
  TEMP <- TEMP[, .(censdate = min(cens_oth_sens)), by = "id"]
  d <- merge(x = d, y = TEMP, by = "id" , all.x = T)
  rm(TEMP)
  gc()
  print("End new code")
  
  ## Now adjust op_end_date to include the censoring
  d$op_end_date <- pmin(as.Date(d$op_end_date, format = '%Y-%m-%d'), as.Date(d$censdate, format = '%Y-%m-%d'), na.rm = T)		
  
  ir.per.tab <- matrix(NA,nrow = nrow(AESI_info), ncol = 12)
  
  for(i in 1:nrow(AESI_info)){
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_info[i,1]
    AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI) 
    
    #if (AESI == "ANAPHYLAXIS") next
    combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
    
    End_of_risk_window <- AESI_info$End_of_risk_window[i]
    lookback_period <- AESI_info$lookback_period[i]
    
    ## excluding pairs who have prior AESI
    if(lookback_period == 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data$Date_HIST, format = "%Y-%m-%d"))])
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]
    }
    if (lookback_period != 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d')) & as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period)]) 
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]      
    }
    
    ## Defining the follow-up days and events, based on end of risk windows for each AESI  
    
    ## Check censoring due to end obs period, risk window or event (other vaccine already included above)
    ## Note: It is assumed that obs_end_dates can never be later than death_date (if not NA). If this is not correct then
    ## the code below will yield erroneous results.
    
    combined_data$End_rw <- NA
    if(End_of_risk_window != 'Any') combined_data$End_rw <- as.Date(combined_data$T0, format = '%Y-%m-%d') + days(as.numeric(End_of_risk_window)) - 1
    combined_data$endfup <- pmin(combined_data$op_end_date, combined_data[,'Date_COUNT'], combined_data$End_rw, na.rm = TRUE)
    if(any(is.na(combined_data$endfup))) stop('NA values in follow-up times; check data and code')
    combined_data$fupdays <- as.numeric(difftime(combined_data$endfup, as.Date(combined_data$T0, format = '%Y-%m-%d'), unit = 'days')) + 1 ## Note: 1 added in line with other tables to avoid 0 fupdays
    
    if(any(combined_data$fupdays < 1)) {warning('Persons with 0 or a negative number of follow-up days in the data, these entries will be removed')
      #combined_data <- combined_data[-which(combined_data$fupdays < 1),]
      idx.negative.fupdays <- which(combined_data$fupdays < 1)
      combined_data <- combined_data[-idx.negative.fupdays,]
    }
    
    combined_data$event <- ifelse(!is.na(combined_data[,'Date_COUNT']) & as.Date(combined_data[,'Date_COUNT'], format = '%Y-%m-%d') == combined_data$endfup, 1, 0)
    
    if (any(table(combined_data$group)) == 0) stop (paste0('0 persons in any of exposed or control for', AESI, '. AESI skipped.')) 
    
    d_E <- subset(combined_data, group == "EXPOSED")
    d_U <- subset(combined_data, group == "CONTROL")
    
    cr.HR <- HR.surv(AESI, combined_data, End_of_risk_window,w.vec = rep(1,nrow(combined_data)))
    cr.RD <- adj.RD(d_E,End_of_risk_window, AESI,w.vec = rep(1,nrow(d_E))) - adj.RD(d_U,End_of_risk_window, AESI, w.vec = rep(1,nrow(d_U))) 
    cr.RD <- round(cr.RD*10000, 2)
    
    if(opt == 1){
      ad.HR <- HR.surv(AESI, combined_data, End_of_risk_window, w.vec = combined_data$IPTW_full_cor_trim)
      ad.RD <- adj.RD(d_E,End_of_risk_window, AESI,w.vec = d_E$IPTW_full_cor_trim) - adj.RD(d_U,End_of_risk_window, AESI, w.vec = d_U$IPTW_full_cor_trim) 
      ad.RD <- round(ad.RD*10000, 2)
    }
    
    if (opt == 0){
      output <- c(cr.HR, rep(NA, 3), cr.RD, rep(NA,5))
    }
    if (opt == 1){
      output <- c(cr.HR, ad.HR, cr.RD, rep(NA,2), ad.RD, rep(NA,2))
    }
    
    ir.per.tab[i,] <- output 
    rm(combined_data,d_E, d_U, output)
    gc()
    #print(i)
  }    
  
  ## output  
  d21 <- data.frame(cbind(AESI_info$Event_name, ir.per.tab))
  
  AESI_Syst <- unique(AESI_info$System)
  sta.v <- sto.v <- d21.p <- NULL
  for (i in 1:length(AESI_Syst)){
    temp <- which(AESI_info$System == AESI_Syst[i])
    sta.v[i] <- min(temp)
    sto.v[i] <- max(temp)
    d21.p <- rbind(d21.p, c(AESI_Syst[i],rep(" ", 12)), d21[sta.v[i]:sto.v[i],])
  }
  colnames(d21.p) <- c("AESI","Cr.HR","Cr.HR.CI.lb","Cr.HR.CI.ub","Adj.HR","Adj.HR.CI.lb","Adj.HR.CI.ub",
                     "Cr.RD","Cr.RD.CI.lb","Cr.RD.CI.ub","Adj.RD","Adj.RD.CI.lb", "Adj.RD.CI.ub")
  return(d21.p)
}
