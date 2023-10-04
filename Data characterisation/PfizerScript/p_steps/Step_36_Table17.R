# Author(s) : Ivonne Martin (I.Martin@umcutrecht.nl)
# Date      : 12 Sep 2022

## This is similar to code 16 with slight change in the censoring criteria. 
## Version 30 Aug 2022 changes the stop criteria due to negative fupdays
## Version 31 Aug 2022 change the merged data.
## Version 12 Sep 2022 return to the version of 30 Aug 

#########################################################################################################
## Function to create table 17
#########################################################################################################

## Internal functions

fun_tab17 <- function(M_Studycohort, AESI_info){
  
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
  
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
  
  ## More data checks
  if(nrow(d) == 0) stop('0 persons with group == EXPOSED or CONTROL')
  if(any(duplicated(d$person_id[d$group == 'Exposed'])))	stop('Duplicated person_id values should not be possible in the exposed population.')
  
  ## Set other to NA if before t0
  d$FIRST_OTHER[as.Date(d$FIRST_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$SECOND_OTHER[as.Date(d$SECOND_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$THIRD_OTHER[as.Date(d$THIRD_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$FOURTH_OTHER[as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  
  ## Find earliest date of other vaccination after t0
  
  d$other_aftert0 <- pmin(as.Date(d$FIRST_OTHER, format = '%Y-%m-%d'), as.Date(d$SECOND_OTHER, format = '%Y-%m-%d'), as.Date(d$THIRD_OTHER, format = '%Y-%m-%d'), as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d'), na.rm = TRUE)
  
  ## For unexposed, also include the date of first pfizer vaccin
  d$oth_pfiz_aftert0 <- d$other_aftert0 
  d$oth_pfiz_aftert0[d$group == 'CONTROL'] <- pmin(as.Date(d$other_aftert0[d$group == 'CONTROL'], format = '%Y-%m-%d'),
                                                   as.Date(d$FIRST_PFIZER[d$group == 'CONTROL'], format = '%Y-%m-%d'), na.rm = TRUE)
  
  ## This piece differentiate the code for table 16 and this table 17
  
  d$cens.sens <- NA
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
  
  ir.per.tab <- matrix(NA,nrow = nrow(AESI_info), ncol = 16)
  
  for(i in 1:nrow(AESI_info)){
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_info[i,1]
    
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
    
    
    colls <- c("person_id",
               "FIRST_PFIZER",
               "SECOND_PFIZER",
               "THIRD_PFIZER",
               "FOURTH_PFIZER",
               "FIRST_OTHER",
               "SECOND_OTHER",
               "THIRD_OTHER",
               "FOURTH_OTHER",
               "op_end_date",
               'Date_HIST',
               'Date_COUNT',
               "id",
               "T0",
               "End_rw",
               "endfup",
               "fupdays",
               "event",
               "group"
    )
    
    d_E <- combined_data[combined_data$group == 'EXPOSED', c(colnames(combined_data) %in% colls)]
    d_U <- combined_data[combined_data$group == 'CONTROL', colnames(combined_data) %in% colls]
    
    if(nrow(d_E) == 0){
      warning(paste0('0 persons in exposed set for ', AESI, '. AESI skipped.'))
      next
    }
    if(nrow(d_U) == 0){
      warning(paste0('0 persons in unexposed set for ', AESI, '. AESI skipped.'))
      next
    }
    
    rm(colls)
    gc()
    
    ir.per.tab[i,] <- c(CumInc(d_E, End_of_risk_window, AESI), IncR(d_E,  AESI), CumInc(d_U, End_of_risk_window, AESI), IncR(d_U, AESI))
    #print(i)
    rm(combined_data,d_E, d_U)
    
  }    
  numdec <- 2  
  d17 <- cbind(AESI_info[,c('System','Event_name')],round(ir.per.tab, numdec))
  colnames(d17)[3:ncol(d17)] <- c("VAC_N","VAC_CumInc","VAC_CI.lb","VAC_CI.ub","VAC_NumPerYear","VAC_IR","VAC_IR_CI.lb","VAC_IR_CI.ub",
                                  "CTR_N","CTR_CumInc","CTR_CI.lb","CTR_CI.ub","CTR_NumPerYear","CTR_IR","CTR_IR_CI.lb","CTR_IR_CI.ub")
  return(d17)
}