### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl) and Roel Elbers (R.J.H.Elbers@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim : 5 Aug 2022

## For development:
# dat.loc <- '.../C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- as.data.frame(readRDS(paste0(dat.loc, 'M_Studycohort3.rds')))
#  AESI_info <- read.csv('.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/p_meta_data/Pfizer AESI_information.csv', sep = ';')
# AESI_info <- AESI_info[AESI_info$System != '' & AESI_info$Event_name != '' & AESI_info$Event_abbreviation != '' & !is.na(AESI_info$System) & !is.na(AESI_info$Event_name) & !is.na(AESI_info$Event_abbreviation),]

## For second interim : 
# 1. A function out.event_dose is created to avoid calculating loop of AESI_info for each dose. 
# 2. different format of output than the first interim.

#########################################################################################################
## Function to create table 9
#########################################################################################################

## NOTE: ANAPHYLAXIS is a  special case here, the results are not IR but proportion*10000, and not follow-up years but person count
## This should be clarified manually in the output

## Start function
fun_tab9 <- function(M_Studycohort, AESI_info){
  
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")
  AESI_info <- AESI_info[-which(AESI_info[,1] == "I_COVID_COV"),]
  ListNames <- dir(aesi_dir)
  
  aesi_files1 <- ListNames[grep("_T0", ListNames)]
  aesi_files2 <- ListNames[grep("_D2", ListNames)]
  aesi_files3 <- ListNames[grep("_D3", ListNames)]
  
  # check whether data on certain vaccination rounds are missing
  aesi_dose <- c(length(aesi_files1),length(aesi_files2), length(aesi_files3))
  if (any(aesi_dose == 0)) warning("All AESI files missing in at least one round of vaccination, values will be NAs")
  
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', 'FOURTH_OTHER', 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_PFIZER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  if(nrow(AESI_info) == 0) stop('AESI_info is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
  
  ## Select all vaccinated (ie before matching)
  MS <- subset(M_Studycohort, group != 'CONTROL')
  rownames(MS) <- NULL
  
  ## More data checks
  if(nrow(MS) == 0) 						stop('0 persons in M_Studycohort with group == EXPOSED or UNMATCHED')
  if(any(duplicated(MS$person_id)))				stop('Duplicated person_id values should not be possible in this selection.')
  ## To do: Add more checks, e.g. check if dates are not in the future, or if indeed all *_HIST dates > first_pfizer
  
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
             "id")
  
  d <- as.data.frame(MS)[, (which(colnames(MS) %in% colls))]
  
  ## Define the date of receiving covid19 vaccine after T0 
  d$other1 <- as.Date(d$FIRST_OTHER, format = '%Y-%m-%d')
  d$other1[is.na(d$other1) | d$other1 < d[, 'FIRST_PFIZER']] <- NA
  d$other2 <- as.Date(d$SECOND_OTHER, format = '%Y-%m-%d')
  d$other2[is.na(d$other2) | d$other2 < d[, 'FIRST_PFIZER']] <- NA
  d$other3 <- as.Date(d$THIRD_OTHER, format = '%Y-%m-%d')
  d$other3[is.na(d$other3) | d$other3 < d[, 'FIRST_PFIZER']] <- NA
  d$other4 <- as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d')
  d$other4[is.na(d$other4) | d$other4 < d[, 'FIRST_PFIZER']] <- NA
  d$other_aftert0 <- pmin(d$other1, d$other2, d$other3, d$other4, na.rm = TRUE)
  d$other1 <- d$other2 <- d$other3 <- d$other4 <- NULL
  
  ## function to return cumulative incidence and incidence rate for each vaccination round. 
  
  out.event.dose <- function(index, idx.used, idx.list, AESI, lookback_period, End_of_risk_window, list_aesi_files, d){
    
    if (idx.used[index] == 999) {vec.AESI <- rep("NA",8)
    } else {
      
      AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI)
      dose <- c("FIRST_PFIZER", "SECOND_PFIZER", "THIRD_PFIZER", "FOURTH_PFIZER")
      
      aesi_files <- list_aesi_files[[index]]
      aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx.list[[index]]])))
      
      if(index !=1) {dd <- d[which(!is.na(d[,dose[index]])),]} else {dd <- d} ## d has not been mentioned earlier
      
      combined_data <- as.data.frame(merge(x = dd, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
      
      ## Check which subjects to remove due to prior AESI
      if(lookback_period == 'Any'){
        combined_data <- combined_data[is.na(as.Date(combined_data$Date_HIST, format = '%Y-%m-%d')),]
      }
      if(lookback_period != 'Any'){
        combined_data <- combined_data[is.na(as.Date(combined_data$Date_HIST, format = '%Y-%m-%d')) | as.Date(combined_data$Date_HIST, format = '%Y-%m-%d') < as.Date(combined_data[,dose[index]], format = '%Y-%m-%d') - days(lookback_period),]
      }
      rownames(combined_data) <- NULL	
      
      ## Defining end of risk window
      combined_data$End_rw <- NA
      if(End_of_risk_window != 'Any') combined_data$End_rw <- as.Date(combined_data[,dose[index]], format = '%Y-%m-%d') + days(as.numeric(End_of_risk_window)) - 1
      combined_data$endfup <- pmin(combined_data$op_end_date, combined_data$other_aftert0, combined_data[,'Date_COUNT'], combined_data$End_rw, combined_data[,dose[index+1]] - 1, na.rm = TRUE)
      
      if(any(is.na(combined_data$endfup))) stop('NA values in follow-up times; check data and code')
      combined_data$fupdays <- as.numeric(difftime(combined_data$endfup, as.Date(combined_data[,dose[index]], format = '%Y-%m-%d'), unit = 'days')) + 1
      ## Note: 1 is added in line with other tables (to avoid 0 fupdays)
      
      if(any(combined_data$fupdays < 1)) {
        warning(paste0('Dose ', index,' calculations stopped due to 0 or negative follow-up times; check data and code (eg check if all ' ,dose[index],' dates < op_end_date). 
                     For the moment, negative follow-up times will be removed'))
        idx.negative.fupdays <- which(combined_data$fupdays < 1)
        combined_data <- combined_data[-idx.negative.fupdays,]
      }			
      
      ## Computation starts here 
      
      combined_data$event1 <- ifelse(!is.na(combined_data[,'Date_COUNT']) & (as.Date(combined_data[,'Date_COUNT'], format = '%Y-%m-%d') == combined_data$endfup), 1, 0)
      n.persons <- nrow(combined_data)
      n.fupdays <- sum(combined_data$fupdays)
      n.events <- sum(combined_data$event1)
      n.fupyrs <- n.fupdays / 365.25
      
      if (End_of_risk_window == "Any") {End_of_risk_window <- "365"}
      
      ## For anapyhaxlis, change fupyrs to n.persons
      if(AESI == 'ANAPHYLAXIS') n.fupyrs <- n.persons
      
      ## KM
      #if(End_of_risk_window != 'Any' & AESI != 'ANAPHYLAXIS'){
      if(AESI != 'ANAPHYLAXIS'){
        survf <- survfit(Surv(time = combined_data$fupdays, event = combined_data$event1) ~ 1)		## Note that this does not work with 0 events (lb and up are both 1)
        ## Note: If max(d$fupdays) < End_of_risk_window, we cannot
        ## determine the 1-KM at end_of_risk_window. So in that case, return NA
        if(max(combined_data$fupdays) < (0 + as.numeric(End_of_risk_window))){
          one_min_km <- one_min_km.lb <- one_min_km.ub <- NA
        }
        if(max(combined_data$fupdays) >= (0 + as.numeric(End_of_risk_window))){
          km <- summary(survf, times = as.numeric(End_of_risk_window))
          one_min_km <- 1 - km$surv
          one_min_km.lb <- 1 - km$upper
          one_min_km.ub <- 1 - km$lower
          one_min_km <- 10000 * one_min_km		
          one_min_km.lb <- 10000 * one_min_km.lb	
          one_min_km.ub  <- 10000 * one_min_km.ub				
        }
      }
      #if(End_of_risk_window == 'Any' | AESI == 'ANAPHYLAXIS'){
      if(AESI == 'ANAPHYLAXIS'){  
        one_min_km <- one_min_km.lb <- one_min_km.ub <- NA
      }
      
      if(AESI != 'ANAPHYLAXIS'){
        ## (exact*) IR + 95% CI: *note; referred to Dobson in SAP but in fact the exact 
        IR <- n.events / n.fupyrs
        IR.lb <- max(0, (qchisq(0.025, df = 2 * n.events) / 2) / n.fupyrs)
        IR.ub <-(qchisq(0.975, df = 2 * (n.events + 1)) / 2) / n.fupyrs
        IR <- 10000 * IR		
        IR.lb <- 10000 * IR.lb		
        IR.ub <- 10000 * IR.ub
      }
      
      if(AESI == 'ANAPHYLAXIS'){	## Note: This is actually the proportion * 10000, not IR.
        IR <- 10000 * n.events / n.persons
        ci.tmp <- binconf(x = n.events, n = n.persons, alpha = 0.05, method = "wilson")
        IR.lb <- 10000 * ci.tmp[2]
        IR.ub <- 10000 * ci.tmp[3]
      }
      
      vec.AESI <- c(n.events, one_min_km, one_min_km.lb, one_min_km.ub, n.fupyrs, IR, IR.lb, IR.ub)}
    
    rm(dd, AESI, IR, IR.lb, IR.ub, lookback_period, n.persons, n.fupdays, n.events, n.fupyrs, End_of_risk_window)
    gc()
    
    return(vec.AESI)
  }
  
  ## Initiate the container 
  
  ir.per.tab <- matrix(NA,nrow = nrow(AESI_info), ncol = 24)
  
  for(i in 1:nrow(AESI_info)){
    
    AESI <- AESI_info$Event_abbreviation[i]
    
    list_aesi_files <- list(aesi_files1, aesi_files2, aesi_files3)
    
    ## Create a variable idx.used to indicate whether AESI at certain vaccination round is missing. 
    
    idx.list <- lapply(list_aesi_files, grep, pattern = toupper(as.character(AESI_info$Event_abbreviation[i])))
    if (all(lapply(idx.list, length) == 0)){
      print(paste0("No files of ",AESI," in any vaccination round"))
    next}  # if there is no files of AESI in each vaccination round, then go to next AESI
    
    idx.used <- c(1:3)
    if(any(lapply(idx.list, length) == 0)) {
      temp <- which(lapply(idx.list, length) == 0) 
      idx.used[temp] <- 999
      print(paste0("No files of ",AESI," in vaccination round",temp))
    } 
    
    End_of_risk_window <- AESI_info$End_of_risk_window[i]
    lookback_period <- AESI_info$lookback_period[i]
    
    ir.per.tab[i,] <- as.numeric(sapply(c(1:3), out.event.dose, idx.used = idx.used,idx.list = idx.list, AESI = AESI, lookback_period = lookback_period, End_of_risk_window = End_of_risk_window, list_aesi_files = list_aesi_files, d = d))
    rm(AESI, idx.list, idx.used, End_of_risk_window, lookback_period)
    print(i)      
  }
  numdec <- 1
  d9 <- round(ir.per.tab, numdec)
  d9[,seq(1,ncol(d9), by = 8)] <- apply(d9[,seq(1,ncol(d9), by = 8)], c(1,2), function(x) ifelse(x < 5 & x != 0,'<5', x))
  d9 <- data.frame(cbind(AESI_info[,1:2],d9))
  
  AESI_Syst <- unique(AESI_info$System)
  sta.v <- sto.v <- d9.p <- NULL
  for (i in 1:length(AESI_Syst)){
    temp <- which(AESI_info$System == AESI_Syst[i])
    sta.v[i] <- min(temp)
    sto.v[i] <- max(temp)
    d9.p <- rbind(d9.p, c(AESI_Syst[i], AESI_Syst[i],rep(" ", 24)), d9[sta.v[i]:sto.v[i],])
  }
  
  d9.p <- d9.p[,-1]
  #d9 <- as.data.frame(apply(d9,c(1,2), as.character))
  colnames(d9.p) <- c('Event_name',"n.events.d1", "CuI.d1", "CuI.lb.d1", "CuI.ub.d1", "fupyrs.d1", "IR.d1", "IR.lb.d1", "IR.ub.d1",
                    "n.events.d2", "CuI.d2", "CuI.lb.d2", "CuI.ub.d2", "fupyrs.d2", "IR.d2", "IR.lb.d2", "IR.ub.d2",
                    "n.events.d3", "CuI.d3", "CuI.lb.d3", "CuI.ub.d3", "fupyrs.d3", "IR.d3", "IR.lb.d3", "IR.ub.d3")
  

  return(d9.p)
}
