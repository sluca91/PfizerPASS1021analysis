### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl) and Roel Elbers (R.J.H.Elbers@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited : 12 Sep 2022

## For development:
# dat.loc <- '..../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- as.data.frame(readRDS(paste0(dat.loc, 'M_Studycohort3.rds')))
#  AESI_info <- read.csv('..../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/p_meta_data/Pfizer AESI_information.csv', sep = ';')
# AESI_info <- AESI_info[AESI_info$System != '' & AESI_info$Event_name != '' & AESI_info$Event_abbreviation != '' & !is.na(AESI_info$System) & !is.na(AESI_info$Event_name) & !is.na(AESI_info$Event_abbreviation),]


#	library(lubridate)
#library(survival)
#library(geeM)
#library(Hmisc)

## For the 2nd interim, 
# Two internal functions are created to simplify the computation of Cumulative incidence and 
# IR. 
# The version of 2 Aug 2022 altered the formulation of end of risk window.
# The version of 30 Aug 2022 removed the stop criteria due to negative or zero follow up.
# The version of 31 Aug 2022 address UOSL which has problem in merging data. 
# Version 12 Sep 2022: return to the version of 30 Aug and eliminate redundancy. Save the dataframe d into g_intermediate to be used in figure 2

#########################################################################################################
## Function to create table 16
#########################################################################################################

## NOTE: ANAPHYLAXIS is a  special case here, the results are not IR but proportion*10000, and not follow-up years but person count
## This should be clarified manually in the output

## CumInc and IncR are two internal functions. 
CumInc <- function(obj.dat, End_of_risk_window, AESI){
  AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI)  
  
  if (End_of_risk_window == "Any") {End_of_risk_window <- "365"}
  
  #Basic info
  n.persons <- nrow(obj.dat)					## Including duplicated IDs, 
  n.persons.un <- length(unique(obj.dat$person_id))	## Unique IDs, 
  n.fupdays <- sum(obj.dat$fupdays)
  n.events <- sum(obj.dat$event)
  n.fupyrs <- n.fupdays / 365.25
  
  if(AESI == 'ANAPHYLAXIS') n.fupyrs <- NA #n.fupyrs <- n.persons
  
  ## This is to calculate the KM, for Exposed group, there should be no duplicated person_id, but in the control group there is 
  ## a possibility that person_id is duplicated. 
  
  #if(AESI != 'ANAPHYLAXIS' & End_of_risk_window != 'Any' & max(obj.dat$fupdays) >= ifelse(End_of_risk_window != 'Any', 0 + as.numeric(End_of_risk_window), -Inf)){ 
  if(AESI != 'ANAPHYLAXIS' & max(obj.dat$fupdays) >= ifelse(End_of_risk_window != 'Any', 0 + as.numeric(End_of_risk_window), -Inf)){ 
    
    ## Note: If max(d$fupdays) < End_of_risk_window, we cannot determine the 1-KM at end_of_risk_window. So in that case, return NA
    
    if(!any(duplicated(obj.dat$person_id))){
      survf <- survfit(Surv(time = obj.dat$fupdays, event = obj.dat$event) ~ 1)								## Note that this does not work with 0 events (lb and up are both 1)
    }
    if(any(duplicated(obj.dat$person_id))){
      obj.dat2 <- obj.dat[order(obj.dat$person_id),]
      survf <- survfit(Surv(time = fupdays, event = event) ~ 1, id = person_id, data = obj.dat2, robust = T)		## Note that this does not work with 0 events (lb and up are both 1) + handling ofclustering needs verification
    }
    km <- summary(survf, times = as.numeric(End_of_risk_window))
    G_one_min_km <- 1 - km$surv
    G_one_min_km.lb <- 1 - km$upper
    G_one_min_km.ub <- 1 - km$lower
    G_one_min_km <- 10000 * G_one_min_km		
    G_one_min_km.lb <- 10000 * G_one_min_km.lb	
    G_one_min_km.ub  <- 10000 * G_one_min_km.ub	
    rm(survf);rm(km)							
  }
  #if(AESI == 'ANAPHYLAXIS' | End_of_risk_window == 'Any' | max(obj.dat$fupdays) < ifelse(End_of_risk_window == 'Any', Inf, 0 + as.numeric(End_of_risk_window))){
  if(AESI == 'ANAPHYLAXIS' | max(obj.dat$fupdays) < ifelse(End_of_risk_window == 'Any', Inf, 0 + as.numeric(End_of_risk_window))){
    G_one_min_km <- G_one_min_km.lb <- G_one_min_km.ub <- NA
  }			
  vec.AESI.km <- c(n.events, G_one_min_km, G_one_min_km.lb, G_one_min_km.ub)
  rm(AESI)
  return( vec.AESI.km)
}


## Incidence Rate

IncR <- function(obj.dat, AESI){
  AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI) 
  n.persons <- nrow(obj.dat)					## Including duplicated IDs, 
  n.persons.un <- length(unique(obj.dat$person_id))	## Unique IDs, 
  n.fupdays <- sum(obj.dat$fupdays)
  n.events <- sum(obj.dat$event)
  n.fupyrs <- n.fupdays / 365.25
  
  if(AESI != 'ANAPHYLAXIS'){
    ## d_U: (exact*) IR + 95% CI: *note; referred to Dobson in SAP but in fact the exact. If duplicated ID values, use geem
    if(!any(duplicated(obj.dat$person_id))){
      G_IR <- n.events / n.fupyrs
      G_IR.lb <- max(0, (qchisq(0.025, df = 2 * n.events) / 2) / n.fupyrs)
      G_IR.ub <-(qchisq(0.975, df = 2 * (n.events + 1)) / 2) / n.fupyrs
    }
    if(any(duplicated(obj.dat$person_id))){
      obj.dat2 <- obj.dat[order(obj.dat$person_id),]
      obj.dat2$ln.fupyrs <- log(obj.dat2$fupdays / 365.25)
      if(!any(obj.dat2$event != 0)){ 	## geem() will fail when there are 0 events.
        G_IR <- G_IR.lb <- G_IR.ub <- NA
      }
      if(any(obj.dat2$event != 0)){
        gee_poisson <- geem(event ~ 1 + offset(ln.fupyrs), id = person_id, family = 'poisson', data = obj.dat2)	
        G_logIR <- as.numeric(coef(gee_poisson))
        G_IR <- exp(G_logIR)
        G_IR.lb <- exp(as.numeric(G_logIR + qnorm(0.025) * summary(gee_poisson)$se.robust))
        G_IR.ub <- exp(as.numeric(G_logIR + qnorm(0.975) * summary(gee_poisson)$se.robust))
      }
      rm(obj.dat2)
    }
    G_IR <- 10000 * G_IR		
    G_IR.lb <- 10000 * G_IR.lb		
    G_IR.ub <- 10000 * G_IR.ub
  } # end of if AESI not equal ANAphylaxis
  
  if(AESI == 'ANAPHYLAXIS'){ ## Note: This is actually the proportion * 10000, not IR.
    n.fupyrs <- NA
    if(!any(duplicated(obj.dat$person_id))){
      G_IR <- 10000 * n.events / n.persons
      ci.tmp <- binconf(x = n.events, n = n.persons, alpha = 0.05, method = "wilson")
      G_IR.lb <- 10000 * ci.tmp[2]
      G_IR.ub <- 10000 * ci.tmp[3]
      rm(ci.tmp)			
    }
    if(any(duplicated(obj.dat$person_id))){
      obj.dat2 <- obj.dat[order(obj.dat$person_id),]
      obj.dat2$offs <- 0	## Check - this effectively means that we do not use an offset. But it's
      ## the only option that returns an estimate (very) close to the sample proportion
      ## (at least based on a quick check)
      if(!any(obj.dat2$event != 0)){ 	## geem() will fail when there are 0 events.
        G_IR <- G_IR.lb <- G_IR.ub <- NA
      }
      if(any(obj.dat2$event != 0)){			
        gee_poisson <- geem(event ~ 1 + offset(offs), id = person_id, family = 'poisson', data = obj.dat2)
        G_logIR <- as.numeric(coef(gee_poisson))
        G_IR <- 10000 * exp(G_logIR)
        G_IR.lb <- 10000 * exp(as.numeric(G_logIR + qnorm(0.025) * summary(gee_poisson)$se.robust))
        G_IR.ub <- 10000 * exp(as.numeric(G_logIR + qnorm(0.975) * summary(gee_poisson)$se.robust))
      }
      rm(obj.dat2)
    }
  }
  
  vec.AESI <- c(n.fupyrs, G_IR, G_IR.lb, G_IR.ub)
  rm(AESI)
  return(vec.AESI)
}


## START FUNCTION

# fun_tab16 <- function(M_Studycohort, AESI_info){
#   
#   AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
#   
#   ## IM added this
#   ListNames <- dir(aesi_dir)
#   aesi_files <- ListNames[grep("_T0",ListNames)]
#   
#   ## A few basic checks on the data:
#   if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
#   if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', "FOURTH_OTHER", 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_OTHER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
#   if(nrow(AESI_info) == 0) stop('AESI_info is empty')
#   if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
#   
#   ## Copy data
#   d <- subset(M_Studycohort, group != 'UNMATCHED')
#   rm(M_Studycohort)
#   gc()
#   
#   ## More data checks
#   if(nrow(d) == 0) stop('0 persons with group == EXPOSED or CONTROL')
#   if(any(duplicated(d$person_id[d$group == 'Exposed'])))	stop('Duplicated person_id values should not be possible in the exposed population.')
#   ## To do: Add more checks, e.g. check if dates are not in the future, or if indeed all *_HIST dates < first_pfizer
#   
#   
#   ## Set other to NA if before t0
#   d$FIRST_OTHER[as.Date(d$FIRST_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
#   d$SECOND_OTHER[as.Date(d$SECOND_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
#   d$THIRD_OTHER[as.Date(d$THIRD_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
#   d$FOURTH_OTHER[as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
#   
#   ## Find earliest date of other vaccination after t0
#   
#   d$other_aftert0 <- pmin(as.Date(d$FIRST_OTHER, format = '%Y-%m-%d'), as.Date(d$SECOND_OTHER, format = '%Y-%m-%d'), as.Date(d$THIRD_OTHER, format = '%Y-%m-%d'), as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d'), na.rm = T)
#   
#   ## For unexposed, also include the date of first pfizer vaccin
#   d$oth_pfiz_aftert0 <- d$other_aftert0 
#   d$oth_pfiz_aftert0[d$group == 'CONTROL'] <- pmin(as.Date(d$other_aftert0[d$group == 'CONTROL'], format = '%Y-%m-%d'),
#                                                    as.Date(d$FIRST_PFIZER[d$group == 'CONTROL'], format = '%Y-%m-%d'), na.rm = T)
#   
#   ## Per pair, select the earliest date of oth_pfiz_aftert0 (this is when the pair should be censored)
#   
#   print("Start new code")
#   TEMP <- as.data.table(d)[,.(person_id, id, oth_pfiz_aftert0)][!is.na(oth_pfiz_aftert0),]
#   TEMP <- TEMP[, .(censdate = min(oth_pfiz_aftert0)), by = "id"]
#   d <- merge(x = d, y = TEMP, by = "id" , all.x = T)
#   rm(TEMP)
#   gc()
#   print("End new code")
#   
#   d$op_end_date <- pmin(as.Date(d$op_end_date, format = '%Y-%m-%d'), as.Date(d$censdate, format = '%Y-%m-%d'), na.rm = T)		
#   
#   ## Save it to be used in table 20 and figure 2. 
#   saveRDS(d, file = paste0(populations_dir, "matched_pop.rds"))
#   
#   ir.per.tab <- matrix(NA,nrow = nrow(AESI_info), ncol = 16)
#   
#   for(i in 1:nrow(AESI_info)){
#     idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
#     if (length(idx) == 0) next
#     aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
#     
#     AESI <- AESI_info[i,1]
#     
#     combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
#     
#     End_of_risk_window <- AESI_info$End_of_risk_window[i]
#     lookback_period <- AESI_info$lookback_period[i]
#     
#     ## excluding pairs who have prior AESI
#     # just to be safe for the moment, because we might differentiate between exposed and control
#     if(lookback_period == 'Any'){
#       pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data$Date_HIST, format = "%Y-%m-%d"))])
#       combined_data <- combined_data[!combined_data$id %in% pairs.excl,]
#     }
#     if (lookback_period != 'Any'){
#       pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d')) & as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period)]) 
#       combined_data <- combined_data[!combined_data$id %in% pairs.excl,]      
#     }
#     
#     ## Check censoring due to end obs period, risk window or event (other vaccine already included above)
#     ## Note: It is assumed that obs_end_dates can never be later than death_date (if not NA). If this is not correct then
#     ## the code below will yield erroneous results.
#     
#     combined_data$End_rw <- NA
#     if(End_of_risk_window != 'Any') combined_data$End_rw <- as.Date(combined_data$T0, format = '%Y-%m-%d') + days(as.numeric(End_of_risk_window)) - 1
#     combined_data$endfup <- pmin(combined_data$op_end_date, combined_data[,'Date_COUNT'], combined_data$End_rw, na.rm = TRUE)
#     if(any(is.na(combined_data$endfup))) stop('NA values in follow-up times; check data and code')
#     combined_data$fupdays <- as.numeric(difftime(combined_data$endfup, as.Date(combined_data$T0, format = '%Y-%m-%d'), unit = 'days')) + 1 ## Note: 1 added in line with other tables to avoid 0 fupdays
#     
#     if(any(combined_data$fupdays < 1)) {warning('Persons with 0 or a negative number of follow-up days in the data, these entries will be removed')
#       #combined_data <- combined_data[-which(combined_data$fupdays < 1),]
#       idx.negative.fupdays <- which(combined_data$fupdays < 1)
#       combined_data <- combined_data[-idx.negative.fupdays,]
#     }
#     
#     combined_data$event <- ifelse(!is.na(combined_data[,'Date_COUNT']) & as.Date(combined_data[,'Date_COUNT'], format = '%Y-%m-%d') == combined_data$endfup, 1, 0)
#     
#     colls <- c("person_id",
#                "FIRST_PFIZER",
#                "SECOND_PFIZER",
#                "THIRD_PFIZER",
#                "FOURTH_PFIZER",
#                "FIRST_OTHER",
#                "SECOND_OTHER",
#                "THIRD_OTHER",
#                "FOURTH_OTHER",
#                "op_end_date",
#                'Date_HIST',
#                'Date_COUNT',
#                "id",
#                "T0",
#                "End_rw",
#                "endfup",
#                "fupdays",
#                "event",
#                "group"
#     )
#     
#     d_E <- combined_data[combined_data$group == 'EXPOSED', c(colnames(combined_data) %in% colls)]
#     d_U <- combined_data[combined_data$group == 'CONTROL', colnames(combined_data) %in% colls]
#     
#     if(nrow(d_E) == 0){
#       warning(paste0('0 persons in exposed set for ', AESI, '. AESI skipped.'))
#       next
#     }
#     if(nrow(d_U) == 0){
#       warning(paste0('0 persons in unexposed set for ', AESI, '. AESI skipped.'))
#       next
#     }
#     
#     rm(colls)
#     gc()
#     
#     ir.per.tab[i,] <- c(CumInc(d_E, End_of_risk_window, AESI), IncR(d_E, AESI), CumInc(d_U, End_of_risk_window, AESI), IncR(d_U, AESI))
#     #print(i)
#     rm(combined_data,d_E, d_U)
#     
#   }    
#   numdec <- 1  
#   d16 <- cbind(AESI_info[,c('System','Event_name')],round(ir.per.tab, numdec))
#   colnames(d16)[3:ncol(d16)] <- c("VAC_N","VAC_CumInc","VAC_CI.lb","VAC_CI.ub","VAC_NumPerYear","VAC_IR","VAC_IR_CI.lb","VAC_IR_CI.ub",
#                                   "CTR_N","CTR_CumInc","CTR_CI.lb","CTR_CI.ub","CTR_NumPerYear","CTR_IR","CTR_IR_CI.lb","CTR_IR_CI.ub")
#   return(d16)
# }

fun_tab16 <- function(d_matchedpop, AESI_info){ # trial version 22-11 - 18
  
  d <- d_matchedpop
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
  AESI_info <- AESI_info[-which(AESI_info[,1] == "I_COVID_COV"), ]
  
  ## IM added this
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
  ir.per.tab <- matrix(NA,nrow = nrow(AESI_info), ncol = 16)
  
  for(i in 1:nrow(AESI_info)){
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_info[i,1]
    print(paste0("Computing AESI : ", AESI))
    
    combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
    
    End_of_risk_window <- AESI_info$End_of_risk_window[i]
    lookback_period <- AESI_info$lookback_period[i]
    
    ## excluding pairs who have prior AESI
    # just to be safe for the moment, because we might differentiate between exposed and control
    if(lookback_period == 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data$Date_HIST, format = "%Y-%m-%d"))])
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]
    }
    if (lookback_period != 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d')) & as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period)]) 
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]      
    }
    
    if (nrow(combined_data) == 0) {warning(paste0("In computing AESI ", AESI, ", all subjects were excluded due to prior outcomes"))
      next}
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
    
    ir.per.tab[i,] <- c(CumInc(d_E, End_of_risk_window, AESI), IncR(d_E, AESI), CumInc(d_U, End_of_risk_window, AESI), IncR(d_U, AESI))
    #print(i)
    rm(combined_data,d_E, d_U)
    
  }    
  numdec <- 2  
  d16 <- data.frame(cbind(AESI_info[,c('Event_name')],round(ir.per.tab, numdec)))
  d16[,c(2,10)] <- apply(d16[,c(2,10)],c(1,2), function(x) ifelse(x < 5 & x != 0,'<5', x))
  
  AESI_Syst <- unique(AESI_info$System)
  sta.v <- sto.v <- d16.p <- NULL
  for (i in 1:length(AESI_Syst)){
    temp <- which(AESI_info$System == AESI_Syst[i])
    sta.v[i] <- min(temp)
    sto.v[i] <- max(temp)
    d16.p <- rbind(d16.p, c(AESI_Syst[i],rep(" ", 16)), d16[sta.v[i]:sto.v[i],])
  }
  
  
  colnames(d16.p)[2:ncol(d16.p)] <- c("VAC_N","VAC_CumInc","VAC_CI.lb","VAC_CI.ub","VAC_NumPerYear","VAC_IR","VAC_IR_CI.lb","VAC_IR_CI.ub",
                                  "CTR_N","CTR_CumInc","CTR_CI.lb","CTR_CI.ub","CTR_NumPerYear","CTR_IR","CTR_IR_CI.lb","CTR_IR_CI.ub")
  return(d16.p)
}
