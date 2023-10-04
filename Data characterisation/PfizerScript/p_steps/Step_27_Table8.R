### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl) and Roel Elbers (R.J.H.Elbers@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim : 3 Aug 2022; 3rd interim : 5 Dec 2022

## For development:
#  dat.loc <- '.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- readRDS(paste0(dat.loc, 'M_Studycohort3.rds'))
#  AESI_info <- read.csv('.../rbor2/Github_Pfizer_addfiles/AESI_information.csv')
#	library(lubridate)
#


## Edited in this version: a function out.event is created to eliminate the use of loop for each 
# time point. 
# Confidence interval is created into two separate columns : lower bound (lb) and upper bound (ub)

## Edited for the fourth interim: remove I_COVID_COV and aesthetic table
#########################################################################################################
## Function to create table 8
#########################################################################################################

## Start function
fun_tab8 <- function(M_Studycohort, AESI_info){
  
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
  
  # Check the reason for removing this
  AESI_info <- AESI_info[-which(AESI_info[,1] == "I_COVID_COV"),]
  
  ## IM added this
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
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
             "Date_HIST",
             "Date_COUNT",
             "id"
  )
  
  d <- as.data.frame(MS)[, (which(colnames(MS) %in% colls))]
  
  ## Check censoring due to end of observation period, other vaccine or having the event post-t0:
  ## Get date of first non-pfizer vaccine after first pfizer
  d$other1 <- as.Date(d$FIRST_OTHER, format = '%Y-%m-%d')
  d$other1[is.na(d$other1) | d$other1 < d$FIRST_PFIZER] <- NA
  d$other2 <- as.Date(d$SECOND_OTHER, format = '%Y-%m-%d')
  d$other2[is.na(d$other2) | d$other2 < d$FIRST_PFIZER] <- NA
  d$other3 <- as.Date(d$THIRD_OTHER, format = '%Y-%m-%d')
  d$other3[is.na(d$other3) | d$other3 < d$FIRST_PFIZER] <- NA
  d$other4 <- as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d')
  d$other4[is.na(d$other4) | d$other4 < d$FIRST_PFIZER] <- NA
  d$other_aftert0 <- pmin(d$other1, d$other2, d$other3, d$other4, na.rm = TRUE)
  d$other1 <- d$other2 <- d$other3 <- d$other4 <- NULL
  
  ## Loop over AESIs
  # ncol refers to number of columns in the table shell
  ir.per.tab <- matrix(NA, nrow = nrow(AESI_info), ncol = 25)
  
  ## function to return the output
  out.event <- function(index, obj.dat){ #returning the number of events per time point of interest 
    
    index <- rep(index, nrow(obj.dat))
    sta <- c(0,42,90, 180, 270)
    ad.vec <- c(41,89, 179, 269, 359)
    
    start1 <- as.Date(ifelse(index == rep(1,nrow(obj.dat)), 
                             as.Date(obj.dat$FIRST_PFIZER, format = '%Y-%m-%d'),
                             as.Date(obj.dat$FIRST_PFIZER, format = '%Y-%m-%d') + days(sta[index])) , format = "%Y-%m-%d", origin = "1970-01-01")
    start1[start1 > obj.dat$endfup] <- NA
    
    end1 <- as.Date(obj.dat$FIRST_PFIZER, format = '%Y-%m-%d') + days(ad.vec[index])
    
    end1[is.na(start1)] <- NA
    end1 <- pmin(end1, obj.dat$endfup)
    
    fupdays1 <- as.numeric(difftime(end1, start1, unit = 'days')) + 1 
    events1 <- ifelse(!is.na(obj.dat[,'Date_COUNT']) & !is.na(fupdays1) & 
                        (as.Date(obj.dat[,'Date_COUNT'], format = '%Y-%m-%d') >= start1 & 
                           as.Date(obj.dat[,'Date_COUNT'], format = '%Y-%m-%d') <= end1),
                      1, 0)
    nevents <- sum(events1)
    pyrs <- sum(fupdays1, na.rm = T) / 365.25
    IR <- 10000 * nevents / pyrs
    CI.l <- 10000 * max(0, (qchisq(0.025, df = 2 * nevents) / 2) / pyrs)
    CI.u <- 10000 * (qchisq(0.975, df = 2 * (nevents + 1)) / 2) / pyrs
    vec.AESI <- c(round(nevents,0), round(pyrs,1), round(IR,2), round(CI.l,2), round(CI.u,2))
    return(vec.AESI)
  }
  
  ## Start loop
  for(i in 1:nrow(AESI_info)){
    
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_info[i,1]
    AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI)
    
    combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
    
    lookback_period <- AESI_info$lookback_period[i]
    
    ## Check which subjects to remove due to prior AESI
    if(lookback_period == 'Any'){
      combined_data <- combined_data[is.na(as.Date(combined_data$Date_HIST, format = '%Y-%m-%d')),]
    }
    if(lookback_period != 'Any'){
      combined_data <- combined_data[is.na(as.Date(combined_data$Date_HIST, format = '%Y-%m-%d')) | as.Date(combined_data$Date_HIST, format = '%Y-%m-%d') < as.Date(combined_data$FIRST_PFIZER, format = '%Y-%m-%d') - days(lookback_period),]
    }
    rownames(combined_data) <- NULL	
    
    # Defining end of follow-up, the min of end of study, other vaccine brand, or the time of the occurence of AESI
    combined_data$endfup <- pmin(combined_data$op_end_date, combined_data$other_aftert0, combined_data[,'Date_COUNT'], na.rm = TRUE)
    if(any(is.na(combined_data$endfup))) stop('NA values in follow-up times; check data and code')
    
    obj.dat <- combined_data
    
    ir.per.tab[i,] <- as.numeric(sapply(c(1:5), out.event, obj.dat = combined_data), by = col)
    
    if (AESI == 'ANAPHYLAXIS'){ # Incidence rate only be calculated at 1st timw window and the interpretation is prevalence per 10,000 people
      n.event <- ir.per.tab[i,1]
      ntotal <- nrow(obj.dat)
      
      pr1 <- n.event/ntotal
      pr1.95ci <- binconf(x = n.event, n = ntotal, alpha = 0.05, method = "wilson")
      window1 <- c(n.event, NA, round(pr1* 10000,2), round(pr1.95ci[2:3] * 10000,2), rep(NA,20) )
      ir.per.tab[i,] <- window1
    }
    rm(combined_data, AESI , obj.dat, aesi_data, lookback_period)
    gc()
    #print(i)
  }
  
  ## Formatting
  
  ir.per.tab[,seq(1, ncol(ir.per.tab), by = 5)] <- apply(ir.per.tab[,seq(1,ncol(ir.per.tab), by = 5)], c(1,2), 
                                                          function(x) ifelse(x < 5 & x != 0,'<5', x))
  d8 <- data.frame(cbind(AESI_info[,c('Event_name','System')],ir.per.tab))
  
  AESI_Syst <- unique(AESI_info$System)
  sta.v <- sto.v <- d8.p <- NULL
  for (i in 1:length(AESI_Syst)){
    temp <- which(AESI_info$System == AESI_Syst[i])
    sta.v[i] <- min(temp)
    sto.v[i] <- max(temp)
    d8.p <- rbind(d8.p, c(AESI_Syst[i], AESI_Syst[i],rep(" ", 25)), d8[sta.v[i]:sto.v[i],])
  }
  
  d8.p <- d8.p[,-2]
  ## Formatting
  
  ## Return output
  print("Note: for Anaphylaxis the incidence rate (IR) is calculated only at the first interval. The IR is the prevalence in 10,000 persons")
  return(d8.p)
}
