### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim: 29 July 2022

	## For development:
	#  dat.loc <- '.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
	 # M_Studycohort3 <- readRDS(paste0(dat.loc, 'M_Studycohort3.rds'))
	 #  AESI_info <- read.csv('.../Github_Pfizer_addfiles/AESI_information.csv')
	#	library(lubridate)
	#

  ## For the second interim :
  # 1. The input has different format than the first interim.
  # 2. The output will be just a content of a table with its header without rownames. 

  ## For the fourth interim 
  # Aesthetic table, "The good things take time, that's why I'm late"
#########################################################################################################
## Function to create table 6
#########################################################################################################

	## Start function
fun_tab6 <- function(M_Studycohort, AESI_info){
  
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "uni")
  ## Note for DM1, we will remove subjects with AESI anytime before T0 since those with DM1 (even more than 1 year before T0) still has it and will never got covid19 vaccination
  
  #AESI_info <- AESI_info[-which(AESI_info$Event_abbreviation == "I_COVID19VAED_AESI"),]
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', 'FOURTH_OTHER', 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_OTHER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  if(nrow(AESI_info) == 0) stop('AESI_info is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
  
  ## Select all vaccinated (ie before matching)
  MS <- subset(M_Studycohort, group != 'CONTROL')
  rownames(MS) <- NULL
  
  ## More data checks
  if(nrow(MS) == 0) 						stop('0 persons in M_Studycohort with group == EXPOSED or UNMATCHED')
  if(any(duplicated(MS$person_id)))				stop('Duplicated person_id values should not be possible in this selection.')
  
  ## To do: Add more checks, e.g. check if dates are not in the future, or if indeed all *_HIST dates > first_pfizer
  
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
  d <- MS
  n.total <- nrow(d)
  AESI_info$n.excl <- NA
  
  for (i in 1:nrow(AESI_info)){
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- readRDS(paste0(aesi_dir,aesi_files[idx]))
    combined_data <- merge(x = as.data.table(d), y = aesi_data, by = c("person_id","id"), all.x = TRUE)
    
    # The rest we will follow the prevous code 
    # ------------------------------------
    lookback_period <- AESI_info$lookback_period[i]
    
    if(lookback_period == 'Any'){
      AESI_info$n.excl[i] <- sum(!is.na(as.Date(combined_data$Date_HIST, format = '%Y-%m-%d') ))
    }
    
    if(lookback_period != 'Any'){
      AESI_info$n.excl[i] <- sum(!is.na(as.Date(combined_data$Date_HIST, format = '%Y-%m-%d')) & as.Date(combined_data$Date_HIST, format = '%Y-%m-%d') >= as.Date(combined_data$FIRST_PFIZER, format = '%Y-%m-%d') - days(lookback_period))
    }
    
    rm(aesi_data); rm(combined_data); rm(lookback_period)
  }
  # ------------------------------------
  # End of previous code
  numdec <- 2
  AESI_info$perc.excl <- round(100 * AESI_info$n.excl / n.total,numdec)
  
  ## Aesthetic part goes here 
  d6 <- as.data.frame(AESI_info[,c('Event_name', 'System','n.excl','perc.excl')])
  
  AESI_Syst <- unique(d6$System)
  sta.v <- sto.v <- n.syst.v <- perc.syst.v <- d6.p <- NULL
  for (i in 1:length(AESI_Syst)){
    temp <- which(d6$System == AESI_Syst[i])
    sta.v[i] <- min(temp)
    sto.v[i] <- max(temp)
    n.syst.v[i] <- sum(d6$n.excl[temp], na.rm = TRUE)
    perc.syst.v[i] <- round(n.syst.v[i]/n.total, numdec)
    d6.p <- rbind(d6.p, c(AESI_Syst[i],AESI_Syst[i],n.syst.v[i], perc.syst.v[i]), d6[sta.v[i]:sto.v[i],])
    }
  
  ## Format table (ready for markdown):
  # numdec <- 1
  # d6 <- AESI_info
  # d6 <- d6[,c('System', 'Event_name', 'n.excl', 'perc.excl')]
  # d6$n.excl <- as.character(d6$n.excl)
  # d6$perc.excl <- as.character(format(round(d6$perc.excl, numdec), nsmall = numdec, trim = TRUE))
  # d6 <- data.frame(
  #   col1 = c('', '', d6$System),
  #  col2 = c('', 'Prior AESI', d6$Event_name),
  #   col3 = c(paste0('Individuals who received a first dose of the Pfizer-BioNTech COVID 19 vaccine (before matching) (N=', n.total, ')'),
  #            'n', d6$n.excl),
  #   col4 = c(paste0('Individuals who received a first dose of the Pfizer-BioNTech COVID 19 vaccine (before matching) (N=', n.total, ')'),
  #            '%', d6$perc.excl)
  #)
  
  # AESI_info$n.excl <- as.character(AESI_info$n.excl)
  # AESI_info$perc.excl <- as.character(round(AESI_info$perc.excl, numdec))
  d6.p <- d6.p[-which(d6.p$Event_name == "Myocarditis and pericarditis"), ]  # For exclusion criteria we will consider either of these therefore myo and peri is not included
  d6.p <- rbind(c("Total vaccinated"," ", n.total, 100), d6.p)
  d6.p <- d6.p[,-2]
  
  d6.p[,2] <- sapply(as.numeric(d6.p[,2]), function(x) ifelse(x < 5 & x!= 0, '<5',x))
  d6.p[,3] <- sapply(as.numeric(d6.p[,3]), function(x) ifelse(x < 0.01 & x!= 0, '<0.01',x))  
  return(d6.p)
}
