### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl) and Roel Elbers (R.J.H.Elbers@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim : 5 August 2022

## For development:
#  dat.loc <- '.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- readRDS(paste0(dat.loc, 'M_Studycohort3.rds'))
#	library(lubridate)

## For the 2nd interim
# 1. Additional cause of censor
# 2. Different output format 
# 3. Reasons for censoring :
# 3.1. Receiving a non-Pfizer vaccine : this holds for EXPOSED and CONTROLS, 
# 3.2. CONTROL received their first PFIZER vaccination.   

#########################################################################################################
## Function to create table 15
#########################################################################################################


## Start function
fun_tab15 <- function(M_Studycohort){
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', 'FOURTH_OTHER', 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_PFIZER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  colls <- c("person_id",
             "death_date",
             "FIRST_PFIZER",
             "SECOND_PFIZER",
             "THIRD_PFIZER",
             "FOURTH_PFIZER",
             "FIRST_OTHER",
             "SECOND_OTHER",
             "THIRD_OTHER",
             "FOURTH_OTHER",
             "op_end_date",
             "T0",
             "id",
             "group"
  )
  
  
  ## Copy data
  d <- as.data.table(subset(M_Studycohort, group != 'UNMATCHED'))
  
  ## More data checks
  if(nrow(d) == 0) stop('0 persons with group == EXPOSED or CONTROL')
  if(any(duplicated(d$person_id[d$group == 'Exposed'])))	stop('Duplicated person_id values should not be possible in the exposed population.')
  
  ## To do: Add more checks, e.g. check if dates are not in the future, or if indeed all *_HIST dates > first_pfizer
  
  
  ## Set other to NA if before t0
  d$FIRST_OTHER[as.Date(d$FIRST_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$SECOND_OTHER[as.Date(d$SECOND_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$THIRD_OTHER[as.Date(d$THIRD_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  d$FOURTH_OTHER[as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d') <= as.Date(d$T0, format = '%Y-%m-%d')] <- NA
  
  ## Find earliest date of other vaccination before t0
  d$other_aftert0 <- pmin(as.Date(d$FIRST_OTHER, format = '%Y-%m-%d'), as.Date(d$SECOND_OTHER, format = '%Y-%m-%d'), as.Date(d$THIRD_OTHER, format = '%Y-%m-%d'),as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d'), na.rm = T)
  
  ## For unexposed, also include the date of first pfizer vaccin
  d$oth_pfiz_aftert0 <- d$other_aftert0 
  d$oth_pfiz_aftert0[d$group == 'CONTROL'] <- pmin(as.Date(d$other_aftert0[d$group == 'CONTROL'], format = '%Y-%m-%d'),
                                                   as.Date(d$FIRST_PFIZER[d$group == 'CONTROL'], format = '%Y-%m-%d'), na.rm = T)
  
  d$pfizer_control <- d$oth_pfiz_aftert0 == d$FIRST_PFIZER 
  
  ## Per pair, select the earliest date of oth_pfiz_aftert0 (this is when the pair should be censored)
  print("Start new code")
  TEMP <- d[,.(person_id, id, oth_pfiz_aftert0)][!is.na(oth_pfiz_aftert0),]
  TEMP <- TEMP[, .(censdate = min(oth_pfiz_aftert0)), by = "id"]
  d <- merge(x = d, y = TEMP, by = "id" , all.x = T)
  rm(TEMP)
  gc()
  print("End new code")
  
  #ind <- unique(d$id[!is.na(d$oth_pfiz_aftert0)])
  #d$censdate <- NA
  #for(i in ind){
  #d$censdate[d$id == i] <- as.character(min(as.Date(d$oth_pfiz_aftert0[d$id == i], format = '%Y-%m-%d'), na.rm = T))
  #}
  
  ## Now include endfup to include the censoring
  d$endfup <- pmin(as.Date(d$op_end_date, format = '%Y-%m-%d'), as.Date(d$censdate, format = '%Y-%m-%d'), na.rm = T)		
  
  ## Include reason of end of follow-up (Note: here it is assumed that death_date corresponds to op_end_date if applicable)
  d$endfup_reason <- 'remain in the study'
  #d$endfup_reason[(d$endfup < min(recommended_end_date, end_study_date)) & (d$endfup == d$censdate)] <- 'Other vaccine (or first Pfizer for unexposed) in pair'
  
  d$endfup_reason[which((d$endfup < min(recommended_end_date, end_study_date)) & d$endfup == d$oth_pfiz_aftert0 & (is.na(d$pfizer_control) | d$pfizer_control == F))] <- 'Non-Pfizer COVID-19 vaccine received in pair'
  d$endfup_reason[which((d$endfup < min(recommended_end_date, end_study_date)) & d$endfup == d$oth_pfiz_aftert0 & (d$pfizer_control == T))] <- 'COVID-19 vaccine received in pair' 
  
  d$endfup_reason[(d$endfup < min(recommended_end_date, end_study_date)) & (d$endfup == as.Date(d$death_date, format = '%Y-%m-%d'))] <- 'death'
  d$endfup_reason[(d$endfup < min(recommended_end_date, end_study_date)) & (d$endfup == d$op_end_date)] <- 'exit data source'
  
  ## Split
  d_E <- d[d$group == 'EXPOSED',]
  d_U <- d[d$group == 'CONTROL',]
  if(nrow(d_E) == 0) 						stop('0 persons in exposed set')
  if(nrow(d_U) == 0) 						stop('0 persons in unexposed set')
  
  d_E$fupdays <- as.numeric(difftime(d_E$endfup, as.Date(d_E$T0, format = '%Y-%m-%d'), unit = 'days')) + 1	## Note: 1 added in line with other tables (to avoid 0 fupdays)
  d_U$fupdays <- as.numeric(difftime(d_U$endfup, as.Date(d_U$T0, format = '%Y-%m-%d'), unit = 'days')) + 1
  
  # convert into person-months. The labels (fupdays, instead of fupmonths) stay the same for simplicity
  d_E$fupdays <- d_E$fupdays/30.5
  d_U$fupdays <- d_U$fupdays/30.5
  
  ## Table information E
  E_n.total <- nrow(d_E)
  E_fupdays.med <- median(d_E$fupdays)
  E_fupdays.q1 <- quantile(d_E$fupdays, .25)
  E_fupdays.q3 <- quantile(d_E$fupdays, .75)
  E_fupdays.min <- min(d_E$fupdays)
  E_fupdays.max <- max(d_E$fupdays)
  
  ## Table information U
  U_n.total <- nrow(d_U)	## length(unique(d_U$person_id))
  U_fupdays.med <- median(d_U$fupdays)
  U_fupdays.q1 <- quantile(d_U$fupdays, .25)
  U_fupdays.q3 <- quantile(d_U$fupdays, .75)
  U_fupdays.min <- min(d_U$fupdays)
  U_fupdays.max <- max(d_U$fupdays)
  
  ## Reason of censoring E
  #E_n.othervacc <- sum(d_E$endfup_reason == 'Other vaccine (or first Pfizer for unexposed) in pair')
  #E_p.othervacc <- 100 * E_n.othervacc / E_n.total
  E_n.nonpfz <- sum(d_E$endfup_reason == 'Non-Pfizer COVID-19 vaccine received in pair')
  E_p.nonpfz <- (E_n.nonpfz / E_n.total) * 100
  E_n.death <- sum(d_E$endfup_reason == 'death')
  E_p.death <- 100 * E_n.death / E_n.total
  E_n.openddate <- sum(d_E$endfup_reason == 'exit data source')
  E_p.openddate <- 100 * E_n.openddate / E_n.total
  
  ## Reason of censoring U
  #U_n.firstPfizer <- length(which(d_U$oth_pfiz_aftert0 == d_U$FIRST_PFIZER))
  #U_p.firstPfizer <- 100 * U_n.firstPfizer / U_n.total
  U_n.nonpfz <- sum(d_U$endfup_reason == 'Non-Pfizer COVID-19 vaccine received in pair')
  U_p.nonpfz <- (U_n.nonpfz / U_n.total) * 100
  U_n.othervacc <- sum(d_U$endfup_reason == 'COVID-19 vaccine received in pair')
  U_p.othervacc <- 100 * U_n.othervacc / U_n.total
  U_n.death <- sum(d_U$endfup_reason == 'death')
  U_p.death <- 100 * U_n.death / U_n.total
  U_n.openddate <- sum(d_U$endfup_reason == 'exit data source')
  U_p.openddate <- 100 * U_n.openddate / U_n.total
  
  ####### check what happened if the two persons in a pair have a different reason for censoring:
  # tmp1 <- unique(d_U$id[d_U$endfup_reason == 'Other vaccine (or first Pfizer for unexposed) in pair'])
  # tmp2 <- unique(d_E$id[d_E$endfup_reason == 'Other vaccine (or first Pfizer for unexposed) in pair'])
  # tmp1[!(tmp1 %in% tmp2)]
  # tmp2[!(tmp2 %in% tmp1)]
  # d[d$id == ...,]
  ####################################################################################################
  
  ## Format and return output:
  numdec <- 1
  d15 <- data.frame(
    col1 = c('LABEL','Total subjects','Person-months of follow-up', '   Median (Q1, Q3)', '   Min, max', 'Reasons for censoring',
             'Non-Pfizer COVID-19 vaccine received, n (%)', 'COVID-19 vaccine received, n(%)',
             'Death (any cause), n (%)','Exit from data source, n (%)'),
    col2 = c('VAC1',as.character(E_n.total),'',
             as.character(format(round(c(E_fupdays.med, E_fupdays.min), numdec), nsmall = numdec, trim = TRUE)),'',
             as.character(E_n.nonpfz), NA,
             as.character(E_n.death),as.character(E_n.openddate) ),	
    col3 = c('VAC2','', '',
             as.character(format(round(c(E_fupdays.q1, E_fupdays.max), numdec), nsmall = numdec, trim = TRUE)),'',
             as.character(format(round(E_p.nonpfz, numdec), nsmall = numdec, trim = TRUE)), NA,
             as.character(format(round(E_p.death, numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(E_p.openddate, numdec), nsmall = numdec, trim = TRUE))),	
    col4 = c('VAC3','','',
             as.character(format(round(E_fupdays.q3, numdec), nsmall = numdec, trim = TRUE)),'',
             '', '' ,'', '', ''),
    col5 = c('CTR1', as.character(U_n.total),'',
             as.character(format(round(c(U_fupdays.med, U_fupdays.min), numdec), nsmall = numdec, trim = TRUE)),'',
             as.character(U_n.nonpfz), as.character(U_n.othervacc),
             as.character(U_n.death),as.character(U_n.openddate)),
    col6 = c('CTR2', '' ,'',
             as.character(format(round(c(U_fupdays.q1, U_fupdays.max), numdec), nsmall = numdec, trim = TRUE)),'',
             as.character(format(round(U_p.nonpfz, numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(U_p.othervacc, numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(U_p.death, numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(U_p.openddate, numdec), nsmall = numdec, trim = TRUE))),	
    col7 = c('CTR3', '', '',
             as.character(format(round(U_fupdays.q3, numdec), nsmall = numdec, trim = TRUE)),'',
             '', '', '', '','')
  )
  
  colnames(d15) <- d15[1,]
  d15 <- d15[-1,]
  return(d15) 
}