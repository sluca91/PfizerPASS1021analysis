### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim : 28 July 2022

## For development:
#  #  dat.loc <- '.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- readRDS(paste0(dat.loc, 'M_Studycohort3.rds'))
#	library(lubridate)

## For second interim :
# 1. Adding the cause of censoring
# 2. Different format of output from the previous interim

#########################################################################################################
## Function to create table 7
#########################################################################################################

## Start function
### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim : 20 July 2022

## For development:
#  #  dat.loc <- '.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- readRDS(paste0(dat.loc, 'M_Studycohort3.rds'))
#	library(lubridate)

## For second interim :
# 1. Adding the cause of censoring
# 2. Different format of output from the previous interim

#########################################################################################################
## Function to create table 7
#########################################################################################################

## Start function
fun_tab7 <- function(M_Studycohort){
  
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  
  ## Select all vaccinated
  d <- subset(M_Studycohort, group != 'CONTROL' )
  rownames(d) <- NULL
  
  ## Check:
  if(nrow(d) == 0)   stop('0 persons in M_Studycohort with group == EXPOSED or UNMATCHED')
  if(any(duplicated(d$person_id))) stop('This subset of the data cannot contain duplicated person_ids')
  
  ## To do: Also add a check to see if end_obs is not in the future
  
  ## Check censoring due to end of observation period, other vaccine, or death:
  ## Get date of first non-pfizer vaccine after first pfizer
  d$other1 <- as.Date(d$FIRST_OTHER, format = '%Y-%m-%d')
  d$other1[is.na(d$other1) | d$other1 < d$FIRST_PFIZER] <- NA
  d$other2 <- as.Date(d$SECOND_OTHER, format = '%Y-%m-%d')
  d$other2[is.na(d$other2) | d$other2 < d$FIRST_PFIZER] <- NA
  d$other3 <- as.Date(d$THIRD_OTHER, format = '%Y-%m-%d')
  d$other3[is.na(d$other3) | d$other3 < d$FIRST_PFIZER] <- NA
  d$other4 <- as.Date(d$FOURTH_OTHER, format = '%Y-%m-%d')
  d$other4[is.na(d$other4) | d$other4 < d$FIRST_PFIZER] <- NA
  d$other_aftert0 <- pmin(d$other1, d$other2, d$other3, d$other4,  na.rm = TRUE)
  d$other1 <- d$other2 <- d$other3 <- d$other4 <- NULL
  
  ## Include censoring due to receiving other vaccine or death in end_obs
  d$endfup <- pmin(d$op_end_date, d$other_aftert0, as.Date(d$death_date, format = '%Y-%m-%d'), na.rm = TRUE)
  
  ## Include reason of censoring - op_end_date, other vaccine, or death
  d$endfup_reason <- 'remain in the study'
  d$endfup_reason[(d$endfup < min(recommended_end_date, end_study_date)) & d$endfup == d$other_aftert0] <- 'other vaccine'
  d$endfup_reason[(d$endfup < min(recommended_end_date, end_study_date)) & !is.na(as.Date(d$death_date, format = "%Y-%m-%d")) & d$endfup == as.Date(d$death_date, format = '%Y-%m-%d')] <- 'death'
  d$endfup_reason[(d$endfup < min(recommended_end_date, end_study_date)) & is.na(as.Date(d$death_date, format = "%Y-%m-%d")) & (d$endfup == d$op_end_date)] <- 'exit data source'
  
  ## fupyears (Note: year instead of month, corresponding to other tables)
  ## Also: 1 is added in line with other tables (to avoid a follow-up of 0)
  d$fupdays <- as.numeric(difftime(d$endfup, as.Date(d$FIRST_PFIZER, format = '%Y-%m-%d'), unit = 'days')) + 1
  d$fupdays <- d$fupdays/30.5 # convert to person-month
  
  ## Table information
  n.total <- nrow(d)
  fupdays.med <- median(d$fupdays)
  fupdays.q1 <- quantile(d$fupdays, .25)
  fupdays.q3 <- quantile(d$fupdays, .75)
  fupdays.min <- min(d$fupdays)
  fupdays.max <- max(d$fupdays)
  
  ## Reason of censoring
  n.othervacc <- sum(d$endfup_reason == 'other vaccine')
  p.othervacc <- 100 * n.othervacc / n.total
  n.death <- sum(d$endfup_reason == 'death')
  p.death <- 100 * n.death / n.total
  n.openddate <- sum(d$endfup_reason == 'exit data source')
  p.openddate <- 100 * n.openddate / n.total
  
  ## Format and return output:
  numdec <- 1
  d7 <- data.frame(
    col1 = c('LABEL','Total vaccinated', 'Person-months of follow-up', '   Median (Q1, Q3)', '   Min, max', 
             'Censored because reception of a non-Pfizer COVID-19 vaccine, n (%)', 'Death from any cause, n (%)','Censored because exit of the data source, n (%)'),
    col2 = c('VAC1', as.character(n.total), '', 
             as.character(format(round(c(fupdays.med, fupdays.min), numdec), nsmall = numdec, trim = TRUE)),
             as.character(n.othervacc),
             as.character(n.death), as.character(n.openddate)),	
    col3 = c('VAC2', '' , '', 
             as.character(format(round(c(fupdays.q1, fupdays.max), numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(p.othervacc, numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(p.death, numdec), nsmall = numdec, trim = TRUE)),
             as.character(format(round(p.openddate, numdec), nsmall = numdec, trim = TRUE))),	
    col4 = c('VAC3', '' , '', 
             as.character(format(round(fupdays.q3, numdec), nsmall = numdec, trim = TRUE)),
             '', '', '' ,'')	
  )
  
  colnames(d7) <- d7[1,]
  d7 <- d7[-1,]
  return(d7)
}
