## To create matched population for computing table 16SX and 20SX
# filename: create_matched_pop_t4.R

Main <- function(){
  M_Studycohort <- readRDS(paste0(populations_dir,"M_Studycohort.rds"))
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', "FOURTH_OTHER", 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_OTHER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  
  ## Copy data
  d <- subset(M_Studycohort, group != 'UNMATCHED')
  rm(M_Studycohort)
  gc()
  
  ## More data checks
  if(nrow(d) == 0) stop('0 persons with group == EXPOSED or CONTROL')
  if(any(duplicated(d$person_id[d$group == 'EXPOSED'])))	stop('Duplicated person_id values should not be possible in the exposed population.')
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
  
  ## Per pair, select the earliest date of oth_pfiz_aftert0 (this is when the pair should be censored)
  
  print("Start new code")
  TEMP <- as.data.table(d)[,.(person_id, id, oth_pfiz_aftert0)][!is.na(oth_pfiz_aftert0),]
  TEMP <- TEMP[, .(censdate = min(oth_pfiz_aftert0)), by = "id"]
  d <- merge(x = d, y = TEMP, by = "id" , all.x = T)
  rm(TEMP)
  gc()
  print("End new code")
  
  d$op_end_date <- pmin(as.Date(d$op_end_date, format = '%Y-%m-%d'), as.Date(d$censdate, format = '%Y-%m-%d'), na.rm = T)		
  
  ## Save it to be used in table 20 and figure 2. 
  saveRDS(d, file = paste0(populations_dir, "matched_pop.rds"))
  
}

Main()
