## Repairing the to_run_t4_new.R specifically for table 16 and its subset 

source(paste0(pre_dir,"functions/", "create_matched_pop_t4.R"))  

d_matchedpop <- readRDS(paste0(populations_dir, "matched_pop.rds"))
#######################################################################################################
## Table 16
#######################################################################################################

source(paste0(pre_dir, 'Step_35_Table16_new.R'))
d16 <- fun_tab16(d_matchedpop = d_matchedpop, AESI_info = AESI_info)
write.csv(d16, paste0(output_dir, DAP, '_Table16_Trial.csv'), row.names = F)
rm(d16)
gc()

#######################################################################################################
## Selection of subsets (for Table 16.S.X and Table 20.S.X)
#######################################################################################################

subset.select <- function(d_matched_pop, M_Studycohort_Covariates_T0, column.select, val){
  if (column.select == "L_SEX_COV"){
    MS <- d_matchedpop[!is.na(column.select),]
    
    ids <- MS$id[which(MS$group == 'EXPOSED' & MS[,as.character(column.select)] == as.character(val))]
    cdat <- MS[which(MS$id %in% ids), ]
    cdat1 <- cdat
  } else {
    
    MS <- M_Studycohort_Covariates_T0[!is.na(column.select),]
    cdat <- merge(x = as.data.table(d_matchedpop), y = MS, by = c("person_id","id"), all.x = TRUE)
    
    ids <- cdat$id[which(cdat$group == 'EXPOSED' & cdat[,as.character(column.select), with = FALSE] == 1)]
    cdat <- cdat[which(cdat$id %in% ids), ]
    idx.col <- grep(column.select, colnames(cdat))
    cdat1 <- cdat[ ,c(1:21,idx.col), with = FALSE]
  }
  return(cdat1)
}

colls <- c('person_id','id', 'group')
MScopy <- d_matchedpop[, colnames(d_matchedpop) %in% colls]
Mdat <- merge(x = MScopy, y = M_Studycohort_Covariates_T0, by = c('person_id','id'), all.x = TRUE)

M_Studycohort2 <- readRDS(paste0(populations_dir,'M_Studycohort2.rds'))
Mdat2 <- merge(x = MScopy, y = M_Studycohort2, by = c('person_id','id'), all.x = TRUE)

rm(MScopy)
#######################################################################################################
## Table 16.S1
#######################################################################################################
## Check if the subgroup is not empty

if (any(grep("I_COVID_COV_T0", colnames(Mdat)))){
  if(any(!is.na(Mdat$I_COVID_COV_T0))){
    if (all(Mdat$I_COVID_COV_T0 == 0)) {stop("All subjects have zero values")}
    MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'I_COVID_COV_T0', val = 1)
  
    d16S1 <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
    write.csv(d16S1, paste0(output_dir,  DAP, '_Table16S1.csv'), row.names = F)
    rm(d16S1, MSdat)
    gc()
  }
}
#######################################################################################################
## Table 16.S2.X (Interim 2)
#######################################################################################################

if(any(!is.na(d_matchedpop$L_SEX_COV))){
  MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_SEX_COV', val = "M")
  
  d16S2M <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
  write.csv(d16S2M, paste0(output_dir,  DAP, '_Table16S2_M.csv'), row.names = F)
  rm(d16S2M, MSdat)
  gc()
}

if(any(!is.na(d_matchedpop$L_SEX_COV))){
  MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_SEX_COV', val = "F")
  
  d16S2F <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
  write.csv(d16S2F, paste0(output_dir,  DAP, '_Table16S2_F.csv'), row.names = T)
  rm(d16S2F, MSdat)
  gc()
}

#######################################################################################################
## Table 16.S3.X (Interim 2)
#######################################################################################################

## For Age categorisation, the following are the concept
vec.low <- c(0,2,5,12,16,18,30,40,50,60,65,70,80 )
vec.upp <- c(1,4,11,15,17,29,39,49,59,64,69,79,150) # the last one is dummy age

for (i in 1:length(vec.low)){
  lb <- vec.low[i]
  ub <- vec.upp[i]
  print(paste0("Calculating age category ", lb, " - ", ub))
  
  lenCohort <- length(which(d_matchedpop$AGE_T0 >= lb & d_matchedpop$AGE_T0 <= ub))
  if (lenCohort == 0) {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub))
    next}
  
  if(any(d_matchedpop$AGE_T0 >= lb & d_matchedpop$AGE_T0 <= ub)){
    MS <- d_matchedpop
    ids <- MS$id[which(MS$group == "EXPOSED" & MS$AGE_T0 >= lb & MS$AGE_T0 <= ub)]
    if(length(ids) == 0) {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub))
      next}
    MSAgeCat <- MS[which(MS$id %in% ids), ]
    
    d16S3Cat <- fun_tab16(d_matchedpop = MSAgeCat, AESI_info = AESI_info)
  } else {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub))
    next}
  
  write.csv(d16S3Cat, paste0(output_dir,  DAP, '_Table16S3', i,'.csv'), row.names = F)
  rm(d16S3Cat, MS, MSAgeCat, ids)
  rm(lb,ub)
}

#######################################################################################################
## Table 16.S4.X (Interim 2)
#######################################################################################################
## Elderly

if(any(d_matchedpop$AGE_T0 >= 65)){
  MS <- d_matchedpop
  ids <- MS$id[which(MS$group == "EXPOSED" & MS$AGE_T0 >= 65)]
  if (length(ids) == 0) {warning('No subpopulation were selected for this age category.')
    next
  }
  MSAgeCat <- MS[which(MS$id %in% ids), ]
  
  d16S4Cat <- fun_tab16(d_matchedpop = MSAgeCat, AESI_info = AESI_info)
  write.csv(d16S4Cat, paste0(output_dir,  DAP, '_Table16S4.csv'), row.names = F)
  rm(d16S4Cat, MS, MSAgeCat, ids)
  gc()
} else {
  warning('No subpopulation were selected for this age category.')
}

#######################################################################################################
## Table 16.S5.X (Interim 2)
#######################################################################################################
## For pregnant

if(any(Mdat$L_PREGNSTATUS_COV_T0 == 1)){
  MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_PREGNSTATUS_COV_T0', val = 1)
  
  d16S5 <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
  write.csv(d16S5, paste0(output_dir,  DAP, '_Table16S5_Trial.csv'), row.names = F)
  rm(d16S5, MSdat)
  gc()
} else {warning('No pregnant subpopulation in the matched cohort were selected.')}

#######################################################################################################
## Table 16.S6.X (Interim 2)
#######################################################################################################
## For immunocompromised

if (any(!is.na(Mdat2$Im_IMMCOMPMATCHPFIZER_COV_T0))){
  MS <- M_Studycohort2[!is.na(M_Studycohort2$Im_IMMCOMPMATCHPFIZER_COV_T0),]
  cdat <- merge(x = as.data.table(d_matchedpop), y = MS, by = c("person_id","id"), all.x = TRUE)
  
  ids <- cdat$id[which(cdat$group == 'EXPOSED' & !is.na(cdat[,'Im_IMMCOMPMATCHPFIZER_COV_T0', with = FALSE]))]
  MSdat <- cdat[which(cdat$id %in% ids), ]
  
  d16S6 <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
  
  write.csv(d16S6, paste0(output_dir,  DAP, '_Table16S6.csv'), row.names = F)
  rm(d16S6, MSdat, ids, cdat, MS)
  gc()
} else {warning('No subpopulation of immunocompromised were selected')}

#######################################################################################################
## Table 16.S7.X (Interim 2)
#######################################################################################################
## For comorbidities or frail

if (any(!is.na(Mdat2$Im_FRAILCOMORB_COV_T0))){
  MS <- M_Studycohort2[!is.na(M_Studycohort2$Im_FRAILCOMORB_COV_T0),]
  cdat <- merge(x = as.data.table(d_matchedpop), y = MS, by = c("person_id","id"), all.x = TRUE)
  
  ids <- cdat$id[which(cdat$group == 'EXPOSED' & !is.na(cdat[,'Im_FRAILCOMORB_COV_T0', with = FALSE]))]
  MSdat <- cdat[which(cdat$id %in% ids), ]
  
  d16S7 <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
  write.csv(d16S7, paste0(output_dir,  DAP, '_Table16S7.csv'), row.names = F)
  rm(d16S7, MSdat,ids, cdat, MS)
  gc()
} else {warning('No subpopulation of frail/comorbid were selected')}

#######################################################################################################
## Table 16.S8.X (Interim 2)
#######################################################################################################
## Excluding those who had contact with healthcare within 7 days before T0

if ("H_HOSPPOP_POP_T0" %in% colnames(M_Studycohort_Covariates_T0)){
  if(any(!is.na(Mdat$H_HOSPPOP_POP_T0))){
    if (all(Mdat$H_HOSPPOP_POP_T0 == 1)) {stop("All subjecst in the matched cohort had contact with healthcare within 7 days prior T0")}
    
    MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'H_HOSPPOP_POP_T0', val = 0)
    
    d16S8 <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
    write.csv(d16S8, paste0(output_dir,  DAP, '_Table16S8.csv'), row.names = F)
    rm(d16S8, MSdat)
    gc()
  } else {warning('There were no subjects selected in the subpopulation')}
} else {warning('Data on healthcare visit is not available. Please check')}

#######################################################################################################
## Table 16.S9.X (Interim 2)
#######################################################################################################
## Female and male in each subgroup

## For Age categorisation, the following are the concept
vec.low <- c(0,2,5,12,16,18,30,40,50,60,65,70,80 )
vec.upp <- c(1,4,11,15,17,29,39,49,59,64,69,79,150) # the last one is dummy

# Female subpopulation
for (i in 1:length(vec.low)){
  lb <- vec.low[i]
  ub <- vec.upp[i]
  print(paste0("Calculating age category ", lb, " - ", ub))
  
  if(any(d_matchedpop$AGE_T0 >= lb & d_matchedpop$AGE_T0 <= ub)){
    MS <- subset(d_matchedpop, L_SEX_COV == "F")
    ids <- MS$id[which(MS$group == "EXPOSED" & MS$AGE_T0 >= lb & MS$AGE_T0 <= ub)]
    if(length(ids) == 0) {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub, " in female subjects"))
      next}
    MSAgeCat <- MS[which(MS$id %in% ids), ]
    
    d16S9FCat <- fun_tab16(d_matchedpop = MSAgeCat, AESI_info = AESI_info)
  } else {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub, " in female subjects."))
    next}
  
  write.csv(d16S9FCat, paste0(output_dir,  DAP, '_Table16S9_F_', i,'.csv'), row.names = F)
  rm(d16S9FCat, MS, MSAgeCat, ids)
  rm(lb,ub)
}

# Male subpopulation
for (i in 1:length(vec.low)){
  lb <- vec.low[i]
  ub <- vec.upp[i]
  print(paste0("Calculating age category ", lb, " - ", ub))
  
  if(any(d_matchedpop$AGE_T0 >= lb & d_matchedpop$AGE_T0 <= ub)){
    MS <- subset(d_matchedpop, group != "UNMATCHED" & L_SEX_COV == "M")
    ids <- MS$id[which(MS$group == "EXPOSED" & MS$AGE_T0 >= lb & MS$AGE_T0 <= ub)]
    if(length(ids) == 0) {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub," in male subjects"))
      next}
    MSAgeCat <- MS[which(MS$id %in% ids), ]
    
    d16S9MCat <- fun_tab16(d_matchedpop = MSAgeCat, AESI_info = AESI_info)
  } else {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub, " in male subjects."))
    next}
  
  write.csv(d16S9MCat, paste0(output_dir,  DAP, '_Table16S9_M_', i,'.csv'), row.names = T)
  rm(d16S9MCat, MS, MSAgeCat, ids)
  rm(lb,ub)
}
