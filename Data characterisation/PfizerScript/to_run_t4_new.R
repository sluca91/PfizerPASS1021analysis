#Author:Rutger van den Bor/Albert Cid Royo/ Ivonne Martin 
#email: R.M.vandenBor@umcutrecht.nl/a.cidroyo@umcutrecht.nl / i.martin@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/02/2022


## Run this script in R-studio 
  ## Specify the correct folder location, select all and click 'Run' on the top right) 

if(!exists("projectFolder") | !exists("DAP")) rm(list=ls())  

  #######################################################################################################
  ## Load packages
  #######################################################################################################
  
  
  if(!require(data.table)){install.packages("data.table")}
  library(data.table)
  if(!require(survival)){install.packages("survival")}
  library(survival)
  if(!require(stddiff)){install.packages("stddiff")}
  library(stddiff)
  if(!require(geeM)){install.packages("geeM")}
  library(geeM)
  if(!require(Hmisc)){install.packages("Hmisc")}
  library(Hmisc)
  if(!require(rstudioapi)){install.packages("rstudioapi")}
  library(rstudioapi)
  if(!require(tableone)){install.packages("tableone")}
  library(tableone)
  if(!require(dplyr)){install.packages("dplyr")}
  library(dplyr)
  if(!require(tidyverse)){install.packages("tidyverse")}
  library(tidyverse)
  if(!require(survminer)){install.packages("survminer")}
  library(survminer)
  if(!require(cobalt)){install.packages("cobalt")}
  library(cobalt)
  if(!require(WeightIt)){install.packages("WeightIt")}
  library(WeightIt)
  if(!require(SCCS)){install.packages("SCCS")}
  library(SCCS)

#######################################################################################################
  
  
  
  
  
  if(!exists("projectFolder") | !exists("DAP")){ 
    projectFolder <- dirname(rstudioapi::getSourceEditorContext()$path)
    
    StudyName <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
    #StudyName <- NULL
    #path <- "TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN"
    DAP <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'

    start_study_date <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
    end_study_date <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
    
      
    if (start_study_date %in% c('TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN') | end_study_date %in% c('TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN')){
      stop('Fill the Start study date or the End study date as you did in the to_run_file')
    }
  
    lookback_period <- 365
    max_spells_gap <- 365
  
  
    source(paste0(projectFolder,"/99_path.R"))
    source(paste0(projectFolder,"/packages.R"))
  
    #Load functions
    source(paste0(pre_dir, "LoadFunctions.R"))
    
    #Set parameters
    source(paste0(pre_dir,"Step_00_SetParameters.R"))
    source(paste0(pre_dir,"Step_00_SetCodeSheets.R"))

  }

  if(!DAP %in% c("TEST","CPRD","ARS","SIMG","PEDIANET","SIDIAP","EPICHRON","UOSL","PHARMO")) stop("Fill DAP with the correct word")
  
  #Add 
  ###
  system.time(source(paste0(pre_dir,"Step_12c_AddWeights_V2.R")))
  system.time(source(paste0(pre_dir,"Step_52_Table18.R")))
  ###
  
  system.time(source(paste0(pre_dir,"Step_20_Table1.R")))
  system.time(source(paste0(pre_dir,"Step_21_Table2.R")))
  system.time(source(paste0(pre_dir,"Step_22_Table3_11.R")))
  system.time(source(paste0(pre_dir,"Step_23_Table4.R")))
  system.time(source(paste0(pre_dir,"Step_24_Table5.R")))
  system.time(source(paste0(pre_dir,"Step_29_Table10.R")))


  #######################################################################################################
  ## Specification of folder location
  #######################################################################################################

  #######################################################################################################
  ## Import required data
  #######################################################################################################
  
  ## Import AESI information:
  AESI_Cov_info <- fread(paste0(meta_dir,"Pfizer AESI_information.csv"), stringsAsFactors = F, na.strings = "")

  ## Exclude rows that should be ignored
  AESI_info <- subset(AESI_Cov_info, Table == "AESI tables")
  # AESI_info <- AESI_info[AESI_info$System != '' & AESI_info$Event_name != '' & AESI_info$Event_abbreviation != '' & !is.na(AESI_info$System) & !is.na(AESI_info$Event_name) & !is.na(AESI_info$Event_abbreviation),]

  ## Import M_Studycohort
  SCRIPT <- RUN_SCRIPT(name = "Step_13_AddAESI.R")
  #M_Studycohort3 <- readRDS(SCRIPT[["OUTPUT1"]][["path"]])
  M_Studycohort <- readRDS(paste0(populations_dir,"M_Studycohort.rds"))
  
  source(paste0(pre_dir,"functions/", "create_healthcarecontact_var.R"))  
  M_Studycohort_Covariates_T0 <- Main()
  
  ## Load a function to cater for different format of AESI_info due to myo and pericarditis requests
  source(paste0(pre_dir,"functions/", "reformat_mpc_AESI.R"))  
  
  ## create matched population for table 16SX and 20SX
  source(paste0(pre_dir,"functions/", "create_matched_pop_t4.R"))  
  d_matchedpop <- readRDS(paste0(populations_dir, "matched_pop.rds"))
  
  #######################################################################################################
  ## Table 6
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_25_Table6.R'))
  d6 <- fun_tab6(M_Studycohort, AESI_info = AESI_info)
  fwrite(d6, file = paste0(output_dir, DAP, '_Table6','.csv'))
  saveRDS(d6, file = paste0(output_dir, DAP, '_Table6','.rds'))
  #write.csv(d6, paste0(output_dir, '/d6.csv'), row.names = F)
  rm(d6)
  gc()
  
  #######################################################################################################
  ## Table 7
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_26_Table7.R'))
  d7 <- fun_tab7(M_Studycohort = M_Studycohort)  
  write.csv(d7, paste0(output_dir, DAP, '_Table7.csv'), row.names = F)
  saveRDS(d7, paste0(output_dir, DAP, '_Table7.rds'))
  rm(d7)
  gc()
  
  #######################################################################################################
  ## Table 8
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_27_Table8.R'))
  d8 <- fun_tab8(M_Studycohort = M_Studycohort, AESI_info = AESI_info)  
  write.csv(d8, paste0(output_dir, DAP, '_Table8.csv'), row.names = F)
  saveRDS(d8, paste0(output_dir, DAP, '_Table8.rds'))
  rm(d8)
  gc()
  
  #######################################################################################################
  ## Table 9
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_28_Table9.R'))
  d9 <- fun_tab9(M_Studycohort = M_Studycohort, AESI_info = AESI_info)  
  write.csv(d9, paste0(output_dir, DAP, '_Table9.csv'), row.names = F)
  saveRDS(d9, paste0(output_dir, DAP, '_Table9.rds'))
  rm(d9)
  gc()

  #######################################################################################################
  ## Table 12
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_31_Table12.R'))
  d12 <- fun_tab12(M_Studycohort = M_Studycohort, M_Studycohort_Covariates_T0 = M_Studycohort_Covariates_T0, AESI_Cov_info = AESI_Cov_info)
  write.csv(d12, paste0(output_dir, DAP, '_Table12.csv'), row.names = F)
  saveRDS(d12, paste0(output_dir, DAP, '_Table12.rds'))
  rm(d12)
  gc()


  #######################################################################################################
  ## Table 13
  #######################################################################################################

  source(paste0(pre_dir, 'Step_32_Table13.R'))
  d13 <- fun_tab13(M_Studycohort = M_Studycohort, M_Studycohort_Covariates_T0 = M_Studycohort_Covariates_T0, AESI_Cov_info = AESI_Cov_info)
  write.csv(d13, paste0(output_dir, DAP, '_Table13.csv'), row.names = F)
  saveRDS(d13, paste0(output_dir, DAP, '_Table13.rds'))
  rm(d13)
  gc()

  #######################################################################################################
  ## Table 14
  #######################################################################################################

  source(paste0(pre_dir, 'Step_33_Table14.R'))
  d14 <- fun_tab14(M_Studycohort = M_Studycohort, AESI_info = AESI_info)
  write.csv(d14, paste0(output_dir, DAP, '_Table14.csv'), row.names = F)
  saveRDS(d14, paste0(output_dir, DAP, '_Table14.rds'))
  rm(d14)
  gc()

  #######################################################################################################
  ## Table 15
  #######################################################################################################

  source(paste0(pre_dir, 'Step_34_Table15.R'))
  d15 <- fun_tab15(M_Studycohort = M_Studycohort)
  write.csv(d15, paste0(output_dir, DAP, '_Table15.csv'), row.names = F)
  saveRDS(d15, paste0(output_dir, DAP, '_Table15.rds'))
  rm(d15)
  gc()

  
  #######################################################################################################
  ## Table 16
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_35_Table16.R'))
  d16 <- fun_tab16(d_matchedpop = d_matchedpop, AESI_info = AESI_info)
  write.csv(d16, paste0(output_dir, DAP, '_Table16.csv'), row.names = F)
  saveRDS(d16, paste0(output_dir, DAP, '_Table16.rds'))
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
      
      ids <- cdat$id[which(cdat$group == 'EXPOSED' & cdat[,as.character(column.select), with = FALSE] == val)]
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
      saveRDS(d16S1, paste0(output_dir,  DAP, '_Table16S1.rds'))
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
    saveRDS(d16S2M, paste0(output_dir,  DAP, '_Table16S2_M.rds'))
    rm(d16S2M, MSdat)
    gc()
  }
  
  if(any(!is.na(d_matchedpop$L_SEX_COV))){
    MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_SEX_COV', val = "F")
    
    d16S2F <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
    write.csv(d16S2F, paste0(output_dir,  DAP, '_Table16S2_F.csv'), row.names = T)
    saveRDS(d16S2F, paste0(output_dir,  DAP, '_Table16S2_F.rds'))
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
    saveRDS(d16S3Cat, paste0(output_dir,  DAP, '_Table16S3', i,'.rds'))
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
    saveRDS(d16S4Cat, paste0(output_dir,  DAP, '_Table16S4.rds'))
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
    write.csv(d16S5, paste0(output_dir,  DAP, '_Table16S5.csv'), row.names = F)
    saveRDS(d16S5, paste0(output_dir,  DAP, '_Table16S5.rds'))
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
    saveRDS(d16S6, paste0(output_dir,  DAP, '_Table16S6.rds'))
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
    saveRDS(d16S7, paste0(output_dir,  DAP, '_Table16S7.rds'))
    rm(d16S7, MSdat,ids, cdat, MS)
    gc()
  } else {warning('No subpopulation of frail/comorbid were selected')}
  
  #######################################################################################################
  ## Table 16.S8.X (Interim 2)
  #######################################################################################################
  ## Excluding those who had contact with healthcare within 7 days before T0
  
    if(any(!is.na(Mdat$H_HOSPPOP_POP_T0))){
      if (all(Mdat$H_HOSPPOP_POP_T0 != 0)) {stop("All subjecst in the matched cohort had contact with healthcare within 7 days prior T0")}
      
      MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'H_HOSPPOP_POP_T0', val = 0)
      
      d16S8 <- fun_tab16(d_matchedpop = MSdat, AESI_info = AESI_info)
      write.csv(d16S8, paste0(output_dir,  DAP, '_Table16S8.csv'), row.names = F)
      saveRDS(d16S8, paste0(output_dir,  DAP, '_Table16S8.rds'))
      rm(d16S8, MSdat)
      gc()
    } else {warning('There were no subjects selected in the subpopulation')}
  
  
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
    saveRDS(d16S9FCat, paste0(output_dir,  DAP, '_Table16S9_F_', i,'.rds'))
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
    saveRDS(d16S9MCat, paste0(output_dir,  DAP, '_Table16S9_M_', i,'.rds'))
    rm(d16S9MCat, MS, MSAgeCat, ids)
    rm(lb,ub)
  }
  
  #######################################################################################################
  ## Table 17
  #######################################################################################################

  source(paste0(pre_dir, 'Step_36_Table17.R'))
  d17 <- fun_tab17(M_Studycohort = M_Studycohort, AESI_info = AESI_info)
  write.csv(d17, paste0(output_dir, DAP, '_Table17.csv'), row.names = F)
  saveRDS(d17, paste0(output_dir, DAP, '_Table17.rds'))
  rm(d17)
  gc()

  #######################################################################################################
  ## Figure 3
  #######################################################################################################

  ## Note: Figure 3 is the same as figure 2, but limited to COVID only and with 12 days as the end of the risk period
  ## So here, we simply edit the one row from AESI_info
  AESI_info_copy <- reformat_mpc_AESI(AESI_info, opt = "multi")
  AESI_info_copy <- AESI_info_copy[AESI_info_copy$Event_abbreviation == 'I_COVID_COV',]
  AESI_info_copy$End_of_risk_window <- 12
  source(paste0(pre_dir, 'Step_41_Figure2.R'))

  fun.fig2(M_Studycohort = M_Studycohort, AESI_info = AESI_info_copy, opt = 3)
  ## Note: If I_COVID_COV is not present in M_Studycohort, this will generate an empty pdf

  rm(AESI_info_copy)

  #######################################################################################################
  ## Table 20
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_39_Table20.R'))
  
  # For the current reporting, we need to check for the adjusted analyses, therefore the opt is assigned to 
  # equal 1 regardless of the negative control outcome. (decided in Pfizer stat meeting UMCU Feb 13, 2023)
  
  #test.prop <- readRDS(paste0(propensity_dir,"Figure3RD.rds"))
  #opt <- ifelse(test.prop[3] > 0.1, 1, 0)
  opt <- 1
  
  if (opt == 1){source(paste0(pre_dir, "Step_38_Table19.R"))}
  
  #opt <- 0  #only calculate the crude association at the moment
  d20 <- fun_tab20(d_matchedpop = d_matchedpop, AESI_info = AESI_info, opt)
  
  write.csv(d20, paste0(output_dir, DAP, '_Table20.csv'), row.names = F)
  saveRDS(d20, paste0(output_dir, DAP, '_Table20.rds'))
  rm(d20)
  gc()
  
  #######################################################################################################
  ## Table 20.S1
  #######################################################################################################
  
  if (any(grep("I_COVID_COV_T0", colnames(Mdat)))){
    if(any(!is.na(Mdat$I_COVID_COV_T0))){
      if (all(Mdat$I_COVID_COV_T0 == 0)) {stop("All subjects have zero values on COVID")}
      MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'I_COVID_COV_T0', val = 1)
      
      d20S1 <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
      write.csv(d20S1, paste0(output_dir,  DAP, '_Table20S1.csv'), row.names = F)
      saveRDS(d20S1, paste0(output_dir,  DAP, '_Table20S1.rds'))
      rm(d20S1, MSdat)
      gc()
    }
  }
  
  #######################################################################################################
  ## Table 20.S2.X
  #######################################################################################################
  
  if(any(!is.na(d_matchedpop$L_SEX_COV))){
    MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_SEX_COV', val = "M")
    
    d20S2M <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
    write.csv(d20S2M, paste0(output_dir,  DAP, '_Table20S2_M.csv'), row.names = F)
    saveRDS(d20S2M, paste0(output_dir,  DAP, '_Table20S2_M.rds'))
    rm(d20S2M, MSdat)
    gc()
  }
  
  if(any(!is.na(d_matchedpop$L_SEX_COV))){
    MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_SEX_COV', val = "F")
    
    d20S2F <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
    write.csv(d20S2F, paste0(output_dir,  DAP, '_Table20S2_F.csv'), row.names = F)
    saveRDS(d20S2F, paste0(output_dir,  DAP, '_Table20S2_F.rds'))
    rm(d20S2F, MSdat)
    gc()
  }
  
  #######################################################################################################
  ## Table 20.S3.X (Interim 2)
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
      
      d20S3Cat <- fun_tab20(d_matchedpop = MSAgeCat, AESI_info = AESI_info, opt)
    } else {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub))
      next}
    
    write.csv(d20S3Cat, paste0(output_dir,  DAP, '_Table20S3', i,'.csv'), row.names = F)
    saveRDS(d20S3Cat, paste0(output_dir,  DAP, '_Table20S3', i,'.rds'))
    rm(d20S3Cat, MS, MSAgeCat, ids)
    rm(lb,ub)
  }
  
  
  
  #######################################################################################################
  ## Table 20.S4.X
  #######################################################################################################
  ## Elderly
  
  if(any(d_matchedpop$AGE_T0 >= 65)){
    MS <- d_matchedpop
    ids <- MS$id[which(MS$group == "EXPOSED" & MS$AGE_T0 >= 65)]
    if (length(ids) == 0) {warning('No subpopulation were selected for this age category.')
      next
    }
    MSAgeCat <- MS[which(MS$id %in% ids), ]
    
    d20S4Cat <- fun_tab20(d_matchedpop = MSAgeCat, AESI_info = AESI_info, opt)
    write.csv(d20S4Cat, paste0(output_dir,  DAP, '_Table20S4.csv'), row.names = F)
    saveRDS(d20S4Cat, paste0(output_dir,  DAP, '_Table20S4.rds'))
    rm(d20S4Cat, MS, MSAgeCat, ids)
    gc()
  } else {
    warning('No subpopulation were selected for this age category.')
  }
  #######################################################################################################
  ## Table 20.S5.X
  #######################################################################################################
  ## For pregnant
  
  if(any(Mdat$L_PREGNSTATUS_COV_T0 == 1)){
    MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'L_PREGNSTATUS_COV_T0', val = 1)
    
    d20S5 <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
    write.csv(d20S5, paste0(output_dir,  DAP, '_Table20S5.csv'), row.names = F)
    saveRDS(d20S5, paste0(output_dir,  DAP, '_Table20S5.rds'))
    rm(d20S5, MSdat)
    gc()
  } else {warning('No pregnant subpopulation in the matched cohort were selected.')}
  
  
  #######################################################################################################
  ## Table 20.S6.X
  #######################################################################################################
  ## For immunocompromised
  
  if (any(!is.na(Mdat2$Im_IMMCOMPMATCHPFIZER_COV_T0))){
    MS <- M_Studycohort2[!is.na(M_Studycohort2$Im_IMMCOMPMATCHPFIZER_COV_T0),]
    cdat <- merge(x = as.data.table(d_matchedpop), y = MS, by = c("person_id","id"), all.x = TRUE)
    
    ids <- cdat$id[which(cdat$group == 'EXPOSED' & !is.na(cdat[,'Im_IMMCOMPMATCHPFIZER_COV_T0', with = FALSE]))]
    MSdat <- cdat[which(cdat$id %in% ids), ]
    
    d20S6 <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
    
    write.csv(d20S6, paste0(output_dir,  DAP, '_Table20S6.csv'), row.names = F)
    saveRDS(d20S6, paste0(output_dir,  DAP, '_Table20S6.rds'))
    rm(d20S6, MSdat, ids, cdat, MS)
    gc()
  } else {warning('No subpopulation of immunocompromised were selected')}
  
  #######################################################################################################
  ## Table 20.S7.X
  #######################################################################################################
  ## For comorbidities or frail
  
  if (any(!is.na(Mdat2$Im_FRAILCOMORB_COV_T0))){
    MS <- M_Studycohort2[!is.na(M_Studycohort2$Im_FRAILCOMORB_COV_T0),]
    cdat <- merge(x = as.data.table(d_matchedpop), y = MS, by = c("person_id","id"), all.x = TRUE)
    
    ids <- cdat$id[which(cdat$group == 'EXPOSED' & !is.na(cdat[,'Im_FRAILCOMORB_COV_T0', with = FALSE]))]
    MSdat <- cdat[which(cdat$id %in% ids), ]
    
    d20S7 <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
    write.csv(d20S7, paste0(output_dir,  DAP, '_Table20S7.csv'), row.names = F)
    saveRDS(d20S7, paste0(output_dir,  DAP, '_Table20S7.rds'))
    rm(d20S7, MSdat,ids, cdat, MS)
    gc()
  } else {warning('No subpopulation of frail/comorbid were selected')}
  
  #######################################################################################################
  ## Table 20.S8.X
  #######################################################################################################

  if(any(!is.na(Mdat$H_HOSPPOP_POP_T0))){
    if (all(Mdat$H_HOSPPOP_POP_T0 != 0)) {stop("All subjecst in the matched cohort had contact with healthcare within 7 days prior T0")}
    
      MSdat <- subset.select(d_matchedpop, M_Studycohort_Covariates_T0, column.select = 'H_HOSPPOP_POP_T0', val = 0)
      
      d20S8 <- fun_tab20(d_matchedpop = MSdat, AESI_info = AESI_info, opt)
      write.csv(d20S8, paste0(output_dir,  DAP, '_Table20S8.csv'), row.names = F)
      saveRDS(d20S8, paste0(output_dir,  DAP, '_Table20S8.rds'))
      rm(d20S8, MSdat)
      gc()
    } else {warning('There were no subjects selected in the subpopulation')}
  
  
  
  #######################################################################################################
  ## Table 20.S9.X
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
      MS <- subset(d_matchedpop, group != "UNMATCHED" & L_SEX_COV == "F")
      ids <- MS$id[which(MS$group == "EXPOSED" & MS$AGE_T0 >= lb & MS$AGE_T0 <= ub)]
      if(length(ids) == 0) {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub, " in female subjects"))
        next}
      MSAgeCat <- MS[which(MS$id %in% ids), ]
      
      d20S9FCat <- fun_tab20(d_matchedpop = MSAgeCat, AESI_info = AESI_info, opt)
    } else {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub, " in female subjects."))
      next}
    
    write.csv(d20S9FCat, paste0(output_dir,  DAP, '_Table20S9_F_', i,'.csv'), row.names = F)
    saveRDS(d20S9FCat, paste0(output_dir,  DAP, '_Table20S9_F_', i,'.rds'))
    rm(d20S9FCat, MS, MSAgeCat, ids)
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
      
      d20S9MCat <- fun_tab20(d_matchedpop = MSAgeCat, AESI_info = AESI_info, opt)
    } else {warning(paste0('No subpopulation were selected for age category ', lb, " - ", ub, " in male subjects."))
      next}
    
    write.csv(d20S9MCat, paste0(output_dir,  DAP, '_Table20S9_M_', i,'.csv'), row.names = F)
    saveRDS(d20S9MCat, paste0(output_dir,  DAP, '_Table20S9_M_', i,'.rds'))
    rm(d20S9MCat, MS, MSAgeCat, ids)
    rm(lb,ub)
  }
  
  rm(Mdat,Mdat2)
  #######################################################################################################
  ## Table 21
  #######################################################################################################

  source(paste0(pre_dir, 'Step_40_Table21.R'))

  #opt <- ifelse(test.prop[3] > 0.1, 1, 0)
  #opt <- 0  #only calculate the crude association at the moment
  opt <- 1 # the same reasoning as table 20
  d21 <- fun_tab21(M_Studycohort = M_Studycohort, AESI_info = AESI_info, opt)

  write.csv(d21, paste0(output_dir,  DAP, '_Table21.csv'), row.names = F)
  saveRDS(d21, paste0(output_dir, DAP, '_Table21.rds'))
  rm(d21)
  gc()

  #######################################################################################################
  ## Table 28
  #######################################################################################################
  
  source(paste0(pre_dir, 'Step_XX_Table28_29_SCRI.R'))
  
  SCRI.list <- fun_tab29(d_matchedpop = d_matchedpop)
  d28 <- SCRI.list[[1]]
  d29 <- SCRI.list[[2]]
  
  write.csv(d28, paste0(output_dir,  DAP, '_Table28.csv'), row.names = F)
  saveRDS(d28, paste0(output_dir, DAP, '_Table28.rds'))
  write.csv(d28, paste0(output_dir,  DAP, '_Table29.csv'), row.names = F)
  saveRDS(d28, paste0(output_dir, DAP, '_Table29.rds'))
  rm(d28,d29)
  gc()
  
  #######################################################################################################
  ## Figure 2.X
  #######################################################################################################
  
  ## Note: The figures are now all stored in one PDF (one page per figure), but can also be stored separate image files if necessary.
  ## Note: For fig 2, the xlimits for myo/pericarditis until day 21, this is done by setting the risk window to 21 days in a copy of AESI_inf
  AESI_info_copy <- AESI_info
  AESI_info_copy <- reformat_mpc_AESI(AESI_info_copy, opt = "multi")
  AESI_info_copy <- AESI_info_copy[-which(AESI_info_copy$Event_name == "Anaphylaxis"),]
  #AESI_info_copy$End_of_risk_window[AESI_info_copy$Event_abbreviation == 'MYOCARD'] <- 21
  #AESI_info_copy$End_of_risk_window[AESI_info_copy$Event_abbreviation == 'PERICARD'] <- 21
  
  source(paste0(pre_dir, 'Step_41_Figure2.R'))
  fun.fig2(M_Studycohort = M_Studycohort, AESI_info = AESI_info_copy, opt = 2)
  
  rm(AESI_info_copy)
  
  #######################################################################################################
  ## Figure 1
  #######################################################################################################
  source(paste0(pre_dir, 'Step_51_Figure1.R'))

###############

  
  #Create logging
  ####
  RUN_LOG(output_dir)
  
  ###Create folder with files for DRE
  output_files <- c(list.files(pattern = ".csv|.pdf|.log|.rds|.zip",output_dir))

  DRE_files <- output_files[!output_files %in% "SUBJECTS_TO_EXTRACT.csv"]
  
  DRE_Folder <- paste0(output_dir,"OutputDRE_",gsub("-","", Sys.Date()))
  
  if(!dir.exists(DRE_Folder)) dir.create(DRE_Folder)
  
  #file.copy(DRE_files, output_dir, paste0(DRE_Folder,"/"),overwrite = T)
  
  file.copy( 
    paste0(output_dir, DRE_files), 
    DRE_Folder,
    overwrite = T
  )
  

  rm(output_files, DRE_files, DRE_Folder)
  gc()
  
  
  ###
  
  
  
  
  