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
  
  # Note: you always have to run this first, as Table2 script creates frail and immunocompromised populations (D3 step)
  # and until line 132 (see note)
  system.time(source(paste0(pre_dir,"Step_20_Table1.R")))

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
  # This object is created in D3
  M_Studycohort <- readRDS(paste0(populations_dir,"M_Studycohort.rds"))
  
  #   M_Studycohort_Covariates_T0 is also created in D3. Below we load it and create a new variable,
  # Hosp_week, which is a funciton of other variables already in the data frame
  source(paste0(pre_dir,"functions/", "create_healthcarecontact_var.R"))  
  M_Studycohort_Covariates_T0 <- Main()
  
  ## Load a function to cater for different format of AESI_info due to myo and pericarditis requests
  source(paste0(pre_dir,"functions/", "reformat_mpc_AESI.R"))  
  
  ## create matched population for table 16SX and 20SX
  source(paste0(pre_dir,"functions/", "create_matched_pop_t4.R"))  
  d_matchedpop <- readRDS(paste0(populations_dir, "matched_pop.rds"))
  
  # NOTE: Run until here before running any future table scripts
  
  #######################################################################################################
  ## Figure 3
  #######################################################################################################

  ## Note: Figure 3 is the same as figure 2, but limited to COVID only and with 12 days as the end of the risk period
  ## So here, we simply edit the one row from AESI_info
  AESI_info_copy <- reformat_mpc_AESI(AESI_info, opt = "multi")
  AESI_info_copy <- AESI_info_copy[AESI_info_copy$Event_abbreviation == 'I_COVID_COV',]
  AESI_info_copy$End_of_risk_window <- 12
  source(paste0(pre_dir, 'Step_41_Figure2.R'))

  # here we save figures, and the object Figures3RD.RDS
  fun.fig2(M_Studycohort = M_Studycohort, AESI_info = AESI_info_copy, opt = 3)
  ## Note: If I_COVID_COV is not present in M_Studycohort, this will generate an empty pdf

  rm(AESI_info_copy)

  
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
  
  
  
  
  