#Author:Rutger van den Bor/Albert Cid Royo 
#email: R.M.vandenBor@umcutrecht.nl/a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/02/2022


## Run this script in R-studio 
## Specify the correct folder location, select all and click 'Run' on the top right) 

if(!exists("projectFolder") | !exists("DAP")) rm(list=ls())  

if(!exists("projectFolder") | !exists("DAP")){ 
  projectFolder <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  StudyName <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  # StudyName <- "Validate"
  
  DAP <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  # DAP <- "TEST"
  
  start_study_date <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  # start_study_date <- "20201201"
  end_study_date <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  # end_study_date <- "20221231"
  
  if (start_study_date %in% c('TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN') | end_study_date %in% c('TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN')){
    stop('Fill the Start study date or the End study date as you did in the to_run_file')
  }
  
  lookback_period <- 365
  max_spells_gap <- 365
  
  
  source(paste0(projectFolder,"/99_path.R"))
  
  
  #Load functions
  source(paste0(pre_dir,"functions/", "FUNCTIONS.R"))
  source(paste0(pre_dir,"functions/", "RUN_SCRIPT.R"))
  source(paste0(pre_dir,"functions/", "CreateBands.R"))
  source(paste0(pre_dir,"functions/", "IMPORT_PATTERN.R"))
  
  #Set parameters
  source(paste0(pre_dir,"Step_00_SetParameters.R"))
  source(paste0(pre_dir,"Step_00_SetCodeSheets.R"))
  
}

if(!DAP %in% c("TEST","CPRD","ARS","SIMG","PEDIANET","SIDIAP","EPICHRON","UOSL","PHARMO")) stop("Fill DAP with the correct word")


system.time(source(paste0(pre_dir,"Step_22_Table3.R")))
rm(col1, col2, countBand, countMonth, countRegion, filledTemplate3, First_Dose_Population, results, table3, template_table3, Three_Dose_Population, uniqueBands)
gc()

###############


#Create logging
####
RUN_LOG(output_dir)

###Create folder with files for DRE
output_files <- c(list.files(pattern = "*_Table3_*",output_dir),"to_run_fix_table_3.log")
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