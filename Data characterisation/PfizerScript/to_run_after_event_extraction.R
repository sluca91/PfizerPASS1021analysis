
#Author:Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/07/2021

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder <- dirname(rstudioapi::getSourceEditorContext()$path)

#TO FILL 
###
#Fill with folder name that consists the CDM. This folder should be located in Pfizer/CDMInstances 
#If you want to use the path on another location fill NULL and fill path with the location
StudyName <- "TOFILL"
#StudyName <- NULL
#path <- "H:/TOFILL/"

#Path for the pregnancy output. Fill the path to the folder where the file D3_pregnancy_final.RData is located. If this is not available fill NULL
preg_dir <- NULL
#preg_dir <- "C:/Users/relbers/Documents/GitHub/C4591021_PfizerUMC/CDMInstances/Validate/"


#FILL DAP NAME. I wrong name the script will be stopped. See row 23
DAP <- "TOFILL"
###

if(!DAP %in% c("TEST","CPRD","ARS","SIMG","PEDIANET","SIDIAP","EPICHRON","NHR","PHARMO")) stop("Fill DAP with the correct word")

if(toupper(Sys.info()["sysname"]) == "WINDOWS"){parallel_method <- "WINDOWS"}else{parallel_method <- "NONE"}

if(!parallel_method %in% c("NONE", "WINDOWS")) stop("Fill parallel_method with the correct word")


###################################################
#Parameters
#################################################
#Set parameters basic parameters

#Start Defined as launch of the vaccin in relevant country
start_study_date <- "20201201"

#Date of last vaccinated subject
end_study_date <- "20221231"

lookback_period <- 365
max_spells_gap <- 365

source(paste0(projectFolder,"/packages.R"))
source(paste0(projectFolder,"/99_path.R"))


#Load functions
source(paste0(pre_dir, "LoadFunctions.R"))

#Set parameters
source(paste0(pre_dir,"Step_00_SetParameters.R"))
source(paste0(pre_dir,"Step_00_SetCodeSheets.R"))

#Create D3 variables for matched pairs
###
system.time(source(paste0(pre_dir,"Step_11_PutConceptsInDatabase.R")))
system.time(source(paste0(pre_dir,"Step_11b_CleanContiousConcepts.R")))
system.time(source(paste0(pre_dir,"Step_11b_CreateAdditionalConcepts.R")))

#Create covid outcomes
###
system.time(source(paste0(pre_dir,"Step_11d_create_covid_episodes.R")))
system.time(source(paste0(pre_dir,"Step_11e_create_covid_outcomes.R")))
###

system.time(source(paste0(pre_dir,"Step_11c_PrepareDatabase.R")))
system.time(source(paste0(pre_dir,"Step_13_AddAESI.R")))
system.time(source(paste0(pre_dir,"Step_12_AddCoVariates.R")))
###

#Additional analyses to check 
system.time(source(paste0(pre_dir,"Step_13b_GetCodeCounts.R")))




#Create log file
RUN_LOG(output_dir)




