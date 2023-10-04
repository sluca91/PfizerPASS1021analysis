
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


#FILL DAP NAME. If wrong name the script will be stopped.
DAP <- "TOFILL"
###

if(!DAP %in% c("TEST","CPRD","ARS","SIMG","PEDIANET","SIDIAP","EPICHRON","UOSL","PHARMO")) stop("Fill DAP with the correct word")


#Some parts in the scripts do use parallel computing. I was only able to make this work properly on windows machines. Therefore, a variable is created that is used
#to determine if some processes are executed in parallel or not.
if(toupper(Sys.info()["sysname"]) == "WINDOWS"){parallel_method <- "WINDOWS"}else{parallel_method <- "NONE"}
if(!parallel_method %in% c("NONE", "WINDOWS")) stop("Fill parallel_method with the correct word")


###################################################
#Parameters
#################################################
#Set parameters basic parameters

#Start Defined as launch of the vaccin in relevant country.
start_study_date <- "20201201"

#Date of last vaccinated subject.
end_study_date <- "20221231"

lookback_period <- 365
max_spells_gap <- 365

source(paste0(projectFolder,"/packages.R"))
source(paste0(projectFolder,"/99_path.R"))


#Load functions
source(paste0(pre_dir, "LoadFunctions.R"))

#Set parameters
source(paste0(pre_dir,"Step_00_SetParameters.R"))
source(paste0(pre_dir,"Step_00_SetDeleteFiles.R"))


#Preparation the data needed for matching. 
system.time(source(paste0(pre_dir,"Step_00_SetCodeSheets.R")))
system.time(source(paste0(pre_dir,"Step_01_CreatePersons.R")))
system.time(source(paste0(pre_dir,"Step_02_CreateSpells.R")))
system.time(source(paste0(pre_dir,"Step_03_GetVaccins.R")))
system.time(source(paste0(pre_dir,"Step_04_CreateVariablesCovidVaccins.R")))
system.time(source(paste0(pre_dir,"Step_04b_CreateStudyPopulation.R")))
system.time(source(paste0(pre_dir,"Step_05_GetConceptsMatching.R")))
system.time(source(paste0(pre_dir,"Step_05b_RunPregnancyAlgorithm.R")))
system.time(source(paste0(pre_dir,"Step_07_CreateSpellsMatching.R")))

#Matching:
###

#Prepare database
system.time(source(paste0(pre_dir,"Step_08_PrepareExposedControlsSpellsCombined.R")))

#Analyse system and matching properties to choose optimal settings matching
STATS <- AnalyseMatching()
if(toupper(Sys.info()["sysname"]) == "WINDOWS") FreeDisk <- GetFreeDiskSpace()
if(exists("FreeDisk")) STATS[["FreeDisk"]] <- FreeDisk
if(exists("FreeDisk")) if(!is.null(FreeDisk)) FreeDisk <- FreeDisk * 0.7
if(!exists("FreeDisk")) FreeDisk <- 20
if(FreeDisk > 281474) FreeDisk <- 280000

if(STATS$mean_nb > 8000){MATCHING_METHOD <- "LOOP"}else{
  MATCHING_METHOD <- "SQL"
  if(STATS$size * STATS$mean_nb > FreeDisk){match_batch <- (STATS$size * STATS$mean_nb) / FreeDisk}else{match_batch <- 1}
}

saveRDS(STATS, paste0(matching_dir,"MATCHING_STATS.rds"))

#Execute matching
system.time(source(paste0(pre_dir,"Step_09_MatchingProcedure.R")))

#Combine results
system.time(source(paste0(pre_dir,"Step_10_CombineExposedControl.R")))

#Logging
RUN_LOG(output_dir)








