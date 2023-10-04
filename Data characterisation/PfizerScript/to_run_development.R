
#Author:Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/07/2021

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)

#TO FILL 
###
#Fill with folder name that consists the CDM. This folder should be located in Pfizer/CDMInstances 
#If you want to use the path on another location fill NULL and fill path with the location
#StudyName <- "RTI_simulated_10000"
#StudyName <- "TEST_SAMPLE"
#StudyName <- "Validate"

StudyName <- NULL
#path <- "H:/RTI_SIM_CSV_20k_20220728/"
path <- "RTI_SIM_CSV_1000k_20220620/"
#path <- NULL

#Path for the pregnancy output
#preg_dir <- NULL
preg_dir <- "C:/Users/relbers/Documents/GitHub/C4591021_PfizerUMC/CDMInstances/Validate/"

#FILL DAP NAME. I wrong name the script will be stopped. See row 23
DAP <- "TEST"
###

if(!DAP %in% c("TEST","CPRD","ARS","SIMG","PEDIANET","SIDIAP","EPICHRON","UOSL","PHARMO")) stop("Fill DAP with the correct word")

#parallel_method <- "NONE"
#parallel_method <- "WINDOWS"
if(toupper(Sys.info()["sysname"]) == "WINDOWS"){parallel_method <- "WINDOWS"}else{parallel_method <- "NONE"}

if(!parallel_method %in% c("NONE", "WINDOWS")) stop("Fill parallel_method with the correct word")


###################################################
#Parameters
#################################################
#Set parameters basic parameters

#Start Defined as launch of the vaccin in relevant country (SAP) (Date creation?)
start_study_date <- "20201201"

#Date of last vaccinated subject according to SAP, then check CDM_Source if recommend end date is earlier
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


#Preparation of analyses input tables to generate inputs for matching. There need to be 3 type of files for the matching. A person level file, a file with the
#Spells and a serie of files in which monthly information is stored. These 3 file types will be the input for the matching
system.time(source(paste0(pre_dir,"Step_00_SetCodeSheets.R")))
system.time(source(paste0(pre_dir,"Step_01_CreatePersons.R")))
peakRAM(source(paste0(pre_dir,"Step_02_CreateSpells.R")))
system.time(source(paste0(pre_dir,"Step_03_GetVaccins.R")))
system.time(source(paste0(pre_dir,"Step_04_CreateVariablesCovidVaccins.R")))
system.time(source(paste0(pre_dir,"Step_04b_CreateStudyPopulation.R")))
system.time(source(paste0(pre_dir,"Step_05_GetConceptsMatching.R")))


system.time(source(paste0(pre_dir,"Step_05b_RunPregnancyAlgorithm.R")))

system.time(source(paste0(pre_dir,"Step_07_CreateSpellsMatching.R")))


#Matching:
#There is a method using SQL or with a loop wise procedure. SQL is efficient if the number of possible matches in low (< 2000). If this number increases
#The loop wise method is advised. In and outputs are equal for all the matching procedures
#system.time(source(paste0(pre_dir,"Step_08_PrepareExposedControls.R")))


system.time(source(paste0(pre_dir,"Step_08_PrepareExposedControlsSpellsCombined.R")))

#Choose optimal sttings matching
###
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

###
#nb_cores <- 12
#for(i in 1:10){
#print(i)  
system.time(source(paste0(pre_dir,"Step_09_MatchingProcedure.R")))
#system.time(source(paste0("H:/","fix_perf_match.R")))
#}


#Test new matching procedure with optimalisation. This consists of a mutation of the database created at step 8 and some little changes in step_9
###
system.time(source(paste0(pre_dir,"Step_08_addition_to_test_matcing_optimalisation.R")))
system.time(source(paste0(pre_dir,"Step_09_MatchingProcedure_new_test_optimalisation.R")))
###



system.time(source(paste0(pre_dir,"Step_10_CombineExposedControl.R")))


#Create D3 variables
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

#parallel_method <- "WINDOWS"
peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates.R")))
#Optimization using the current data model in concepts.db. This optimization is only effective when using parallel computing
#peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates_new.R")))

#parallel_method <- "NONE"
#peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates.R")))
#To optimize without parallels computing the model needs to be changed. In new2 a start is made using only the binary concepts.
#Note that the reforming of the model is also done within this script which also needs execution time.
#peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates_new2.R")))

system.time(source(paste0(pre_dir,"Step_13b_GetCodeCounts.R")))

#Only in development script
###
system.time(source(paste0(pre_dir,"Test_matching_COV_Equal.R")))
system.time(source(paste0(pre_dir,"Step_99_CreateCodebookAutomated.R")))
system.time(source(paste0(pre_dir,"Step_99_CreateFlowchartAutomated.R")))

#On a first run on a new data sample you can run this script to sample 1 subject with all the study variables 5 days before T0.
#Then in a next run in Test_matching_COV_Equal it is tested which variables are still missing.
#system.time(source(paste0(pre_dir,"Sample_all_studyvars.R")))
###

#Create log file
RUN_LOG(output_dir)


#Not applicable anymore since all the input sheets are changed drastically. It needs to be redone which is a lot of work and effort
###
if(!is.null(StudyName)){
  
  if(StudyName == "Validate"){
    system.time(source(paste0(path_dir,"ValidateScript.R")))
  }
}

###