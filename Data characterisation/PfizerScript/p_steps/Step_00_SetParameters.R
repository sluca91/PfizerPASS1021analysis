#Author: Roel Elbers Drs.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


############################################################################################
#Get cdm_source file name
cdm_source_file <- list.files(path_dir, pattern="CDM_SOURCE")
#Get DAP info and date createion fro CDM_SOURCE
CDM_SOURCE <- fread(paste0(path_dir, cdm_source_file))
date_creation <- CDM_SOURCE[,date_creation]
data_access_provider_name <- CDM_SOURCE[,data_access_provider_name]
data_source_name <- CDM_SOURCE[,data_source_name]
recommended_end_date <- as.Date(as.character(CDM_SOURCE$recommended_end_date),"%Y%m%d")
rm(CDM_SOURCE, cdm_source_file)
############################################################################################

#########################################################
#date transformations
#######################################################
start_study_date <- as.Date(start_study_date,"%Y%m%d")
end_study_date <- as.Date(end_study_date,"%Y%m%d")
date_creation<-as.Date(as.character(date_creation),"%Y%m%d")

##########################################################
print("Check date creation and recommend_end_date. If prior to end_study_date change end_study_date accordingly")
end_study_date <- min(end_study_date,date_creation,recommended_end_date,na.rm = T)
intv <- as.Date(c(start_study_date, end_study_date))


start_study_date2 <- paste0(year(start_study_date),sprintf("%02d",month(start_study_date)),sprintf("%02d",day(start_study_date)))
end_study_date2 <- paste0(year(end_study_date),sprintf("%02d",month(end_study_date)),sprintf("%02d",day(end_study_date)))

ExclusionMatchCohort <- list(
  No_sex = expression(!is.na(sex_at_instance_creation)),
  No_year_of_birth = expression(!is.na(year_of_birth)),
  Invalid_date = expression(!is.na(birth_date)),
  No_year_of_death = expression(!(is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death)))),
  FirstVac_Within_StudyPeriod = expression(FIRST_PFIZER >= start_study_date | FIRST_OTHER >= start_study_date | (is.na(FIRST_PFIZER) & is.na(FIRST_OTHER))),
  No_observation_time = expression(!is.na(num_spell) & !is.na(op_start_date) & !is.na(op_end_date)),
  No_op_start_date = expression(!is.na(op_start_date)),
  OP_START_DATE_before_OP_END_DATE = expression(op_start_date < op_end_date),
  Study_Period_and_spell_overlap  = expression(op_start_date %between% intv| op_end_date %between% intv | (op_start_date < start_study_date & op_end_date > end_study_date)),
  Spells_less_then_lookback_period = expression(op_end_date - op_start_date > lookback_period | op_start_date == birth_date),
  Remaning_time_to_end_study_date_less_then_lookback_period = expression(end_study_date - op_start_date > lookback_period),
  Time_before_FIRST_PFIZER_more_than_lookback = expression(Used_spell_exposed == F | ((FIRST_PFIZER - op_start_date) > lookback_period |  (op_start_date==birth_date))),
  Filter_out_FIRST_PFIZER_prior_in_database = expression(is.na(FIRST_PFIZER) | (!is.na(FIRST_PFIZER) & !FIRST_PFIZER < op_start_date)),
  Filter_out_FIRST_OTHER_prior_in_database = expression(is.na(FIRST_OTHER) | (!is.na(FIRST_OTHER) & !FIRST_OTHER < op_start_date))
)

#Load program steps. There is a csv file for the in and output names. This is used at the start end end of every script and for the generation of the metadata
###
STEPS <-fread(paste0(meta_dir,"Program.csv"), stringsAsFactors = F, na.strings = "")
STEPS <- STEPS[,TYPE_ID := seq_len(.N), by = c("PROGRAM","TYPE")] 

temp <- as.data.table(unique(STEPS[["PROGRAM"]]))[,STEP_ID := seq_len(.N)]
setnames(temp, "V1", "PROGRAM")

PROGRAM <- merge(STEPS, temp, by = "PROGRAM")

#Problem with ifelse in data.table
###
x <- list()
for(i in 1:nrow(PROGRAM)){x[i] <- get(PROGRAM[i,][["FOLDER_VAR"]])}
PROGRAM$FOLDER <- x
rm(x)

PROGRAM <- PROGRAM[, VAR := paste0(TYPE,TYPE_ID)]
PROGRAM <- PROGRAM[, result := ifelse(FORMAT == "CDM",FILE, paste0(FOLDER,FILE,".",FORMAT)), by = row.names(PROGRAM)]
rm(temp, STEPS)
###

#Get codes info
###
start_with_colls <- c("ICD10CM","ICD10","ICD10DA","ICD9CM","MTHICD9", "ATC", "vx_atc")
###

###
#In windows machines parallel computing is used in some parts of the script. This parallel computing is done in:
#- Step_09_MatchingProcedure
#- Step_12_AddCoVariates
#- Step_13_AddAESI
#When computing parallel not merely 1 core is used but instead several cores are used. Because other processes on the computer also may need a core
#I did decide to not to use all the cores. Instead I take half of the available cores.
#Therefore a variable is created nb_cores to specify the number of cores that are going to be used in the parts where parallel computing is used.
n.cores <- detectCores()
nb_cores <- ceiling(n.cores/2)
###

###
#Make reusable object with agebands
bands1 <- as.data.table(CreateBands(c(0,6,12,18,30,40,50,60,70,80), NEWBORNS = F))
bands2 <- as.data.table(CreateBands(c(80,130), NEWBORNS = F))[band0 == "080-129", band0 := "80+"]

bands <- rbind(bands1, bands2)[,.(INT, band0)]
setnames(bands, "band0", "band")

rm(bands1, bands2)

###

###
store_dir <- paste0(tmp,"parameters.RData")

###

#Databases
###
dbconcepts <- paste0(concepts_dir,"concepts.db")


###
#Set variables to make direct the several study variables through the program. Study variable names are set to capital letters on all places.

#Get parameter files from meta data that are needed to set all the environmental variables used throughout the whole script. 
TEMP1 <- IMPORT_PATTERN(pat = "Pfizer_study_variables.csv", dir = meta_dir)[!VarName %in% c("L_RACE_COV"), ]
if(any(duplicated(TEMP1$VarName))) stop("Duplicated study varaibles in study variable table")
TEMP2 <- IMPORT_PATTERN(pat = "Pfizer_algorithms.csv", dir = meta_dir)[, WEIGHT := as.numeric(WEIGHT)]

#Check if all new concepts have equal information. For future consider adding this in the study variables file instead of the algorithms file. 
if(!nrow(unique(TEMP2[,.(NEW_CONCEPT, TYPE, SCRIPT, FUNC)])) == length(unique(TEMP2$NEW_CONCEPT))) stop("Algorithm file not filled correctly.")

#Determine based on the score file if a variable needs to be scored and add that to TEMP1. Later tis is used to make variables.
TEMP1 <- TEMP1[, SCORE := fifelse(VarName %in% unique(IMPORT_PATTERN(pat = "Pfizer_scores.csv", dir = meta_dir)[DAP_NAME == "ALL" | DAP_NAME == DAP,][["CONCEPT"]]),
                                  T,
                                  F)]
#Determine what variables are an algorithm or an input for an algorithm
TEMP1 <- TEMP1[, Algorithm := fifelse(toupper(VarName) %in% toupper(unique(TEMP2$NEW_CONCEPT)),
                                  T,
                                  F)]

TEMP1 <- TEMP1[, Algorithm_input := fifelse(toupper(VarName) %in% toupper(unique(TEMP2$CONCEPT)),
                                      T,
                                      F)]


#Check if OR algorithm inputs are not an algorithm itself. This input may not be created when it is needed.
if(nrow(TEMP1[VarName %in% unique(TEMP2[TYPE == "OR",][["CONCEPT"]]) & Algorithm == T,]) > 0) stop("Check algorithms. An OR input is also and algorithm.")

lapply(c("AESI","COV","EXPOS","MATCHING","Algorithm", "PERSON_CDM", "SCORE"), function(x) TEMP1 <- TEMP1[, eval(x) := as.logical(get(x))])

#Weights and FUNPRIOR variables are algorithms that are created after GetDatesIR and extracted in by GetDatesIR. This in contrast to the other algorithms 
TEMP1 <- TEMP1[, WEIGHT := fifelse(
  VarName %in% unique(TEMP2[TYPE == "WEIGHT",][["NEW_CONCEPT"]]),
  T,
  F
)
]

TEMP1 <- TEMP1[, FUNPOST := fifelse(
  VarName %in% unique(TEMP2[TYPE == "FUNPOST",][["NEW_CONCEPT"]]),
  T,
  F
)
]


ALL_CONCEPTS <- unique(toupper(TEMP1[["VarName"]]))

#Because CPRD included variables in the code sheet that are not binary these are excluded from the concepts that are needed in the codesheets 
#ALL_CONCEPTS_CODESHEETS <- ALL_CONCEPTS[!ALL_CONCEPTS %in% c("L_BMI_COV", "L_BMICALC_COV", "L_WEIGHT_COV", "L_HEIGHT_COV")]
ALL_CONCEPTS_CODESHEETS <- unique(toupper(TEMP1[!TYPE %in% c("CAT", "NUM"),][["VarName"]]))

#Split matching concepts form from the rest of the concepts. This variable is used to determine which concepts needs to be imported in step_05 or step_11.
#Concepts that are composed from other do not need to be imported and are removed from these variables
###
MATCH_CONCEPTS <- unique(toupper(TEMP1[MATCHING == T & Algorithm == F & PERSON_CDM == F,][["VarName"]]))
if(DAP %in% 'PEDIANET'){
  MATCH_CONCEPTS <- MATCH_CONCEPTS[!MATCH_CONCEPTS %in% 'L_GEOREGION_COV']
}
NMATCH_CONCEPTS <- unique(toupper(TEMP1[MATCHING == F & Algorithm == F & PERSON_CDM == F,][["VarName"]]))
###

#Store the variables that do not need to be imported directly from the CDM since they are composed from other primary concepts.
###
MATCH_ALG <-  unique(toupper(TEMP1[MATCHING == T & Algorithm == T,][["VarName"]]))
NMATCH_ALG <-  unique(toupper(TEMP1[MATCHING == F & Algorithm == T,][["VarName"]]))
###

#Add concepts that are needed for algorithms but missing in Study Variables overview. This line of code should not be needed but is added to be sure.
###
MATCH_CONCEPTS <- na.omit(unique(toupper(c(MATCH_CONCEPTS, TEMP2[NEW_CONCEPT %in% MATCH_ALG ,][["CONCEPT"]]))))
NMATCH_CONCEPTS <- na.omit(unique(toupper(c(NMATCH_CONCEPTS, TEMP2[NEW_CONCEPT %in% NMATCH_ALG ,][["CONCEPT"]]))))
###

#Remove overlap in concepts that are in concepts for algorithms in both matching and not matching
NMATCH_CONCEPTS <- NMATCH_CONCEPTS[!NMATCH_CONCEPTS %in% MATCH_CONCEPTS]

warningMisAlg1 <- na.omit(unique(TEMP2$NEW_CONCEPT[!toupper(TEMP2$NEW_CONCEPT) %in% toupper(TEMP1[Algorithm == T,]$VarName)]))
warningMisAlg2 <- na.omit(unique(TEMP2$CONCEPT[!toupper(TEMP2$CONCEPT) %in% toupper(TEMP1$VarName)]))
if(length(warningMisAlg1) > 0) warning("Check column if all new concepts (NEW_CONCEPT) in algorithms are available in study varaibles")
if(length(warningMisAlg2) > 0) warning("Check column if all input concepts (CONCEPT) in algorithms are available in study varaibles")

#Define which variables need to be appended because they are an OR algorithm
###
#Needed in step_05
#In the matching there is an exception for CDC. This weighting variable is not automated and hard coded in script 5. 
#this is in contrast with the covariates extraction at step 12 where weighting is automated
MATCH_OR <- TEMP1[MATCHING == T & Algorithm == T & toupper(VarName) %in% unique(toupper(TEMP2[TYPE == "OR",][["NEW_CONCEPT"]])),][["VarName"]]
MATCH_OR <- toupper(c(MATCH_OR, "V_CDC_COV")) #Note that CDC is not belonging here but added as an exception that is hardcoded in script 5

#Needed in step_11b.
NMATCH_OR <- toupper(TEMP1[MATCHING == F & Algorithm == T & toupper(VarName) %in% unique(toupper(TEMP2[TYPE == "OR",][["NEW_CONCEPT"]])),][["VarName"]])
###


#This variable represents the variables that eventually need to be in the output in the files M_Studycohort_Covariates_T0 and folder populations/AESI
###
AESI <- unique(toupper(TEMP1[AESI == T,][["VarName"]]))
COV <- unique(toupper(TEMP1[COV == T,][["VarName"]]))
###

#Variables regarding vaccines and naming. Needed in step_04. Not fully developed but and for interim 4 this can be further automated.  
###
Exposure <- unique(toupper(TEMP1[EXPOS == T,][["VarName"]]))
nb_vaccins <- 4
manufacturer <- "pfizer"
cols_vac <- toupper(c(paste0(1:nb_vaccins,"_", manufacturer),paste0(1:nb_vaccins,"_", "OTHER")))
cols_vac_new <- toupper(c(paste0(english::ordinal(1:nb_vaccins),"_", manufacturer),paste0(english::ordinal(1:nb_vaccins),"_", "OTHER")))
###


#Separate variables that are in person table and do not vary in time from information that has a date. Person level information needs ro be renamed by variables 
#ending on _name
#In the preparation of the matching this is important to separate the matching factors that need spell creation. 
###
time_dep_match <- toupper(TEMP1[PERSON_CDM == F & MATCHING == T,][["VarName"]])
time_indep_match <- toupper(TEMP1[PERSON_CDM == T & MATCHING == T,][["VarName"]])
time_indep_match_name <- TEMP1[PERSON_CDM == T & MATCHING == T,][["PERSON_VAR"]]

time_indep_nmatch <- toupper(TEMP1[PERSON_CDM == T & MATCHING == F,][["VarName"]])
time_indep_nmatch_name <- TEMP1[PERSON_CDM == T & MATCHING == F ,][["PERSON_VAR"]]
time_dep_nmatch <- toupper(TEMP1[PERSON_CDM == F & MATCHING == F ,][["VarName"]])

###

#For matching and covariates there are 2 kind of variables. Variables with a Boolean (TRUE or FALSE) and variables with a category or value.
#This means that for the extraction of the covariates in step_12 different variables need to be selected. For TF only the Date and for CAT also the Value
###
MATCH_TF <- toupper(TEMP1[toupper(VarName) %in% time_dep_match & TYPE == "TF" ,][["VarName"]]) #& !CAT_PRIOR %in% c("BETWEEN", "CLOSE_PRIOR_POST")
#In the matching the look back time is imported for the generation of the spells. For _CAT the closest information is taken.
MATCH_TF_T <- TEMP1[toupper(VarName) %in% time_dep_match & TYPE == "TF" ,][["CAT_PRIOR"]]

#PREGNACY status is a exeption and set to NA because here look back time is not relevant while it is a binary variable
MATCH_TF_T[MATCH_TF_T %in% c("BETWEEN", "CLOSE_PRIOR_POST")] <- NA
MATCH_TF_T <- as.integer(MATCH_TF_T)
###

#These variables are needed to determine which concepts need to be set to integer in the database and evaluated with the dictionary
###
MATCH_CAT <- toupper(TEMP1[toupper(VarName) %in% time_dep_match & TYPE == "CAT",][["VarName"]]) #Also needed to deviate which query to take at step 12, and which spell functions for matching
NMATCH_CAT <- toupper(TEMP1[toupper(VarName) %in% time_dep_nmatch & TYPE %in% "CAT",][["VarName"]]) #Also needed to deviate which query to take at step 12
CAT_PER <- toupper(TEMP1[toupper(VarName) %in% time_indep_match & TYPE %in% "CAT",][["VarName"]])
###

#These variables are needed to deviate which query to take at step 12
###
NMATCH_SUM <- toupper(TEMP1[toupper(VarName) %in% time_dep_nmatch & TYPE == "SUM",][["VarName"]])
NMATCH_NUM <- toupper(TEMP1[toupper(VarName) %in% time_dep_nmatch & TYPE %in% "NUM",][["VarName"]])
###

#Within the algorithms there is a distinction between TF algorithms and algorithms that needs scoring. These 2 types of algorithms are handled in different parts of
#the program. TF based on multiple sub concepts are created directly after the extraction from the CDM (step_11) while scoring and calculations (BMI) is performed after
#the selection of the cases based on T0/time of 3th dose in the function GetDatesIR.R. (Step_13)

#These variables are created after the function GetDatesIR.R while not scoring algorithms are already composed and concepts on itself
MATCH_SCORE <- toupper(TEMP1[SCORE == T & MATCHING == T,][["VarName"]])
MATCH_WEIGHT <- toupper(TEMP1[WEIGHT == T & MATCHING == T,][["VarName"]])
MATCH_FUNPOST <- toupper(TEMP1[FUNPOST == T & MATCHING == T,][["VarName"]])
#
NMATCH_SCORE <- toupper(TEMP1[SCORE == T & MATCHING == F,][["VarName"]])
NMATCH_WEIGHT <- toupper(TEMP1[WEIGHT == T & MATCHING == F,][["VarName"]])
NMATCH_FUNPOST <- toupper(TEMP1[FUNPOST == T & MATCHING == F,][["VarName"]])
#


#This are the concepts that are needed for the algorithms calculated after the function GetDatesIR.R is applied. This in contrast to algorithms that are handled before GetDatesIR.R
#The naming of this variable does not make the distinction between post and prior algorithms which may be misleading.
MATCH_ALG_CONCEPTS <- toupper(TEMP2[NEW_CONCEPT %in% c(MATCH_WEIGHT, MATCH_FUNPOST) ,][["CONCEPT"]])
NMATCH_ALG_CONCEPTS <- toupper(TEMP2[NEW_CONCEPT %in% c(NMATCH_WEIGHT, NMATCH_FUNPOST) ,][["CONCEPT"]])
#

#This is related to MATCH_ALG_CONCEPTS and NMATCH_ALG_CONCEPTS and has the aim to specify the input concepts that are not binary 
# _CAT is added to make it clear that not only the date is needed but also the VALUE. Later on NUM was added
NMATCH_ALG_CONCEPTS_CAT <- toupper(TEMP1[toupper(VarName) %in% NMATCH_ALG_CONCEPTS & TYPE %in% c("CAT", "NUM"),][["VarName"]])
MATCH_ALG_CONCEPTS_CAT <- toupper(TEMP1[toupper(VarName) %in% MATCH_ALG_CONCEPTS & TYPE %in% c("CAT", "NUM"),][["VarName"]])
###

#After extracting and scoring all coavariates rows outside lookback period are removed based on this file. This is done after extraction so that distinctions within 
#Algoritms and the underlying concepts can be made. So a simple covariate that also is an input for a scoring algorithm can have a different value lookback time in both
#Situations
LOOKBACK_COV <- TEMP1[COV == T & !is.na(CAT_PRIOR) & !CAT_PRIOR %in% c("BETWEEN", "CLOSE_PRIOR_POST"),][, .(CAT_PRIOR, VarName, PRIOR_UNIT)][, VarName := toupper(VarName)][, CAT_PRIOR := as.numeric(CAT_PRIOR)]
LOOKBACK_COV <- LOOKBACK_COV[PRIOR_UNIT == "year", DAYS := CAT_PRIOR * 365.25]
LOOKBACK_COV <- LOOKBACK_COV[PRIOR_UNIT == "day", DAYS := CAT_PRIOR]
LOOKBACK_COV <- LOOKBACK_COV[PRIOR_UNIT == "month", DAYS := CAT_PRIOR * 30]
LOOKBACK_COV <- LOOKBACK_COV[PRIOR_UNIT == "week", DAYS := CAT_PRIOR * 7]

#These variable are needed to generate the eventual covariate files per cohort
###
COV_TF <- unique(toupper(TEMP1[COV == T & TYPE == "TF",][["VarName"]]))
COV_CAT <- unique(toupper(TEMP1[COV == T & TYPE %in% c("CAT", "NUM"),][["VarName"]]))
COV_SCORE <- unique(toupper(TEMP1[COV == T & SCORE == T,][["VarName"]]))

#Default manner to extract covariates is by taking the nearest in the past witin a loockback period
#Distinct between covariates that have a start and end period and therefore need to be extracted using a between in the sql query 
COV_BETWEEN <- unique(toupper(TEMP1[COV == T & CAT_PRIOR == "BETWEEN",][["VarName"]]))
#Also some variables are extracted by first loking back and if nothing found looking forward
COV_CLOSE_PRIOR_POST <- unique(toupper(TEMP1[COV == T & TYPE == "CAT" & CAT_PRIOR == "CLOSE_PRIOR_POST",][["VarName"]]))

###   

programPaths <- dir(pre_dir, full.names = TRUE, recursive=TRUE)
functionsPaths <- dir(paste0(pre_dir,"functions"), full.names = TRUE, recursive=TRUE)
metaPaths <- dir(meta_dir, full.names = TRUE, recursive=TRUE)
zip::zipr(zipfile = paste0(output_dir,"sourceCode.zip"), files = c(programPaths, functionsPaths, metaPaths))
file.copy(paste0(path_dir,'CDM_SOURCE.csv'), output_dir, overwrite = T)



  
#Check which concepts may be expected in the results depended of which DAP
###
if(!is.null(StudyName)){ if(StudyName == "TEST_SAMPLE"){
  source(paste0(pre_dir,"functions/check_meta_files.R"))
  check_meta_files()
  
}
}
rm(TEMP1, TEMP2)

#Store supported units. Note that because / is not allowed in object name that the first string in the vector is the unit name used to standardize.
supportedUnits <-list( 
                    kg = c("kg", "kilogram","kilograms", "kilogramme", "kilo"),
                    g = c("g", "gram","grams", "gramme"),
                    m = c("m", "meter","meters", "metre"),
                    cm =  c("cm", "centimeter","centimetre"),
                    kgm2 = c("kg/m2", "kgperm2", "kg/m\\^2", "kg/m^2"),
                    cigd = c("cig/d")
)



