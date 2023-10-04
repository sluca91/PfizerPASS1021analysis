#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Take the vaccines from the common data model need for the matching. This are the Covid vaccines and the influenza vaccines 

##in/output
#Input 1: VACCINES.csv (CDM)
#Input 2: VAC4EU PASS CovidVaccineMonitoringVariables-3.xlsx
#Output 1: CoV.rds
#Output 2: INF.rds 

SCRIPT <- RUN_SCRIPT(name = "Step_03_GetVaccins.R")

#Import vaccines from CDM. This is done separated from the other concepts in step 5 a 11 because more vaccine specific columns are needed for the exposure.
#So it is not done directly in a database but first in R.
###
VACCINES <- IMPORT_PATTERN(
  dir = unlist(SCRIPT[["INPUT1"]][["folder"]]), 
  pat = SCRIPT[["INPUT1"]][["name"]],
  colls = c("person_id","vx_record_date","vx_admin_date","vx_atc","vx_manufacturer","vx_type","vx_dose","meaning_of_vx_record"),
  date.colls = c("vx_record_date","vx_admin_date"),
  #exprss = expression(person_id %in% PERSONS_OF_INTEREST0)
  exprss = NULL
  
)

print("If vx_admin_date is empty, replace with vx_record_date")
VACCINES <- VACCINES[,Date := fifelse(is.na(vx_admin_date),vx_record_date,vx_admin_date)][,vx_admin_date := NULL][,vx_record_date := NULL]
VACCINES <- VACCINES[, code := fifelse(is.na(vx_atc) | vx_atc == "", vx_type, vx_atc)]

#Clean rows with no valid date
###
check1 <- nrow(VACCINES)
VACCINES <-VACCINES[!is.na(Date),]
VACCINES <-VACCINES[Date < end_study_date,]
check2 <- nrow(VACCINES)

if(check1 != check2) print(paste0("In imported VACCINES ", check1 - check2, " row(s) with a NA for date are found or the date is after end of study date. These rows are excluded"))
rm(check1, check2)
###



#Problem with the string INF. This is imported as Inf since it is a R annotation to refer to infinity
VACCINES <- VACCINES[toupper(code) == "INF"| is.infinite(code) ,  code := "INF" ]

VACCINES <- VACCINES[, voc := fifelse(is.na(vx_atc) | vx_atc == "", "vx_type", "vx_atc")]

#Get ATC codes needed for extraction. The file is changed in a way that it can be used by the function CreateConceptDatasets
###

#Get ATC codes and vx_type
FILE <- readRDS(file = SCRIPT[["INPUT2"]][["path"]])[toupper(Outcome) %in% c(MATCH_CONCEPTS, Exposure), ]


#Run function to collect and store vaccines per vaccine type. 
###

CreateConceptDatasets(
  
  codesheet = FILE,
  c.voc = "CodeSystem",
  c.concept = "Outcome",
  c.codes = "Code",
  file = VACCINES,
  f.code = "code",
  f.voc =  "voc",
  f.date = "Date",
  f.id = "person_id",
  c.startwith = start_with_colls,
  path =  SCRIPT[["OUTPUT1"]][["folder"]],
  standardized.cols = F
)

###

#Make a copy of the influenza rds files and store them in the database for later use when covariates are calculated.For the further pre match porces rds files are
#needed because the matching demands rds files as an input.

#Open the connection
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

for(i in unique(FILE$Outcome)[!unique(FILE$Outcome) %in% Exposure]){
    if(file.exists(paste0(concepts_dir,i,".rds"))){
        
        TEMP <- unique(readRDS(paste0(concepts_dir,i,".rds")))
        setnames(TEMP, c("code", "voc"), c("Value", "Voc"))
        TEMP <- TEMP[,c("person_id", "Outcome", "Value", "Voc", "Date" ), with = F]
        dbWriteTable(mydb, i ,TEMP, overwrite = T, append = F)
        rm(TEMP)
        gc()
        
    }  
} 

dbDisconnect(mydb)



rm(FILE,VACCINES, SCRIPT)

