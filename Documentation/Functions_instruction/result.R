
rm(list=ls())

library("data.table")
library(DBI)
library(RSQLite)

dir <- "C:/Users/relbers/Documents/GitHub/Pfizer/Documentation/Functions_instruction/Input files"
functionsDir <- "C:/Users/relbers/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_steps/functions"
outputDir <- "D:/test"

functions <- list.files(functionsDir, full.names = F)
invalidFunctions <- c("create_matched_pop_t4.R", "checkingTable16SX.R", "FlowChart.R", "Format_scheme.R", "Step_09_MatchingProcedure.R", "CreateConcepts_V3.R", "FUNCTIONS.R")
functions <- functions[!functions %in% invalidFunctions]

lapply(functions, function(x){print(x); 
                              source(paste0(functionsDir,"/", x))}
       )




outcomes <- IMPORT_PATTERN(
                          pat = "EVENTS.csv",
                          dir = dir,
                          date.colls = "start_date_record"
                          
                        )


codelist <- IMPORT_PATTERN(
                            pat = "codelist",
                            dir = dir
  
)


system.time(CreateConceptDatasets(
  
  codesheet = codelist,
  c.voc = "system",
  c.concept = "event_abbreviation",
  c.codes = "code", 
  file = outcomes,
  f.code = "event_code", 
  f.voc = "event_record_vocabulary", 
  f.date = "start_date_record" , 
  f.id = "person_id", 
  path = outputDir,
  f.name = "concepts",
  group = T, 
  standardized.cols = T,
  
  c.startwith = "ICD9CM"
  
))




codelist <- IMPORT_PATTERN(
  pat = "additional_concepts_format",
  dir = dir
)

mydb <- dbConnect(RSQLite::SQLite(), "")

CreateConceptDatasetsMultipleVars(
  codesheet = codelist,
  file = outcomes,
  f.id = "person_id",
  c.keep = "keep",
  c.columns = c("col1", "col2", "col3"),
  c.values = c("val1", "val2", "val3"),
  c.outcome = "StudyVar",
  c.date = "date_column",
  db = mydb
  
)

dbReadTable(mydb, "P_CARESP_AESI_LEFT")
dbReadTable(mydb, "P_CARESP_AESI_LAT")

concept <- readRDS(paste0(outputDir,"/COVID19DX.rds"))



conceptCleaned <- CleanOutcomes(
                                Dataset = concept,
                                Person_id = "person_id",
                                rec.period = c(10,5),
                                c.date = "Date"
)






for(i in c("COVID19DX", "COVID19POS")){
            dbWriteTable(mydb, i, readRDS(paste0(outputDir,"/",i,".rds")))
          }

conceptCovid <- AppendConcepts(DB = mydb,
                               CONCEPTS = c("COVID19DX", "COVID19POS"),
                               NAME = "COVID"
                               )


dbDisconnect(mydb)


