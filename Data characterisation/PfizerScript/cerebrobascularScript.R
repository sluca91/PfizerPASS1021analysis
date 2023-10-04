rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder <- dirname(rstudioapi::getSourceEditorContext()$path)

StudyName <- NULL
path <- "~/Documents/GitHub/Astrazeneca-PASS-study/CDMInstances/RTI_SIM_CSV_10K/"

DAP <- "SIDIAP"
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



system.time(source(paste0(pre_dir,"Step_20_Table1.R")))
system.time(source(paste0(pre_dir,"Step_23_Table4.R")))
system.time(source(paste0(pre_dir,"Step_29_Table10.R")))

SCRIPT <- RUN_SCRIPT(name = "Step_12_AddCoVariates.R")

#Set up database connection 
###
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Get the 
load(file = store_dir)
##########


concept <- 'N_CEREBROVASC_AESI'


if(concept %in% dbListTables(mydb)){
  
  #Create data frame with all concepts and there settings for the function that creates the queries. (GetDatesIR) Every row in the scheme contains the settings
  #for the query
  ###
  
  #It is only needed to extract the available concepts. An extra variable is used for if things need to be added.
  cols_scheme <- concept
  
  #Build the framework for the scheme based on the needed covariates for the T0, D2 and D3 cohort.
  scheme <- as.data.table(rbind(
    cbind(Concept = cols_scheme, FILE =rep("M_Studycohort",length(cols_scheme)),Start_date =rep("T0",length(cols_scheme)),  type = rep("COV",length(cols_scheme)))#,            
    #cbind(Concept = cols_scheme, FILE =rep("EXPOSED",length(cols_scheme)),Start_date =rep("SECOND_PFIZER",length(cols_scheme)), type = rep("COV",length(cols_scheme))),
    #cbind(Concept = cols_scheme, FILE =rep("EXPOSED",length(cols_scheme)),Start_date =rep("THIRD_PFIZER",length(cols_scheme)),  type = rep("COV",length(cols_scheme)))
  ))
  
  rm(cols_scheme)
  
  #Create the column names that will be used in the wide format end table's. There will be 3 separate tables (T0, D2, D3) with a column per covariate.
  ###
  scheme <- scheme[Start_date == "T0" & FILE == "M_Studycohort", c.name := paste0(Concept,"_T0")]
  #scheme <- scheme[Start_date == "SECOND_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D2")]
  #scheme <- scheme[Start_date == "THIRD_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D3")]
  ###
  
  #Most of the covariates are extracted with no lookback time (999 year). This is done because some concepts are part of an algorithm calculated after 
  #the execution of the query. So the lookback period per covariate is handled at the end of this script after all algorithm variables are created.
  scheme <- scheme[type == "COV", lookback := 999]
  
  #The base file is created but there are still some exceptions that need to be included in the scheme.
  ###
  #For non binary concepts also the value/result needs to be extracted and so be a part of the SELECT statement.
  scheme <- scheme[ , coll :=  fifelse(Concept %in% c(MATCH_CAT, NMATCH_CAT, NMATCH_NUM, NMATCH_ALG_CONCEPTS_CAT, MATCH_ALG_CONCEPTS_CAT)
                                       & !Concept %in% MATCH_SCORE,
                                       "Value", "")]
  
  #For some categorical variables, those used in the matching in Pfizer, a different assumptions is taken. Normally, occurrences before T0 are extracted. But in
  #this exception if no occurrences are found then also the instances after T0 are extracted. So on this point prior and post need to be extracted  
  scheme <- scheme[ , post :=  fifelse(Concept %in% COV_CLOSE_PRIOR_POST, T, F) ]
  #The end point after T0 is set to always as 999 year. endpoint is only used when post is TRUE so t1.op_end_date if false is irrelevant
  scheme <- scheme[ , endpoint :=  fifelse(post, paste0("t1.",Start_date," + (999 * 365.25)"), "t1.op_end_date") ]
  
  #ARS is delivering the pregnancy status with a start and end data meaning that extraction needs to be done via a between WHERE statement.
  ###
  scheme <- scheme[ , between :=  fifelse(Concept %in% COV_BETWEEN , T, F) ]
  #If between only that query is needed.
  scheme <- scheme[ , prior :=  fifelse(between, F,T) ]
  scheme <- scheme[between == T , `:=` (endpoint = "", prior.col = "") ] #So prior.col and endpoint are not relevant. 
  ###
  
  #HARDCODED. For most covariates the most recent in recent is time is taken. Height is an exception as decided in some meeting at some point. Taking the nearest
  #is done by means of sorting and then take the first. Therefore, for height the sorting variable needs to be changed to Value instead of Date. The highest date
  #is equal nearest, and for the height we want to have the highest value.
  scheme <- scheme[, prior.col :=  fifelse(Concept == "L_HEIGHT_COV", "Value", "Date")]
  
  #Some covariates need to be give a sum instead of 1/0. These are extracted by a different query
  scheme <- scheme[ , prior.sum :=  fifelse(Concept %in% NMATCH_SUM , T, F) ]
  #Not sure why the value needs to be extracted but it will not harm so I leave it in.
  scheme <- scheme[prior.sum == T , coll :=  "Value" ]
  
  #Within the calculation/query of the summing the lookback period is relevant. So this needs to be added in contrast to the other concepts in which this can be done later.
 # scheme <- merge(x = scheme, y = unique(LOOKBACK_COV), by.x = "Concept", by.y =  "VarName", all.x = T, allow.cartesian = F)
  #scheme <- scheme[prior.sum == T , lookback := CAT_PRIOR][, CAT_PRIOR := NULL]

  scheme <- scheme[Concept %in% concept, lookback := 10]
  
  COV2 <- lapply(1:nrow(scheme), FUN =  function(i) GetDatesIR(
    Concept = scheme[i,][["Concept"]], 
    Start_date = scheme[i,][["Start_date"]], 
    FILE = scheme[i,][["FILE"]], 
    c.name = scheme[i,][["c.name"]],
    lookback = scheme[i,][["lookback"]],
    prior = scheme[i,][["prior"]],
    post = scheme[i,][["post"]],
    between = scheme[i,][["between"]],
    endpoint = scheme[i,][["endpoint"]],
    coll = scheme[i,][["coll"]],
    prior.col = scheme[i,][["prior.col"]],
    db = dbconcepts,
    keep_start_date = T,
    prior.sum = scheme[i,][["prior.sum"]]
  ))
  
  COV3 <- COV2[[1]][["file1"]]
  
  dbDisconnect(mydb)
  rm(SCRIPT, mydb)
  gc()
  
  # mydb <- dbConnect(RSQLite::SQLite(), dbconcepts_cerebrovascular)
  # dbWriteTable(mydb,name = 'cerebrovasc_cases',value = COV3)
  saveRDS(COV3,paste0(tmp,'cerebrovasc_cases.rds'))
  
  M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort.rds')))
  
  COV4<-COV3
  COV4$Date<-as.Date(COV4$Date,origin='1970-01-01')
  COV4$REFDT<-as.Date(COV4$REFDT,origin='1970-01-01')
  M_Studycohort<-subset(M_Studycohort,M_Studycohort$group!="UNMATCHED")[,c("person_id","id","group","T0")]
  M_Studycohort<-merge(COV4,M_Studycohort,by=c('person_id','id'),all.y=T)
  M_Studycohort$cerebro<-as.numeric(!is.na(M_Studycohort$Date))
  
  tb<-table(M_Studycohort$cerebro,M_Studycohort$group)
  
  resultsCerebrovas <- list(
    'N_CEREBROVASC_AESI - Exposed - N' = tb[2,2],
    'N_CEREBROVASC_AESI - Exposed - PER' =  tb[2,2]/sum(tb[,2]),
    'N_CEREBROVASC_AESI - Control - N' = tb[2,1],
    'N_CEREBROVASC_AESI - Control - PER' = tb[2,1]/sum(tb[,1]),
    'ASD'=ASD.binary(group=M_Studycohort$group,obs=M_Studycohort$cerebro)
  )
  
  resultsCerebrovas2 <- data.table(Result = names(resultsCerebrovas), Value = unlist(resultsCerebrovas))
  
  fwrite(resultsCerebrovas2,paste0(output_dir,'resultCerebrovascular.csv'))
  print('Cerebrobascular results saved!')
}else{
  print('ATTENTION --------- ')
  print(paste0(concept, ' not available'))
  print('ATTENTION ---------')
  
  # 
  # MATCHES <- readRDS(populations_dir,'MATCH_PAIRS.rds')
  # 
  # #Via this variable it is possible to only import needed information. First we did this, but later we wanted to distinct
  # #Between 0 and NA in an automated way. Therefore, this variable is not used (see row 39/40)
  # PERSONS_OF_INTEREST2 <- c(unique(MATCHES[["Exposed"]]),unique(MATCHES[["Control"]]))
  # rm(MATCHES)
  # 
  # dbconcepts_cerebrovascular <- paste0(concepts_dir,'dbConcepts_CERBASC.db')
  # mydb <- dbConnect(RSQLite::SQLite(), dbconcepts_cerebrovascular)
  # 
  # concept <- 'N_CEREBROVASC_AESI'
  # 
  # CombineConceptsFunctions(
  #   t.interest = c("EVENTS"),
  #   additional = T,
  #   #expr = expression( person_id %in% PERSONS_OF_INTEREST2),
  #   expr = NULL,
  #   concepts = concept
  # )
  
  
}


print('EXECUTION FINISHED')





