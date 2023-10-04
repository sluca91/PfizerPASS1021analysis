SCRIPT <- RUN_SCRIPT(name = "Step_12_AddCoVariates.R")

#Get the 
load(file = store_dir)
##########

#if(length(Available_cov) > 0){
  
  #Create data frame with all concepts and there settings for the function that creates the queries. (GetDatesIR) Every row in the scheme contains the settings
  #for the query
  ###
  
  #It is only needed to extract the available concepts. An extra variable is used for if things need to be added.
  cols_scheme <- Available_cov
  
  #Build the framework for the scheme based on the needed covariates for the T0, D2 and D3 cohort.
  scheme <- as.data.table(rbind(
    cbind(Concept = cols_scheme, FILE =rep("M_Studycohort",length(cols_scheme)),Start_date =rep("T0",length(cols_scheme)),  type = rep("COV",length(cols_scheme))),            
    cbind(Concept = cols_scheme, FILE =rep("EXPOSED",length(cols_scheme)),Start_date =rep("SECOND_PFIZER",length(cols_scheme)), type = rep("COV",length(cols_scheme))),
    cbind(Concept = cols_scheme, FILE =rep("EXPOSED",length(cols_scheme)),Start_date =rep("THIRD_PFIZER",length(cols_scheme)),  type = rep("COV",length(cols_scheme)))
  ))
  
  rm(cols_scheme)
  
  #Create the column names that will be used in the wide format end table's. There will be 3 separate tables (T0, D2, D3) with a column per covariate.
  ###
  scheme <- scheme[Start_date == "T0" & FILE == "M_Studycohort", c.name := paste0(Concept,"_T0")]
  scheme <- scheme[Start_date == "SECOND_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D2")]
  scheme <- scheme[Start_date == "THIRD_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D3")]
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
  scheme <- merge(x = scheme, y = unique(LOOKBACK_COV), by.x = "Concept", by.y =  "VarName", all.x = T, allow.cartesian = F)
  scheme <- scheme[prior.sum == T , lookback := CAT_PRIOR][, CAT_PRIOR := NULL]
  
  #HARDCODED: for some concepts the unit is relevant. This is stored in the Voc column. An alternative manner of doing this is making study variables per unit. 
  #So by example L_WEIGHT_COV_KG/L_WEIGHT_COV_G. Then those can be included in the algorithm.
  #Note that the first in the vector must contain the value. If you make it Unit,Value it goes wrong later in the script
  scheme <- scheme[Concept %in% c("L_BMI_COV","L_WEIGHT_COV","L_HEIGHT_COV"), coll := "Value,Unit"  ]
  
 


#"Concept"    "FILE"       "Start_date" "type"       "c.name"     "lookback"   "coll"       "post"       "endpoint"   "between"    "prior"     
#"prior.col"  "prior.sum"  "PRIOR_UNIT" "DAYS"

#unique(scheme[,.(coll, post, endpoint, between, prior, prior.col, prior.sum)])

#start new code. Only for simple binary  to start with. This are most of the concepts.  
###
  source(paste0(pre_dir,"functions/", "GetDatesIR2.R"))
#First we need to make a new data model. 
###
  
dbconcepts2 <- "C:/Users/relbers/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/g_intermediate/concepts/concepts_new.db"
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts2)

p <- dbSendStatement(mydb, paste0("ATTACH DATABASE '",dbconcepts,"' AS tempdb"))
dbClearResult(p)

p <- dbSendStatement(mydb, "
          
           DROP TABLE IF EXISTS cohort
           
           ")

dbClearResult(p)

p <- dbSendStatement(mydb, "
          
           CREATE TABLE cohort AS
           
           SELECT person_id, id, T0 AS ref_date, 'T0' AS cohort FROM tempdb.M_StudyCohort 
           
           UNION
           
           SELECT person_id, id, SECOND_PFIZER AS ref_date, 'D2' AS cohort FROM tempdb.Exposed WHERE SECOND_PFIZER IS NOT NULL 
           
           UNION
           
           SELECT person_id, id, THIRD_PFIZER AS ref_date, 'D3' AS cohort FROM tempdb.Exposed WHERE THIRD_PFIZER IS NOT NULL 
           
           ")

dbClearResult(p)

dbReadTable(mydb, "cohort")

neededTF <- scheme[
                    coll == "" &
                    post == F &
                    between == F &
                    prior == T &
                    prior.sum == FALSE &
                    prior.col == "Date",
                    ][["Concept"]]


unionQuery <- paste0(paste0("SELECT person_id, Date, '",neededTF,"' AS concept  FROM tempdb.", neededTF), collapse = " UNION ")

p <- dbSendStatement(mydb, " DROP TABLE IF EXISTS concepts_tf ")
dbClearResult(p)

p <-  dbSendStatement(mydb, paste0("CREATE TABLE concepts_tf AS ", unionQuery))
dbClearResult(p)
dbReadTable(mydb, "concepts_tf")

###

#Then the query as ran previously needs to be adapted.GetDatesIR2!!
###


fileresult <-   GetDatesIR2(
  Concept = "concepts_tf", 
  Start_date = "ref_date", 
  FILE = "cohort", 
  c.name = scheme[i,][["c.name"]],
  lookback = 999,
  prior = T,
  post = F,
  between = F,
  endpoint = " ref_date + (999 * 365) ",
  coll = "",
  prior.col = "Date",
  db = dbconcepts2,
  keep_start_date = T,
  prior.sum = F)[["file1"]]



fileresult <- merge(x = fileresult, y = unique(LOOKBACK_COV[,.(VarName, DAYS)]), by.x = "concept", by.y =  "VarName", all.x = T, allow.cartesian = F)
fileresult <- fileresult[!is.na(REFDT) & !is.na(Date) , Check := (as.numeric(REFDT) - as.numeric(Date)) ]
fileresult <-fileresult[Check <= DAYS | (is.na(DAYS) |  is.na(Date)), ]

#Set TF vars to T
###
fileresult <- fileresult[concept %in% c(COV_TF) , ][, Result := 1]


#mydb2 <- dbConnect(RSQLite::SQLite(), dbconcepts)
#aatest <- AppendConcepts(mydb2, neededTF, NAME = "concepts_tf", colls = "person_id, Date ")
#dbDisconnect(mydb2)


dbDisconnect(mydb)


#COV2[, col := paste0(concept, "_", cohort)]

#Make a wide with per person_id and match pair id only 1 row with all the covariates
if(nrow(fileresult) > 0){
  resultTF <- data.table::dcast(fileresult , person_id + id + cohort ~ concept , value.var = "Result", fill = 0)
}else{resultTF <- fileresult[, .(person_id, id, cohort)]}






















