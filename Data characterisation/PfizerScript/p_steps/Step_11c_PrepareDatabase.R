
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Prepare database and check content of database. Indexes are added and the M_studycohort table is added. Also the Dictionary in p_meta is applied for adding the  
#integer values

##in/output
#Input 1: M_Studycohort.rds
#Input 2: Dictionary.rds
#Input 3: CODES_EVENTS.rds
#Output 1: added tables M_Studycohort and Exposed to database.db and added indexes to all tables in database.db
#Output 2: Dictionary_result.rds (this contains the label per integer value vor categorical study variables)
#(Output 3: parameters.RData. This is an object with some additional environmental variables that can be used later in the script)

SCRIPT <- RUN_SCRIPT(name = "Step_11c_PrepareDatabase")

#Open connection with database
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Put cohort table in database. This table is needed for the T0. Later on this T0 is used as reference date to retrieve the
#covariates and AESI's (step 12 and 13).
###
M_Studycohort <- readRDS(SCRIPT[["INPUT1"]][["path"]])
setnames(M_Studycohort, "group", "S_Group")
dbWriteTable(mydb ,"M_Studycohort",M_Studycohort , overwrite = T, append = F)
rm(M_Studycohort)
###

#Set categorical values to integer values and update the dictionary accordingly
###
#Remove updated dictionary if it was already created in an earlier run. This to make it possible to run this script independently. 
if(file.exists(paste0(tmp,"Dictionary_result.rds"))) file.remove(paste0(tmp,"Dictionary_result.rds"))

  #Get the dictionary that is stored in 00_SetCodesheets. This contains the information needed to translate labels that are
  #dap specific to the labels desired by the study team. 
  Dic <- readRDS(SCRIPT[["INPUT2"]][["path"]])
  #Score <- readRDS(SCRIPT[["INPUT4"]][["path"]])
  
  #Add integer values for categorical concepts that are not in the dictionary or in scoring sheet. 
  ###
  #Get all the available categorical concepts
  to_label <- dbListTables(mydb)[dbListTables(mydb) %in% c(NMATCH_CAT)]
  #In M_Studycohort study variables that are person level information are stored. Also they need to be set to integer if categorical
  to_label <- c(to_label, CAT_PER)
  #It is not needed to relabel variables that will not be a co variate
  to_label <- to_label[to_label %in% COV_CAT]
  
  
  #See which categorical concepts that are available are not specified in the dictionary.
  VarName <- to_label[!to_label %in% unique(Dic[["VarName"]])]# & !to_label %in% unique(Score[["CONCEPT"]])]
  
  #Add those missing concepts to the dictionary
  Dic <-rbindlist(list(Dic, as.data.table(cbind(VarName))), fill = T, use.names = T)
  rm(to_label, VarName)
  ###
  
  ###First the concepts will be updated according the the assigned integers in the dictionary
  #Get concepts that are within the dictionary and in the database and so not a person level variable
  to_update <- unique(Dic$VarName)[!unique(Dic$VarName) %in% CAT_PER]
  to_update <- to_update[to_update %in% dbListTables(mydb)]
  
  #Apply the function. This function gives back a file resembling the dic but with a column action. This is describing if a random integer is assigned or the dic is used
  result <- lapply(to_update, function(x) ApplyDictionary(StudyVar = x, Table = x) )
  meta_info1 <- do.call(rbindlist, list(result, fill = T, use.names = T))
  rm(result, to_update)
  ###
  
  ###Secondly M_Studycohort updated in the same manner as the concepts.
  to_update <- unique(Dic$VarName)[unique(Dic$VarName) %in% c(time_indep_match, time_indep_nmatch)]
  #to_update <- to_update[to_update %in% dbListTables(mydb)]
  
  result <- lapply(to_update, function(x) ApplyDictionary(StudyVar = x, WHEN.col = x, Table = "M_Studycohort"))
  meta_info2 <- do.call(rbindlist, list(result, fill = T, use.names = T))
  rm(result, to_update)
  ###
  
  #Save new dictionary with the logging/actio column 
  meta_info <- rbindlist(list(meta_info1, meta_info2), fill = T, use.names = T)
  saveRDS(meta_info, SCRIPT[["OUTPUT3"]][["path"]])
  rm(meta_info, meta_info1, meta_info2, Dic)
  gc()
###


#Create/prepare a copy of the table with only the exposed subjects for the D2 and D3 cohort.In step 12/13 many queries are done using this table.
#In these queries it is not needed anymore to filter the needed rows. (exposed) Not sure if this is the best approach.
###
DeleteConcepts(db = mydb, concepts = "EXPOSED") 
p <- dbSendStatement(mydb, "CREATE TABLE EXPOSED AS SELECT * FROM M_Studycohort WHERE S_Group = 'EXPOSED' OR S_Group = 'UNMATCHED'")
dbClearResult(p)
###
  

#Set indexes on cohort tables to enhance performance in the queries in step 12/13 
###
indexes_available <- dbGetQuery(mydb,"SELECT name  FROM sqlite_master WHERE type = 'index'")[["name"]]

# Idea for an additiional index, needs to test if its worth
# if(!"M_Studycohort_index1" %in% indexes_available){
#   p <- dbSendStatement(mydb, "CREATE INDEX M_Studycohort_index1 ON M_Studycohort(person_id,T0,op_end_date)")
#   dbClearResult(p)
# }
# 
if(!"EXPOSED_index_V2" %in% indexes_available){
  p <- dbSendStatement(mydb, "CREATE INDEX EXPOSED_index_V2 ON EXPOSED(person_id)")
  dbClearResult(p)
}
###

#Make variables that specify what concepts are available and which not
#Distinct between covariates and AESI's and between missing and empty.
###

#Check AESI's
Available_AESI <- AESI[AESI %in% dbListTables(mydb)]
Missing_AESI <- AESI[!AESI %in% dbListTables(mydb)]

#Give feedback in the console to the user
if(length(Missing_AESI) > 0) print(paste0("AESI's ",paste0(Missing_AESI, collapse = " ")," not available in database"))

#See if there are tables with 0 rows. I think that this will not happen so maybe this step can be removed.
if(length(Available_AESI) > 0){
    Empty_AESI <- as.data.table(t(sapply(Available_AESI , function(x) list(NB = as.integer(dbGetQuery(mydb, paste0("SELECT COUNT(*) FROM ",x))), Cov = x))), keep.rownames = F)[NB == 0,]
    Empty_AESI <- unlist(Empty_AESI[["Cov"]])
    if(length(Empty_AESI) > 0) print(paste0("AESI's ",paste0(Empty_AESI, collapse = " ")," have 0 cases"))
}else{Empty_AESI <- NULL}

#Get all the needed concepts that are need to be extracted form the database in step 12/13
COV_check <- unique(c(COV[!COV %in% c(MATCH_WEIGHT, NMATCH_WEIGHT, time_indep_nmatch, time_indep_match, NMATCH_FUNPOST, MATCH_FUNPOST)], 
                      MATCH_ALG_CONCEPTS, NMATCH_ALG_CONCEPTS, NMATCH_SUM))

#Relate this to what is available in the database
Available_cov <- COV_check[COV_check %in% dbListTables(mydb)]
Missing_cov <- COV_check[!COV_check %in% dbListTables(mydb)]

#Feed back to the user what concepts are missing so they can check if this can be correct and if it is not correct fix using instructions in documentation
if(length(Missing_cov) > 0) print(paste0("Concepts ",paste0(Missing_cov, collapse = " ")," not available in database. These concepts are needed for the creation of the covariates"))

#See if there are tables with 0 rows. I think that this will not happen so maybe this step can be removed.
if(length(Available_cov) > 0){
    Empty_cov <- as.data.table(t(sapply(Available_cov , function(x) list(NB = as.integer(dbGetQuery(mydb, paste0("SELECT COUNT(*) FROM ",x))), Cov = x))), keep.rownames = F)[NB == 0,]
    Empty_cov <- unlist(Empty_cov[["Cov"]])
    if(length(Empty_cov) > 0) print(paste0("Covariates ",paste0(Empty_cov, collapse = " ")," have 0 cases"))
}else{Empty_cov <- NULL}

###

#Prepare all the concept tables by deleting duplicate rows and adding indexes. This to enhnace performence in step 12 and 13.
if(length(c(Available_AESI, Available_cov)) > 0){
  for(i in unique(c(Available_AESI, Available_cov))){
    
    #Remove duplicates
    p <- dbSendStatement(mydb,
                         paste0(
                           "DELETE FROM ",i,"
                            WHERE rowid NOT IN (select min(rowid)
                            FROM ",i,"
                            group by ",paste0(dbListFields(mydb, i), collapse = " , "),")
                            "                    
                         )
                         
                         )
    dbClearResult(p)
    
    # #To add indexes dynamically get all the columns.
    # cols_tmp <- paste0(dbListFields(mydb,i), collapse = ",")
    # 
    # #Only add the indexes if this is not done in a previous run. This if the script is reran.
    # if(!paste0(i,"_index") %in% indexes_available){
    # #Add indexes to all columns. Not sure what is optimal in terms of which columns to index and if the order of indexing matters.
    # 
    #   #p <- dbSendStatement(mydb, paste0("CREATE INDEX ",i,"_index ON ",i,"(",cols_tmp,")"))
    #   dbClearResult(p)
    #   
    # }
    
    # Get all the columns of the table
    table_columns <- dbListFields(mydb,i)
    
    # Check if both 'person_id' and 'Date' columns exist in the table
    if ("person_id" %in% table_columns && "Date" %in% table_columns) {
      # Create the index for most concepts
      # Check if the index hasn't been added in a previous run
      if (!paste0(i, "_index") %in% indexes_available) {
        p <- dbSendStatement(mydb, paste0("CREATE INDEX ", i, "_index_V2 ON ", i, "(person_id, Date)"))
        dbClearResult(p)
      } else { ("person_id" %in% table_columns) 
        # Create the index for non starndart concepts
          if (!paste0(i, "_index") %in% indexes_available) {
          p <- dbSendStatement(mydb, paste0("CREATE INDEX ", i, "_index_V2b ON ", i, "(person_id)"))
          dbClearResult(p)
          }
        }
    }
    
    
  }
}



dbDisconnect(mydb)

save(AESI, COV_check, Available_AESI, Available_cov, Missing_AESI, Missing_cov, Empty_AESI, Empty_cov, file = store_dir)

rm(SCRIPT, mydb, p, Available_AESI, Available_cov, Missing_AESI, Missing_cov, Empty_AESI, Empty_cov)
gc()



