
#AIM: from the CDM 3 types of smoking status variables are coming in. Therefore, these are combined to 1 in this script.

#The value to search for in the dictionary.
refValue = "current"

#Check if refValue is defined in the dictionary
refAvailable <- readRDS(paste0(tmp, "Dictionary.rds"))[VarName == 'L_SMOKESTATUSCAT_COV' & DAP_NAME == DAP & category == refValue,][,c("oriVal"), with = F]

#Create the value that needs to be added instead of a code or the number of cigarettes per day.
if(length(refAvailable) > 0){value <- refAvailable[1]}else{
  value <- refValue
  warning(paste0("Dictionary does not contain refValue for smoking status. Check dictionary for availability of '",refValue, "' in the column category"))
}

#Later in the program integers are added

table <- "L_SMOKESTATUSCAT_COV"
if("Value_original" %in% dbListFields(mydb, table)){

#Later in the process indexes are created. If an index on a column it cannot be deleted
p <- dbSendStatement(mydb, paste0("DROP INDEX IF EXISTS ",table,"_index")) 
dbClearResult(p)

#remove the value which was already set to integer
p <- dbSendStatement(mydb, paste0("ALTER TABLE ",table," DROP Value")) 
dbClearResult(p)

#make a new value column to restore the original value
p <- dbSendStatement(mydb, paste0("ALTER TABLE ",table," ADD Value TEXT")) 
dbClearResult(p)

#fill the new value column with the original value
p <- dbSendStatement(mydb, paste0("UPDATE ",table," SET Value = Value_original")) 
dbClearResult(p)

p <- dbSendStatement(mydb, paste0("ALTER TABLE ",table," DROP Value_original")) 
dbClearResult(p)
}


DeleteConcepts(concepts = "L_SMOKESTATUSALG_COV" )

newConcept <- AppendConcepts(DB = mydb, 
                       CONCEPTS = c("L_SMOKESTATUS_COV", "L_SMOKESTATUSCAT_COV", "L_SMOKESTATUSCIGD_COV"), 
                       NAME = "L_SMOKESTATUSALG_COV", 
                       colls = "person_id, Outcome, Value, Voc, Date"
                       )


newConcept[Outcome == "L_SMOKESTATUSCIGD_COV" & Value != "0", Value := refValue]
newConcept[Outcome == "L_SMOKESTATUS_COV" , Value := refValue]
newConcept <- newConcept[Value != "0",]

dbWriteTable(mydb, "L_SMOKESTATUSALG_COV", newConcept)

  
rm(refValue, refAvailable, value, table, newConcept, p)
gc()

