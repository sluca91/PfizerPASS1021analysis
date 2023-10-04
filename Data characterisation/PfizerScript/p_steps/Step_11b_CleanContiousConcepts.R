#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 19/04/2023

##Aim
#The CDM does not capture information concerning formatting. Therefore, numeric information as in continuous variables are loaded in character format.
#Moreover, information concerning the units is not always available and the used units may be different among the databases or the unit is unknown.
#The aim of this step is to standardize and clean these variables before they are used.

#Input 1: Ranges.rds
#Input 2: PERSONS.rds, this is needed for the birth dates of all the subjects (we did load concepts for all subjects while before interim 4 we did only load the concepts 
#         for the subjects that where in the population. This to make an automated distinction between 0 and NA)
#Input 3: concepts from the concepts.db
#Output 1: cleaned concepts and with an added column unit

#Set this to TRUE if you would like to restore the situation of a previous run. So the cleaning is then undone and redone. If false the cleaned table is cleaned again.
restoreOriginal <- T

SCRIPT <- RUN_SCRIPT(name = "Step_11b_CleanContiousConcepts.R")

#Connect to database
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Extent database with needed information
dbWriteTable(mydb, "units", readRDS(file = SCRIPT[["INPUT1"]][["path"]]), overwrite = T)
dbWriteTable(mydb, name = "personstemp", value = readRDS(file = SCRIPT[["INPUT2"]][["path"]])[,.(person_id, birth_date)], overwrite = TRUE)


concepts <- unique(dbReadTable(mydb, "units")[["StudyVar"]])
concepts <- concepts[concepts %in% dbListTables(mydb)]

#i <- "L_SMOKESTATUSCIGD_COV"
#i <- "L_HEIGHT_COV" 

for(i in concepts){
    
    #Get the units that are defined in the metadata layer for this specific study variable
    unitsVar <-  unique(as.data.table(dbReadTable(mydb, "units"))[StudyVar == i,][["UNIT"]])     
    
    #Make a empty object to append SQL code to
    renameQuery <- NULL
    
    #Append 
    for(j in 1:length(supportedUnits)){
        #Extract the unit name on the list. This cannot be done via names() because an object name can not handle sign like by example / or ^
        unitStandardised <-  supportedUnits[[j]][1]
        
        #If a list of synonyms is available for a certain unit a case when statement is created to set to a standardized unit
        if(unitStandardised %in% unitsVar){
            renameQuery <- append(renameQuery,
                                  paste0("WHEN UPPER(unit_extracted) IN (",paste0(paste0("'",toupper(supportedUnits[[j]]),"'"), collapse = ","),") THEN '",unitStandardised,"'")
            )
        }
        
        
    }
    
    #Finish the statement.
    if(!is.null(renameQuery)) renameQuery <- glue(paste0(", CASE ",paste0(renameQuery, collapse = " "), " ELSE NULL END AS unit_extracted_standardized "))
    
    
    #Write the total query that joins the ranges to the data and adds column that may be needed for the future.
    queryUnits <- glue(paste0(
                 "
                  
                  CREATE TABLE ",i,"_RANGES AS
                  
                  SELECT * ",renameQuery,", 
                  cast((Value / FACTOR) AS 'TEXT') AS Value_new,
                  UNIT_FROM AS Unit_new
                  
                  FROM(
                  
                  SELECT 
                      person_id,
                      Outcome,
                      Value,
                      Voc,
                      Date,
                      unit_estimated,
                      CASE
                      WHEN INSTR(unit_found, '|') > 0 THEN SUBSTR(unit_found, 1, INSTR(unit_found, '|') - 1)
                      ELSE unit_found
                      END AS unit_extracted,
                      age,
                      FACTOR,
                      UNIT_FROM
                  
                  FROM(
                    
                      SELECT 
                      t1.*,
                      t2.UNIT AS unit_estimated,
                      t2.FACTOR,
                      t2.UNIT_FROM,
                      CASE WHEN INSTR(t1.Voc, 'unit.') > 0 THEN SUBSTR(t1.Voc, INSTR(t1.Voc, 'unit.') + LENGTH('unit.')) 
                      ELSE NULL
                      END AS unit_found
                      
                      FROM (SELECT  t2.person_id, t2.Outcome, t2.Value, t2.Voc, t2.Date , ROUND((t2.Date - t1.birth_date)/365.25) AS age  FROM ",i," t2 LEFT JOIN personstemp t1 ON(t1.person_id = t2.person_id)) t1
                      
                      LEFT JOIN units t2 ON(t1.Outcome = t2.StudyVar AND t1.Value BETWEEN t2.MIN AND t2.MAX AND age BETWEEN MIN_AGE AND MAX_AGE)
                  )
                  
                  )
                  "
    
    ))
    
    #If you want to restore the situation of a previous run the cleaned table is removed and restored by the stored original information. Then a rerun gives an identical result.
    if(restoreOriginal){ 
        if(paste0(i,"_RANGES") %in% dbListTables(mydb)){
             p <-  dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS ",i))
             dbClearResult(p)
             
             p <- dbSendQuery(mydb, paste0("CREATE TABLE ",i," AS SELECT person_id, Outcome, Value, Voc, Date FROM ",i,"_RANGES"))
             dbClearResult(p)
         } 
    }
    
    #First delete an old version of the table for if the script is reran
    p <-  dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS ",i,"_RANGES"))
    dbClearResult(p)
    
    #Run the query to create a side table
    p <-  dbSendStatement(mydb, queryUnits)
    dbClearResult(p)
    
    #dbReadTable(mydb, "L_HEIGHT_COV_RANGES")
    
    #For now we only continue with the estimated unit based on the ranges as was done in AZ. Count how many rows that are going to be deleted because the value falls not in a range
    countExcluded <- dbGetQuery(mydb, paste0("SELECT COUNT(*) AS count FROM ",i,"_RANGES WHERE unit_estimated IS NULL"))[["count"]]
    
    #Give feedback about the number of invalid values
    if(countExcluded > 0) warning(paste0(countExcluded," rows are deleted from the concept table for ",i, " because the values do not fall in the correct range or are not numeric"))
    
    #Overwrite the old concpet table with the cleaned table with an added column unit.
    ###
    p <-  dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS ",i,""))
    dbClearResult(p)
    
    p <- dbSendQuery(mydb, paste0("CREATE TABLE ",i," AS SELECT person_id, Outcome, Value_new AS Value, Voc, Date, Unit_new AS Unit FROM ",i,"_RANGES WHERE unit_estimated NOT NULL"))
    dbClearResult(p)
    ###
    rm(queryUnits, unitsVar, renameQuery, countExcluded)
    
    }

dbRemoveTable(mydb, "personstemp")

dbDisconnect(mydb)


###









