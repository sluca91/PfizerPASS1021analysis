#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Combine simple concepts to new concepts that are calculated using the simple concepts  

##in/output
#Input 1: PERSONS3.rds
#Input 2:COVID19DX.rds
#Input 3: New concepts in database.db

SCRIPT <- RUN_SCRIPT(name = "Step_11b_CreateAdditionalConcepts")

mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Simple algorithms with only OR
###


#Delete the tables that are already there to make it possible to rerun the script independently. Note that vaccines are already in so they need to be excluded form the
#tables to delete.
DeleteConcepts(concepts = NMATCH_OR )

ALG <- readRDS(SCRIPT[["INPUT3"]][["path"]])

#Per OR algorithm
for(i in NMATCH_OR ){
  
  #Get the concpets that need to be appended to create the new concepts. This varaible is the input for the AppendConcepts function
  to_append <- unique(ALG[NEW_CONCEPT == i,]$CONCEPT)
  
  #Create the new table in an R object.
  TEMP <- AppendConcepts(DB = mydb, CONCEPTS = to_append, NAME = i)
  
  #Write to the database
  if(!is.null(TEMP)){
    dbWriteTable(mydb, i, TEMP)
  }
  
  rm(TEMP, to_append)
  gc()
  
}

rm(ALG)

###


#Create death
###

#Death date is retrieved from the persons table and stored as DEATHANY.
DEATH <- unique(readRDS(SCRIPT[["INPUT1"]][["path"]])[,.(person_id, death_date)])[!is.na(death_date),]
setnames(DEATH, "death_date", "Date")

dbWriteTable(mydb ,"O_DEATHANY_AESI",DEATH , overwrite = T, append = F)

rm(DEATH)
gc()

###


#TTS
###

if(!any(!c("B_SPLACHNICVT_AESI", 
           "B_TP_AESI", 
           "N_CVST_AESI", 
           "V_OTHERVTE_AESI", 
           "N_STROKEISCH_AESI",
           "R_PE_AESI",
           "C_AMI_AESI",
           "V_DVT_AESI") %in% dbListTables(mydb))){

#Delete if already there to make it possible to rerun the script  
DeleteConcepts(concepts = "B_TTS_AESI")  
  
#Create TTS (if multiple of this sort ask Albert for function for this)
###
p <- dbSendStatement(mydb,
                     
                     "
 
                      CREATE TABLE B_TTS_AESI AS
                      
                      SELECT DISTINCT 
                                  person_id,
                                  MIN(DateNew) as Date
                      
                                  FROM (
                      
                      SELECT *,
                      CASE 
                      	 WHEN DateTP < DateOR THEN DateTP
                           ELSE DateOR 
                      	 END as DateNew
                      	 
                      	 FROM(
                      	
                      
                        SELECT DISTINCT
                           t1.person_id,
                           t1.Date as DateTP,
                      	   t2.Date as DateOR
                      	 
                        
                      	 FROM B_TP_AESI t1
                      	 
                      	 LEFT JOIN (
                      
                      			SELECT DISTINCT *  FROM B_SPLACHNICVT_AESI
                      			UNION ALL
                      			SELECT DISTINCT *  FROM N_CVST_AESI
                      			UNION ALL
                      			SELECT DISTINCT *  FROM V_OTHERVTE_AESI
                      			UNION ALL
                      			SELECT DISTINCT *  FROM N_STROKEISCH_AESI
                      			UNION ALL
                      			SELECT DISTINCT *  FROM R_PE_AESI
                      			UNION ALL
                      			SELECT DISTINCT *  FROM C_AMI_AESI
                      			UNION ALL
                      			SELECT DISTINCT *  FROM V_DVT_AESI
                      
                      	) t2 ON (t1.person_id = t2.person_id 
                      	     AND (t2.Date BETWEEN (t1.Date - 10) AND (t1.Date + 10)))
                      
                      	WHERE 
                      	NOT DateTP IS NULL AND
                        NOT DateOR IS NULL 
                      )
                      
                      )
                      
                      GROUP BY person_id, DateTP
                      	
                      
                     
                     "           
                     
                     
)

dbClearResult(p)


}else{"Not all tables for TTS are available in database so TTS is not created"}

###

#Create smoke status
###
##AIM: from the CDM 3 types of smoking status variables are coming in. Therefore, these are combined to 1 in this script.
#First I wanted to make FUNCPOST of it like bmi. However, If we want to remove contrasting historical information we cannot do this with the FUNCPOST.
#What to do if the most recent records says never while an earier records says current or former?

if(any(c("L_SMOKESTATUS_COV", "L_SMOKESTATUSCAT_COV", "L_SMOKESTATUSCIGD_COV") %in% dbListTables(mydb))){

#The value to search for in the dictionary.
refValue = "current"

#Check if refValue is defined in the dictionary
refAvailable <- readRDS(paste0(tmp, "Dictionary.rds"))[VarName == 'L_SMOKESTATUSALG_COV' & DAP_NAME == DAP & category == refValue,][,c("oriVal"), with = F]

#Create the value that needs to be added instead of a code or the number of cigarettes per day.
if(length(refAvailable) > 0){value <- refAvailable[1]}else{
  value <- refValue
  warning(paste0("Dictionary does not contain refValue for smoking status. Check dictionary for availability of '",refValue, "' in the column category"))
}

#Get the components and put them in 1 table. If L_SMOKESTATUSCAT_COV would be a covariate of it's own this step may go wrong if rerunning. (Value_original is then unknown)  
newConcept <- AppendConcepts(DB = mydb, 
                             CONCEPTS = c("L_SMOKESTATUS_COV", "L_SMOKESTATUSCAT_COV", "L_SMOKESTATUSCIGD_COV"), 
                             NAME = "L_SMOKESTATUSALG_COV",
                             colls = "person_id, Outcome, Value, Voc, Date"
)

#Remove if cigarettes per day is 0. This is equal to not having a code. Check with table shells how 0 is handled in comparison with never 
newConcept <- newConcept[Outcome == "L_SMOKESTATUSCIGD_COV" , cigd := Value][,cigd := as.numeric(replace(cigd, is.na(cigd), "0"))]
newConcept <- newConcept[(Outcome == "L_SMOKESTATUSCIGD_COV" & cigd > 0) | Outcome != "L_SMOKESTATUSCIGD_COV",][, cigd := NULL]

#Rename the value for the not categorical components
newConcept[Outcome %in% c("L_SMOKESTATUS_COV", "L_SMOKESTATUSCIGD_COV") , Value := value]


#Write new concept to database.
dbWriteTable(mydb, "L_SMOKESTATUSALG_COV", newConcept, overwrite = T)


rm(refValue, refAvailable, value, newConcept)
gc()

}else{"No inputs for L_SMOKESTATUSALG_COV"}

###

dbDisconnect(mydb)

rm(mydb)
