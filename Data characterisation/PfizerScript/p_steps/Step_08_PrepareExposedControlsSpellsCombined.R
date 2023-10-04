
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/05/2022

##Aim
#Create 2 files, Exposed and Controls that are prepared to join by matching criteria and T0 between a start and end date. Therfore, the files stored under the names 
#in the variable time_dep_match and the file OBS need to be merged to 1 spell with a start and end date 

##in/output
#Input 1: PERSONS3.rds
#Input 2: OBS_SPELL1.rds
#Input 3-8: REG_SPELL.rds CDC_SPELL.rds COV_SPELL.rds INF_SPELL.rds IMC_SPELL.rds PREG_SPELL.rds

#Output 1: Exposed.db
#Output 2: Controls.db
#Output 3: DIC_PER.rds
#Output 4: DIC_REG.rds


SCRIPT <- RUN_SCRIPT(name = "Step_08_PrepareExposedControlsSpellsCombined.R")

#files <- c("INF", "COV", "REG", "IMC", "PREG", "CDC")
files <- time_dep_match[sapply(time_dep_match, function(x) file.exists(paste0(matching_dir, x, "_SPELLS.rds")))]

### Loading Persons03
PER <- readRDS(SCRIPT[["INPUT1"]][["path"]])

#[,  `:=` (month_t0 = NULL, death_date = NULL, SECOND_PFIZER = NULL, THIRD_PFIZER = NULL, SECOND_OTHER = NULL, THIRD_OTHER = NULL)]
lapply(c("month_t0", cols_vac_new[substr(cols_vac_new, 1, 5) != "FIRST"] ), function(x) PER <- PER[, eval(x) := NULL])

#Setting the per person_id and sex as integer and saving it into a dictionary
PER <- SetToInteger(PER, c("person_id", "sex_at_instance_creation"))
Dictionary <- PER$Dictionary
saveRDS(Dictionary, SCRIPT[["OUTPUT1"]][["path"]])


PER <- PER$Data


### Setting the IDs of the Observation Periods (OBS) to integer, as done to Persons Table
#Steps:
# IDs edited based on the dictionary defined after setting to integer the 'person_id' from the PER (persons) table
OBS <- RenameId(readRDS(SCRIPT[["INPUT2"]][["path"]]), Dictionary = Dictionary, colls = "person_id", AddId = T)[, num_spell := NULL]



### Setting the IDs of the time dependent matching variables (time_dep_match, now known as files) to integer, as done to Persons Table
#Steps:
# Files are loaded, 
# IDs edited based on the dictionary defined after setting to integer the 'person_id' from the PER (persons) table
# Edited file is assigned to a new variable named as the original file
#Write to SQLight database


if(file.exists(dbmatching)) file.remove(dbmatching)
#opening a SQLite database
mydb <- dbConnect(RSQLite::SQLite(), dbmatching)


#i=x[4]

x <- c(files, "PER", "OBS")
for(i in x){
  
  if(i %in% files) TEMP <- RenameId(readRDS(paste0(matching_dir,i,"_SPELLS.rds")), Dictionary = Dictionary, colls = "person_id", AddId = T)
  if(i == "PER"){
                TEMP <- PER
                rm(PER)
                }
  if(i == "OBS"){
                TEMP <- OBS
                rm(OBS)
              }
    
  if(i %in% MATCH_CAT & class(TEMP[[i]]) == "character"){ 
        TEMP <- SetToInteger(TEMP, i)
        Dictionary_TEMP <- TEMP$Dictionary
        saveRDS(Dictionary_TEMP, paste0(matching_dir,"DIC_",i,".rds"))
        rm(Dictionary_TEMP)
        
        TEMP <- TEMP$Data
  }
        
        colls <-colnames(TEMP)
        f.types <- rep("INT", length(colls))
        names(f.types) <- colls
        dbWriteTable(mydb, i ,TEMP, overwrite = T, append = F, field.types = f.types) 
        
        rm(TEMP, colls, f.types)
        gc()
        
  
  
  
  
  
}

rm(Dictionary)
rm(x)


dbListTables(mydb)




seqnb <- 2:length(files) #Index kilst of time dependent matching variables

#Generating a query for selecting columns ST and EN from each of the tables
# Example:
# When 2, we are using file CDC then the query is
# t2.CDC, t2.ST AS ST2, t2.EN AS EN2
# THis will be used with a SELECT statement later on and it is needed so we give a unique name to each column of each variable
CODE_SELECT <- paste0(
  
  "
  t",seqnb,".",files[seqnb],",
  t",seqnb,".ST AS ST",seqnb,",
  t",seqnb,".EN AS EN",seqnb,"
  "
  , collapse = ","
)

x <- 1:length(files)
#CODE_MAX and CODE_MIN are strings withthe ST (start) and EN (end) dates for each of the time dependent matching variables
# Example of CODE_MIN
# "t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN"

CODE_MAX <- paste0("t",x,".ST", collapse = ",")
CODE_MIN <- paste0("t",x,".EN", collapse = ",")

CODE_JOIN <- paste0(
  
  "
            INNER JOIN ",files[seqnb]," t",seqnb," ON(
            
            t1.person_id = t",seqnb,".person_id AND
            
            (
            t",seqnb,".ST BETWEEN max(",CODE_MAX,") AND min(",CODE_MIN,") OR
            t",seqnb,".EN BETWEEN max(",CODE_MAX,") AND min(",CODE_MIN,") OR
            (t",seqnb,".ST  < max(",CODE_MAX,") AND t",seqnb,".EN  > min(",CODE_MIN,"))
            )
            
            
            )
  
  
  "
  , collapse = " "
)

#Example of CODE_JOIN
#   INNER JOIN CDC t2 
#   ON  t1.person_id = t2.person_id 
#   AND (t2.ST BETWEEN max(t1.ST,t2.ST,t3.ST,t4.ST,t5.ST,t6.ST) AND min(t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN) 
#   OR t2.EN BETWEEN max(t1.ST,t2.ST,t3.ST,t4.ST,t5.ST,t6.ST) AND min(t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN) 
#   OR (t2.ST < max(t1.ST,t2.ST,t3.ST,t4.ST,t5.ST,t6.ST) AND t2.EN > min(t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN))))"



#The following statement inner joins the Region rows with each of the time dependent matching variables where
# ID of the person is the same and:
# the start of the matching variable (e.g. CDC) is between the max of all the starts and the minimum of all the ends (including region t1)
# or the end of the matching variable is between the max of all the starts and between the minimum of all the ends
# or the start of the matching variable is before than the max of all the starts of all matching variables and the end bigger than the min of all ends of all thematching variables

#This is done for all the matching variables

p <- dbSendStatement(mydb,
  
  paste0(
    " 
      CREATE TABLE TEMP1 AS
      SELECT
      t1.*,
      ",CODE_SELECT,",
      ROW_NUMBER() OVER() tmp
      
      
      FROM ",files[1]," t1
      
            ",CODE_JOIN,"
      
      
      
      "))
dbClearResult(p)

CODE_MAX2 <- paste("ST, ", paste0(" ST",seqnb, collapse = ","))
#Example "ST,   ST2, ST3, ST4, ST5, ST6"
CODE_MIN2 <- paste("EN, ", paste0(" EN",seqnb, collapse = ","))
#Example #"EN,   EN2, EN3, EN4, EN5, EN6"
CODE_GROUP <- paste0(files, collapse = ",")
#exampple #"REG,CDC,COV,INF,IMC,PREG"

#test <- as.data.table(test)[, tmp := seq_len(.N)]


#In the statement below, from all the starts and ends that are joined in the 
#previous statement (resulting in TEMP1) we are selecting the minimun start and 
#the max end between all the ST and EN columns. This results in ONE spell each
# status of a person. Every status is defined by a combination of time dependent variables
p <- dbSendStatement(mydb,
  
  paste0(
    "       CREATE TABLE TEMP2 AS
             SELECT DISTINCT * FROM(
              SELECT DISTINCT
              person_id,
              max(",CODE_MAX2,") AS ST,
              min(",CODE_MIN2,") AS EN,
              ",CODE_GROUP,"
              
              FROM TEMP1
              
              GROUP BY tmp
             )
             WHERE EN - ST >= 0
              
            
              "
    
  ))

dbClearResult(p)
p <- dbSendStatement(mydb, paste0("CREATE INDEX PER_index_V2 ON PER(person_id)"))
dbClearResult(p)
p <- dbSendStatement(mydb, paste0("CREATE INDEX OBS_index_V2 ON OBS(person_id)"))
dbClearResult(p)
#test2 <- merge(as.data.table(test2), PER[, .(person_id, sex_at_instance_creation, YEAR_BIRTH, FIRST_COV_INF, FIRST_PFIZER, FIRST_OTHER)], by.x = "ID", by.y = "person_id", all.x = T )


#In the statement below we are including all the time-independent matching variables
# information into to the result of the TEMP2 (status spells)

p <- dbSendStatement(mydb,
  
  "
  CREATE TABLE TEMP3 AS
  SELECT DISTINCT
  t1.*,
  t2.sex_at_instance_creation, 
  t2.YEAR_BIRTH, 
  t2.FIRST_PFIZER, 
  t2.FIRST_OTHER
  
  
  FROM TEMP2 t1
  
  LEFT JOIN PER t2 ON(t1.person_id = t2.person_id)
  
  "
  
  
)

#Finally we divide the spells of the controls and exmposed into two different tables
#Creation of the exposed and control files for the matching 

dbClearResult(p)

p <- dbSendStatement(mydb,
  
  "
  CREATE TABLE Controls AS
  SELECT DISTINCT
  t1.*,
  t2.op_start_date, 
  t2.op_end_date,
  t2.Used_spell_exposed,
  
  CASE
      WHEN t1.FIRST_PFIZER IS NULL THEN t1.FIRST_OTHER
      ELSE t1.FIRST_PFIZER
  END AS VAC_DATE1,
  
  max(t2.op_start_date, t1.ST) AS ST2,
  min(t2.op_end_date, t1.EN) AS EN2
  
  
  FROM TEMP3 t1
  
  INNER JOIN OBS t2 ON(t1.person_id = t2.person_id AND t2.Used_spell_exposed = FALSE AND 
  
            (
            t1.ST BETWEEN t2.op_start_date AND t2.op_end_date OR
            t1.EN BETWEEN t2.op_start_date AND t2.op_end_date OR
            (t1.ST  < t2.op_start_date AND t1.EN  > t2.op_end_date)
            )
  
  
  )
  
  
  
  "
  
  
)

dbClearResult(p)

p <- dbSendStatement(mydb,
  
  "
  CREATE TABLE Exposed AS
  SELECT DISTINCT  
  t1.*,
  t2.op_start_date, 
  t2.op_end_date,
  t2.Used_spell_exposed
  
  FROM TEMP3 t1
  
  INNER JOIN OBS t2 ON(
                      t1.person_id = t2.person_id AND 
                      t2.Used_spell_exposed = TRUE AND 
                      t1.FIRST_PFIZER IS NOT NULL AND 
                      t1.FIRST_PFIZER BETWEEN t1.ST AND t1.EN AND 
                      t1.FIRST_PFIZER BETWEEN t2.op_start_date AND t2.op_end_date
                      
                      ) 
  
  
  
  
  
  
  "
  
  
)

dbClearResult(p)

#Deleting abundant columns. Because older version s of sqlite the drop columns statements needs to be replaced by select,  drop rename if version is lower then.
###

version <- dbGetQuery(mydb, "select sqlite_version();")
version <- stringr::str_split(string = as.character(version), pat = "[.]", simplify = F)
if(version[[1]][1] > 3 | (version[[1]][1] == 3 & version[[1]][2] > 35)){

        p <- dbSendStatement(mydb, "ALTER TABLE Controls DROP COLUMN FIRST_OTHER")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Controls DROP COLUMN Used_spell_exposed")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Controls DROP COLUMN ST")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Controls DROP COLUMN EN")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Controls DROP COLUMN op_start_date")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Controls DROP COLUMN op_end_date")
        dbClearResult(p)
        
        
        p <- dbSendStatement(mydb, "ALTER TABLE Exposed DROP COLUMN FIRST_OTHER")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Exposed DROP COLUMN Used_spell_exposed")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Exposed DROP COLUMN op_start_date")
        dbClearResult(p)
        p <- dbSendStatement(mydb, "ALTER TABLE Exposed DROP COLUMN op_end_date")
        dbClearResult(p)

}else{
      col <- colnames(dbGetQuery(mydb, "SELECT * FROM Controls LIMIT 1"))
      col <- col[!col %in% c("ST", "EN", "FIRST_OTHER", "Used_spell_exposed", "op_start_date", "op_end_date" )]
      col <- paste0(col, collapse = ",")
      
      p <- dbSendStatement(mydb, paste0("CREATE TABLE Controlstmp AS SELECT ",col," FROM Controls"))
      dbClearResult(p)
      
      p <- dbSendStatement(mydb, "DROP TABLE Controls")
      dbClearResult(p)
      
      p <- dbSendStatement(mydb, "ALTER TABLE Controlstmp RENAME TO Controls")
      dbClearResult(p)
      
      rm(col)
      
      col <- colnames(dbGetQuery(mydb, "SELECT * FROM Exposed LIMIT 1"))
      col <- col[!col %in% c("FIRST_OTHER", "Used_spell_exposed", "op_start_date", "op_end_date" )]
      col <- paste0(col, collapse = ",")
      
      p <- dbSendStatement(mydb, paste0("CREATE TABLE Exposedtmp AS SELECT ",col," FROM Exposed"))
      dbClearResult(p)
      
      p <- dbSendStatement(mydb, "DROP TABLE Exposed")
      dbClearResult(p)
      
      p <- dbSendStatement(mydb, "ALTER TABLE Exposedtmp RENAME TO Exposed")
      dbClearResult(p)
      
      rm(col,p)  
}       

rm(version)  

# ORIGINAL INDEX      
 #p <- dbSendStatement(mydb, paste0("CREATE INDEX Exposed_index ON Exposed(person_id, ST, EN, sex_at_instance_creation, YEAR_BIRTH, FIRST_PFIZER,",paste0(files, collapse = ","),")"))
 #dbClearResult(p)
# 
 #p <- dbSendStatement(mydb, paste0("CREATE INDEX Controls_index ON Controls(person_id, ST2, EN2, sex_at_instance_creation, YEAR_BIRTH, VAC_DATE1,",paste0(files, collapse = ","),")"))
 #dbClearResult(p)
 
 p <- dbSendStatement(mydb, paste0("CREATE INDEX Exposed_index_V2 ON Exposed(",paste0(files, collapse = ","),", sex_at_instance_creation, YEAR_BIRTH, FIRST_PFIZER)"))
 dbClearResult(p)

 p <- dbSendStatement(mydb, paste0("CREATE INDEX Controls_index_V2 ON Controls(",paste0(files, collapse = ","), ", sex_at_instance_creation, YEAR_BIRTH)"))
 dbClearResult(p)
 
 
# -- Exposed table indexes
# CREATE INDEX idx_exposed_first_pfizer ON Exposed(FIRST_PFIZER);
#p <- dbSendStatement(mydb, paste0("CREATE INDEX idx_exposed ON Exposed(person_id)"))
#dbClearResult(p)

# CREATE INDEX idx_controls_composite ON Controls(V_CDC_COV, IM_IMC_COV, I_COVID_COV, sex_at_instance_creation);
#p <- dbSendStatement(mydb, paste0('CREATE INDEX idx_controls_file ON Controls(sex_at_instance_creation,',paste0(files, collapse = ","),')'))
#dbClearResult(p)



#Add code to delete cases that have categorical variables that have "UNK" which is reflecting to NA 
####
# queryDelete <- unname(sapply(MATCH_CAT[!MATCH_CAT %in% MATCH_SCORE], function(i){
#   if(file.exists(paste0(matching_dir,"DIC_",i,".rds"))){
#   findUnk <- readRDS(paste0(matching_dir,"DIC_",i,".rds"))
#   findUnk <- findUnk[[i]][get(i) == "UNK",][["ID"]] 
#   paste0(i, " = ", findUnk)
#   }else{findUnk <- NA}
# }))
# queryDelete <- paste0(na.omit(queryDelete)[1], collapse = " OR ")
# 
# p <- dbSendStatement(mydb, paste0("DELETE FROM Exposed WHERE ", queryDelete))
# dbClearResult(p)
# p <-dbSendStatement(mydb, paste0("DELETE FROM Controls WHERE ", queryDelete))
# dbClearResult(p)
# rm(queryDelete)

###


dbDisconnect(mydb)




rm(mydb, p, SCRIPT, CODE_GROUP, CODE_JOIN, CODE_MAX, CODE_MAX2, CODE_MIN, CODE_MIN2, CODE_SELECT, files, seqnb, x)
gc()

