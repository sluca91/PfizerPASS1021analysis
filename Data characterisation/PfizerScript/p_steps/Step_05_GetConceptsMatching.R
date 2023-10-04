#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#To get all the cases of Covid and put them in 1 file. Because there can be many other outcomes occur within an event table we use temporary sqlite database.
#It is going loop wise. So first grap 1 event file, than import needed variables and remove first all rows that are not needed (check row 32 in code). Than
#the codes are selected and appended in the temporary sqlite database. This is done for all the EVENT files and when the loop is completed the covid table is
#loaded back to R environment and saved as RDS.

##in/output
#Input 1: CDM (csv)
#Input 2: ..CODES.rds
#Output 1: concepts in .db files

SCRIPT <- RUN_SCRIPT(name = "Step_05_GetConceptsMatching.R")

#Via this variable it is possible to only import needed information. First we did this, but later we wanted to distinct
#Between 0 and NA in an automated way. Therefore, this variable is not used (see row 39/40)
PERSONS_OF_INTEREST1 <- readRDS(paste0(tmp,"PERSONS_OF_INTEREST1.rds"))

#Open the connection
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Delete the tables that are already there to make it possible to rerun the script independently. Note that vaccines are already in so they need to be excluded form the
#tables to delete.
DeleteConcepts(concepts = MATCH_CONCEPTS[!MATCH_CONCEPTS %in% unique(readRDS(paste0(tmp, "CODES_VACCINES.rds"))[["Outcome"]])])

#Load all the concepts needed for matching process. Some DAP's  are only allowed to extract data for subjects
#that are in the study population and this is only known after matching. I made the script equal for all the databases
#So all the other DAP's are treated as if they also use a pre match procedure. Making different scripts would make it more
#complex to maintain code.
CombineConceptsFunctions(
  t.interest = c("MEDICINES", "EVENTS"),
  additional = T,
  expr = NULL,
  #expr = expression( person_id %in% PERSONS_OF_INTEREST1),
  concepts = MATCH_CONCEPTS
)



###

#Apply OR algorithms that are needed for matching
###

DeleteConcepts(concepts = MATCH_OR)

#get needed meta information
ALG <- readRDS(file = SCRIPT[["INPUT4"]][["path"]])

#Append the tables and make an rds copy. This rds copy is needed for the matching because the matching module used
#in Pfizer is a first version that demands rds files for the creation of the spell files.
for(i in MATCH_OR){
  #Retrieve the concepts that need to be appended to create a new concept
  tmp_concepts <- ALG[NEW_CONCEPT == i, ][["CONCEPT"]]
  TEMP <- AppendConcepts(DB = mydb, CONCEPTS = tmp_concepts, NAME = i, colls = "person_id, Date, Voc, Value, Outcome") 
  if(!is.null(TEMP)) {
    #also make the copy if any information is found
    saveRDS(TEMP, paste0(concepts_dir, i,".rds"))
    
    #Weighting is handled via a different approach in step_12
    if(!i %in% MATCH_WEIGHT) dbWriteTable(mydb, i ,TEMP, overwrite = F, append = T)
  }
  rm(tmp_concepts, TEMP)
}


###

#Get concepts need for the matching and make a rds copy of it needed as input for matching procedure
###

#Get the concepts for the matching 
NAppend <-  MATCH_CONCEPTS[!MATCH_CONCEPTS %in% ALG[["CONCEPT"]]]

#HARDCODED: vaccines are already handled in step_03 so excluded from this script.
NAppend <- NAppend[!NAppend %in% "INF"]

for(i in NAppend){
  #This also could be done via a dbReadTable().. Not sure why I did choose the appand function.
  TEMP <- AppendConcepts(DB = mydb, CONCEPTS = i, NAME = i)
  if(!is.null(TEMP)){
  #Save the copy
  saveRDS(TEMP, paste0(concepts_dir, i,".rds"))}
  rm(TEMP)
  gc()
}

dbDisconnect(mydb)

###

rm(NAppend, ALG)




#Additional algorithm to score CDC, used overwrite to not have additional files.
#HARDCODED: This is an alternative method of scoring for the matching. In step 12 all the covariates are scored in a generic way. 
###

if(file.exists(paste0(SCRIPT[["OUTPUT1"]][["folder"]],"V_CDC_COV.rds"))){

        CDC <- unique(readRDS(paste0(SCRIPT[["OUTPUT1"]][["folder"]],"V_CDC_COV.rds"))[, .(person_id, Date, Outcome)])
        
        #CDC <- rbind(CDC,copy(CDC)[, Date := Date + 10])
        
        #Get first diagnosis per outcome ?What to do with uniqueness, do we count multiple diagnosis of the same event multiple times or once. Now I assumed only once.
        setorder(CDC, person_id, Outcome, Date)
        CDC <- CDC[, order := seq_len(.N), by = c("person_id", "Outcome")][ order == 1,][, order := NULL]
        
        #Sort and count dates 
        setorder(CDC, person_id, Date)
        CDC <- CDC[, count := seq_len(.N), by = "person_id"][, .(person_id, count, Date)]
        
        #Score and add 1 day because you look back
        CDC <- CDC[count > 1, count := 2][, Date := Date + 1] 
        
        #Get the first date when 2 is reached
        setorder(CDC, person_id, count, Date)
        CDC <- CDC[, double := seq_len(.N), by = c("person_id", "count")][ double == 1,][, double := NULL]
        
        #Add 0 before 1 was scored. Not needed to add op_start_date because function CreateSpell takes tha into account
        CDC_p <- copy(CDC)[, .(Date = min(Date) - 1), by = "person_id" ][, count := 0]
        CDC <- rbindlist(list(CDC, CDC_p), use.names = T)
        rm(CDC_p)
        gc()
        
        #If 2 scored on 1 date because 2 diagnosis on 1 date then take the higest
        setorder(CDC, person_id, Date, -count)
        CDC <- CDC[, double := seq_len(.N), by = c("person_id", "Date")][ double == 1,][, double := NULL]
        
        
        setnames(CDC, "count", "Value" )
        
        #Overwrite
        ###
        #saveRDS(CDC, SCRIPT[["OUTPUT3"]][["path"]])
        #Hardcoded change for intirim 3
        saveRDS(CDC, paste0(SCRIPT[["OUTPUT1"]][["folder"]],"V_CDC_COV.rds"))
        
        
        
        ###
        
        rm(CDC)
        gc()

}

###


#Add categories if continues values DAP specific
#This was done under time pressure at interim 2 because at some point a DAP wanted to deviate the format of the data..
###

SCORE <-  readRDS(paste0(tmp, "Scores.rds"))[, c("CONCEPT","INTEGER_CODE","LOWER","UPPER"), with = F]

SCORE <- SCORE[CONCEPT %in% c(MATCH_CAT) & !CONCEPT %in% "V_CDC_COV",]

if(nrow(SCORE) > 0){
  
  conceptsScore <- unique(SCORE[["CONCEPT"]])
  #i = 1
  for(i in 1:length(conceptsScore)){
    if(file.exists(paste0(SCRIPT[["OUTPUT1"]][["folder"]],conceptsScore[i],".rds"))){
      print(conceptsScore[i])
      tmpConcept <- unique(readRDS(paste0(SCRIPT[["OUTPUT1"]][["folder"]],conceptsScore[i],".rds")))[, Value := as.numeric(Value)][, dummy := Value]
      
      setkeyv(tmpConcept, c("Value", "dummy"))
      
      tmpConcept <- foverlaps(x = SCORE[CONCEPT == conceptsScore[i],], 
                              y = tmpConcept, 
                              by.x = c("LOWER","UPPER"), 
                              nomatch = 0L, 
                              type = "any")[, `:=` (dummy = NULL, Value = NULL, LOWER = NULL, UPPER = NULL, CONCEPT = NULL)]
      
      setnames(tmpConcept, "INTEGER_CODE", "Value" )
      saveRDS(tmpConcept, paste0(SCRIPT[["OUTPUT1"]][["folder"]],conceptsScore[i],".rds"))
    } 
    
  }
  
  
  
}


###









rm(SCRIPT)















