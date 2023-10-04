
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#We know which subjects are relevant after the matching. This script is taking all the concepts and put it in a sqlite database per concept 

##in/output
#Input 1: MATCHED_PAIRS.rds
#Input 2: CODES_EVENTS
#Input 3: EVENTS (CDM)
#Input 4: CODES_MEDICINES
#Input 5: MEDICINES
#Input 6: CODES_ADDITIONAL

#Output 1: Concepts in sqlite database named database.db

SCRIPT <- RUN_SCRIPT(name = "Step_11_PutConceptsInDatabase")

MATCHES <- readRDS(SCRIPT[["INPUT1"]][["path"]])

#Via this variable it is possible to only import needed information. First we did this, but later we wanted to distinct
#Between 0 and NA in an automated way. Therefore, this variable is not used (see row 39/40)
PERSONS_OF_INTEREST2 <- c(unique(MATCHES[["Exposed"]]),unique(MATCHES[["Control"]]))
rm(MATCHES)

mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Delete the tables that are already there to make it possible to rerun the script independently
DeleteConcepts(concepts = NMATCH_CONCEPTS)

CombineConceptsFunctions(
  t.interest = c("MEDICINES", "EVENTS", "VACCINES"),
  additional = T,
  #expr = expression( person_id %in% PERSONS_OF_INTEREST2),
  expr = NULL,
  concepts = NMATCH_CONCEPTS
)

###


dbDisconnect(mydb)
rm(SCRIPT, mydb)
gc()









