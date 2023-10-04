
#Aim: Create outcomes covid severity
#Input: covid_episiodes database 
#Output: new concepts sev1-sev3 in database

SCRIPT <- RUN_SCRIPT(name = "Step_11e_create_covid_outcomes")


#Creating sev2 using the function
CovidSeverity(
  or.cols = c("R_ARDS_AESI", "H_HOSPNUM_COV", "H_EMERG_COV"),
    and.col = "I_COVIDINF_AESI",
  db.path = SCRIPT[["INPUT1"]][["path"]],
  outcome.name = "I_COVIDSEV2_AESI",
  t.post = 30,
  t.prior = 0
  
)

#Creating sev3 using the function
CovidSeverity(
  or.cols = c("O_DEATHANY_AESI", "O_DEATHSUDDEN_AESI"),
  and.col = "I_COVIDINF_AESI",
  db.path = SCRIPT[["INPUT1"]][["path"]],
  outcome.name = "I_COVIDSEV3_AESI",
  t.post = (7 * 8),
  t.prior = 0
  
)


#Create D3 output file
mydb <- dbConnect(RSQLite::SQLite(), SCRIPT[["INPUT1"]][["path"]])

#Retreive all wanted infromation from the database for the output table. First merging so that combined variables can be created later. 
#If the MORE varaibles will not be relevant anymore convert this to a union instead of a join
covidOutcomes <- as.data.table(dbGetQuery(mydb, 
           "
           SELECT  
           t1.person_id,
           t1.Date,
           t1.NB as I_COVIDSEV1_AESI,
           t2.NB as I_COVIDSEV2_AESI,
           t3.NB as I_COVIDSEV3_AESI
           
           FROM (SELECT * FROM I_COVIDINF_AESI  WHERE NB = 1) t1
           
                LEFT JOIN (SELECT person_id, COVID_NB, NB   FROM   I_COVIDSEV2_AESI) t2 ON (t1.person_id = t2.person_id AND t1.COVID_NB = t2.COVID_NB) 
                
                  LEFT JOIN (SELECT person_id, COVID_NB, NB  FROM   I_COVIDSEV3_AESI) t3 ON (t1.person_id = t3.person_id AND t1.COVID_NB = t3.COVID_NB) 
                  
                  
           
           "
           
           
           
           )
)

#Create MORE variables
#covidOutcomes <- covidOutcomes[, COVIDANY := I_COVIDSEV1_AESI]
covidOutcomes <- covidOutcomes[I_COVIDSEV2_AESI == 1 | I_COVIDSEV3_AESI == 1, `:=` (I_COVIDSEV1_AESI = NA, I_COVID19VAED_AESI = 1 ) ]
covidOutcomes <- covidOutcomes[I_COVIDSEV3_AESI == 1, `:=` (I_COVIDSEV2_AESI = NA ) ]


#Define output variables for the output dataset
outputVars <- c("I_COVIDSEV1_AESI", "I_COVIDSEV2_AESI", "I_COVIDSEV3_AESI", "I_COVID19VAED_AESI")

#Melting is sensitive for the format of the columns that need to melted. Therfore set to integer all
lapply(outputVars , function(x) covidOutcomes <- covidOutcomes[, eval(x) :=  as.integer(get(x))])

#Make a long table
covidOutcomes <- data.table::melt(covidOutcomes, id.vars = c("person_id", "Date"), measure.vars = outputVars, na.rm = T, variable.name = "STUDY_VARIABLE")
covidOutcomes <- covidOutcomes[value == 1,][, value := NULL]

covidOutcomes <- covidOutcomes[, Date := as.Date(Date, origin = "1970-01-01")] 

#I need the AESI's in the database.
for(i in outputVars){
  dbWriteTable(mydb, i, unique(covidOutcomes[STUDY_VARIABLE == i,][, .(person_id, Date)]),   overwrite = T, append =F)
}



dbDisconnect(mydb)

rm(covidOutcomes, mydb, outputVars, SCRIPT)





