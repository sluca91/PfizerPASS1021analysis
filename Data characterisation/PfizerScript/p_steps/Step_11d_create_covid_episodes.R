#Aim: Clean covid
#Input: I_COVID_COV from concepts.db
#Outputs: COVID_EPISODES in concepts.db

SCRIPT <- RUN_SCRIPT(name = "Step_11d_create_covid_episodes")


#Connect to the database
mydb <- dbConnect(RSQLite::SQLite(), SCRIPT[["INPUT1"]][["path"]])


#Create COVID_INFECTION
###
#First we want to collect all data needed. This can be done in 1 step for all the study variables but for now I will only apply for covid.

covidCases <- as.data.table(dbReadTable(mydb, "I_COVID_COV"))[, Date := as.Date(Date, origin = "1970-01-01")]

#Then we want to apply a wash out period of 60 days. I use the function CleanOutcomes Albert extended this, we may need to have alook in that function.
covidCasesClean <- CleanOutcomes(
                                    Dataset = copy(covidCases)[,.(person_id, Date)][, Outcome := "COVIDINFECTION"], 
                                    Person_id = "person_id",
                                    rec.period = 60,
                                    outcome = c("COVIDINFECTION"),
                                    c.outcome = "Outcome",
                                    c.date = "Date"
                                                  )

#Add instance of the covid infection. Moreover, NB is added and serves as an Boolean later on in the query. Originally, NB did have also a different function which
#was to make a distinction between first, second... vaccination and the first, second duplication in the data of that vaccination. It was not clear in the beginning 
#If we would want the first as the vaccination date of that vaccination. Feel free to change it and remove NB from the query in step 11e.
setorder(covidCasesClean, person_id, Date)
covidCasesClean <- covidCasesClean[, COVID_NB := seq_len(.N), by = "person_id"][, NB := 1]
dbWriteTable(mydb, "I_COVIDINF_AESI" ,covidCasesClean, overwrite = T, append = F)



dbDisconnect(mydb)

rm(covidCases, covidCasesClean, SCRIPT, mydb)
gc()
###






