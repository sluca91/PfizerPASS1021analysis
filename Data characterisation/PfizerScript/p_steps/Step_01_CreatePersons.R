#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Essential dates for the next steps will be inputted and date format is created form day month year

##in/output
#Input 1: PERSONS.csv (CDM)
#Input 2: OBS_SPELLS.rds 
#Output 1: PERSONS.rds
#Output 2: INPUTED.rds 

SCRIPT <- RUN_SCRIPT(name = "Step_01_CreatePersons.R")

print('Import and append persons files')
PERSONS <- IMPORT_PATTERN(
  pat = SCRIPT[["INPUT1"]][["name"]], 
  dir = path_dir,
  colls = c("person_id","day_of_birth","month_of_birth","year_of_birth","day_of_death","month_of_death","year_of_death","sex_at_instance_creation", "race")
  )

if(any(duplicated(PERSONS[["person_id"]]))) stop("Duplicates in person table") 

print('Set date variables (day/month/year) to integer')
dates_persons <- c("year_of_birth", "month_of_birth","day_of_birth","year_of_death", "month_of_death","day_of_death")
invisible(lapply(dates_persons, function (x) if (class(PERSONS[[x]]) != "integer") PERSONS[, eval(x) := as.integer(get(x)) ]))


print('Inpute birth and death day and month')
PERSONS[is.na(day_of_birth) & is.na(month_of_birth) & !is.na(year_of_birth), ':=' (day_of_birth = 1, month_of_birth = 6, inputed_birth_day = T,inputed_birth_month = T)]
PERSONS[is.na(day_of_birth) & !is.na(year_of_birth), ':=' (day_of_birth = 16, inputed_birth_day = T)]
PERSONS[is.na(month_of_birth) & !is.na(year_of_birth), ':=' (month_of_birth = 6, inputed_birth_month = T)]
PERSONS[inputed_birth_month == T & day_of_birth == 31, ':=' (day_of_birth = 30)]

PERSONS[is.na(day_of_death) & is.na(month_of_death) & !is.na(year_of_death), ':=' (day_of_death = 1, month_of_death = 6, inputed_death_day = T,inputed_death_month = T)]
PERSONS[is.na(day_of_death) & !is.na(year_of_death), ':=' (day_of_death = 16, inputed_death_day = T)]
PERSONS[is.na(month_of_death) & !is.na(year_of_death), ':=' (month_of_death = 6, inputed_death_month = T)]
PERSONS[inputed_death_month == T & day_of_death == 31, ':=' (day_of_death = 30)]

#To monitor, make a overview of the cells that are inputed
INPUTED <- PERSONS[inputed_birth_day == T |inputed_birth_month == T|inputed_death_day == T| inputed_death_month == T ,.(person_id,inputed_birth_day,inputed_birth_month,inputed_death_day,inputed_death_month)]
saveRDS(INPUTED,file = SCRIPT[["OUTPUT2"]][["path"]])
rm(INPUTED)
gc()

#Remove variables that are not needed anymore
lapply(c("inputed_birth_day","inputed_birth_month","inputed_death_day","inputed_death_month"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

#Create birth and death dates
PERSONS <- PERSONS[!is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth),birth_date := as.Date(paste0(year_of_birth, sprintf("%02d",month_of_birth),sprintf("%02d",day_of_birth)),"%Y%m%d")]
PERSONS <- PERSONS[!is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death),death_date := as.Date(paste0(year_of_death, sprintf("%02d",month_of_death),sprintf("%02d",day_of_death)),"%Y%m%d")]
PERSONS <- PERSONS[,YEAR_BIRTH := year(birth_date)]

#Store persons that are still relavant in future steps
PERSONS_OF_INTEREST0 <- unique(PERSONS[["person_id"]])

saveRDS(PERSONS, file = SCRIPT[["OUTPUT1"]][["path"]])

rm(PERSONS, SCRIPT)
gc()


