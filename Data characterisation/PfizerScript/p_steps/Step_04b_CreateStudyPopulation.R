# Author: Albert Cid Royo MSc.
# email:acidroyo@umcutrecht.nl
# Organisation: UMC Utrecht, Utrecht, The Netherlands
# Date: 10/1/2022

## Aim
# Apply exclusion criteria and remove all person and spell data that we can not use.Because In the exposed group 1 spell is taken and in the control group
# All spells can be used therefore, the analyses is done for the PERSONS table first. After that the spells table is analyzed with the persons that where kept
# In the PERSON analyses

## in/output
# Input 1: SPELLS.rds
# Output 1: PERSONS1.rds
# Output 2: SPELLS1.rds
# Output 3: FlowChart



checkList_Fun <- function(file, expr_list, by_cols){
  #TEMP <- copy(file)
  for (j in 1:length(expr_list)){
    file <- file[, eval(names(expr_list)[j]) :=  fifelse(eval(expr_list[[j]]), T, F)]
  }  
  file[, c(by_cols,eval(names(expr_list))), with= F] 
}

###

SCRIPT <- RUN_SCRIPT(name = "Step_04b_CreateStudyPopulation.R")

PERSONS <- readRDS(SCRIPT[["INPUT1"]][["path"]])[, `:=` (day_of_birth = NULL,month_of_birth = NULL)]
SPELLS <- readRDS(SCRIPT[["INPUT2"]][["path"]])[, `:=` (death_date = NULL, birth_date = NULL)]

cols_per <- colnames(PERSONS)
cols_per <- cols_per[!cols_per %in% c("year_of_birth", "year_of_death", "day_of_death", "month_of_death")]
cols_spell <- c(colnames(SPELLS), "Used_spell_exposed")

SPELLS <- merge(x = PERSONS, y = SPELLS, by = "person_id")

rm(PERSONS)
gc()

#vaccineStudy <- 'astrazeneca' #define it in the general 
SPELLS <- SPELLS[, Used_spell_exposed := fifelse(FIRST_PFIZER %between% list(op_start_date, op_end_date) & FIRST_PFIZER %between% intv, T, F, na = F)]

#To filter out subjects that have a vaccination before they where in the database, which means they cannot be a control anymore and they are unneeded.
#First make a column with the minimal op_start_date per subject
#SPELLS <- SPELLS[, op_start_date_min := min(op_start_date), by = "person_id"]
#Then the following expression is added in ExclusionMatchCohort
#is.na(FIRST_PFIZER) | (!is.na(FIRST_PFIZER) & !FIRST_PFIZER < op_start_date_min)


##### MATCHING POPULATIONS SPELLS #####


SPELLS_CheckList1 <- checkList_Fun(file = copy(SPELLS),
                                   expr_list = ExclusionMatchCohort, by_cols = c("person_id", "op_start_date", "op_end_date")) #

SPELLS_CheckList1 <- SPELLS_CheckList1[, 
                                       All_passed := fifelse(rowSums(.SD) == length(names(ExclusionMatchCohort)),T,F), 
                                       .SDcols = names(ExclusionMatchCohort)] #Defining if the SPELL passed all the Checklist conditions

SPELLS <- SPELLS[, `:=` (year_of_birth = NULL, year_of_death = NULL, day_of_death = NULL, month_of_death = NULL)]

#Selecting SPELLS that passed ALL the Conditions

MatchingSpells <- merge(x = SPELLS, y = SPELLS_CheckList1[All_passed == T,], by = c("person_id", "op_start_date", "op_end_date"))
SPELLS_CheckList1 <- merge(x = SPELLS_CheckList1, y = SPELLS, by = c("person_id", "op_start_date", "op_end_date"))

#Dividing Spells into Exposed and Unexposed Spells, needed for editing the Exposed Spells
exposedSpells <- MatchingSpells[Used_spell_exposed == TRUE] 
unexposedSpells <- MatchingSpells[Used_spell_exposed == FALSE]

rm(SPELLS,MatchingSpells)
gc()

# Splitting Exposed Spells into Eposed_Control and Exposed Spells
exposedSpells_control <- copy(exposedSpells) # Control Spells coming from an Exposed Spell

exposedSpells[, "op_start_date"] <- exposedSpells[, "FIRST_PFIZER"] # Change the op stat date date of the Exposed Spell

exposedSpells_control[, "op_end_date"] <- exposedSpells_control[, "FIRST_PFIZER"] - 1 # Change the end date of the Control Spell
exposedSpells_control[, "Used_spell_exposed"] <- FALSE #The Control Part of a Exposed Spells is reverted to a Control Spell
exposedSpells_control[, "num_spell"] <- exposedSpells_control[, "num_spell"] - 0.5 #The spell number is changed

MatchingSpells <- rbind(exposedSpells,exposedSpells_control,unexposedSpells)

MatchingSpells <- MatchingSpells[Used_spell_exposed == FALSE, op_start_date := op_start_date + lookback_period] #Deleting 1-year-enrollment period to the start date of Control Spells

PERSONS_OF_INTEREST1 <- unique(MatchingSpells[["person_id"]])


setorder(MatchingSpells, person_id, op_start_date)

saveRDS(unique(MatchingSpells[, cols_per , with = F]) ,SCRIPT[["OUTPUT1"]][["path"]])
saveRDS(unique(MatchingSpells[, cols_spell , with = F]) ,SCRIPT[["OUTPUT2"]][["path"]])
saveRDS(SPELLS_CheckList1 ,SCRIPT[["OUTPUT3"]][["path"]])
saveRDS(PERSONS_OF_INTEREST1 , paste0(tmp,"PERSONS_OF_INTEREST1.rds"))

rm(SCRIPT, MatchingSpells,exposedSpells,exposedSpells_control,unexposedSpells, PERSONS_OF_INTEREST1, cols_per, SPELLS_CheckList1) #cols_spell
gc()




