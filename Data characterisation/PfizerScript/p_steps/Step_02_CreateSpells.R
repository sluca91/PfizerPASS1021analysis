
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#The aim of this step is to make sure all subjects not have overlapping periods as is the diff between op_start_date and an op_end_date. Overlaps will be merged 
#If 20 days or less between the spells than also the spell is merged. Op_start and op_end dates are corrected with birth and death date. If no op_end_date then
#End of study date is inputed.

##in/output
#Input 1: OBSERVATION_PERIODS.csv (CDM table)
#Input 1: PERSONS (CDM table)
#Output 1: OBS_SPELLS.rds
#Output 2: FlowChartCreateSpells.rds 

SCRIPT <- RUN_SCRIPT(name = "Step_02_CreateSpells.R")

print('Import and append observation periods files. Select only rows with an op_start_date.')

#Function IMPORT_PATTERN is importing all csv files in the defined folder (dir) starting with pat. These files are appended. Formats are also regulated.
#I only importing the columns needed for CreateSpells
OBSERVATION_PERIODS <- IMPORT_PATTERN(
  pat = SCRIPT[["INPUT1"]][["name"]], 
  dir = path_dir, 
  colls = c("person_id","op_start_date","op_end_date"), 
  date.colls = c("op_start_date","op_end_date"),
  exprss = expression(!is.na(op_start_date))
  #exprss = expression(as.numeric(substr(start_study_date2,1,4)) - as.numeric(substr(op_end_date,1,4)) < (11))
  )

print('Set start and end date to date format and if end date is empty fill with end study date')

OBSERVATION_PERIODS <- OBSERVATION_PERIODS[is.na(op_end_date), op_end_date := end_study_date]

FlowChartCreateSpells <- list()

print(paste0("Run create spells / allow ",max_spells_gap," days between the spells"))

before <- nrow(OBSERVATION_PERIODS)

OBSERVATION_PERIODS1 <- CreateSpells(
  dataset = OBSERVATION_PERIODS,
  id="person_id" ,
  start_date = "op_start_date",
  end_date = "op_end_date",
  overlap = FALSE,
  only_overlaps = F,
  gap_allowed = max_spells_gap
)

rm(OBSERVATION_PERIODS)
gc()

after <- nrow(OBSERVATION_PERIODS1)

FlowChartCreateSpells[["Spells1"]]$step <- "Run_CreateSpells"
FlowChartCreateSpells[["Spells1"]]$before <- before
FlowChartCreateSpells[["Spells1"]]$after <- after
rm(before,after)

setnames(OBSERVATION_PERIODS1, "entry_spell_category", "op_start_date")
setnames(OBSERVATION_PERIODS1, "exit_spell_category", "op_end_date")

#To monitor, the number of rows before and after steps that affect the data are counted and stored.
FlowChartCreateSpells <- as.data.table(do.call(rbind,FlowChartCreateSpells))

#Correct op_start and op_end date
####
#To correct birth date and date of death the first and last spell are selected
PERSONS <- readRDS(SCRIPT[["INPUT2"]][["path"]])[,.(person_id, birth_date, death_date)]
OBSERVATION_PERIODS1 <- merge(OBSERVATION_PERIODS1, PERSONS, all.x = F, all.y = F, by = "person_id")
OBSERVATION_PERIODS1 <- OBSERVATION_PERIODS1[op_start_date < birth_date, op_start_date := birth_date]
OBSERVATION_PERIODS1 <- OBSERVATION_PERIODS1[op_end_date > death_date, op_end_date := death_date]
OBSERVATION_PERIODS1 <- OBSERVATION_PERIODS1[,op_end_date := min(end_study_date,op_end_date,date_creation,recommended_end_date),by = list(row.names(OBSERVATION_PERIODS1))]

rm(PERSONS)
gc()
###

saveRDS(OBSERVATION_PERIODS1, file = SCRIPT[["OUTPUT1"]][["path"]])
saveRDS(FlowChartCreateSpells, file = SCRIPT[["OUTPUT2"]][["path"]])


rm(OBSERVATION_PERIODS1,FlowChartCreateSpells, SCRIPT)
gc()




