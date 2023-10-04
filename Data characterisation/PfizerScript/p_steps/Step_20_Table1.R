#Author: Albert Cid Royo MSc.
#email: a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/01/2022

##Aim
#Generate Table 1.  Counts and proportions of administered Pfizer-BioNTech COVID‑19 vaccine doses patterns

##in/output
#Input 1: PERSONS3.rds

#Output 1: Table1

###SCRIPT SECTION

Main <- function(){
  SCRIPT <- RUN_SCRIPT(name = "Step_04b_CreateStudyPopulation.R")
  
  #this flowchart shows the subjects and spells before and after applying exclusion criteria.
  FlowChart <- readRDS(SCRIPT[["OUTPUT3"]][["path"]])
  
  CleanedPopulationSpells <- FlowChart[No_sex == TRUE & No_year_of_birth == TRUE & Invalid_date == TRUE 
                                       & No_year_of_death == TRUE & No_observation_time  == TRUE & No_op_start_date  == TRUE & OP_START_DATE_before_OP_END_DATE  == TRUE ]
  
  #ExposedPopulation <- FlowChart[Used_spell_exposed == TRUE]
  
  AllVaccinated <- CleanedPopulationSpells[!is.na(FIRST_PFIZER) == TRUE | !is.na(SECOND_PFIZER) == TRUE | !is.na(THIRD_PFIZER) | !is.na(FOURTH_PFIZER) == TRUE] #After excluding the persons with the wrong dates we have our All Vaccinated Population
  ObservedPopulation <- AllVaccinated[All_passed == TRUE] #Subject with at least 12month of enrollment passed all the data requiements
  
  M_Studycohort_Covariates_T0 <- readRDS(paste0(populations_dir,'/M_Studycohort_Covariates_T0.rds'))

  M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort.rds')))
  ExposedPersons <- M_Studycohort[group == 'EXPOSED' | group == 'UNMATCHED' & !is.na(FIRST_PFIZER),]
  
  Observed_pregnant <- merge(ExposedPersons, M_Studycohort_Covariates_T0[,.(person_id,id,L_PREGNSTATUS_COV_T0)], on = 'person_id')
  n1 = length(unique(AllVaccinated$person_id))
  n2 = length(unique(ObservedPopulation$person_id))
  n3 = ExposedPersons[,.N] #The Spells of the the First Pfizer Vaccinated population were labelles with User_spell_exposed as TRUE
  n4 = n3
  n5 = Observed_pregnant[L_PREGNSTATUS_COV_T0 == 1,.N]
  
  table1 <- data.table(c(n1,n2,n3,n4,n5))
  
  table1 <- cbind(table1, format(round((100*table1/table1[[1]][1]), 2), nsmall = 2))# combinbe first and second column
  setnames(table1, c("N", "PER"))
  template_table1 <- c(
                              'Received a dose of Pfizer-BioNTech COVID-19 vaccine',
                              '    Had ≥12 months continuous enrolment (a) PRIOR to receiving a first dose of Pfizer-BioNTech COVID-19 vaccine',
                              '    Received no prior COVID-19 vaccination AND had ≥12 months continuous enrolment (a) PRIOR to receiving a first Pfizer-BioNTech COVID-19 vaccine',
                              '    Total Pfizer-BioNTech COVID-19 vaccinated cohort (b)',
                              'Pregnant women in Pfizer-BioNTech COVID-19 vaccinated cohort'
                             )
  
  filledTemplate1 <- cbind(setnames(as.data.table(template_table1),'LABEL'),table1)
  fwrite(filledTemplate1, file = paste0(output_dir,DAP,'_Table1.csv'))

}

Main()


