#Author: Albert Cid Royo MSc.
#email: a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/01/2022

##Aim
#Generate Table 10.  Counts and proportions of administered Pfizer-BioNTech COVID‑19 vaccine doses patterns

##in/output
#Input 1: PERSONS3.rds

#Output 1: table10 (Word Version)

###SCRIPT SECTION

Main <- function(){
  M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort.rds')))
  M_Studycohort_Covariates_T0 <- as.data.table(readRDS(paste0(populations_dir,'M_Studycohort_Covariates_T0.rds')))
  
  names_t0 <- str_remove(colnames(M_Studycohort_Covariates_T0),'_T0')
  load(paste0(dir_base,"/Data characterisation/PfizerScript/g_intermediate/tmp/parameters.RData"))
  
  missingAvailable_T0 <- Available_cov[!Available_cov %in% names_t0]
  invisible(lapply(missingAvailable_T0, function(x) M_Studycohort_Covariates_T0[,eval(x) := 0]))
  
  pregnancy <- M_Studycohort_Covariates_T0[,.(person_id,id,L_PREGNSTATUS_COV_T0)]
  setnames(M_Studycohort,'group','S_Group')
  VaccinesMatched <- M_Studycohort[!is.na(FIRST_PFIZER) & S_Group == 'EXPOSED']
  ControlMatched <- M_Studycohort[S_Group == 'CONTROL']
  
  ExposedControlMatched<-merge(x=VaccinesMatched, y=ControlMatched, by='person_id', all.x=FALSE)
  MatchinCounts = ControlMatched[,.N,by = person_id]
  Distribution_MatchCounts = summary(MatchinCounts$N)
  
  
  
  SCRIPT <- RUN_SCRIPT(name = "Step_04b_CreateStudyPopulation.R")
  
  #this flowchart shows the subjects and spells before and after applying exclusion criteria.
  FlowChart <- readRDS(SCRIPT[["OUTPUT3"]][["path"]])
  
  CleanedPopulationSpells <- FlowChart[No_sex == TRUE & No_year_of_birth == TRUE & Invalid_date == TRUE 
                                       & No_year_of_death == TRUE & No_observation_time  == TRUE & No_op_start_date  == TRUE & OP_START_DATE_before_OP_END_DATE  == TRUE ]
  
  #ExposedPopulation <- FlowChart[Used_spell_exposed == TRUE]
  
  AllVaccinated <- CleanedPopulationSpells[!is.na(FIRST_PFIZER) == TRUE | !is.na(SECOND_PFIZER) == TRUE | !is.na(THIRD_PFIZER) | !is.na(FOURTH_PFIZER) == TRUE] #After excluding the persons with the wrong dates we have our All Vaccinated Population
  ObservedPopulation <- AllVaccinated[All_passed == TRUE]
  
  n1 = length(unique(AllVaccinated$person_id))

  n2 =  M_Studycohort[S_Group == 'EXPOSED' | S_Group == 'UNMATCHED' & !is.na(FIRST_PFIZER),.N] #The Spells of the the First Pfizer Vaccinated population were labelles with User_spell_exposed as TRUE
  
  pregInfoPOP <- merge(M_Studycohort[S_Group == 'EXPOSED' | S_Group == 'UNMATCHED' & !is.na(FIRST_PFIZER)],pregnancy, on = c('person_id')) #TODO check this
  n3 = pregInfoPOP[L_PREGNSTATUS_COV_T0 == 1,.N]
  
  #Final Population included
  
  col1_1 <- data.table(c(n1,n2,n3))
  setnames(col1_1,'N')
  col2_1 <- data.table(format(round((100*col1_1/n1), 2), nsmall = 2))
  setnames(col2_1,'PER')
  col1_2 <- list('',
                         'N_VacMatch' = VaccinesMatched[,.N],
                         'N_ExposedControl' = ExposedControlMatched[,.N],
                         'N_ControlMatch' = ControlMatched[,.N],
                         'N_UniqueMatch' = length(unique(ControlMatched[,person_id])),
                         'mean_n' =  format(round(Distribution_MatchCounts[[4]], 2), nsmall = 2) ,
                         'median' = format(round(Distribution_MatchCounts[[3]], 2), nsmall = 2)  ,
                         'min' = Distribution_MatchCounts[[2]],
                         'n_1' = sum(MatchinCounts$N == 1),
                         'n_2' = sum(MatchinCounts$N == 2),
                         'n_3' = sum(MatchinCounts$N == 3),
                         'n_4' = sum(MatchinCounts$N == 4),
                         'n_5' = sum(MatchinCounts$N >= 5)
  )
  
  col2_2 <- list('','','','','','',
                            'q3' = paste0(format(round(Distribution_MatchCounts[[1]], 2), nsmall = 2),'-',format(round(Distribution_MatchCounts[[5]], 2), nsmall = 2)),
                            'max' = Distribution_MatchCounts[[6]],
                            per_1 =  format(round((100*col1_2$n_1/ControlMatched[,.N]), 2), nsmall = 2) ,
                            per_2 =   format(round((100*col1_2$n_2/ControlMatched[,.N]), 2), nsmall = 2) , 
                            per_3 =   format(round((100*col1_2$n_3/ControlMatched[,.N]), 2), nsmall = 2) ,
                            per_4 =   format(round((100*col1_2$n_4/ControlMatched[,.N]), 2), nsmall = 2) ,
                            per_5 =   format(round((100*col1_2$n_5/ControlMatched[,.N]), 2), nsmall = 2)
  )
  
  col1_2 <- data.table(col1_2)
  setnames(col1_2,'N')
  col2_2 <- data.table(col2_2)
  setnames(col2_2,'PER')
  table10 <- cbind(rbind(col1_1,col1_2),rbind(col2_1,col2_2))
  #setnames(table10, new = c("N","PER"))
  #Preparing results:
  

  labels <- as.data.table(c(
                               '   Received a dose of Pfizer-BioNTech COVID-19 vaccine',
                               # '   Has at least 12 month (or lifetime enrollment if <12months) in the database',
                               # '   No prior COVID-19 vaccination',
                               '  Total vaccinated included in matched cohort',
                               '  Pregnant women in vaccinated cohort',
                               'Matched cohort, n (%)',
                               '  Vaccinees matched',
                               '    Served as control before vaccination',
                               ' Unvaccinated matched',
                               '    Unique unvaccinated included after matching',
                               '    Number of times a comparator was selected for matching',
                               '      Median (Q1-Q3)',
                               '      Min-Max',
                               '      1 time, n (%)',
                               '      2  times, n (%)',
                               '      3 times, n (%)',
                               '      4 times, n (%)',
                               '      5 times or more, n (%)'))
  setnames(labels,new = 'LABELS')
  
  table10 <- cbind(labels,table10)

  fwrite(table10, file = paste0(output_dir,DAP,'_Table10.csv'))
}
Main()
