
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/05/2022

##Aim
#This script gets all concepts that are needed for the matching for the variables that vary depended on T0. The concepts are converted to files with a start and 
#end date and a column with the condition in the spell. 

##in/output
#Input 1: PERSONS3.rds
#Input 2-7: REG.rds CDC.rds COV.rds INF.rds IMC.rds PREG.rds
#Output 1-6: REG_SPELL.rds CDC_SPELL.rds COV_SPELL.rds INF_SPELL.rds IMC_SPELL.rds PREG_SPELL.rds


SCRIPT <- RUN_SCRIPT(name = "Step_07_CreateSpellsMatching.R")

PER <- readRDS(SCRIPT[["INPUT1"]][["path"]])

#Make spells for region
###
#i <- MATCH_CAT[3]

for(i in MATCH_CAT){
  
  if(file.exists(paste0(concepts_dir,i,".rds"))){
  
  print(i)
  
  TEMP <- unique(readRDS(paste0(concepts_dir,i,".rds"))[,c("person_id", "Date", "Value"), with = F])
  setnames(TEMP, "Value", i)
  
  TEMP <- MakeSpells(
    Data = TEMP,
    c.id = "person_id",
    c.value = i,
    c.date = "Date",
    start_study_date = start_study_date,
    end_study_date = end_study_date
  )[, c("person_id", "ST", "EN", i) , with = F]  
  
  TEMP <- merge(x = as.data.table(PER[, person_id]), y = TEMP, all.x = T, by.y = "person_id", by.x = "V1")
  
  setnames(TEMP, "V1", "person_id")
  
  if(class(TEMP[[i]]) == "character"){ 
    TEMP <- TEMP[is.na(ST),eval(i)  := "UNK"][is.na(ST), `:=`  (ST = start_study_date, EN = end_study_date)]
  }
  
  if(class(TEMP[[i]]) %in% c("numeric", "integer")){ 
    TEMP <- TEMP[is.na(ST), eval(i) := 0][is.na(ST), `:=`  (ST = start_study_date, EN = end_study_date)]
  }
  
  if(any(is.na(TEMP))) warning("NA's in spells file")
  
  saveRDS(TEMP, paste0(matching_dir,i,"_SPELLS.rds"))
  
  rm(TEMP)
  gc()
  
  }
}


###

###



#Make spells for IMC, COV and INF based on T/F
###

#i <- 2
for(i in 1:length(MATCH_TF)){
  
  if(file.exists(paste0(concepts_dir,MATCH_TF[i],".rds"))){
  
  var <- MATCH_TF[i]
  print(var)
  
  prior <- MATCH_TF_T[i]
  
  
  
  if(!is.na(prior)){
  TEMP <- unique(readRDS(paste0(concepts_dir,var,".rds"))[,c("person_id", "Date"), with = F])  
  if(prior == 999) TEMP <- TEMP[, ":=" (ST = Date + 1, EN = end_study_date)][,.(person_id, ST, EN)]
  #if(prior < 999) TEMP <- TEMP[, ":=" (ST = Date + 1, EN = Date + 1 + (prior * 365.25))][,.(person_id, ST, EN)]
  if(prior < 999) TEMP <- TEMP[, ":=" (ST = Date + 1, EN = Date  + (prior * 365.25))][,.(person_id, ST, EN)]
  
  
  }else{
    TEMP <- unique(readRDS(paste0(concepts_dir,var,".rds"))[,c("person_id", "ST","EN"), with = F])
  }
  
  if(nrow(TEMP) > 0){
  TEMP <- CreateSpells(TEMP, "person_id", "ST", "EN")
  setnames(TEMP, c("entry_spell_category","exit_spell_category"), c("ST", "EN"))
  
  TEMP <- DevideTF(
    FILE = TEMP,
    FILEP = PER,
    c.start = "ST",
    c.end = "EN" ,
    id = "person_id",
    start_study_date = start_study_date,
    end_study_date = end_study_date
  )
  
  setnames(TEMP, "Status", var)
  
  if(any(is.na(TEMP))) warning("NA's in spells file")
  
  saveRDS(TEMP, paste0(matching_dir,var,"_SPELLS.rds"))
  }
  
  rm(TEMP, var, prior)
  gc()
  
  
  } 
}


