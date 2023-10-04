
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Make a file with all the relavant information for the matched pairs

##in/output
#Input 1: MATCHED_PAIRS.rds
#Input 2: PERSONS3.rds
#Input 3: OBS_SPELLS.rds
#Output 1: M_Studycohort.rds
#Output 2: SUBJECTS_TO_EXTRACT.csv (if prematched)

SCRIPT <- RUN_SCRIPT(name = "Step_10_CombineExposedControl.R")

MATCHED <- readRDS(SCRIPT[["INPUT1"]][["path"]])
MATCHED[, exclude := 0]
if('L_GEOREGION_COV' %in% colnames(MATCHED)){
  MATCHED[L_GEOREGION_COV %in% 'UNK', exclude := 1]
}
if('L_SOCIOECO_COV' %in% colnames(MATCHED)){
  MATCHED[L_SOCIOECO_COV %in% 'UNK', exclude := 1]
}
ExcludedPairs_Matching <- MATCHED[exclude == 1]
saveRDS(ExcludedPairs_Matching,paste0(matching_dir,'ExcludedPairs_Matching'))

MATCHED <-  MATCHED[exclude == 0][, exclude := NULL]

PERSONS <- readRDS(SCRIPT[["INPUT2"]][["path"]])
SPELLS <- readRDS(SCRIPT[["INPUT3"]][["path"]])

#colls_matching <- c(time_indep_match ,time_dep_match)[sapply(c(time_indep_match ,time_dep_match), function(x) x %in% colnames(MATCHED))]
#addVarMissings <- MATCHED[, missing_match_var := fifelse(L_GEOREGION_COV == "UNK" |  L_SOCIOECO_COV == "UNK", T, F) ,][,.(id, missing_match_var)]


Exposed <- MATCHED[,c("Exposed","Control","T0","id","nb_match"), with = F][, group := "EXPOSED"][is.na(Control), group := "UNMATCHED"][, Control := NULL]
Control <- MATCHED[,c("Control","T0","id","nb_match"), with = F][, group := "CONTROL"][!is.na(Control),]
setnames(Exposed,"Exposed","person_id")
setnames(Control,"Control","person_id")

M_Studycohort <- rbindlist(list(Exposed,Control), use.names = T)

###
new_person <- c(time_indep_match, time_indep_nmatch)[c(time_indep_match, time_indep_nmatch) %in% COV]
old_person <- c(time_indep_match_name, time_indep_nmatch_name)[c(time_indep_match, time_indep_nmatch) %in% COV]
setnames(PERSONS, old_person, new_person)

###

M_Studycohort <- merge(x = PERSONS[,c("person_id", "birth_date", "death_date", cols_vac_new, new_person), with = F], y = M_Studycohort, by = "person_id")
#sex_at_instance_creation, birth_date, death_date
rm(new_person, old_person)

M_Studycohort <- M_Studycohort[, AGE_T0 := floor(time_length(interval(birth_date, T0),"year"))]


#Add Age bands. bands is an object created at Step_00_Set_Parameters
M_Studycohort <- merge(x = M_Studycohort, y = bands, by.x = "AGE_T0",by.y = "INT")

#Add original op_start_date and op_end_date of the relevant spell
M_Studycohort <- sqldf(
  
  "
SELECT DISTINCT
t1.*,
t2.op_start_date,
t2.op_end_date

FROM M_Studycohort t1

left join SPELLS t2 on (t1.person_id = t2.person_id AND  t1.T0 BETWEEN t2.op_start_date AND t2.op_end_date)

"
)    


#ExcludedCasesMatching <- M_Studycohort[is.na(L_SOCIOECO_COV) | is.na(L_GEOREGION_COV)]

if(any(is.na(M_Studycohort[["op_start_date"]]))) stop("NA in op_start_date")
if(any(is.na(M_Studycohort[["op_end_date"]]))) stop("NA in op_end_date")
if(nrow(M_Studycohort) !=  (nrow(Control) + nrow(Exposed))) stop("Multiple spells overlap with an t0")

setorder(M_Studycohort, id, group)

#M_Studycohort <- merge(M_Studycohort, addVarMissings, by = "id", all.x = T)
saveRDS(M_Studycohort, SCRIPT[["OUTPUT1"]][["path"]])
fwrite(x = data.table(ID = unique(M_Studycohort[["person_id"]])), file = SCRIPT[["OUTPUT2"]][["path"]])

rm(M_Studycohort, MATCHED, Exposed, Control, SCRIPT, PERSONS, SPELLS)
gc()
