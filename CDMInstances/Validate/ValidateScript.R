


MATCH_PAIRS <- readRDS(paste0(populations_dir, "MATCH_PAIRS.rds"))
M_Studycohort_Covariates_T0 <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_T0.rds"))
#M_Studycohort_Covariates_dic_T0 <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_dic_T0.rds"))
M_Studycohort_Covariates_D3 <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_D3.rds"))
#M_Studycohort_Covariates_dic_D3 <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_dic_D3.rds"))
M_Studycohort <- readRDS(paste0(populations_dir, "M_Studycohort.rds"))

Dictionary <- readRDS(paste0(tmp, "Dictionary_result.rds"))
#SCORE <- readRDS(paste0(meta_dir, ".rds"))


HFAESI_T0 <-readRDS(paste0(aesi_dir, "B_ITP_AESI_T0.rds"))
HFAESI_D3 <-readRDS(paste0(aesi_dir, "B_ITP_AESI_D3.rds"))


print("Test if AESI's are detected correctly")

print(sum(
as.character(HFAESI_T0[person_id == "P099a",][["Date_COUNT"]]) == "2021-01-01",
as.character(HFAESI_T0[person_id == "P099b",][["Date_HIST"]]) == "2016-12-31",
as.character(HFAESI_D3[person_id == "P099a",][["Date_COUNT"]]) == "2021-03-02",
as.character(HFAESI_D3[person_id == "P099a",][["Date_HIST"]]) == "2021-02-03"
) == 4)

rm(HFAESI_D3, HFAESI_T0)


print("Test if pairs match a/b")
test <- MATCH_PAIRS[substr(Exposed,1,4) == substr(Control,1,4) &  substr(Exposed,5,5) == "a" & substr(Control,5,5) == "b" ]
print(nrow(test) == 4)
print(sum(test$nb_match == 1) == 4)


print("Test BMI calculation")
test <- M_Studycohort_Covariates_T0[,.(person_id, L_BMICALC_COV_T0)]
print(test[person_id == "P002a",][["L_BMICALC_COV_T0"]] == 1)
print(test[person_id == "P005a",][["L_BMICALC_COV_T0"]] == 2)
rm(test)

test <- M_Studycohort_Covariates_D3[,.(person_id, L_BMICALC_COV_D3)]
print(test[person_id == "P002a",][["L_BMICALC_COV_D3"]] == 2)
print(test[person_id == "P005a",][["L_BMICALC_COV_D3"]] == 1)
rm(test)

print("Test if a covariate with a category gives the most recent result compared to reference times (T0 vs D3)")
test <- M_Studycohort_Covariates_D3[,.(person_id, L_SMOKESTATUSALG_COV_D3)]
test <- merge(x = test, y = unique(Dictionary[VarName == "L_SMOKESTATUSALG_COV", .(integerVal, category) , with = T]), by.x = "L_SMOKESTATUSALG_COV_D3", by.y = "integerVal")
print(test[person_id == "P002a",][["category"]] == "former")
rm(test)

test <- M_Studycohort_Covariates_T0[,.(person_id, L_SMOKESTATUSALG_COV_T0)]
test <- merge(x = test, y = unique(Dictionary[VarName == "L_SMOKESTATUSALG_COV", .(integerVal, category) , with = T]), by.x = "L_SMOKESTATUSALG_COV_T0", by.y = "integerVal")
print(test[person_id == "P002a",][["category"]] == "current")
rm(test)

print("Test lookback time covariates edges")
test <- M_Studycohort_Covariates_T0[,.(person_id, I_SEPSIS_COV_T0, L_CHARLSON_COV_T0,  IM_ALLERGIES_COV_T0,  INF_T0 )][person_id %in% c("P002a", "P005a"),]
#test <- merge(x = test, y = Dictionary[VarName == "L_CHARLSON_COV", .(integerVal, oriVal) , with = T], by.x = "L_CHARLSON_COV_T0", by.y = "integerVal")
print(sum(as.character(unname(test[person_id == "P005a",])) == c("P005a", "1", "2", "1", "1")) == 5)
print(sum(as.character(unname(test[person_id == "P002a",])) == c("P002a", "0", "0", "0", "0")) == 5)
rm(test)

test <- M_Studycohort_Covariates_D3[,.(person_id, I_SEPSIS_COV_D3, L_CHARLSON_COV_D3,  IM_ALLERGIES_COV_D3,  INF_D3 )][person_id %in% c("P002a", "P005a"),]
print(sum(as.character(unname(test[person_id == "P005a",])) == c("P005a", "0", "0", "0", "1")) == 5)
print(sum(as.character(unname(test[person_id == "P002a",])) == c("P002a", "0", "0", "0", "0")) == 5)
rm(test)


print("Check some exclusions (not complete yet)")

FlowChart <- readRDS(paste0(tmp, "Flowchart.rds"))
print(sum(
FlowChart[person_id == "P009" & as.character(op_start_date) == "1974-07-16",][["Study_Period_and_spell_overlap"]],
FlowChart[person_id == "P009" & as.character(op_start_date) == "2020-01-02",][["Time_before_FIRST_PFIZER_more_than_lookback"]],
FlowChart[person_id == "P010" & as.character(op_start_date) == "1974-07-06",][["Study_Period_and_spell_overlap"]],
FlowChart[person_id == "P008" & as.character(op_start_date) == "1974-07-06",][["Study_Period_and_spell_overlap"]],
FlowChart[person_id == "P000a" & as.character(op_start_date) == "2022-01-02",][["Remaning_time_to_end_study_date_less_then_lookback_period"]],
FlowChart[person_id == "P011" & as.character(op_start_date) == "2020-10-06",][["Spells_less_then_lookback_period"]]
) == 0)

PER <- readRDS(paste0(tmp, "PERSONS2.rds"))

print(sum(PER[["person_id"]] %in% c("P009", "P010", "P008","P000a", "P011'")) == 0)

rm(FlowChart, PER)

#P009 Loockback less then 1 year to vaccin second spell, first spell no overlap with study
#P010 No overlap with study period
#p008 No overlap with study period by dead
#p000a not 365 days to end study
#p011 lees then 1 year between op_start and op end


print("Test if updating smokestaus is correct")

mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)
test <- dbGetQuery(mydb,"SELECT * FROM L_SMOKESTATUSALG_COV ")

#Subject P001a should not be in this table because it has rows with 0, -3 and characters 
print(any(sort(unique(test$person_id)) == c("P002a", "P002b")))
print(all(sort(unique(test$Voc)) == c("ICD10CM","NO_VOC|mo_meaning.cig/day","NO_VOC|mo_meaning.status")))


print("Test SUM varaible ")

print(M_Studycohort_Covariates_T0[person_id == "P001a",]$H_HOSPNUM_COV_T0 == 1)
print(M_Studycohort_Covariates_D3[person_id == "P001a",]$H_HOSPNUM_COV_D3 == 2)


print("Test start with ATC")
print(M_Studycohort_Covariates_T0[person_id == "P001a",]$DP_COVCANCER_T0 == 1)

