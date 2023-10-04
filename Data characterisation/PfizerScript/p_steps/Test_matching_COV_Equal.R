#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/05/2022

##Aim
#Because the matching and co variate calculation use a different independent method for calculating the same prior T0 status, comparing the result is used to test
#the script.

##in/output
#Input 1: MATCH_PAIRS.rds
#Input 2: M_Studycohort_Covariates_T0.rds & M_Studycohort_Covariates_D3.rds
#Input 3: Dictionary_result.rds

#Output 1: Console results

#Load needed D3 output.
###
MATCH_PAIRS_spells <- readRDS(paste0(populations_dir, "MATCH_PAIRS.rds"))
M_Studycohort_Covariates_T0 <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_T0.rds"))
M_Studycohort_Covariates_T0_deleted <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_T0_deleted.rds"))
M_Studycohort_Covariates_T0 <- rbind(M_Studycohort_Covariates_T0, M_Studycohort_Covariates_T0_deleted)
setnames(M_Studycohort_Covariates_T0, "L_SEX_COV", "L_SEX_COV_T0")[, L_SEX_COV_T0 := as.integer(L_SEX_COV_T0) ]
M_Studycohort_Covariates_D3 <- readRDS(paste0(populations_dir, "M_Studycohort_Covariates_D3.rds"))
setnames(M_Studycohort_Covariates_D3, "L_SEX_COV", "L_SEX_COV_D3")[, L_SEX_COV_D3 := as.integer(L_SEX_COV_D3) ]
Dic <- readRDS(paste0(tmp, "Dictionary_result.rds"))
###


compareCovMatch <- function(Group, Cohort, suffix){
    
  if(suffix == "_D3"){
                    boolMergeX <- T
                    boolMergeY <- F
  }
  
  if(suffix == "_T0"){
    boolMergeX <- F
    boolMergeY <- T
  }
    
    for(i in c(MATCH_CAT, MATCH_TF, "L_SEX_COV")){
            
            #Group <- "Exposed"
            Var <- i 
            
            if(i %in%colnames(MATCH_PAIRS_spells)){
            
                  test <- merge(
                        x= copy(Cohort)[,c("person_id", "id", paste0(Var,suffix)), with = F],
                        y = copy(MATCH_PAIRS_spells)[,c(Group, "id", Var), with = F],
                        by.x = c("person_id", "id") ,
                          by.y = c(Group, "id"),
                          all.x = boolMergeX,
                          all.y = boolMergeY
                  )
                  
                  dicTmp <- Dic[VarName == i,][,.(integerVal, oriVal)]
                  if(nrow(dicTmp) > 0){
                    
                    
                    if(nrow(dicTmp) == 1 & is.na(dicTmp$integerVal[1])){
                      test <- test[, eval(paste0(Var,suffix)) := as.character(get(paste0(Var,suffix)))][get(paste0(Var,suffix)) == "0", eval(paste0(Var,suffix)) := "UNK"]
                    }else{
                      test <- merge(test, dicTmp, by.x = paste0(Var,suffix), by.y = "integerVal", all.x = T)[get(paste0(Var,suffix)) == 0, oriVal := "UNK"]
                      test <- test[, eval(paste0(Var,suffix)) := NULL][, eval(paste0(Var,suffix)) := oriVal]
                    }
                  }
                  
                  
                
                  test <- test[get(Var) == "MISSING", eval(Var):= "UNK"]
                  test <- test[get(paste0(Var,suffix)) == get(Var), check := T ][!is.na(person_id),][is.na(check), check := F ]
                  
                  print(paste0(Var," = ",sum(test$check)/nrow(test)))
                  
                  rm(test, dicTmp)
                  
            }else{print(paste0(i," not as matching variable"))}
    
    }


}

#Check 
print("Result should be 1")
compareCovMatch(Group = "Exposed", Cohort = M_Studycohort_Covariates_T0, suffix = "_T0")
print("Result should be 1")
compareCovMatch(Group = "Control", Cohort = M_Studycohort_Covariates_T0, suffix = "_T0")
print("Result should be < 1 and L_SEX_COV = 1")
compareCovMatch(Group = "Exposed", Cohort = M_Studycohort_Covariates_D3, suffix = "_D3")

rm(MATCH_PAIRS_spells, M_Studycohort_Covariates_D3, Dic)

#Check if all variables in the study are captured if a subject was sampled into the data
if(file.exists(paste0(path_dir, "VACCINES_sample_all.csv"))){
  
  #Get the person_id from the sampled subject
  pid <- unique(fread(paste0(path_dir, "VACCINES_sample_all.csv"))[["person_id"]])
  
  #Get this subject from the co variate file. 
  aatest2 <- M_Studycohort_Covariates_T0[person_id == pid, ]
  
  #Check if all co variates are in the file. Sex is an exception so is excluded from this check for now
  colls <- colnames(M_Studycohort_Covariates_T0)
  colls <- substr(colls, 1,nchar(colls) - 3) 
  colls <- COV[!COV %in% colls]
  colls <- colls[!colls %in% "L_SEX_COV"]
  if(length(colls) > 0) print(paste0(paste0(colls, collapse = ","), " are not in the file"))
  rm(colls)
  
  #Check if all the co variates are not missing for the sampled subject. death any is an exeption and is excluded for now
  colls <- colnames(as.data.frame(aatest2)[1,  which(aatest2[1,] == 0)  ])
  colls <- colls[!colls %in% "O_DEATHANY_AESI_T0"]
  if(length(colls) > 0) print(paste0(paste0(colls, collapse = ","), " are 0 for the subject that should be > 1 because for this subject all study variables are sampled in 5 days before T0"))
  rm(colls)
  
  #Check if all aesi files are in the folder. death is an exeption and for now not tested.
  colls <- unique(substr(list.files(aesi_dir), 1,nchar(list.files(aesi_dir)) - 7)) 
  colls <- AESI[!AESI %in% colls]
  colls <- colls[!colls %in% "O_DEATHANY_AESI"]
  if(length(colls) > 0) print(paste0(paste0(colls, collapse = ","), " have no file in AESI while all files should be there with at least 1 row"))
  rm(colls)
  
  
  rm(aatest2, pid, M_Studycohort_Covariates_T0)
}



 