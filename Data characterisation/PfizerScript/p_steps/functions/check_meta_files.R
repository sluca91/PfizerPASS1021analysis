

check_meta_files <- function(){
  SCRIPT <- RUN_SCRIPT(name = "Step_00_SetCodeSheets.R")
  
  if(nrow(TEMP1[Algorithm == T & Algorithm_input == T,]) > 0) warning("Some variables are an input as well as an algorithm result")
  
  alginp_not_in_sv <- unique(TEMP2[["CONCEPT"]][!toupper(TEMP2[["CONCEPT"]]) %in% toupper(TEMP1[["VarName"]])])
  alginp_in_sv <- unique(TEMP2[["CONCEPT"]][toupper(TEMP2[["CONCEPT"]]) %in% toupper(TEMP1[["VarName"]])]) 
  
  if(length(alginp_not_in_sv) > 0) print(paste0("The following algorithm inputs are not defined in study variables: " ,paste0(alginp_not_in_sv, collapse = ", ")))
  
  alg_not_in_sv <- unique(TEMP2[["NEW_CONCEPT"]][!toupper(TEMP2[["NEW_CONCEPT"]]) %in% toupper(TEMP1[Algorithm == T,][["VarName"]])])
  alg_in_sv <- unique(TEMP2[["NEW_CONCEPT"]][toupper(TEMP2[["NEW_CONCEPT"]]) %in% toupper(TEMP1[Algorithm == T,][["VarName"]])]) 
  
  if(length(alg_not_in_sv) > 0) print(paste0("The following algorithm inputs are not defined in study variables: " ,paste0(alg_not_in_sv, collapse = ", ")))
  
  
  concepts_specified <- unique(c(
    unique(toupper(fread(SCRIPT[["INPUT1"]][["path"]])[, full_name := paste0(system,"_", event_abbreviation, "_", type)][["full_name"]])), 
    unique(toupper(fread(SCRIPT[["INPUT2"]][["path"]])[["StudyVar"]])),
    unique(toupper(fread(SCRIPT[["INPUT3"]][["path"]])[["StudyVar"]])),
    unique(toupper(fread(SCRIPT[["INPUT4"]][["path"]])[["drug_abbreviation"]]))
  ))
  
  concepts_needed <- unique(toupper(c(TEMP1[Algorithm == F & PERSON_CDM == F,][["VarName"]],
                                      alginp_in_sv
  )))
  
  if(!length(unique(c(MATCH_CONCEPTS,NMATCH_CONCEPTS))) == length(concepts_needed)) warning("Not all needed concepts in MATCH_CONCEPTS OR NMATCH_CONCEPTS")
  
  missing_concepts_specified <- concepts_needed[!concepts_needed %in% concepts_specified]
  
  if(length(missing_concepts_specified) > 0) print(paste0("The following concepts are not specified and need attention: " ,paste0(missing_concepts_specified, collapse = ", ")))
  
  
  alg_affected <- toupper(unique(TEMP2[CONCEPT %in% missing_concepts_specified,][["NEW_CONCEPT"]]))
  
  if(length(alg_affected) > 0) print(paste0("The missing concepts also affect the algortihms: " ,paste0(alg_affected, collapse = ", ")))
  
  #missing_concepts_specified[!missing_concepts_specified %in% yesterday]
  #yesterday[!yesterday %in% missing_concepts_specified]
  
  ###
  
}