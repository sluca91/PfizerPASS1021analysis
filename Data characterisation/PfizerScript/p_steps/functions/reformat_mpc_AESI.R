reformat_mpc_AESI <- function(AESI_info, opt = "uni"){
  
  # The current csv files of Pfizer_AESI_COV_information might contains NA and different
  # string formats moreover, different table shells require different AESI, especially 
  # with the new requests of myo, peri and myopericarditis at different risk windows. 
  # This function caters all the information above
  
  AESI_info$Event_abbreviation[AESI_info$Event_abbreviation == "L_COVID_COV"] <- "I_COVID_COV"
  
  if (opt != "uni"){
    idx.myo <- grep("C_MYOCARD_AESI", AESI_info$Event_abbreviation); AESI_info$Event_abbreviation[idx.myo] <- "C_MYOCARD_AESI"
    idx.peri <- grep("C_PERICARD_AESI", AESI_info$Event_abbreviation); AESI_info$Event_abbreviation[idx.peri] <- "C_PERICARD_AESI"
    idx.mpc <- grep("C_MYOPERICARD_AESI", AESI_info$Event_abbreviation); AESI_info$Event_abbreviation[idx.mpc] <- "C_MYOPERICARD_AESI"
    AESI_info$lookback_period[is.na(AESI_info$lookback_period)] <- 365
  } else {
    AESI_info <- AESI_info[-c(grep("_14",AESI_info$Event_abbreviation), grep("_21", AESI_info$Event_abbreviation))]
    AESI_info$Event_abbreviation[grep("_7",AESI_info$Event_abbreviation)] <- gsub("_7","",AESI_info$Event_abbreviation[grep("_7",AESI_info$Event_abbreviation)])
    AESI_info$lookback_period[is.na(AESI_info$lookback_period)] <- 365
  }
  
  return(AESI_info)  
}