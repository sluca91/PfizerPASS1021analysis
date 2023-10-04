## The function to create variable that represents the healthcare contact within 7 days prior T0

Main <- function(){
  M_Studycohort_Covariates_T0 <- readRDS(paste0(populations_dir,"M_Studycohort_Covariates_T0.rds"))
  cov.vec <- c("H_HOSPNUM_COV_T0","H_EMERG_COV_T0","H_NURSING_COV_T0","H_PRIMARYHCUSE_COV_T0",
                          "TP_CANCERSCREEN_COV_T0","H_PREVENTIVE_COV_T0","TP_COVID19TEST_COV_T0")
  
  idx.cov <- which(colnames(M_Studycohort_Covariates_T0) %in% c("person_id","id", cov.vec))
  if (length(idx.cov) == 2){stop("Data on healthcare visit is not available")}
  
  M_Studycohort_Covariates_T0$Hosp_week <- as.integer(rowSums(M_Studycohort_Covariates_T0[,idx.cov[3:length(idx.cov)], with = FALSE]))
  
  ## Save it to be used in table 20 and figure 2. 
  return(M_Studycohort_Covariates_T0)
  
}

