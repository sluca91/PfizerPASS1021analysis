M_Studycohort <- readRDS(paste0(populations_dir,"M_Studycohort2.rds"))
#saveRDS(M_Studycohort, "C:/test.rds")




##Overal cencoring criteria
#Another covid vaccin or a second vaccin of another brand (is the latter correct mentioned oin the SAP??)
#Op_end_date ends 
#Death 

TEMP <- M_Studycohort[,.(person_id ,T0,ALI,op_end_date, id,group, FIRST_PFIZER,FIRST_OTHER, SECOND_OTHER)][group != "UNMATCHED",]
TEMP <- TEMP[group == "EXPOSED", ':=' (FIRST_PFIZER = NA, FIRST_OTHER = NA)]
TEMP <- TEMP[group == "CONTROL", ':=' (SECOND_OTHER = NA)]
TEMP <- TEMP[, FU_END := min(op_end_date, FIRST_PFIZER, FIRST_OTHER, SECOND_OTHER, na.rm = T), by = id]

AESI <- "ALI"
Risk_window_lower <-  1
Risk_window_upper <- 42

##Exclude pair if
#Control or exposed have an AESI 365 days prior
#Contact with healthcare system 7 days prior (functions for this available. covariate or matrix)

##Cencoring when
#Riskwindow reached
#Occurence of the AESI

TEMP2 <- TEMP[, eval(paste0("FU_END_",AESI)) := min(FU_END, (T0 + Risk_window_upper), get(AESI), na.rm = T),  by = id]











