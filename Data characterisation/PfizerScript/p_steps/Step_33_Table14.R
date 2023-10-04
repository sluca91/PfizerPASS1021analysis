### Author(s): Ivonne Martin (i.martin@umcutrecht.nl)
### Date: 29 July 2022

	## For development:
	# The table with absolute standardized difference is created using tableone package. 

#########################################################################################################
## Function to create table 14
#########################################################################################################

	## Start function
fun_tab14 <- function(M_Studycohort, AESI_info){
  
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "uni")
  AESI_info <- AESI_info[-which(AESI_info$Event_abbreviation == "C_MYOPERICARD_AESI"),]
  AESI_info <- AESI_info[-which(AESI_info$Event_abbreviation == "I_COVID19VAED_AESI"),]
  
  
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  aesi_files_test <- tolower(gsub("_T0.rds","",ListNames[grep("_T0",ListNames)]))
  
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', 'FOURTH_OTHER', 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_OTHER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  if(nrow(AESI_info) == 0) stop('AESI_info is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
  
  ## Select all vaccinated (ie before matching)
  MS <- subset(M_Studycohort, group != 'UNMATCHED')
  rownames(MS) <- NULL
  
  ## More data checks
  if(nrow(MS) == 0) 						stop('0 persons in M_Studycohort with group == EXPOSED or CONTROL')
  if(any(duplicated(MS$person_id[MS$group == 'Exposed'])))	stop('Duplicated person_id values should not be possible in the exposed population.')
  
  
  ## Checking whether some AESI files are not presents. 
  if(any(!c(tolower(AESI_info$Event_abbreviation)) %in% aesi_files_test)) warning ('Some AESI files are missing, the corresponding AESI will be NA')
  
  idx.excl <- which(!c(tolower(AESI_info$Event_abbreviation)) %in% aesi_files_test)
  
  ## Procedure to handle this problem
  
  d <- MS[,c('person_id','id','group','T0','FIRST_PFIZER')]
  combined_data <- as.data.frame(d)
  
  i = 1
  while(i <= nrow(AESI_info)){
    
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) { 
      AESI <- AESI_info[i,1]
      aesi_data <- cbind(combined_data[,c(1:2)],aesi_temp = rep(3,nrow(combined_data)))
      colnames(aesi_data)[3] <- paste0(AESI,"T0_HIST")
      combined_data <- merge(x = combined_data, y = aesi_data, by = c("person_id","id"), all.x = TRUE)
      rm(AESI); rm(aesi_data)
      i <- i + 1
      next }
    
    
    AESI <- AESI_info[i,1]
    aesi_data <- readRDS(paste0(aesi_dir,aesi_files[idx]))
    colnames(aesi_data)[3] <- paste0(AESI,"T0_HIST")
    aesi_data <- aesi_data[,-4]
    combined_data <- merge(x = combined_data, y = aesi_data, by = c("person_id","id"), all.x = TRUE)
    
    lookback_period <- AESI_info$lookback_period[i]
    
    if(lookback_period == 'Any'){
      combined_data[, paste0(AESI,"T0_HIST")] <- as.factor(!is.na(as.Date(combined_data[,paste0(AESI,"T0_HIST")], format = '%Y-%m-%d')))
    }
    if(lookback_period != 'Any'){
      combined_data[, paste0(AESI,"T0_HIST")] <- as.factor(!is.na(as.Date(combined_data[,paste0(AESI,"T0_HIST")], format = '%Y-%m-%d')) & 
                                                             as.Date(combined_data[,paste0(AESI,"T0_HIST")], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period))
    }
    
    
    rm(AESI); rm(aesi_data); rm(lookback_period)
    i <- i + 1
    #print(i)
  }
  
  vars <- colnames(combined_data)[grep("HIST",colnames(combined_data))]
  
  combined_data$group <- relevel(as.factor(combined_data$group), ref = "EXPOSED")
  TabUnmatched <- CreateTableOne(vars = vars, strata = "group", factorVars = vars, smd = TRUE, data = combined_data, test = FALSE)
  
  T14 <- print(TabUnmatched, noSpaces = TRUE,smd = TRUE,formatOptions = list(scientific = FALSE))
  
  # Fill - in the missing AESI 

  create.output <- function(obj){
    Mat.Val <- matrix(NA,nrow = length(obj), ncol = 2)
    for (i in 1:length(obj)){
      if(all((dim(obj[[i]]) != 2) & any(!obj[[i]]['level'] == TRUE))) {
        val.n <- diff(as.numeric(obj[[i]][c(5,1)]))
        val.perc <- as.numeric(val.n/obj[[i]][1])
        out.val <- c(val.n, val.perc)
      } else {
        out.val <- obj[[i]][which(obj[[i]]$level == "TRUE"),5:6]
      }
      Mat.Val[i,] <- round(as.numeric(out.val),2)
      }
      rownames(Mat.Val) <- names(obj)
      return(Mat.Val)
      }
   
  
  
  d14 <- cbind(create.output(TabUnmatched$CatTable$EXPOSED), create.output(TabUnmatched$CatTable$CONTROL),T14[2:nrow(T14),3] )
  
  if (length(idx.excl) != 0) {
    d14[idx.excl,] <- NA
  }
  
  d14.p <- d14 
  d14.p[,c(1,3)] <- apply(d14.p[,c(1,3)], c(1,2), function(x) 
    x <- ifelse(as.numeric(x) < 5 & as.numeric(x)!= 0 & !is.na(as.numeric(x)), '<5', as.numeric(x)))
  
  d14.p <- rbind(c(T14[1,1], 100, T14[1,2],100, " "),d14.p)
  d14.p <- cbind(c("Total",AESI_info$Event_name),d14.p)
  d14.p <- d14.p[-which(d14.p[,1] %in% c("Death (any causes)","Sudden death")),]
  colnames(d14.p) <- c("PriorAESI","vac.n", "vac.perc", "ctr.n", "ctr.perc","ASD")
  
  return(d14.p)
}



