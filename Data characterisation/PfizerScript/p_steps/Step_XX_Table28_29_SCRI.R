##SCRI : 

# Three functions : format the scri data, perform the analysis, sensitivity 
# when formatting the data : needs to take care the following : 
# 1. M_Studycohort -> unselect the CONTROL
# 2. for each AESI, we join the M_Studycohort and AESI files
# 3. Exclude subjects with previous AESI
# 4. Format the SCRI : create risk, control, washout, start and end periods depending on the 
#    option : whether main analysis or sensitivity, if it is sensitivity then ask for which dose. 
#.   4.a. For the main analyses : consider only the first and second dose. 
#.   4.b. For the sensitivity analyses: consider the first, second and third dose. But Third dose has not been calculated here 
# 5. Create the function of estimating the relative incidence. 


Main.SCRI <- function(cdata, End_rw){
  End_rw <- as.numeric(End_rw)
  temp.end <- as.numeric(pmax(cdata$aedrug1, cdata$aedrug2, na.rm = TRUE))
  cdata$aend <- temp.end + as.numeric(End_rw) + 7
  
  # if event occur within the SCRI observation period for specific subjects then indicator == 1, i.e. to be included  
  indicator <- ifelse(pmin(cdata$aevent, cdata$aend) == cdata$aevent, 1,0)
  if(length(which(indicator == 0)) != 0){
    cdata <- cdata[-which(indicator == 0),]} else {cdata <- cdata}
  
  check.overlap <- function(data, ctr.period){
    # check whether the combined data has overlap,
    # if overlap, then create the new end of observation period. 
    # else, keep the end as before. 
    data$over.indic <- ifelse((data$adrug2 - data$adrug1 > ctr.period), 1, 0)
    len.rw1 <- data$adrug2 - data$adrug1
    data$rw.combined <- rep(ctr.period*2, nrow(data))
    data$rw.combined[which(data$over.indic == 0)] <- len.rw1[which(data$over.indic == 0)] + ctr.period
    data$aend[which(data$over.indic == 0)] <- data$aedrug2[which(data$over.indic == 0)] + 7 + data$rw.combined[which(data$over.indic == 0)]
    idx.sel <- which((data$aevent >= data$adrug1 & data$aevent <= data$aedrug1) | 
                       (data$aevent >= data$adrug2 & data$aevent <= data$aedrug2) | 
                       (data$aevent >= data$aedrug2 + 8 & data$aevent <= data$aend))
    data <- data[idx.sel, ]
    
    data$event.indic <- ifelse((data$aevent >= data$adrug1 & data$aevent <= data$aedrug1) | 
                                 (data$aevent >= data$adrug2 & data$aevent <= data$aedrug2),1,2)
    data$ctr.dur <- data$rw.combined
    ndata <- data[,c('person_id.tr','rw.combined','ctr.dur','event.indic')]
    ndata.long <- melt(setDT(ndata), id = c("person_id.tr",'event.indic'), variable.names = 'duration')
    ndata.long$loginterval <- log(ndata.long$value)
    ndata.long$event <- ifelse((ndata.long$variable == "rw.combined" & ndata.long$event.indic == 1) | 
                                 (ndata.long$variable == "ctr.dur" & ndata.long$event.indic == 2) ,1,0)
    ndata.long$adrug <- ifelse(ndata.long$variable == "rw.combined", 1, 0) 
    return(ndata.long)  
  }
  
  ndat.scri.long <- check.overlap(cdata, End_rw)  
  
  
  return(ndat.scri.long)
}


Strat.SCRI <- function(data, End_rw, dose){
  
  End_rw <- as.numeric(End_rw)
  temp.end <- as.numeric(pmax(data$aedrug1, data$aedrug2, data$aedrug3, na.rm = TRUE))
  # 1. define the distance matrix between event and each start dates of each vaccination round 
  Mat.dist <- data$aevent - data[,c('adrug1','adrug2','adrug3')]
  
  # 2. define the distance matrix between event and each
  Mat.dist.drug <- t(apply(data[,c('adrug1','adrug2','adrug3')], 1, function(x) diff(x)))
  
  # 3. Define the length of risk window for each dose 
  len.rw <- pmin(Mat.dist.drug[,dose], End_rw, na.rm = TRUE)
  
  # 4. The control period will start from the end of the aedrug of last available vaccination round. 
  aend.v <- temp.end + 7 + len.rw
  
  # 5. Define the indicator whether event is included in the analysis, i.e. if event lies in the rw or cw 
  
  event.in <- which((Mat.dist[,dose] >= 0 & Mat.dist[,dose] <= len.rw) | ((Mat.dist[,dose] >= temp.end + 7 ) & Mat.dist[,dose] <= aend.v))
  
  
  data$event.indic <- ifelse((Mat.dist[,dose] >= 0 & Mat.dist[,dose] <= len.rw) ,1,2)
  data$rw.dur <- len.rw
  data$ctr.dur <- data$rw.dur
  ndata <- data[event.in,c('person_id.tr','rw.dur','ctr.dur','event.indic')]
  ndata.long <- melt(setDT(ndata), id = c("person_id.tr",'event.indic'), variable.names = 'duration')
  ndata.long$loginterval <- log(ndata.long$value)
  ndata.long$event <- ifelse((ndata.long$variable == "rw.dur" & ndata.long$event.indic == 1) | 
                               (ndata.long$variable == "ctr.dur" & ndata.long$event.indic == 2) ,1,0)
  ndata.long$adrug <- ifelse(ndata.long$variable == "rw.dur", 1, 0) 
  return(ndata.long)}

print.result <- function(data){ # They actually can be collapsed into no condition. Should be done later. 
    n.event.rw <- sum(data$event[which(data$variable != "ctr.dur")])
    n.event.cw <- sum(data$event[which(data$variable == "ctr.dur")])
    total.time.rw <- sum(data$value[which(data$variable != "ctr.dur")])
    total.time.cw <- sum(data$value[which(data$variable == "ctr.dur")])
    
    if(any(c(n.event.rw, n.event.cw) == 0)) {
      warning("Complete separation, estimate will be Inf") # For programmer's sanity
      est.vec <- c(1,-1,1)*Inf
      est.adj <- c(NA,NA,NA)
    } else {
      mod <- clogit(event ~ adrug + strata(person_id.tr) + offset(loginterval), data = data, control=coxph.control(iter.max=10000))
      est.vec <- summary(mod)$coef[c(1,3)]
      est.adj <- c(NA,NA,NA)
    }
   vec.val <- c(n.event.rw, total.time.rw, n.event.cw, total.time.cw,exp(est.vec[1]), exp(est.vec[1] + c(-1,1)*1.96*est.vec[2]), est.adj) 
  
  return(vec.val)
}

fun_tab29 <- function(d_matchedpop){
  
## load the table containing AESIs to be analyzed with SCRI main and stratify analysis   
## Also to correct for lookback period or length of risk window 
  
  AESI_SCRI <- fread(paste0(meta_dir,"Pfizer_AESI_SCRI.csv"), stringsAsFactors = F, na.strings = "")
  AESI_SCRI <- subset(AESI_SCRI, SCRI == 1)
  AESI_SCRI$CONTROL_window[which(AESI_SCRI[,1] == "Im_ANAPHYLAXIS_AESI")] <- 18
  AESI_SCRI$lookback_period[is.na(AESI_SCRI$lookback_period)] <- 365
  AESI_SCRI$Event_abbreviation[which(AESI_SCRI$Event_abbreviation %in% 
                                       c("C_MYOCARD_AESI_14","C_PERICARD_AESI_14")) ] <- 
    c("C_MYOCARD_AESI","C_PERICARD_AESI")  
  
  d <- subset(M_Studycohort, group != "CONTROL")
  #AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
  #AESI_info <- AESI_info[-which(AESI_info$Event_abbreviation == "I_COVID19VAED_AESI"),]
  
  ## IM added this
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', "FOURTH_OTHER", 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_OTHER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  if(nrow(AESI_SCRI) == 0) stop('AESI_SCRI is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period', 'CONTROL_window') %in% colnames(AESI_SCRI)))  stop('Required columns in AESI_info not available.')
  
  # Defining matrix for main analysis 
  Mat.Est <- Mat.Est.D1 <- Mat.Est.D2 <- matrix(NA, nrow = nrow(AESI_SCRI),ncol = 10)
  
  for(i in 1:nrow(AESI_SCRI)){
    idx <- grep(tolower(as.character(AESI_SCRI$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_SCRI[i,1]
    AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI) 
    
    #if (AESI == "ANAPHYLAXIS") next
    combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
    
    End_of_risk_window <- AESI_SCRI$End_of_risk_window[i]
    lookback_period <- AESI_SCRI$lookback_period[i]
    
    ## excluding pairs who have prior AESI
    if(lookback_period == 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data$Date_HIST, format = "%Y-%m-%d"))])
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]
    }
    if (lookback_period != 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d')) & as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period)]) 
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]      
    }
    
    ## 1. From the complete combined_data removing those with cases 
    
    idx.presence <- which(!is.na(combined_data$Date_COUNT))
    if(length(idx.presence) == 0) next
    
    combined_data <- combined_data[which(!is.na(combined_data$Date_COUNT)),]
    
    # 2. removing subjects with NA first Pfizer 
    
    combined_data <- combined_data[which(!is.na(combined_data$FIRST_PFIZER)),]
    
    ## 3. Formatting SCRI cohort according to SCCS package
    
    combined_data$astart <- combined_data$adrug1 <- rep(0, nrow(combined_data))
    combined_data$adrug2 <- as.numeric(difftime(as.Date(combined_data$SECOND_PFIZER),as.Date(combined_data$FIRST_PFIZER), units = c("days")))
    combined_data$adrug3 <- as.numeric(difftime(as.Date(combined_data$THIRD_PFIZER),as.Date(combined_data$FIRST_PFIZER), units = c("days")))
    combined_data$aevent <- as.numeric(difftime(as.Date(combined_data$Date_COUNT),as.Date(combined_data$FIRST_PFIZER), units = c("days")))
    combined_data$end_date <- as.numeric(difftime(as.Date(combined_data$op_end_date), as.Date(combined_data$FIRST_PFIZER), units = c("days")))
    combined_data$person_id.tr <- c(1:nrow(combined_data))
    
    ## 4. EXTRA CHECK: be careful that Date_COUNT might still occur before the first vaccination. 
    if(length(which(combined_data$aevent < 0)) != 0){
      combined_data <- combined_data[-which(combined_data$aevent < 0),] } else {combined_data <- combined_data} 
    
    dur.rw <- as.numeric(End_of_risk_window) - 1
    combined_data$aedrug1 <- rep(dur.rw,nrow(combined_data))
    combined_data$aedrug2 <- combined_data$adrug2 + dur.rw
    combined_data$aedrug3 <- combined_data$adrug3 + dur.rw
    
    # ------------------------------------
    # -----  main analysis starts here
    # ------------------------------------  
    
     
    dat.scri.main <- Main.SCRI(combined_data, as.numeric(End_of_risk_window))  
   
    Mat.Est[i, ] <- print.result(dat.scri.main)
  
  # ------------------------------------
  # -----  main analysis stops here
  # ------------------------------------  
    dat.scri.strat <- Strat.SCRI(combined_data, End_of_risk_window, dose = 1)  
    Mat.Est.D1[i, ] <- print.result(dat.scri.strat)
    
    dat.scri.strat2 <- Strat.SCRI(combined_data, End_of_risk_window, dose = 2)  
    Mat.Est.D2[i, ] <- print.result(dat.scri.strat2)
    print(i)
  }
    # Note when the model does not converge, what should we write in the table?
  
  
  Mat.Est.Strat <- rbind(Mat.Est.D1, Mat.Est.D2)
  
  colnames(Mat.Est) <- colnames(Mat.Est.Strat) <- c('n.rw','total.rw','n.cw','total.cw','cr.est','cr.est.lower','cr.est.upper',
                         'adj.est','adj.lower','adj.upper')
  d28 <- cbind(AESI_SCRI[,1:2],Mat.Est)
  d29 <- cbind(rbind(AESI_SCRI[,1:2], AESI_SCRI[,1:2]), Mat.Est.Strat)
  return(list('d28' = d28,'d29' = d29))
}

