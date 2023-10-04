## The new function for table 20. 

## Author   : Ivonne Martin (I.Martin@umcutrecht.nl)
## Date     : 12 Sep 2022; edited 24 Jan 2023

## In the version 21 Nov 2022, use the matched population as input as well as check whether the combined_data is null
## before proceeding the calculation.
## Include the weights from PS model
## Version 24 Jan 2023 : include the calculation of Anaphylaxis

## Internal functions of table 20 to calculate the hazard ratio and risk difference 
HR.surv <- function(AESI, obj.dat, End_of_risk_window, w.vec){ #when the w.vec = 1s for all subject, this function calculates the crude HR
  if (AESI != "ANAPHYLAXIS"){
    
    n.event <- as.numeric(c(sum(obj.dat$event[!is.na(obj.dat$event) & obj.dat$group == "EXPOSED"]),
                            sum(obj.dat$event[!is.na(obj.dat$event) & obj.dat$group == "CONTROL"])))
    
    if(prod(n.event) != 0) { # when at least one group has zero events, crude HR will not be calculated and return NA
      modPH.cr <- coxph(Surv(fupdays, event) ~ group + cluster(person_id), data = obj.dat, weights = w.vec, robust = TRUE)
      coxobj_summary <- summary(modPH.cr)$coefficients
      
      coef.PH <- coxobj_summary[1] 
      CI <- coef.PH + c(-1,1)*1.96*coxobj_summary[4]
      crude.vec <- round(exp(c(coef.PH, CI)),2)
    } else {
      crude.vec <- rep(NA,3)
      warning('At least one group of vaccination has zero event, NA is returned for hazard ratio and its CI')} 
  }# calculation of non anaphylaixs 
  
  
  if (AESI == "ANAPHYLAXIS"){
    
    #if(all(c(sum(obj.dat$event[obj.dat$group == "EXPOSED"]), sum(obj.dat$event[obj.dat$group == "CONTROL"])) == 0)){
    if(sum(obj.dat$event[obj.dat$group == 'EXPOSED']) == 0 | sum(obj.dat$event[obj.dat$group == 'CONTROL']) == 0){
      crude.vec <- rep(NA,3)
      warning("no ANAPHYLAXIS occur in each cohort, NAs will be returned")
    } else {
      obj.dat$ln.fupdays <- 0
      mA <- geem(event ~  1 + group + offset(ln.fupdays), id = person_id,  family = 'poisson', data = obj.dat[order(obj.dat$person_id),],
                 weights = w.vec)
      if(mA$converged == FALSE){ warnings('Poisson GEE does not converged, estimates for Anaphylaxis will be returned as NA')
        crude.vec <- rep(NA,3)
      } else {
        G_logIR <- as.numeric(coef(mA)[2])
        G_IR <- exp(G_logIR)
        G_IR.lb <- exp(as.numeric(G_logIR + qnorm(0.025) * summary(mA)$se.robust[2]))
        G_IR.ub <- exp(as.numeric(G_logIR + qnorm(0.975) * summary(mA)$se.robust[2]))
        crude.vec <- c(G_IR, G_IR.lb, G_IR.ub)
      }
    }
    
  }
  return(crude.vec)
}

adj.RD <- function(obj.dat, End_of_risk_window, AESI, w.vec){
  
  if (End_of_risk_window == "Any") {End_of_risk_window <- "365"}
  
  #if(AESI != 'ANAPHYLAXIS' & End_of_risk_window != 'Any' & max(obj.dat$fupdays) >= ifelse(End_of_risk_window != 'Any', 0 + as.numeric(End_of_risk_window), -Inf)){ 
  if(AESI != 'ANAPHYLAXIS' & max(obj.dat$fupdays) >= ifelse(End_of_risk_window != 'Any', 0 + as.numeric(End_of_risk_window), -Inf)){ 
    
    ## Note: If max(d$fupdays) < End_of_risk_window, we cannot determine the 1-KM at end_of_risk_window. So in that case, return NA
    
    if(!any(duplicated(obj.dat$person_id))){
      survf <- survfit(Surv(time = obj.dat$fupdays, event = obj.dat$event) ~ 1, weights = w.vec)								## Note that this does not work with 0 events (lb and up are both 1)
    }
    if(any(duplicated(obj.dat$person_id))){
      obj.dat2 <- obj.dat[order(obj.dat$person_id),]
      survf <- survfit(Surv(time = fupdays, event = event) ~ 1, id = person_id, data = obj.dat2, weights = w.vec,robust = T)		## Note that this does not work with 0 events (lb and up are both 1) + handling ofclustering needs verification
    }
    km <- summary(survf, times = as.numeric(End_of_risk_window))
    G_one_min_km <- (1 - km$surv) 
    rm(survf);rm(km)							
  } else {
    #G_one_min_km <- 1 - NA
    G_one_min_km <- NA
  }
  
  if (AESI == "ANAPHYLAXIS"){
    
    #if(all(c(sum(obj.dat$event[obj.dat$group == "EXPOSED"]), sum(obj.dat$event[obj.dat$group == "CONTROL"])) == 0)){
      if(sum(obj.dat$event[obj.dat$group == 'EXPOSED']) == 0 | sum(obj.dat$event[obj.dat$group == 'CONTROL']) == 0){
      G_one_min_km <- NA
    } else {
      obj.dat$ln.fupdays <- 0
      mA <- geem(event ~  1 + offset(ln.fupdays), id = person_id,  family = 'poisson', data = obj.dat[order(obj.dat$person_id),], weights = w.vec)
      if(mA$converged == FALSE){ warnings('Poisson GEE does not converged, values will be returned as NA')
        G_one_min_km <- NA
      } else {
        G_logIR <- as.numeric(coef(mA))
        G_IR <- exp(G_logIR)
        G_one_min_km <- G_IR
      }
    }
    
    
  }
  return(G_one_min_km)}

#########################################################################################################
## Function to create table 20
#########################################################################################################

fun_tab20 <- function(d_matchedpop, AESI_info, opt){
  
  d <- d_matchedpop
  AESI_info <- reformat_mpc_AESI(AESI_info, opt = "multi")  
  AESI_info <- AESI_info[-which(AESI_info$Event_abbreviation == "I_COVID_COV"),]
  if(nrow(AESI_info) == 0) stop('AESI_info is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
  
  ## IM added this
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
  
  
  if (opt == 1){
    propensity_var_file <- readRDS(paste0(populations_dir, "weights.rds"))
    propensity_var <- propensity_var_file %>% select(person_id, id, IPTW_full_cor_trim)
    d <- merge(x = d, y = propensity_var, by = c('person_id','id'), all.x = TRUE)
  } 
  
  
  ir.per.tab <- matrix(NA,nrow = nrow(AESI_info), ncol = 12)
  
  for(i in 1:nrow(AESI_info)){
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_info[i,1]
    AESI <- gsub(".*[_]([^.]+)[_].*", "\\1", AESI) 
    
    print(paste0("Computing AESI: ", AESI))
    
    #if (AESI == "ANAPHYLAXIS") next
    combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
    
    End_of_risk_window <- AESI_info$End_of_risk_window[i]
    lookback_period <- AESI_info$lookback_period[i]
    
    ## excluding pairs who have prior AESI
    if(lookback_period == 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data$Date_HIST, format = "%Y-%m-%d"))])
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]
    }
    if (lookback_period != 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d')) & as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period)]) 
      combined_data <- combined_data[!combined_data$id %in% pairs.excl,]      
    }
    
    if (nrow(combined_data) == 0) {warning(paste0("In computing AESI ", AESI, ", all subjects were excluded due to prior outcomes"))
      next}
    
    ## Defining the follow-up days and events, based on end of risk windows for each AESI  
    
    ## Check censoring due to end obs period, risk window or event (other vaccine already included above)
    ## Note: It is assumed that obs_end_dates can never be later than death_date (if not NA). If this is not correct then
    ## the code below will yield erroneous results.
    
    combined_data$End_rw <- NA
    if(End_of_risk_window != 'Any') combined_data$End_rw <- as.Date(combined_data$T0, format = '%Y-%m-%d') + days(as.numeric(End_of_risk_window)) - 1
    combined_data$endfup <- pmin(combined_data$op_end_date, combined_data[,'Date_COUNT'], combined_data$End_rw, na.rm = TRUE)
    if(any(is.na(combined_data$endfup))) stop('NA values in follow-up times; check data and code')
    combined_data$fupdays <- as.numeric(difftime(combined_data$endfup, as.Date(combined_data$T0, format = '%Y-%m-%d'), unit = 'days')) + 1 ## Note: 1 added in line with other tables to avoid 0 fupdays
    
    if(any(combined_data$fupdays < 1)) {warning('Persons with 0 or a negative number of follow-up days in the data, these entries will be removed')
      #combined_data <- combined_data[-which(combined_data$fupdays < 1),]
      idx.negative.fupdays <- which(combined_data$fupdays < 1)
      combined_data <- combined_data[-idx.negative.fupdays,]
    }
    
    combined_data$event <- ifelse(!is.na(combined_data[,'Date_COUNT']) & as.Date(combined_data[,'Date_COUNT'], format = '%Y-%m-%d') == combined_data$endfup, 1, 0)
    
    if (any(table(combined_data$group)) == 0) stop (paste0('0 persons in any of exposed or control for', AESI, '. AESI skipped.')) 
    
    
    
    d_E <- subset(combined_data, group == "EXPOSED")
    d_U <- subset(combined_data, group == "CONTROL")
    
    cr.HR <- HR.surv(AESI,combined_data, End_of_risk_window,w.vec = rep(1,nrow(combined_data)))
    cr.RD <- adj.RD(d_E,End_of_risk_window, AESI,w.vec = rep(1,nrow(d_E))) - adj.RD(d_U,End_of_risk_window, AESI, w.vec = rep(1,nrow(d_U))) 
    cr.RD <- round(cr.RD*10000, 2)
    
    if(opt == 1){
      
      ad.HR <- HR.surv(AESI,combined_data, End_of_risk_window, w.vec = combined_data$IPTW_full_cor_trim)
      ad.RD <- adj.RD(d_E,End_of_risk_window, AESI,w.vec = d_E$IPTW_full_cor_trim) - adj.RD(d_U,End_of_risk_window, AESI, w.vec = d_U$IPTW_full_cor_trim) 
      ad.RD <- round(ad.RD*10000, 2)
    }
    
    
    if (opt == 0){
      output <- c(cr.HR, rep(NA, 3), cr.RD, rep(NA,5))
    }
    if (opt == 1){
      output <- c(cr.HR, ad.HR, cr.RD, rep(NA,2), ad.RD, rep(NA,2))
    }
    
    ir.per.tab[i,] <- output 
    rm(combined_data,d_E, d_U, output)
    gc()
    #print(i)
  }    
  
  ## output  
  d20 <- data.frame(cbind( AESI_info$Event_name, ir.per.tab))
  
  AESI_Syst <- unique(AESI_info$System)
  sta.v <- sto.v <- d20.p <- NULL
  for (i in 1:length(AESI_Syst)){
    temp <- which(AESI_info$System == AESI_Syst[i])
    sta.v[i] <- min(temp)
    sto.v[i] <- max(temp)
    d20.p <- rbind(d20.p, c(AESI_Syst[i],rep(" ", 12)), d20[sta.v[i]:sto.v[i],])
  }
  
  
  colnames(d20.p) <- c("AESI","Cr.HR","Cr.HR.CI.lb","Cr.HR.CI.ub","Adj.HR","Adj.HR.CI.lb","Adj.HR.CI.ub",
                     "Cr.RD","Cr.RD.CI.lb","Cr.RD.CI.ub","Adj.RD","Adj.RD.CI.lb", "Adj.RD.CI.ub")
  return(d20.p)
}