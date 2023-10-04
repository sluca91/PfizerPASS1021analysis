
if(!exists("projectFolder") | !exists("DAP")) rm(list=ls())  

#######################################################################################################
## Load packages
#######################################################################################################



if(!require(survival)){install.packages("survival")}
library(survival)
if(!require(stddiff)){install.packages("stddiff")}
library(stddiff)
if(!require(geeM)){install.packages("geeM")}
library(geeM)
if(!require(Hmisc)){install.packages("Hmisc")}
library(Hmisc)
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
if(!require(tableone)){install.packages("tableone")}
library(tableone)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
if(!require(survminer)){install.packages("survminer")}
library(survminer)
if(!require(cobalt)){install.packages("cobalt")}
library(cobalt)
if(!require(WeightIt)){install.packages("WeightIt")}
library(WeightIt)
if(!require(lubridate)){install.packages("lubridate")} 
library(lubridate) 
if(!require(parallel)){install.packages("parallel")} 
library(parallel)
if(!require(data.table)){install.packages("data.table")}
library(data.table)




#######################################################################################################





if(!exists("projectFolder") | !exists("DAP")){ 
  projectFolder <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  #StudyName <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  StudyName<- NULL
  path <- "/Users/acidroyo/Documents/GitHub/Astrazeneca-PASS-study/CDMInstances/RTI_SIM_CSV/" 
  #DAP <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  DAP <- "ARS"
  
  #start_study_date <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  start_study_date <- "20201201"
  #end_study_date <- 'TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN'
  end_study_date <- "20221231"
  
  if (start_study_date %in% c('TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN') | end_study_date %in% c('TO_BE_FILLED_IF_COMPUTER_CLOSED_AFTER_TO_RUN')){
    stop('Fill the Start study date or the End study date as you did in the to_run_file')
  }
  
  lookback_period <- 365
  max_spells_gap <- 365
  
  
  source(paste0(projectFolder,"/99_path.R"))
  
  
  #Load functions
  source(paste0(pre_dir,"functions/", "FUNCTIONS.R"))
  source(paste0(pre_dir,"functions/", "RUN_SCRIPT.R"))
  source(paste0(pre_dir,"functions/", "CreateBands.R"))
  source(paste0(pre_dir,"functions/", "IMPORT_PATTERN.R"))
  source(paste0(pre_dir, "functions/", "AppendConcepts.R"))
  source(paste0(pre_dir, "functions/", "GetDatesIR.R"))
  source(paste0(pre_dir,"functions/", "weightedASD_functions.R"))
  
  #Set parameters
  source(paste0(pre_dir,"Step_00_SetParameters.R"))
  source(paste0(pre_dir,"Step_00_SetCodeSheets.R"))
  
}

if(!DAP %in% c("TEST","CPRD","ARS","SIMG","PEDIANET","SIDIAP","EPICHRON","UOSL","PHARMO")) stop("Fill DAP with the correct word")


#######################################################################################################
## Specification of folder location
#######################################################################################################

#######################################################################################################
## Import required data
#######################################################################################################

## Import AESI information:
AESI_Cov_info <- fread(paste0(meta_dir,"Pfizer AESI_information.csv"), stringsAsFactors = F, na.strings = "")

## Exclude rows that should be ignored
AESI_info <- subset(AESI_Cov_info, Table == "AESI tables")
# AESI_info <- AESI_info[AESI_info$System != '' & AESI_info$Event_name != '' & AESI_info$Event_abbreviation != '' & !is.na(AESI_info$System) & !is.na(AESI_info$Event_name) & !is.na(AESI_info$Event_abbreviation),]

## Import M_Studycohort
SCRIPT <- RUN_SCRIPT(name = "Step_13_AddAESI.R")
#M_Studycohort3 <- readRDS(SCRIPT[["OUTPUT1"]][["path"]])
M_Studycohort <- readRDS(paste0(populations_dir,"M_Studycohort.rds"))

source(paste0(pre_dir,"functions/", "create_healthcarecontact_var.R"))  
M_Studycohort_Covariates_T0 <- Main()

## Load a function to cater for different format of AESI_info due to myo and pericarditis requests
source(paste0(pre_dir,"functions/", "reformat_mpc_AESI.R"))  

## create matched population for table 16SX and 20SX
source(paste0(pre_dir,"functions/", "create_matched_pop_t4.R"))  
d_matchedpop <- readRDS(paste0(populations_dir, "matched_pop.rds"))



#######################################################################################################
## Function Definition 
#######################################################################################################

plotkm_extra <- function(obj.surv, AESI, ddata, End_rw, opt,tit){
  ## This is the new function to plot the cumulative incidence using 
  ## Collect all informations with regard to the timing
  
  if(opt == 99){
    if (End_rw != "Any"){ End_rw <- as.numeric(End_rw)
    if(End_rw!= 365){
      maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 14) * 14
      break.time <- 14
    } else {
      maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 60) * 60
      break.time <- 60
    }
    } else {
      maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 60) * 60  
      break.time <- 60
    }
    
    if (AESI == "Myocarditis" | AESI == "Pericarditis" | AESI == "Myocarditis and pericarditis") {
      AESI <- paste0(AESI, " ", End_rw," days")
    }
    
    labels_list <- c("Unvaccinated", "Vaccinated")
    par(mar = c(2.5, 2.5, 1, 2), mgp = c(3.5, 1, 0), oma = c(1, 1, 1, 1), xpd = T)
    p <- ggsurvplot(obj.surv, data = ddata,
                    conf.int = TRUE,
                    conf.int.style = "step",
                    ggtheme = theme_classic(), # Change ggplot2 theme
                    #palette = "Dark2", # 
                    #palette = c("#FF9E29", "#86AA00"),
                    palette = c("dodgerblue3", "firebrick3"),
                    fun = function(y) (10000 * (1 - y)),
                    xlim = c(0, maxfup),
                    break.time.by = break.time,
                    ylab = "Cumulative Incidence (1-KM) \nper 10,000",
                    xlab = "Time (days)",
                    title = paste0(AESI," ( ",tit," )"),
                    legend.title = " ", 
                    legend.labs = labels_list ,
                    censor = FALSE,
                    risk.table = TRUE,
                    cumevents = T,
                    tables.height = 0.2,
                    risk.table.y.text.col = T, # colour risk table text annotations.
                    risk.table.y.text = T,
                    cumevents.y.text = T,
                    tables.theme = theme_cleantable(),
                    #risk.table.col = "strata",
                    font.x = c(12, "bold"), # "red"),
                    font.y = c(14, "bold"), # "darkred"),
                    font.tickslab = c(10, "plain"), # "darkgreen")),
                    fontsize = 3.5
    )
  } 
  
  return(p)
}


extra_fig <- function(M_Studycohort, AESI_info,opt){
  
  ListNames <- dir(aesi_dir)
  aesi_files <- ListNames[grep("_T0",ListNames)]
  
  ## A few basic checks on the data:
  if(nrow(M_Studycohort) == 0) stop('M_Studycohort is empty')
  if(any(!c('person_id', 'group', 'FIRST_OTHER', 'SECOND_OTHER', 'THIRD_OTHER', 'FOURTH_OTHER', 'FIRST_PFIZER', 'SECOND_PFIZER', 'THIRD_PFIZER', 'FOURTH_PFIZER', 'op_end_date') %in% colnames(M_Studycohort))) stop('Required columns are missing in M_Studycohort')
  if(nrow(AESI_info) == 0) stop('AESI_info is empty')
  if(any(!c('Event_abbreviation', 'Event_name', 'System', 'End_of_risk_window', 'lookback_period') %in% colnames(AESI_info)))  stop('Required columns in AESI_info not available.')
  
  
  d <- readRDS(paste0(populations_dir, "matched_pop.rds"))
  
  ## Start loop
  plot_list <- list()
  
  i<-1
  
  idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
  
  if (length(idx)==0) {print(paste0("The AESI: ",AESI_info$Event_abbreviation[i]," doesn't exist in the data.base"))} else {
    aesi_data <- as.data.frame(readRDS(paste0(aesi_dir,aesi_files[idx])))
    
    AESI <- AESI_info[i,1]
    AESI_name <- AESI_info[i,2]
    
    combined_data <- as.data.frame(merge(x = d, y = aesi_data, by = c("person_id","id"), all.x = TRUE))
    
    End_of_risk_window <- AESI_info$End_of_risk_window[i]
    lookback_period <- AESI_info$lookback_period[i]
    
    print(paste0(i," from ", nrow(AESI_info), " AESI's"))
    
    ## Check which pairs (!) to remove due to prior AESI ** In both sets, remove the pairs (!) in which one or both persons had the event previously.
    if(lookback_period == 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d'))])
      combined_data <- combined_data[!(combined_data$id %in% pairs.excl),]
    }
    if(lookback_period != 'Any'){
      pairs.excl <- unique(combined_data$id[!is.na(as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d')) & 
                                              as.Date(combined_data[,'Date_HIST'], format = '%Y-%m-%d') >= as.Date(combined_data$T0, format = '%Y-%m-%d') - days(lookback_period)])
      combined_data <- combined_data[!(combined_data$id %in% pairs.excl),]
    }
    
    if (nrow(combined_data) == 0) {warning(paste0("In computing AESI ", AESI, ", all subjects were excluded due to prior outcomes"))
      next}
    
    if (any(table(combined_data$group)) == 0) { warning(paste0('0 persons at either exposed or control group for ', AESI, '. AESI skipped.'))
      next}
    
    ## Check censoring due to end obs period, risk window or event (other vaccine already included above)
    ## Note: It is assumed that obs_end_dates can never be later than death_date (if not NA). If this is not correct then
    ## the code below will yield erroneous results.
    combined_data$End_rw <- NA
    if(End_of_risk_window != 'Any') combined_data$End_rw <- as.Date(combined_data$T0, format = '%Y-%m-%d') + days(as.numeric(End_of_risk_window)) - 1
    combined_data$endfup <- pmin(combined_data$op_end_date, combined_data[,'Date_COUNT'], combined_data$End_rw, na.rm = TRUE)
    if(any(is.na(combined_data$endfup))) stop('NA values in follow-up times; check data and code')
    combined_data$fupdays <- as.numeric(difftime(combined_data$endfup, as.Date(combined_data$T0, format = '%Y-%m-%d'), unit = 'days')) + 1 ## Note: 1 added in line with other tables to avoid 0 fupdays
    
    if(any(combined_data$fupdays < 1)) stop('Persons with 0 or a negative number of follow-up days in the data, check data or script')
    combined_data$event <- ifelse(!is.na(combined_data[,'Date_COUNT']) & as.Date(combined_data[,'Date_COUNT'], format = '%Y-%m-%d') == combined_data$endfup, 1, 0)
    
    
    ## this needs to be replaced. 
    
    d_total1 <- combined_data %>% select(person_id, id, group, fupdays, event,AGE_T0,L_SEX_COV )
    d_total2<-subset(d_total1,L_SEX_COV=="F" & AGE_T0>=15 & AGE_T0<=50)
    d_total3<-subset(d_total1,L_SEX_COV=="F" & AGE_T0>=20 & AGE_T0<=30)
    d_total4<-subset(d_total1,L_SEX_COV=="F" & AGE_T0>=50 & AGE_T0<=60)
    d_total5<-subset(d_total1,L_SEX_COV=="F" & AGE_T0>=65 & AGE_T0<=75)
    
    
    fit1 <- survfit(Surv(fupdays, event) ~ group, id = person_id, robust = T, data = d_total1)
    fit2 <- survfit(Surv(fupdays, event) ~ group, id = person_id, robust = T, data = d_total2)
    fit3 <- survfit(Surv(fupdays, event) ~ group, id = person_id, robust = T, data = d_total3)
    fit4 <- survfit(Surv(fupdays, event) ~ group, id = person_id, robust = T, data = d_total4)
    fit5 <- survfit(Surv(fupdays, event) ~ group, id = person_id, robust = T, data = d_total5)
    
    fig1 <- plotkm_extra(fit1, AESI_name, d_total1, End_of_risk_window, opt,tit="ALL")
    fig2 <- plotkm_extra(fit2, AESI_name, d_total2, End_of_risk_window, opt,tit="Female [15y - 50y]")
    fig3 <- plotkm_extra(fit3, AESI_name, d_total3, End_of_risk_window, opt,tit="Female [20y - 30y]")
    fig4 <- plotkm_extra(fit4, AESI_name, d_total4, End_of_risk_window, opt,tit="Female [50y - 60y]")
    fig5 <- plotkm_extra(fit5, AESI_name, d_total5, End_of_risk_window, opt,tit="Female [65y - 75y]")
    
    # 
    plot_list[[1]] <- fig1
    plot_list[[2]] <- fig2
    plot_list[[3]] <- fig3
    plot_list[[4]] <- fig4
    plot_list[[5]] <- fig5
    
    # 
    names(plot_list)[[1]] <- paste0(DAP,"_",AESI,"_ALL")
    names(plot_list)[[2]] <- paste0(DAP,"_",AESI,"_Femalle_15_50")
    names(plot_list)[[3]] <- paste0(DAP,"_",AESI,"_Femalle_20_30")
    names(plot_list)[[4]] <- paste0(DAP,"_",AESI,"_Femalle_50_60")
    names(plot_list)[[5]] <- paste0(DAP,"_",AESI,"_Femalle_65_75")
    
    
    # 
    rm(combined_data, AESI, d_total1, d_total2, d_total3, d_total4, d_total5, pairs.excl,fit1, End_of_risk_window, lookback_period, fig2)  
    
    
    #saveRDS(plot_list, file = paste0(output_dir, DAP, '_list_ExtraFig','.rds'))
    
    if(opt == 99){
      pdf.options(reset = TRUE)
      pdf(file = paste0(output_dir, DAP, "_ExtraFig.pdf", sep = ""))
      print(plot_list)
      dev.off()
    }
    
    
  }
  
  
  rm(plot_list)
}


#######################################################################################################
## EXTRA Figure 
#######################################################################################################

## Note: Figure 3 is the same as figure 2, but limited to COVID only and with 12 days as the end of the risk period
## So here, we simply edit the one row from AESI_info


AESI_info_copy <- reformat_mpc_AESI(AESI_info, opt = "multi")
AESI_info_copy <- AESI_info_copy[AESI_info_copy$Event_abbreviation == 'G_SECONDARYAMENORRHEA_AESI',]
extra_fig(M_Studycohort = M_Studycohort, AESI_info = AESI_info_copy,opt=99)
rm(AESI_info_copy)

