### Author(s): Rutger van den Bor (r.m.vandenbor@umcutrecht.nl) and Roel Elbers (R.J.H.Elbers@umcutrecht.nl); Ivonne Martin (I.Martin@umcutrecht.nl)
### Date: 11 Mar 2022; edited for 2nd interim : 3 Aug 2022; 
### 3rd interim: 29 Nov 2022

## For development:
#  dat.loc <- '.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_intermediate/populations/'
# M_Studycohort3 <- as.data.frame(readRDS(paste0(dat.loc, 'M_Studycohort3.rds')))
#  AESI_info <- read.csv('.../GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/p_meta_data/Pfizer AESI_information.csv', sep = ';')
#	library(lubridate)
#library(survival)

## For second interim, the format of M_Studycohort and AESI are different from the 1st interim
## Version Sep 12 2022, eliminate redundancy, use the dataset that has been formatted in table 16. 
## version Jul 21 2023: adding png output

#########################################################################################################
## Function to create figure 2
#########################################################################################################

## Internal function used to create the figures
plotkm_new <- function(obj.surv, AESI, ddata, End_rw, opt){
  ## This is the new function to plot the cumulative incidence using 
  ## Collect all informations with regard to the timing
  
  if(opt == 2){
    if (End_rw != "Any"){ End_rw <- as.numeric(End_rw)
      if(End_rw!= 365){
        if(End_rw == 183) {
          maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 30) * 30
          break.time <- 30  
        } else {
        maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 7) * 7
        break.time <- 7}
      } else {
        maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 60) * 60
        break.time <- 60
      }
    } else {
      # if end_rw is "any" then calculate x breaks as a function of maxfup
      # ensure there are 7 break points
        maxfup <- ceiling(max(ddata$fupdays, na.rm = T) / 60) * 60  
        break.time <- maxfup/6
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
                    title = AESI,
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
    
    # if(End_rw == "Any" | End_rw == 365){
    #   a <- p
    #   
    #   # extract ggplot object from ggsurvplot
    #   p1 <- a$plot 
    #   p1 <- p1 + scale_x_continuous(breaks = c(0,61,122,182,243,304,365))
    #   
    #   # extract table object from ggsurvplot
    #   tab <- a$table
    #   tab$layers = NULL # clear labels
    #   tab <- tab + 
    #     geom_text(aes(x = time, y = rev(strata), label = llabels), data = tab$data[tab$data$time %in%  c(0,61,122,182,243,304,365),]) +
    #     scale_x_continuous(breaks = c(0,61,122,182,243,304,365))
    #   
    #   # extract cumevents object from ggsurvplot
    #   tab2 <- a$cumevents
    #   tab2$layers = NULL # clear labels
    #   tab2 <- tab2 + 
    #     geom_text(aes(x = time, y = rev(strata), label = cum.n.event), data = tab$data[tab$data$time %in%  c(0,61,122,182,243,304,365),]) +
    #     scale_x_continuous(breaks = c(0,61,122,182,243,304,365))
    #   
    #   
    #   # Add plots back
    #   a$plot <- p1
    #   a$table <- tab
    #   a$cumevents <- tab2
    #   
    #   p <- a
    # }
  } 
  
  if(opt == 3){
    End_rw <- as.numeric(End_rw)
    maxfup <- End_rw
    break.time <- 2
    
    labels_list <- c("Unvaccinated", "Vaccinated")
    
    df <- broom::tidy(obj.surv) 
    fit_df <- df  %>% 
      #group by strata
      group_by(strata) %>% 
      #get day  of interest 
      filter(time == 12) %>% 
      arrange(time) %>% 
      # pulls last date
      do(tail(.,3)) # risk difference at 3 decimal places
    
    # cumulative difference between vaccinated and unvaccinated
    dif <- (fit_df$estimate[1]-fit_df$estimate[2])
    if (dif >= -0.001 & dif <= 0.001) {
      dif.val <- '< 0.001'
    } else { dif.val <- base::abs(round(dif,3))}
    
    vec.neg <- c(fit_df$estimate[1], fit_df$estimate[2], dif)
    ## save the result as an input for tables 20 and 21
    # this is the risk in both groups and their difference
    saveRDS(vec.neg, paste0(propensity_dir,"Figure3RD.rds"))
    
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
                    title = AESI,
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
    
    p$plot <- p$plot + geom_vline(xintercept = 12, lty=2) +
      # adding the text with the difference   
      geom_text(aes(x = 11,y=10,label = paste0("Risk difference \nat day 12 = " ,dif.val) ),size=2.5)
    
  }
  return(p)
}

## main function

fun.fig2 <- function(M_Studycohort, AESI_info, opt){
  
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
  for(i in 1:nrow(AESI_info)){
    
    idx <- grep(tolower(as.character(AESI_info$Event_abbreviation[i])),tolower(aesi_files))
    if (length(idx) == 0) next
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
    
    d_total <- combined_data %>% select(person_id, id, group, fupdays, event)
    fit1 <- survfit(Surv(fupdays, event) ~ group, id = person_id, robust = T, data = d_total)
    
    fig2 <- plotkm_new(fit1, AESI_name, d_total, End_of_risk_window, opt)

    plot_list[[i]] <- fig2
    # 
    names(plot_list)[[i]] <- paste0(DAP,"_",AESI)
     pdf(file = paste0(output_dir, DAP, "_Fig",opt,"_", i, ".pdf", sep = ""))
     print(fig2, newpage = FALSE)
     dev.off()
     png(file = paste0(output_dir, DAP, "_Fig",opt,"_", i, ".png", sep = ""))
     print(fig2, newpage = FALSE)
     dev.off()
    # 
    rm(combined_data, AESI, d_total, pairs.excl,fit1, End_of_risk_window, lookback_period, fig2)  
  }

  # If it's figure 2, then ALSO combine everything in one new pdf file
  if(opt == 2){
  # this is a list of plot objects, always called _Fig2, so only make it for figure 2
  saveRDS(plot_list, file = paste0(output_dir, DAP, '_list_Fig2','.rds'))
    
  pdf.options(reset = TRUE)
  pdf(file = paste0(output_dir, DAP, "_Fig2.pdf", sep = ""))
  print(plot_list)
  dev.off()
  }

  rm(plot_list)
}

