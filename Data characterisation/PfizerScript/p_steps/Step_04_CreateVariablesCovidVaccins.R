
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Create the variables for the Covid vaccination dates. These are merged on person level in a way that only 1 row per subject occurs 

##in/output
#Input 1: CoV.rds
#Input 2: PERSONS.rds
#Output 1: CoV2.rds
#Output 2: PERSONS2.rds


InPutManufacturerVaccines <- function(VP_COVID_EXP_table,CleanVaccines,vaccines.record.period,nameExtension){
  ##############################
  # Inpute manufacturer if missings
  ##############################
  
  if (any(is.na(CleanVaccines[["vx_manufacturer"]]))) {
    
    
    MANF_COMPLETE <- CleanVaccines[!is.na(vx_manufacturer) , ][, inputed := F]
    MANF_INCOMPLETE <- CleanVaccines[is.na(vx_manufacturer), ] 
    
    #Add window of inputations
    window.input <- data.table(cbind(NB = seq(length(vaccines.record.period)), 'window' = vaccines.record.period))
    MANF_INCOMPLETE <- merge(MANF_INCOMPLETE, window.input, on = "NB")
    
    # Select all records with a manufacturer
    MANF <- VP_COVID_EXP_table[!is.na(vx_manufacturer) & person_id %in% CleanVaccines$person_id, .(person_id, Date, vx_manufacturer, Outcome)]
    # JOIN on cases that are witin the 14 days interval
    MANF2 <- sqldf(paste0(
      "

    SELECT

    t1.person_id,
    t1.Date,
    t1.NB,
    t2.vx_manufacturer AS vx_manufacturer_est1,
    t2.Date as date_available

    FROM MANF_INCOMPLETE t1

    INNER JOIN MANF t2 on (t1.person_id = t2.person_id AND ((t2.Date - t1.Date) BETWEEN 1 AND t1.window",")  )



    "
    )) #The window is between 1 and t1.window, since we don't want to assign 
    
    # Make data.table object again
    MANF2 <- as.data.table(MANF2)[, date_available := as.Date(date_available, origin = "1970-01-01")]
    
    # Select the first option per dose
    setorder(MANF2, person_id, NB, date_available)
    MANF2 <- MANF2[, .SD[1], by = c("person_id", "NB")][, date_available := NULL]
    
    # Merge the estimated manufactuer back to main file
    MANF_INCOMPLETE <- merge(x = MANF_INCOMPLETE, y = MANF2, by = c("person_id", "NB", "Date"), all.x = T)[, inputed := as.logical()][,window := NULL]
    
    MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(vx_manufacturer), ":="(inputed = T, vx_manufacturer = vx_manufacturer_est1)]
    
    MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(inputed), inputed := F][, vx_manufacturer_est1 := NULL]
    
    CleanVaccines2 <- rbindlist(list(MANF_COMPLETE, MANF_INCOMPLETE), fill = T, use.names = T)
    CleanVaccines <- CleanVaccines2
    rm(MANF_INCOMPLETE, MANF_COMPLETE, MANF2, MANF,CleanVaccines2)
    gc()
  } else {
    CleanVaccines <- CleanVaccines[, inputed := F]
  }
  
  CleanVaccines <- CleanVaccines[, dose_check := fifelse(suppressWarnings(as.numeric(vx_dose)) == NB, "EQUAL", "NOT_EQUAL", na = "UNK")]
  CleanVaccines <- CleanVaccines[is.na(vx_manufacturer), vx_manufacturer := "UNK"]
  
  saveRDS(CleanVaccines, paste0(vaccins_dir,Exposure,'_CleanVaccines_',nameExtension,'.rds'))
  
  CleanVaccines <- CleanVaccines[, v.man := fifelse(like(vx_manufacturer, manufacturer, ignore.case = T ),toupper(manufacturer),"OTHER") ][, var := paste0(NB,"_",v.man)][,.(person_id,var,Date)]
  CleanVaccines <- data.table::dcast(CleanVaccines, person_id  ~  var, value.var = "Date")
  
  
  #cols_vac <- c("1_PFIZER","2_PFIZER","3_PFIZER","1_OTHER","2_OTHER","3_OTHER")
  
  cols_vac_mis <- cols_vac[!cols_vac %in% colnames(CleanVaccines)]
  
  if(length(cols_vac_mis) > 0){
    for(i in cols_vac_mis){CleanVaccines <- CleanVaccines[, eval(i) := as.Date(x = integer(0), origin = "1970-01-01")]}
    print(paste0(cols_vac_mis, " not not available. |", collapse = " "))
  }
  
  #setnames(VAC2, cols_vac, c("FIRST_PFIZER","SECOND_PFIZER","THIRD_PFIZER","FIRST_OTHER","SECOND_OTHER","THIRD_OTHER"), skip_absent=TRUE)
  setnames(CleanVaccines, cols_vac, cols_vac_new, skip_absent=TRUE)
  
  
  
  rm(cols_vac_mis)
  
  # ###
  # 
  # 
  # Casted_Vaccines <- data.table::dcast(CleanVaccines, person_id ~ NB, value.var = c("date", "vx_manufacturer"))
  # #Creation of missing expected columns
  # expectedCastedColumns <- list("person_id" = 'chr',"date_1"= 'date',"date_2"= 'date',"date_3"= 'date',"vx_manufacturer_1"= 'chr',"vx_manufacturer_2"= 'chr',"vx_manufacturer_3"= 'chr')
  # missingCols <- expectedCastedColumns[!names(expectedCastedColumns) %in% names(Casted_Vaccines)]
  # 
  # if (!is.null(length(missingCols)) == TRUE) { invisible(lapply(names(missingCols), function(x) {
  #   if(missingCols[[x]] == 'chr'){
  #     Casted_Vaccines[, eval(x) := as.character(NA)]
  #   }else if(missingCols[[x]] == 'date'){
  #     Casted_Vaccines[, eval(x) := as.Date(NA, originDate)]
  #   }
  #   
  # }
  # )) }
  # 
  # order_cols <- names(expectedCastedColumns)
  # Casted_Vaccines <- Casted_Vaccines[, ..order_cols]
  # newColNames <- c("person_id", vac_colNames, manu_vac_colNames)
  # setnames(Casted_Vaccines, names(Casted_Vaccines), newColNames, skip_absent = TRUE)
  return(CleanVaccines)
}



SCRIPT <- RUN_SCRIPT(name = "Step_04_CreateVariablesCovidVaccins.R")

if(is.null(StudyName)){recPeriod <- c(14, 90, 90, 90)}else{
    if(StudyName != "Validate"){recPeriod <- c(14, 90, 90, 90)}else{recPeriod <- c(14, 14, 14, 14)}
}  

#recPeriod <- c(14, 90, 90, 90)
#Clean the vaccines by removing if the number of days is less than 14 days between the vaccination date within a person 
###

VAC <- readRDS(SCRIPT[["INPUT1"]][["path"]])[,.(person_id, Date, vx_manufacturer, Outcome, vx_dose,meaning_of_vx_record)]
VAC[, meaning_of_vx_record := toupper(meaning_of_vx_record)]

if(DAP %in% 'PHARMO'){
  #Define quality of record for PHARMO dataset
  recordsQuality <- as.data.table(list(meaning_of_vx_record = toupper(c('GP_medication','GP_INVOICE','GP_Journal','GP_episode')), quality = c(1,1,2,2)))
  VAC <- merge(VAC,recordsQuality, by = 'meaning_of_vx_record', all.x = TRUE)
}

VAC2 <- CleanOutcomes_v2(
  
  Dataset = VAC,
  Person_id = "person_id",
  rec.period = recPeriod,
  outcome =  Exposure,
  c.outcome = "Outcome",
  c.date = "Date",
  c.hierarchy = if(DAP %in% 'PHARMO'){ 'quality' }else{ NULL }
  
)



VAC2 <- VAC2[, Iteration := NULL][, Outcome := NULL]


###


#Assign a sequence number to make an estimation of first, second or third vaccine
###
setorder(VAC2,person_id, Date)
VAC2 <- VAC2[,NB := seq_len(.N) , by = person_id]
if(max(VAC2$NB) > nb_vaccins) print("Cases with more than 4 covid vaccins are in the data")
###


#Keep only the first, second and third vaccination
###
before <- nrow(VAC2)
VAC2 <- VAC2[NB <= nb_vaccins,]
after <- nrow(VAC2)
print(paste0(before - after," cases removed because they where a ",nb_vaccins + 1,"th or higher instance of the covid vaccin"))
rm(before,after)

###

#NEW
#Inpute manufacturer if missings
##############################

# if(any(is.na(VAC2[["vx_manufacturer"]]))){
#   
#   persons <- VAC2[is.na(vx_manufacturer), ][["person_id"]]
#   MANF_COMPLETE <- VAC2[!person_id %in% persons,][, inputed := F]
#   MANF_INCOMPLETE <- VAC2[person_id %in% persons,]
#   
#   #Select all records with a manufacturer
#   MANF <- VAC[!is.na(vx_manufacturer) & person_id %in% persons,.(person_id, Date, vx_manufacturer, Outcome)]
#   #JOIN on cases that are witin the 14 days interval
#   
#   #Add window of inputations
#   windowInput <- data.table(cbind(NB = seq(length(recPeriod)), 'window' = recPeriod))
#   MANF_INCOMPLETE <- merge(MANF_INCOMPLETE,  windowInput, on = "NB")
#   
#   
#   
#   MANF2 <- sqldf(
#     "
#      
#     SELECT
#     
#     t1.person_id,
#     t1.Date,
#     t1.NB,
#     t2.vx_manufacturer AS vx_manufacturer_est1,
#     t2.Date as Date_available
#     
#     FROM MANF_INCOMPLETE t1
#     
#     INNER JOIN MANF t2 on (t1.person_id = t2.person_id AND ((T2.Date - t1.Date) BETWEEN 0 AND t1.window)  )
#     
#     
#     
#     "
#     
#     
#   )
#   
#   #Make data.table object again
#   MANF2 <- as.data.table( MANF2)[, Date_available := as.Date(Date_available, origin = "1970-01-01")]
#   
#   #Select the first option per dose
#   setorder(MANF2,person_id,NB,Date_available, vx_manufacturer_est1)
#   MANF2 <- MANF2[,.SD[1], by = c("person_id", "NB")][, Date_available := NULL]
#   
#   #Merge the estimated manufactuer back to main file
#   MANF_INCOMPLETE <- merge(x = MANF_INCOMPLETE,y = MANF2, by = c("person_id", "NB", "Date"), all.x = T )[, inputed := as.logical()]
#   
#   #
#   # shift <- 1
#   # 
#   # while((shift < max(MANF_INCOMPLETE[["NB"]]) | shift == 1) & any(is.na(MANF_INCOMPLETE[["vx_manufacturer"]]))){
#   #   
#   #   MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est2 := shift(vx_manufacturer_est1, shift), by = "person_id"]
#   #   MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est3 := shift(vx_manufacturer_est1, -shift), by = "person_id"]
#   #   
#   #   MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est :=  fifelse(is.na(vx_manufacturer_est1), vx_manufacturer_est2, vx_manufacturer_est1) ]
#   #   MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est :=  fifelse(is.na(vx_manufacturer_est), vx_manufacturer_est3, vx_manufacturer_est) ]
#   #   
#   #   MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(vx_manufacturer) & !is.na(vx_manufacturer_est), ':=' (inputed = T, vx_manufacturer = vx_manufacturer_est)]
#   #   
#   #   MANF_INCOMPLETE <- MANF_INCOMPLETE[, ':=' (vx_manufacturer_est = NULL, vx_manufacturer_est2 = NULL, vx_manufacturer_est3 = NULL)]
#   #   
#   #   shift <- shift + 1
#   #   
#   # }
#   # rm(shift)
#   
#   #If remove while loop use this
#   
#   MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(vx_manufacturer) & !is.na(vx_manufacturer_est1) , ':=' (inputed = T, vx_manufacturer = vx_manufacturer_est1)]
#   
#   MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(inputed) , inputed := F][,vx_manufacturer_est1 := NULL]
#   
#   VAC2 <- rbindlist(list(MANF_COMPLETE, MANF_INCOMPLETE), fill = T, use.names = T)
#   rm(MANF_INCOMPLETE,  MANF_COMPLETE, MANF2, MANF, persons)
#   gc()
# }else{
#   VAC2 <- VAC2[, inputed := F]
# }
# 
# VAC2 <- VAC2[,dose_check := fifelse(suppressWarnings(as.numeric(vx_dose)) == NB, "EQUAL", "NOT_EQUAL", na = "UNK")]
# VAC2 <- VAC2[is.na(vx_manufacturer), vx_manufacturer := "UNK"]
# 
# rm(VAC)
# gc()
# 
# 
# saveRDS(VAC2, SCRIPT[["OUTPUT1"]][["path"]])


VAC2 <- InPutManufacturerVaccines(VAC,VAC2,recPeriod,'1')

PERSONS <- readRDS(SCRIPT[["INPUT2"]][["path"]])
PERSONS <-  merge(x = PERSONS, y = VAC2, by = "person_id", all.x = T, all.y = F, allow.cartesian = F)
PERSONS <- PERSONS[!is.na(FIRST_PFIZER),month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),"-",year(FIRST_PFIZER))]



saveRDS(PERSONS, SCRIPT[["OUTPUT2"]][["path"]])

rm(SCRIPT, VAC2, PERSONS, recPeriod)

