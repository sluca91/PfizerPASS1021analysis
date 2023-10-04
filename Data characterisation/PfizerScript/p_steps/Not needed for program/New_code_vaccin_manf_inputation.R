
#fwrite(VAC,"D:/dummy_vaccins.csv", sep = ";")

#Read intermediate test table
VAC <- fread("D:/dummy_vaccins.csv", na.strings = "")[!is.na(person_id),][, Date := as.Date(Date, "%d-%m-%Y")]

VAC2 <- CleanOutcomes(
  
  Dataset = VAC,
  Person_id = "person_id",
  Rec_period = c(14),
  Outcomes =  c("CoV"),
  Name_event = "code3",
  Date_event = "Date"
  
  
)

VAC2 <- VAC2[, Iteration := NULL][, code3 := NULL]

VAC2 <- VAC2[,NB := seq_len(.N) , by = person_id]

#START NEW CODE
###

if(any(is.na(VAC2[["vx_manufacturer"]]))){
  
  persons <- VAC2[is.na(vx_manufacturer), ][["person_id"]]
  MANF_COMPLETE <- VAC2[!person_id %in% persons,][, inputed := F]
  MANF_INCOMPLETE <- VAC2[person_id %in% persons,]
  
  #Select all records with a manufacturer
  MANF <- VAC[!is.na(vx_manufacturer) & person_id %in% persons,.(person_id, Date, vx_manufacturer, code3)]
  #JOIN on cases that are witin the 14 days interval
  MANF2 <- sqldf(
    "
     
    SELECT
    
    t1.person_id,
    t1.Date,
    t1.NB,
    t2.vx_manufacturer AS vx_manufacturer_est1,
    t2.Date as Date_available
    
    FROM MANF_INCOMPLETE t1
    
    INNER JOIN MANF t2 on (t1.person_id = t2.person_id AND ((T2.Date - t1.Date) BETWEEN 0 AND 14)  )
    
    
    
    "
    
    
  )
  
  #Make data.table object again
  MANF2 <- as.data.table( MANF2)[, Date_available := as.Date(Date_available, origin = "1970-01-01")]

#Select the first option per dose
  setorder(MANF2,person_id,NB,Date_available)
  MANF2 <- MANF2[,.SD[1], by = c("person_id", "NB")][, Date_available := NULL]
  
  #Merge the estimated manufactuer back to main file
  MANF_INCOMPLETE <- merge(x = MANF_INCOMPLETE,y = MANF2, by = c("person_id", "NB", "Date"), all.x = T )
  
  #
  shift <- 1
  
   
  
  while(shift < max(MANF_INCOMPLETE[["NB"]]) & any(is.na(MANF_INCOMPLETE[["vx_manufacturer"]]))){
  
  MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est2 := shift(vx_manufacturer_est1, shift), by = "person_id"]
  MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est3 := shift(vx_manufacturer_est1, -shift), by = "person_id"]
  
  MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est :=  fifelse(is.na(vx_manufacturer_est1), vx_manufacturer_est2, vx_manufacturer_est1) ]
  MANF_INCOMPLETE <- MANF_INCOMPLETE[, vx_manufacturer_est :=  fifelse(is.na(vx_manufacturer_est), vx_manufacturer_est3, vx_manufacturer_est) ]
  
  #MANF_INCOMPLETE <- MANF_INCOMPLETE[, inputed := fifelse(is.na(vx_manufacturer) & !is.na(vx_manufacturer_est), T, F)][inputed == T, vx_manufacturer := vx_manufacturer_est]
  MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(vx_manufacturer) & !is.na(vx_manufacturer_est), ':=' (inputed = T, vx_manufacturer = vx_manufacturer_est)]
  
  #MANF_INCOMPLETE <- MANF_INCOMPLETE[, ':=' (vx_manufacturer_est = NULL, vx_manufacturer_est1 = NULL, vx_manufacturer_est2 = NULL, vx_manufacturer_est3 = NULL)]
  MANF_INCOMPLETE <- MANF_INCOMPLETE[, ':=' (vx_manufacturer_est = NULL, vx_manufacturer_est2 = NULL, vx_manufacturer_est3 = NULL)]
  
  shift <- shift + 1
  
  }
  rm(shift)
  
  MANF_INCOMPLETE <- MANF_INCOMPLETE[is.na(inputed) , inputed := F][,vx_manufacturer_est1 := NULL]
  
  
  #MANF_INCOMPLETE2 <- MANF_INCOMPLETE[, ':=' (sum = sum(!is.na(vx_manufacturer_est)), max = max(NB)), by = c("person_id")]
  
  VAC2 <- rbindlist(list(MANF_COMPLETE, MANF_INCOMPLETE), fill = T, use.names = T)
  
  
  
}else{
  VAC2 <- VAC2[, inputed := F]
}

VAC2 <- VAC2[,dose_check := fifelse(vx_dose == NB, "EQUAL", "NOT_EQUAL", na = "UNK")]
  
#MANF <- VAC[!is.na(vx_manufacturer),.(person_id, Date, vx_manufacturer, code3)]
#MANF <- MANF[, min := min(Date) , by = "person_id"][, dif := as.integer(Date - min)]
#MANF <- MANF[, estimate := dif/14]



