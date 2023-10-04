
id <- "#ID-00247543#"
date <- "20210603"
suffix <- "1"

#id <- "#ID-00209595#"
#date <- "20211228"
#suffix <- "2"
 
EVENTS <- fread(paste0(path_dir,"EVENTS.csv"))[0]

PER <- fread(paste0(path_dir,"PERSONS.csv"))

Codes <- fread(paste0(meta_dir,"20221208_ALL_full_codelist.csv"))

Codes <- unique(Codes[,.(coding_system, code)])

setnames(Codes, c("code","coding_system"), c("event_code","event_record_vocabulary"))

Codes <- Codes[, person_id := id][, start_date_record := date]

EVENTS <- rbindlist(list(EVENTS, Codes), fill = T, use.names = T)

MED <- fread(paste0(path_dir,"MEDICINES.csv"))[0]

Codes2 <- fread(paste0(meta_dir,"20221208_ALL_drug_proxies_codelist.csv"))

Codes2 <- unique(Codes2[,.(atc_codes)])

setnames(Codes2, c("atc_codes"), c("medicinal_product_atc_code"))

Codes2 <- Codes2[, person_id := id][, date_dispensing := date]

MED <- rbindlist(list(MED, Codes2), fill = T, use.names = T)

VAC <- fread(paste0(path_dir,"VACCINES.csv"))[0]

Codes3 <- fread(paste0(meta_dir,"Pfizer_vaccines_codelist.csv"))[!StudyVar %in% c("INF", "CoV"),]

Codes3 <- unique(Codes3)

setnames(Codes3, c("atc_codes"), c("vx_atc"))

Codes3 <- Codes3[, person_id := id][, vx_admin_date := date][,StudyVar := NULL]

VAC <- rbindlist(list(VAC, Codes3), fill = T, use.names = T)


fwrite(EVENTS, paste0(path_dir,"EVENTS_subject1_",suffix,".csv"))
fwrite(MED, paste0(path_dir,"MEDICINES_subject1_",suffix,".csv"))
fwrite(VAC, paste0(path_dir,"VACCINES_subject1_",suffix,".csv"))

#baseFile <- fread(paste0(meta_dir,"Pfizer_additional_concepts.csv"))[DAP_NAME == "TEST",]
baseFile <- IMPORT_PATTERN(dir = meta_dir, pat = "Pfizer_additional_concepts.csv")

#i <- "MEDICAL_OBSERVATIONS"
for(i in unique(baseFile$table)){
  filePrint <- paste0(path_dir,i,"_add_concepts_subject1_",suffix,".csv")
  if(file.exists(filePrint)) file.remove(filePrint)
  fwrite(fread(list.files(path = path_dir, pattern = i, full.names = T)[1])[0], filePrint)
  
  rm(filePrint)
}

#i=5

cols <- colnames(baseFile)[substr(colnames(baseFile),1,3) == "col" & nchar(colnames(baseFile)) < 6]
vals <- colnames(baseFile)[substr(colnames(baseFile),1,3) == "val"]

for(i in 1:nrow(baseFile)){
  tempFile <- baseFile[i,]
  
  filePrint <- paste0(path_dir,tempFile[["table"]],"_add_concepts_subject1_",suffix,".csv")
  fileCsv <- IMPORT_PATTERN(dir = path_dir, pat = paste0(tempFile[["table"]],"_add_concepts_subject1_",suffix,".csv"))
  add <- fileCsv[0][nrow(fileCsv) + 1,] 
  j=1
  keepCol <- tempFile[["keep"]]
  
  
  for(j in 1:length(cols)){
    col <- tempFile[[cols[j]]]
    val <- tempFile[[vals[j]]]
    
    if(!is.na(col) & !is.na(val)){
      if(j == 1)addTemp <- copy(add)
      addTemp[nrow(addTemp), which(colnames(addTemp) == col)] <- val
      if(!is.na(keepCol)) if(is.na(addTemp[[keepCol]])) addTemp[nrow(addTemp), which(colnames(addTemp) == keepCol)] <- "50"
      
      
    }
    fileCsv <- rbind(fileCsv,addTemp)
    rm(col, val)
  }
  
  dateCol <- tempFile[["date_column"]]
  if(tempFile[["table"]] == "VACCINES") dateCol <- "vx_record_date"
  fwrite(fileCsv[, person_id := id][, eval(dateCol) := date],filePrint)
  
  rm(dateCol, tempFile, filePrint, fileCsv, add, keepCol)
}

#if(tempFile[["StudyVar"]] == "H_EMERG_COV"){}

tempFile <- fread(paste0(path_dir,"MEDICAL_OBSERVATIONS","_add_concepts_subject1_",suffix,".csv"))

tempFile <- tempFile[mo_meaning == "emergency",][, mo_date := as.character(as.numeric(mo_date)-1) ]
fwrite(tempFile, paste0(path_dir,"MEDICAL_OBSERVATIONS","_add_concepts_sum_subject1_",suffix,".csv"))
rm(tempFile)
