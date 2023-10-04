

#------------------------------------------------------------------------------------------------------------
#Make corrections
#------------------------------------------------------------------------------------------------------------

#Table 7: spelling error to be changed
Correct <- readRDS(paste0(InterMediateFolder, "/Table7.rds"))
Correct <- Correct[DAP == "UOSL" & LABEL == "Person days of follow-up", LABEL := "Person-months of follow-up"]
saveRDS(Correct, paste0(InterMediateFolder, "/Table7.rds"))
rm(Correct)

#Table 11: region cutout

for(j in c("Table11", "Table3_FirstDose", "Table3_ThirdDose")){
  Correct <- readRDS(paste0(InterMediateFolder, "/",j,".rds"))
  st <- which(Correct[["LABEL"]] == "Geographic region, n (%)")
  en <- which(Correct[["LABEL"]] == "Residency in a long-term care facility, n (%)")
  
  for(i in 1:length(st)){
    
    if(i == 1) vec <- c(st[i]:(en[i]-1))else{ vec <- c(vec, c(st[i]:(en[i]-1)))}
    
  }
  
  vec <- c(1:nrow(Correct))[!c(1:nrow(Correct)) %in% vec]
  
  Correct <- Correct[vec,]
  saveRDS(Correct, paste0(InterMediateFolder, "/",j,".rds"))
  rm(Correct, vec)
  
}
#Table 3:  merge doses
###
first <- readRDS(paste0(InterMediateFolder, "/Table3_FirstDose.rds"))[DAP != "UOSL",]
second <- readRDS(paste0(InterMediateFolder, "/Table3_ThirdDose.rds"))[DAP != "UOSL",]

total <- cbind(first, second) 

sum(total[,1] == total[, 5]) == nrow(total)

total <- total[, c(1,2,3,6,7,8)]
colnames(total) <- c("LABEL", "N1", "PER1", "N3", "PER3", "DAP")

saveRDS(total, paste0(InterMediateFolder, "/","Table3",".rds"))

#Change tabletiltels
TableTitles <- unique(TableTitles[tableName %in%  c("Table3_FirstDose", "Table3_ThirdDose"), tableName := "Table3"])[tableNameExtension %in% c("possible"),]
TableList <- pull(TableTitles, tableName)
###

#Table 15
###


Correct <- readRDS(paste0(InterMediateFolder, "/Table15.rds"))
Correct <- Correct[DAP == "UOSL" & LABEL == "Person days of follow-up", LABEL := "Person-months of follow-up"]
Correct <- Correct[LABEL != "Reasons for censoring"]
saveRDS(Correct, paste0(InterMediateFolder, "/Table15.rds"))
rm(Correct)

###


###
#Table 16 and 20

###






x <- list.files(InterMediateFolder, "Table16|Table20")


for(i in 1:length(x)){

        table <- readRDS(paste0(InterMediateFolder,"/", x[i]))
        
        rename <- NULL
        if("AESI" %in% colnames(table)) rename <- "AESI"
        if(!is.null(rename)) setnames(table, rename, "Event_name")
        
        affected <- c("Myocarditis", "Pericarditis", "Myocarditis and pericarditis")
        
        table[, cor := seq_len(.N), by = c("DAP", "Event_name")]
        
        table[Event_name %in% affected & cor == 1, Event_name := paste0(Event_name, " (7 days)")]
        table[Event_name %in% affected & cor == 2, Event_name := paste0(Event_name, " (14 days)")]
        table[Event_name %in% affected & cor == 3, Event_name := paste0(Event_name, " (21 days)")][, cor := NULL]
        
        if(!is.null(rename)) setnames(table, "Event_name", rename)
        
        saveRDS(table, paste0(InterMediateFolder,"/", x[i]))


}

rm(x)