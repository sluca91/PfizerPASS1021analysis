

#------------------------------------------------------------------------------------------------------------
#Make corrections
#------------------------------------------------------------------------------------------------------------

#Table 7: spelling error to be changed
Correct <- readRDS(paste0(OutputFolder, "/Table7.rds"))
Correct <- Correct[DAP == "NHR" & LABEL == "Person days of follow-up", LABEL := "Person-months of follow-up"]
saveRDS(Correct, paste0(OutputFolder, "/Table7.rds"))
rm(Correct)

#Table 11: spelling error to be changed
Correct <- readRDS(paste0(OutputFolder, "/Table11.rds"))
Correct <- Correct[LABEL == "Demographics", LABEL := "Demographics, N (%)"]
saveRDS(Correct, paste0(OutputFolder, "/Table11.rds"))
rm(Correct)

#Table 12: spelling error to be changed
Correct <- readRDS(paste0(OutputFolder, "/Table12.rds"))
Correct <- Correct[LABEL == "N", LABEL := "Total N"]
saveRDS(Correct, paste0(OutputFolder, "/Table12.rds"))
rm(Correct)

#Table 13: spelling error to be changed
Correct <- readRDS(paste0(OutputFolder, "/Table13.rds"))
Correct <- Correct[LABEL == "N", LABEL := "Total N"]
saveRDS(Correct, paste0(OutputFolder, "/Table13.rds"))
rm(Correct)

#Table 14: spelling error to be changed
#Correct <- readRDS(paste0(OutputFolder, "/Table14.rds"))
#Correct <- Correct[LABEL == "N", LABEL := "Total N"]
#saveRDS(Correct, paste0(OutputFolder, "/Table14.rds"))
#rm(Correct)

#Table 15: spelling error to be changed
Correct <- readRDS(paste0(OutputFolder, "/Table15.rds"))
Correct <- Correct[LABEL == "Total subjects", LABEL := "Total, n (%)"]
saveRDS(Correct, paste0(OutputFolder, "/Table15.rds"))
rm(Correct)

# #Table 3 en 11: Socioeconomic status cutout
# j <- "Table3_FirstDose"
# 
# for(j in c("Table11","Table3_FirstDose", "Table3_ThirdDose")){
#   Correct <- readRDS(paste0(OutputFolder, "/",j,".rds"))
#   st <- which(Correct[["LABEL"]] == "Residency in a long-term care facility, n (%)")
#   en <- which(Correct[["LABEL"]] == "Geographic region, n (%)")
#   
#   for(i in 1:length(st)){
#     
#     if(i == 1) vec <- c(st[i]:(en[i]-1))else{ vec <- c(vec, c(st[i]:(en[i]-1)))}
#     
#   }
#   
#   vec <- c(1:nrow(Correct))[!c(1:nrow(Correct)) %in% vec]
#   
#   Correct <- Correct[vec,]
#   saveRDS(Correct, paste0(OutputFolder, "/",j,".rds"))
#   rm(Correct, vec)
#   
# }

#Table 11, Table3_FirstDose and Table3_ThirdDose : region cutout
#Regions that are cutted out of the original file to match number of records in file for each DAP

# j <- "Table3_FirstDose"

for(j in c("Table11","Table3_FirstDose", "Table3_ThirdDose")){
  Correct <- readRDS(paste0(OutputFolder, "/",j,".rds"))
  
  startvector <- which(Correct[["LABEL"]] == "Geographic region, n (%)")
  # Vector with recordnumbers that locate the specific LABEL VALUe "Geographic region, n (%)"
  # example of startvector 
  
  endvector <- which(Correct[["LABEL"]] == "Residency in a long-term care facility, n (%)")
  # Vector with recordnumbers that locate the specific LABEL VALUe "Residency in a long-term care facility, n (%)"
  
  for(i in 1:length(startvector)){
    # if i  
    if(i == 1) RecordsToRemove <- c(startvector[i]:(endvector[i]-1))else{ RecordsToRemove <- c(RecordsToRemove, c(startvector[i]:(endvector[i]-1)))}

  }

  RecordsToKeep <- c(1:nrow(Correct))[!c(1:nrow(Correct)) %in% RecordsToRemove]

  Correct <- Correct[RecordsToKeep,]
  saveRDS(Correct, paste0(OutputFolder, "/",j,".rds"))
  rm(Correct, RecordsToKeep, RecordsToRemove)

}







#Table 3:  merge doses
###
# first <- readRDS(paste0(OutputFolder, "/Table3_FirstDose.rds"))[DAP != "UOSL",]
# second <- readRDS(paste0(OutputFolder, "/Table3_ThirdDose.rds"))[DAP != "UOSL",]

first <- readRDS(paste0(OutputFolder, "/Table3_FirstDose.rds"))
second <- readRDS(paste0(OutputFolder, "/Table3_ThirdDose.rds"))




total <- cbind(first, second)

sum(total[,1] == total[, 5]) == nrow(total)

total <- total[, c(1,2,3,6,7,8)]
colnames(total) <- c("LABEL", "N1", "PER1", "N3", "PER3", "DAP")

saveRDS(total, paste0(OutputFolder, "/","Table3",".rds"))

#Change tabletiltels
TableTitles <- unique(TableTitles[tableName %in%  c("Table3_FirstDose", "Table3_ThirdDose"), tableName := "Table3"])[tableNameExtension %in% c("possible"),]
TableList <- pull(TableTitles, tableName)
###

#Table 15
###


Correct <- readRDS(paste0(OutputFolder, "/Table15.rds"))
Correct <- Correct[DAP == "NHR" & LABEL == "Person days of follow-up", LABEL := "Person-months of follow-up"]
Correct <- Correct[LABEL != "Reasons for censoring"]
saveRDS(Correct, paste0(OutputFolder, "/Table15.rds"))
rm(Correct)

###


###
#Table 16 and 20

###






x <- list.files(OutputFolder, "Table16|Table20")


for(i in 1:length(x)){

        table <- readRDS(paste0(OutputFolder,"/", x[i]))

        rename <- NULL
        if("AESI" %in% colnames(table)) rename <- "AESI"
        if(!is.null(rename)) setnames(table, rename, "Event_name")

        affected <- c("Myocarditis", "Pericarditis", "Myocarditis and pericarditis")

        table[, cor := seq_len(.N), by = c("DAP", "Event_name")]

        table[Event_name %in% affected & cor == 1, Event_name := paste0(Event_name, " (7 days)")]
        table[Event_name %in% affected & cor == 2, Event_name := paste0(Event_name, " (14 days)")]
        table[Event_name %in% affected & cor == 3, Event_name := paste0(Event_name, " (21 days)")][, cor := NULL]

        if(!is.null(rename)) setnames(table, "Event_name", rename)

        saveRDS(table, paste0(OutputFolder,"/", x[i]))


}

rm(x)
