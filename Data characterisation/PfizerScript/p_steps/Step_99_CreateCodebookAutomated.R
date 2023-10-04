#Author: Roel Elbers Drs.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/04/2023

#Aim: create an overview of all the tables in the data pipeline of a run. This can be ran on sample data to provide an insight 
#in all the tables, columns and when the number of values in a column is lower then 10 also the values.

#Input 1: PROGRAM
#Output 1: Codebook in meta_dir in folder Codebook

#Because there are tables in database file and in rds files first extract all database files.
databases <- list.files(g_intermediate , all.files = T, include.dirs = F, recursive = T, pattern = ".db$", full.names = T)
databaseTables <- NULL

#Put all the database tables that are specified in the program file or that contain a concepts name in an overview
###
#First collect all the tables
for(i in databases){
  mydb <- dbConnect(RSQLite::SQLite(), i)
  tables <- dbListTables(mydb)
  dbDisconnect(mydb)
  databaseTables <- rbind(databaseTables, cbind(FILE = tables, result = rep(i, length(tables))))
  rm(tables)
}

#Then create an overview and keep the needed tables
databaseTables <- as.data.table(databaseTables)[FILE %in% PROGRAM[TYPE == "OUTPUT" & FORMAT == "db",][["FILE"]] |
                              grepl(x=  FILE, pattern =  paste0(ALL_CONCEPTS, collapse = "|"))
                              ,][, FORMAT := "db"][, FORMAT := "db"]


###

#Combine the database tables with the rds tables in 1 file
rdsTables <- unique(PROGRAM[TYPE == "OUTPUT" & FORMAT %in% c("rds"),][, .(FILE, result, FORMAT)])
tablesALL <- rbind(rdsTables, databaseTables)

#Prepare a framework to print summarizing information to per table. This becomes the first sheet in the excel.
tables <- matrix(ncol = 2,nrow = nrow(tablesALL))
colnames(tables) <- c("Name","N_rows") #,"Key"

#Make the object to collect the information that neeeds to be printed to the excel file at the end.
wb <- createWorkbook()


#addWorksheet(wb, "Program steps")
#writeDataTable(wb, "Program steps", PROGRAM[,.(PROGRAM,TYPE, FILE,FORMAT,FOLDER,FUNCTIONS)], tableStyle = "TablestyleLight8")
#setColWidths(wb, sheet = "Program steps", cols = c(1:6), widths = c(50,20,50,20,160,50))

#addWorksheet(wb, "Flowchart program steps")
#insertImage(wb, "Flowchart program steps", file = paste0(pre_dir,"/Flowchart.png"), width = 12, height = 8)

#update <- fread(paste0(meta_dir,"Codebook/Update_codebook.csv"))

for (i in 1:nrow(tablesALL)) {
  #Check if the needed file is available
  if(file.exists(tablesALL[i,][["result"]])){
    
    #Load the table depending on the type, database or rds
    if(tablesALL[i,][["FORMAT"]] == "rds")  temp2 <- readRDS(tablesALL[i,][["result"]])
    if(tablesALL[i,][["FORMAT"]] == "db"){
      #Connect to database
      mydb <- dbConnect(RSQLite::SQLite(), tablesALL[i,][["result"]])
      temp2 <- as.data.table(dbReadTable(mydb, tablesALL[i,][["FILE"]]))
      dbDisconnect(mydb)
    }
   
    if(class(temp2)[1] != "list"){ 
      
      #Collect the colnames, format and values in vectors that can be used to make a dataframe
      cols <- colnames(temp2)
      format <- unlist(lapply(cols, function(x) class(temp2[[x]])))
      order <- c(1:length(cols))
      
      values <- lapply(cols, function(x){if(length(unique(temp2[[x]])) < 10){paste0(unique(temp2[[x]]), collapse = "|")}})
      
      #Make the dataframe using the vectores per table
      temp3 <- as.data.table(cbind(cols,format,order, values))
      
      #update_temp <- update[table == temp[i,]$FILE,]
      #temp3 <- merge(temp3, update_temp[,.(cols, origin,Description)], by = "cols", all.x = T)
      #tmp.c <- c(colnames(temp3),"origin2")
      #setorder(temp3, order)[,order := NULL]
      
      #Make a sheet name containing the needed infor. Note that max length of the sheetname is 31
      if(tablesALL[i,][["FORMAT"]] == "db"){ 
      sheetName <- paste0(unlist(strsplit(tablesALL[i,][["result"]], "/+"))[length(unlist(strsplit(tablesALL[i,][["result"]], "/+")))],
             " ", tablesALL[i,]$FILE
             )
      if(nchar(sheetName) > 31) sheetName <- tablesALL[i,]$FILE
      }
      
      if(tablesALL[i,][["FORMAT"]] == "rds"){sheetName <- paste0("rds ", tablesALL[i,]$FILE)}
      
      sheetName <- substr(sheetName, 1,31)
      
      #Write to a new sheet for the excel
      addWorksheet(wb, sheetName)
      writeDataTable(wb, sheetName, temp3, tableStyle = "TablestyleLight8",)
      setColWidths(wb, sheet = sheetName,cols = 1:ncol(temp3) , widths = 30)
      
      #Fill the summary table for on the first sheet.This is printed later when it is filled completely
      tables[i,"N_rows"] <- nrow(temp2)
      tables[i,"Name"] <- tablesALL[i,]$FILE
      #tables[i,"Key"] <- temp[i,]$KEY
      
      rm(temp2, cols,format,temp3, values, sheetName)
      gc()
    }
  }else{
    tables[i,"N_rows"] <- 0
    tables[i,"Name"] <- tablesALL[i,]$FILE
    #tables[i,"Key"] <- temp[i,]$KEY
    
  }
  
 
}

#Write the overview sheet
addWorksheet(wb, "Overview")
writeDataTable(wb, "Overview", as.data.frame(tables), tableStyle = "TablestyleLight8")
setColWidths(wb, sheet = "Overview",cols = 1:ncol(tables) , widths = 30)
#worksheetOrder(wb) <- c(1,2,length(names(wb)), c(3:(length(names(wb)) - 1)))

#Make sure the overview is on the first sheet.
worksheetOrder(wb) <- c(length(names(wb)), c(1:(length(names(wb)) - 1)))

#Write the eventual excel
saveWorkbook(wb, file = paste0(meta_dir,"Codebook/","Codebook",".xlsx"), overwrite = TRUE)



rm(tables,  databases, databaseTables, mydb) #update, update_temp,
gc()






