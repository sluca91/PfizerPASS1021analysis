
#folders = folders
#Daplist =  DAPlist 
#OutputFolder = OutputFolder
#tableList = TableList



Append_Tables_Daps_To_One <- function(folders,Daplist,OutputFolder,tableList){
  for(i in 1:length(DAPlist)){
    
    print(DAPlist[i])
    # browser()
    # grepl(toupper(DAPlist[i]), toupper(folders))
    # if(sum(grepl(toupper(DAPlist[i]), toupper(folders))) == 1){
    # Vul path vector met folder waar de DAPnaam met volgnummer i voorkomt in de foldernaam. 
    path <- folders[
      #grepl geeft een lijst van true en false terug voor elke match van een string in een vector 
      grepl(toupper(DAPlist[i]), toupper(folders))
    ]
    
    
    
    Loop_Trough_TableList(path, OutputFolder, tableList, DAPlist[i])
    
    
  }
  
  
  
}





Loop_Trough_TableList <- function(path, OutputFolder, tableList,DAP){
  
  for(j in 1:length(TableList)){
    # browser()
    # fileLocation is het  totale pad en bestandsnaam naar een bestand met volnummer j 
    # waar naartoe weggeschreven wordt als tussenstap voor een tabel              
    fileLocation <- paste0(OutputFolder,'/',TableList[j],'.rds')
    printfile <- paste0(TableList[j], '.rds')
    # file_temp is het bestand van de lijst met inputbestandjes met volgnummer j en is gematched. het eerste deel van de naam van het bestand komt niet overeen
    # het patroon is bijv: TEST_Table1.csv, maar het bestand heet ARS_TEST_Table1.csv en zou ook anders kunnen heten als het laatste deel maar matcht
    file_temp <- list.files(
      pattern = paste0(TableList[j],'.csv'), 
      path
    )
    # onderstaande regel is omdat niet elke DAP hetzelfde aantal grafiekjes hoeft te hebben. @@todo begrijp ik nog niet 
    if (length(file_temp) == 1) {
      # Inlezen in temp van het bestandje wat is aangetroffen
      # browser()
      # temp <- fread(paste0(path,'/',file_temp),colClasses = 'character')
      temp <- read.csv(paste0(path,'/',file_temp),sep=",")
      
      for(i in 1:ncol(temp)) {       # for-loop over columns
        
        for(j in 1:length(temp[,i])){
          
          temp[j,i] <- format_strnr(temp[j,i])
          # if(as.numeric(temp[j,i])%% 1 == 0){temp[j,i] <- formatC(temp[j,i], digits = 0, big.mark = ",", format= "f")}                                      
          # if(is.numeric(temp[,i])){temp[,i] <- formatC(temp[,i], digits = 2, decimal.mark = ".",big.mark = ",", format= "f")}
          
        }
        
      }
      
      
      temp <- data.table(temp)
      
      
      
      
      # Temp[['DAP']] daarmee maak je een nieuwe kolom aan in temp
      # @@ todo  Dap wordt ook toegevoegd als waarde aan de eerste twee rijen moet dat wel of niet?
      temp[['DAP']] <- DAP
      
      if(file.exists(fileLocation)) {
        print(printfile)
        saveRDS(
          rbindlist(
            list(
              readRDS(fileLocation),
              # temp[rijen,kolommen]
              temp[c(1:nrow(temp)) ,]
            ),
            use.names = T,
            fill = F
          ),
          fileLocation
        )
        
      }
      
      if(!file.exists(fileLocation)) {
        print(printfile)
        saveRDS(temp,fileLocation)
      }
      
    }
    
    
    
  }
  
}


#------------------------------------------------------------------------------------------------------------
# Print the tables for each DAP into one folder. One .docx document for one table
#------------------------------------------------------------------------------------------------------------


#folders = folders 
#OutputFolder =  OutputFolder
#Daplist =  DAPlist
#tablelist =  TableList 
#tabletitles =  TableTitles





Print_Tables <- function(folders, OutputFolder, Daplist, tablelist, tabletitles){
  
  #------------------------------------------------------------------------------------------------------------  
  # Loop through the tablelist 
  #------------------------------------------------------------------------------------------------------------    
  
  for(TableNr in 1:length(tablelist)){
    #----------------------------------------------------------------------------------------------------------
    # start the debugger
    # browser()
    #----------------------------------------------------------------------------------------------------------
    
    # TableNr = 22 
    tabletitle <- tabletitles[TableNr]
    
    
    TableFormats <- fread(paste0(projectFolder,"/a_configuration/02_TableFormats/FormatTable",as.integer(pull(tabletitle, tableNumber)),".csv"), na.strings = NULL ,colClasses = "character")
    TableFormatsTarget <- fread(paste0(projectFolder,"/a_configuration/02_TableFormats/FormatTable",as.integer(pull(tabletitle, tableNumber)),"Target.csv"), na.strings = NULL ,colClasses = "character")
    
    #nr_daps_on_one_page <- 2
    DAP_tables_Location <- paste0(OutputFolder,'/',tablelist[TableNr],'.rds')
    
    if(file.exists(DAP_tables_Location)){
      
      
      
      distinct_daps <- unique(readRDS(DAP_tables_Location)[["DAP"]])
      
      nr_daps_on_one_page <- pull(tabletitle, ndaps)
      if(nr_daps_on_one_page == "ALL"){nr_daps_on_one_page <- length(distinct_daps)}else{nr_daps_on_one_page <- as.integer(nr_daps_on_one_page)}
      
      needed_pages <- ceiling(length(distinct_daps)/nr_daps_on_one_page)
      
      print(tabletitle[["tableName"]])
      
      
      for(PageNr in 1:needed_pages){
        
        print(paste0("Page: ",PageNr, " of ", needed_pages))
        
        st <- 1 + ((PageNr - 1) * nr_daps_on_one_page)
        en <- nr_daps_on_one_page + ((PageNr - 1) * nr_daps_on_one_page)
        
        selection_daps <- distinct_daps[st:en]
        
        #Get table
        DAP_tables <- readRDS(DAP_tables_Location)[DAP %in% selection_daps,]
        
        by_vars <- colnames(DAP_tables[,1:as.integer(pull(tabletitle, nby_vars))])
        
        
        test <- unique(DAP_tables[, .(SUM = .N) , by = "DAP"][["SUM"]])
        
        
        DAP_tables <- DAPTablesTest1(test, DAP_tables, by_vars)
        
        DAP_tables <- DAPTablesTest2(test, DAP_tables, by_vars)
        
        
        # Check de tabel van iedere lijst en verwerk de variabelen naargelang de behoefte 
        
        
        if (tablelist[TableNr] %in% c("Table1","Table2","Table2_00-01",
                                      "Table2_02-04",
                                      "Table2_05-11",
                                      "Table2_12-15",
                                      "Table2_16-17",
                                      "Table2_18-29",
                                      "Table2_30-39",
                                      "Table2_40-49",
                                      "Table2_50-59",
                                      "Table2_60-69",
                                      "Table2_70-79",
                                      "Table2_80",
                                      "Table2_F",
                                      "Table2_M",
                                      "Table10")) {
          amount <- 'N'
          perc <- 'PER'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          
        }
        
        else if (tablelist[TableNr] %in% c("Table6")) {
          amount <- 'n.excl'
          perc <- 'perc.excl'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          
        } 
        
        else if (tablelist[TableNr] %in% c("Table12","Table13","Table14")) {
          amount <- 'vac.n'
          perc <- 'vac.perc'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          
          amount <- 'ctr.n'
          perc <- 'ctr.perc'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          
        }             
        
        else if (tablelist[TableNr] %in% c("Table3","Table4","Table5") ) {
          amount <- 'N1'
          perc <- 'PER1'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          
          amount <- 'N3'
          perc <- 'PER3'
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
        }
        
        
        
        
        else if (tablelist[TableNr] %in% c("Table11")) {
          amount <- 'N'
          perc <- 'PER'
          
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          amount <- 'N.1'
          perc <- 'PER.1'
          
          
          
          DAP_tables <- MergeColumns(amount,perc,table = DAP_tables)
          
          
        }    
        
        else if (tablelist[TableNr] %in% c("Table7")) {
          col1 <- 'VAC1'
          col2 <- 'VAC2'
          col3 <- 'VAC3'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumns(col1,col2,col3,table = DAP_tables)
          
          
        }            
        
        else if (tablelist[TableNr] %in% c("Table15")) {
          col1 <- 'VAC1'
          col2 <- 'VAC2'
          col3 <- 'VAC3'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumns(col1,col2,col3,table = DAP_tables)
          
          col1 <- 'CTR1'
          col2 <- 'CTR2'
          col3 <- 'CTR3'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumns(col1,col2,col3,table = DAP_tables)
          
        }   
        
        else if (tablelist[TableNr] %in% c("Table16","Table17")) {
          col1 <- 'VAC_CumInc'
          col2 <- 'VAC_CI.lb'
          col3 <- 'VAC_CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)
          
          col1 <- 'VAC_IR'
          col2 <- 'VAC_IR_CI.lb'
          col3 <- 'VAC_IR_CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)
          
          col1 <- 'CTR_CumInc'
          col2 <- 'CTR_CI.lb'
          col3 <- 'CTR_CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)              
          
          col1 <- 'CTR_IR'
          col2 <- 'CTR_IR_CI.lb'
          col3 <- 'CTR_IR_CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)  
          
          
          
          
        }   
        
        else if (tablelist[TableNr] %in% c("Table20")) {
          col1 <- 'Cr.HR'
          col2 <- 'Cr.HR.CI.lb'
          col3 <- 'Cr.HR.CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)
          
          col1 <- 'Adj.HR'
          col2 <- 'Adj.HR.CI.lb'
          col3 <- 'Adj.HR.CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)
          
          col1 <- 'Cr.RD'
          col2 <- 'Cr.RD.CI.lb'
          col3 <- 'Cr.RD.CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)              
          
          col1 <- 'Adj.RD'
          col2 <- 'Adj.RD.CI.lb'
          col3 <- 'Adj.RD.CI.ub'
          # table <- DAP_Tables
          
          
          DAP_tables <- MergeThreeColumnsCI(col1,col2,col3,table = DAP_tables)  
          
          
          
          
        }                           
        
        
        
        else if (tablelist[TableNr] %in% c("Table8")) {
          col1 <- 'w1.ci.l'
          col2 <- 'w1.ci.u'
          # table <- DAP_Tables
          
          
          
          DAP_tables <- MergeTwoColumnsCI(col1,col2,table=DAP_tables)
          
          
        }            
        
        
        
        # DAP_tables2 <- DAP_tables[,.(N1PER1=paste0(N1,"(",PER1,")"))]
        # DAP_tables2 <- DAP_tables[,':='(N3PER3=paste0(N3,"(",PER3,")"),N1PER1=paste0(N1,"(",PER1,")"))]
        
        
        
        #Provide needed variables
        #storeOrder <- unique(DAP_tables[DAP == selection_daps[1], by_vars, with = F][, order := seq_len(.N)])
        
        #Provide an id to test
        #setorderv(DAP_tables, c(by_vars, "DAP"))
        #DAP_tables <- DAP_tables[,id := seq_len(.N), by = c(by_vars, "DAP") ]
        # browser()
        by_vars <- c(by_vars, "id")
        
        
        data_vars <- colnames(DAP_tables)[!colnames(DAP_tables) %in% c(by_vars, "DAP")]
        
        # if (data_vars == "NPER"){precast_expr <- paste0(paste0(by_vars, collapse = " + "), paste0(" ~ ","NPER_","DAP")) }
        # else {precast_expr <- paste0(paste0(by_vars, collapse = " + "), " ~ DAP")}
        
        cast_expr <- formula(paste0(paste0(by_vars, collapse = " + "), " ~ DAP"))
        
        #Make new table
        order <- lapply(selection_daps, function(x) paste0(data_vars,"_", x))
        order2 <- NULL 
        for(k in 1:length(order)){order2 <- c(order2, unlist(order[k]))}
        rm(order)
        
        
        
        # TableFormats_new <-  do.call(cbind, lapply(c(1:nr_daps_on_one_page), function(x) TableFormats[,c(data_vars)  , with = F]))
        # colnames(TableFormats_new) <- order2 
        # browser()
        TableFormatsTarget_new <-  do.call(cbind, lapply(c(1:nr_daps_on_one_page), function(x) TableFormatsTarget[,c(data_vars)  , with = F]))
        colnames(TableFormatsTarget_new) <- order2
        
        
        
        
        
        
        label2 <- as.data.frame(rbind(rep(selection_daps, each = length(data_vars))))
        colnames(label2) <- order2
        
        TableFormatsTarget_new <- rbind(label2, TableFormatsTarget_new)
        
        # There is a glitch in the dcast transpose function. If there is only one variable to be cast in the DAP column variables,
        # Then there is no concatenation of the column name and the name in the DAP table. If the DAP is called Pedianet and the value variable is NPER
        # Then the new column name depends on how many values have to be cast. If it is only one then the new variablename will be called Pedianet
        # and not NPER_Pedianet as is the case with tables where more value columns have to be cast. This gives a problem later on.
        # My solution: Generate an extra column and use that as extra value columns to cast their values in. All the extra generated columns have to be 
        # removed afterwards
        
        
        DAP_tables[,NPERCopy:=1]
        data_vars <- c(data_vars,"NPERCopy")
        
        
        # the dcast
        DAP_tables2 <- data.table::dcast(DAP_tables, formula = cast_expr , value.var =  data_vars )
        
        # DAP_tables2[, names(DAP_tables2) := setdiff(names(DAP_tables2), names(DAP_tables2)[startsWith(names(DAP_tables2),"NPERCopy")])]
        DAP_tables2 <- DAP_tables2[, !startsWith(names(DAP_tables2),"NPERCopy"), with = FALSE]
        
        
        DAP_tables[,NPERCopy:=NULL]
        data_vars <- data_vars[data_vars != "NPERCopy"]
        
        #Restore original order that changes by dcast
        
        
        
        setorder(DAP_tables2, "id")[, id := NULL]
        #[, id := NULL]
        
        DAP_tables2 <-  rbindlist(list(TableFormatsTarget_new, DAP_tables2), use.names = , fill = T)
        
        setcolorder(DAP_tables2, c(by_vars[!by_vars %in% "id"], order2))
        
        DAP_tables2[is.na(DAP_tables2)] <- ""
        
        
        
        justTitle <- pull(tabletitle, tabletitle)
        
        
        
        ft <- CreateFlextable(TableFormatsTarget_new, DAP_tables2, justTitle)
        
        
        
        
        
        
        
        CreateAndPrintToWordDocument(projectFolder, ft, tabletitle, orient, OutputFolder, tablelist, TableNr, PageNr) 
        
        
        
        
        
        
        
        
        
        
        
        
        rm(ft)
        rm(PageNr, st,en, selection_daps, by_vars, data_vars, cast_expr, order2, TableFormatsTarget_new, DAP_tables2 )
        
      }
      
    }
    
    
  }
  
  
  
  
}


fun1 <- function(v){
  if(any(is.na(v))) stop('NA values not allowed')
  if(length(v) == 1) return(list(v, 1))
  if(length(v) > 1){
    tmp <- data.frame(v = v, cnt = sequence(rle(as.character(v))$lengths))
    tmp$id <- NA
    tmp$id[1] <- 1
    for(x in 2:nrow(tmp)){
      if(tmp$v[x] == tmp$v[x-1]) tmp$id[x] <- tmp$id[x-1] else tmp$id[x] <- tmp$id[x-1] + 1
    }
    tmp <- tmp[order(tmp$id, tmp$cnt * -1),]
    tmp <- do.call(rbind, lapply(unique(tmp$id), FUN = function(x) tmp[tmp$id == x,][1,]))
    tmp$id <- NULL
    return(list(tmp$v, tmp$cnt))
  }}




