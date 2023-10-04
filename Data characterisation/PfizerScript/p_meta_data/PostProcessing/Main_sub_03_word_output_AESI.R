x <-  c("Table16","Table20")
TableNr <- c(16,20)

 i=1

for(i in 1:length(x)){

  
  
  table <- readRDS(paste0(OutputFolder,"/", x[i], ".rds"))
  
  cols <- colnames(table)[!colnames(table) %in% c("Event_name", "AESI", "System", "DAP")]
  
  format <- fread(paste0(projectFolder,"/a_configuration/02_TableFormats/Format", x[i], ".csv"))
  format <- format[seq_len(.N) == nrow(format), DAP := "DAP"][, c("DAP",cols) , with = F]
  formatNew <- fread(paste0(projectFolder,"/a_configuration/02_TableFormats/Format", x[i], "Target.csv"))
  risk_window <- fread(paste0(projectFolder,'/a_configuration/03_RiskWindow/RiskWindow.csv'))
  
  tabletitle <- TableTitles[tableNumber == TableNr[i]]
  
  
  
  
  if("AESI" %in% colnames(table)) col <- "AESI"
  if("Event_name" %in% colnames(table)) col <- "Event_name"
  table[get(col) == "Coagulation disorders: thromboembolism", eval(col) := "Coagulation disorders thromboembolism"]
  
  aesi <- unique(table[[col]])
  
   j=1
  
  for(j in 1:length(aesi)){
    tableTmp <- table[get(col) == aesi[j] ,]
    tableTmp <- tableTmp[, c("DAP",cols) , with = F]
  
  
   if (x[i] == "Table16") {
    col1 <- 'VAC_CumInc'
    col2 <- 'VAC_CI.lb'
    col3 <- 'VAC_CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)
    
    col1 <- 'VAC_IR'
    col2 <- 'VAC_IR_CI.lb'
    col3 <- 'VAC_IR_CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)
    
    col1 <- 'CTR_CumInc'
    col2 <- 'CTR_CI.lb'
    col3 <- 'CTR_CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)              
    
    col1 <- 'CTR_IR'
    col2 <- 'CTR_IR_CI.lb'
    col3 <- 'CTR_IR_CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)  
    
    
    
    
  }   
  
   if (x[i] == "Table20") {
    col1 <- 'Cr.HR'
    col2 <- 'Cr.HR.CI.lb'
    col3 <- 'Cr.HR.CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)
    
    col1 <- 'Adj.HR'
    col2 <- 'Adj.HR.CI.lb'
    col3 <- 'Adj.HR.CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)
    
    col1 <- 'Cr.RD'
    col2 <- 'Cr.RD.CI.lb'
    col3 <- 'Cr.RD.CI.ub'
    # table <- table
    
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)              
    
    col1 <- 'Adj.RD'
    col2 <- 'Adj.RD.CI.lb'
    col3 <- 'Adj.RD.CI.ub'
    # table <- table
    
   
    
    tableTmp <- MergeThreeColumnsCI(col1,col2,col3,table = tableTmp)
   }
    
    
    by_vars <- colnames(tableTmp[,1:as.integer(pull(tabletitle, nby_vars))])
    data_vars <- colnames(tableTmp)[!colnames(tableTmp) %in% c(by_vars, "DAP")]
    
    # order <- lapply(selection_daps, function(x) paste0(data_vars,"_", x))
    # order2 <- NULL 
    # for(k in 1:length(order)){order2 <- c(order2, unlist(order[k]))}
    # rm(order)
    # 
    
    
    tableTmp <-  rbindlist(list(formatNew, tableTmp), use.names = , fill = T)
    
    setcolorder(tableTmp, c(by_vars,data_vars ))
    
    if(x[i] == "Table16"){
      
      tableTmp[,Event_name := NULL]  
      
    }
    else if (x[i] == "Table20" ){
      
      tableTmp[,AESI := NULL]
      
    }
    
    
    
    rw <- risk_window[AESI==aesi[j],]

    if(nrow(rw) == 0){

      rw <- data.table(AESI="Empty", "RiskWindow" = "not found")

    }
    
    
    
    tableTmp[is.na(tableTmp)] <- ""
    
    filename = paste0(OutputFolder,"/01_docx/",x[i],"_",aesi[j],".docx")
    
    justTitle <- pull(tabletitle,tabletitle)
    justTitle <- sprintf(justTitle,aesi[j],rw[,"RiskWindow"])
    justTitle <- str_replace(justTitle,' percent','%')
    
    ft <- CreateFlextable(formatNew, tableTmp, justTitle)
    
    CreateAndPrint_AESI_ToWordDocument(projectFolder, ft, filename, tabletitle, orient = 'landscape', OutputFolder, tablelist = TableList, TableNr[i], 1)
    
   
    
  } 
  
}
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

