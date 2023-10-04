
#folders = folders
#Daplist =  DAPlist 
#intermediateFolder = InterMediateFolder
#tableList = TableList



Append_Tables_Daps_To_One <- function(folders,Daplist,intermediateFolder,tableList){
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

    
    
    Loop_Trough_TableList(path, intermediateFolder, tableList, DAPlist[i])
    

  }
  
  

}





Loop_Trough_TableList <- function(path, IntermediateFolder, tableList,DAP){
  
  for(j in 1:length(TableList)){
    # browser()
    # fileLocation is het  totale pad en bestandsnaam naar een bestand met volnummer j 
    # waar naartoe weggeschreven wordt als tussenstap voor een tabel              
    fileLocation <- paste0(IntermediateFolder,'/',TableList[j],'.rds')
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
#IntermediateFolder =  InterMediateFolder
#Daplist =  DAPlist
#tablelist =  TableList 
#tabletitles =  TableTitles





Print_Tables <- function(folders, IntermediateFolder, Daplist, tablelist, tabletitles){

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

    
    TableFormats <- fread(paste0(projectFolder,"/FormatTable",as.integer(pull(tabletitle, tableNumber)),".csv"), na.strings = NULL ,colClasses = "character")
    TableFormatsTarget <- fread(paste0(projectFolder,"/FormatTable",as.integer(pull(tabletitle, tableNumber)),"Target.csv"), na.strings = NULL ,colClasses = "character")
  
    #nr_daps_on_one_page <- 2
    DAP_tables_Location <- paste0(IntermediateFolder,'/',tablelist[TableNr],'.rds')
    
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
          

          
          
          
          
          
          CreateAndPrintToWordDocument(projectFolder, ft, tabletitle, orient, InterMediateFolder, tablelist, TableNr, PageNr) 
            

          
          
          
          
          
          
          

          
          
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





































CreateFlextable <- function(TableFormats_new, DAP_tables2, tabletitle) {
  
  header_rows <- nrow(TableFormats_new)
  
  ft <- flextable(DAP_tables2[c((header_rows + 1):nrow(DAP_tables2)),]) ## Flextable of the data (excl. header(s))
  ft <- delete_part(ft, part = "header")  ## Remove the header
  
  
  for(RowNr in header_rows:1){
    ft <- add_header_row(x = ft, values = fun1(as.character(DAP_tables2[RowNr,]))[[1]], colwidths = fun1(as.character(DAP_tables2[RowNr,]))[[2]]) ## Adds second row
  }
  
  # ft <- colformat_num( ft, big.mark = ",", decimal.mark = ".") 
  # ft <- colformat_int(ft, big.mark = ",")
  # ft <- colformat_char(ft, prefix = "test: ")
  #ft <- merge_v(ft, j = ~ col1)          ## Merge similar values of AECAT 
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- bold(ft, bold = TRUE, part = "header")
  # ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- border_inner(ft)
  ft <- border_outer(ft)
  ft <- set_caption(ft, caption = tabletitle, style = "Table Caption")
  ft <- font(ft, fontname = "Times New Roman", part = "all") 
}         

      


CreateAndPrintToWordDocument <- function(projectFolder, ft, tabletitle, orient, InterMediateFolder, tablelist, TableNr, PageNr) {
  
  TemplatePath <- paste0(projectFolder,"/t5functions/wordTemplates/template_word.docx")
  
  
  doc_section_1 <- read_docx(path = TemplatePath)
  
  # Add flextable to doc body
  doc_section_1 <- body_add_flextable( doc_section_1, value = ft, align = "left", keepnext = F,split=T)
  
  # create a prop_section to orient the page in a landscape format and make it an oddPage(?)
  ps <- prop_section( page_size = page_size(orient = pull(tabletitle, orient)), type = "oddPage")
  
  # Add the doc_section_1 to itself using a body_end_block_section with the property value of the block_section equal to the previous prop_section ps
  doc_section_1 <- body_end_block_section( x = doc_section_1,  value = block_section(property = ps))
  
  # print doc_section_ to its target
  print(doc_section_1, target = paste0(InterMediateFolder,"/temp_docx_output/",tablelist[TableNr],"_instance_",PageNr,".docx"))
  
}


CreateAndPrint_AESI_ToWordDocument <- function(projectFolder, ft,filename, tabletitle, orient, InterMediateFolder, tablelist, TableNr, PageNr) {
  
  TemplatePath <- paste0(projectFolder,"/t5functions/wordTemplates/template_word.docx")
  
  
  doc_section_1 <- read_docx(path = TemplatePath)
  
  # Add flextable to doc body
  doc_section_1 <- body_add_flextable( doc_section_1, value = ft, align = "left", keepnext = F,split=T)
  
  # create a prop_section to orient the page in a landscape format and make it an oddPage(?)
  ps <- prop_section( page_size = page_size(orient = pull(tabletitle, orient)), type = "oddPage")
  
  # Add the doc_section_1 to itself using a body_end_block_section with the property value of the block_section equal to the previous prop_section ps
  doc_section_1 <- body_end_block_section( x = doc_section_1,  value = block_section(property = ps))
  
  # print doc_section_ to its target
  print(doc_section_1, target = filename)
  
}







DAPTablesTest1 <- function(test, DAP_tables, by_vars) {
  
  if(length(test) != 1){warning("Tables are not similar in number of rows  between DAPS and not processed")}else{
    # browser()
    DAP_tables[["id"]] <- rep(c(1:test), length(unique(DAP_tables[["DAP"]])))
    if(nrow(unique(DAP_tables[, c(by_vars ,"id"), with = F])) != test){stop("Order is not similar between DAPS")}
  }
  return(DAP_tables)
}


DAPTablesTest2 <- function(test, DAP_tables, by_vars) {
  if(length(test) != 1){
    print(unique(DAP_tables[, .(SUM = .N) , by = c(by_vars)]))
    if(any(unique(DAP_tables[, .(SUM = .N) , by = c(by_vars, "DAP")])[["SUM"]] > 1)) stop("Exclude this table")
    
    #library(sqldf)
    #sqldf(paste0("SELECT ",by_vars,", DAP, COUNT(",by_vars,") AS N FROM DAP_tables GROUP BY ",by_vars,", DAP"))
    
    maxDap <-DAP_tables[, .(SUM = .N) , by = c("DAP")][max(SUM) == SUM,][1,][["DAP"]]
    
    
    storeOrder <- unique(DAP_tables[DAP == maxDap, by_vars, with = F][, id := seq_len(.N)])
    DAP_tables <- merge(DAP_tables, storeOrder, all = F, by = by_vars)
    rm(storeOrder, maxDap)
  }
  return(DAP_tables)
  rm(test)
}


MergeColumns <- function(amount,perc,table) {
  
  
  
  table <- copy(table)
  c.name <- paste0(amount,perc)
  
  colOrderOriginal <- colnames(table)
  
  colPos <- min(which(colOrderOriginal %in% c(amount,perc)))
  
  colOrder <- c(colOrderOriginal[1:colPos-1],c.name,colOrderOriginal[colPos:length(colOrderOriginal)])
  colOrder <- colOrder[!colOrder %in% c(amount,perc)]
  
  
  setnames(table,c(amount,perc),c("amount","perc"))
  
  # DAP_tables2 <- table[,':='(N1PER1=paste0(get(amount),"(",get(perc),")"))]
  DAP_tables2 <- table[amount != '' & perc != '',eval(c.name):=paste0(amount," (",perc,")")]
  DAP_tables2 <- DAP_tables2[amount != '' & (is.na(perc) | perc == ''),eval(c.name):=amount]
    DAP_tables2 <- DAP_tables2[,amount:=NULL][,perc:=NULL]
  setcolorder(DAP_tables2,colOrder)
  
  return(DAP_tables2)
}


MergeTwoColumnsCI <- function(col1,col2,table) {
  
  
  
  table <- copy(table)
  c.name <- paste0(col1,col2)
  
  colOrderOriginal <- colnames(table)
  
  colPos <- min(which(colOrderOriginal %in% c(col1,col2)))
  
  colOrder <- c(colOrderOriginal[1:colPos-1],c.name,colOrderOriginal[colPos:length(colOrderOriginal)])
  colOrder <- colOrder[!colOrder %in% c(col1,col2)]
  
  
  setnames(table,c(col1,col2),c("col1","col2"))
  
  # DAP_tables2 <- table[,':='(N1PER1=paste0(get(amount),"(",get(perc),")"))]
  # [col1 != '' & col2 != ''& col3 != '',eval(c.name):=paste0(col1,"(",col2,",",col3,")")]
  # [col1 != '' & is.na(col2)& is.na(col3),eval(c.name):=col1]
  # [col1 != '' & col2 != ''& is.na(col3),eval(c.name):=paste0(col1,"(",col2,")")]
  DAP_tables2 <- table[col1 != '' & col2 != '',eval(c.name):=paste0(" (",col1,", ",col2,")")]
  DAP_tables2 <- DAP_tables2[,col1:=NULL][,col2:=NULL]
  setcolorder(DAP_tables2,colOrder)
  
  return(DAP_tables2)
}







MergeThreeColumns <- function(col1,col2,col3,table) {
  
  
  
  table <- copy(table)
  c.name <- paste0(col1,col2,col3)
  
  colOrderOriginal <- colnames(table)
  
  colPos <- min(which(colOrderOriginal %in% c(col1,col2,col3)))
  
  colOrder <- c(colOrderOriginal[1:colPos-1],c.name,colOrderOriginal[colPos:length(colOrderOriginal)])
  colOrder <- colOrder[!colOrder %in% c(col1,col2,col3)]
  
  
  setnames(table,c(col1,col2,col3),c("col1","col2","col3"))
  
  # DAP_tables2 <- table[,':='(N1PER1=paste0(get(amount),"(",get(perc),")"))]
  # [col1 != '' & col2 != ''& col3 != '',eval(c.name):=paste0(col1,"(",col2,",",col3,")")]
  # [col1 != '' & is.na(col2)& is.na(col3),eval(c.name):=col1]
  # [col1 != '' & col2 != ''& is.na(col3),eval(c.name):=paste0(col1,"(",col2,")")]
  DAP_tables2 <- table[col1 != '' & is.na(col2)& is.na(col3),eval(c.name):=col1]
  DAP_tables2 <- DAP_tables2[col1 != '' & col2 != ''& is.na(col3),eval(c.name):=paste0(col1," (",col2,")")]
  DAP_tables2 <- DAP_tables2[col1 != '' & col2 != ''& col3 != '',eval(c.name):=paste0(col1," (",col2,", ",col3,")")]
  DAP_tables2 <- DAP_tables2[,col1:=NULL][,col2:=NULL][,col3:=NULL]
  setcolorder(DAP_tables2,colOrder)
  
  return(DAP_tables2)
}

MergeThreeColumnsCI <- function(col1,col2,col3,table) {
  
  
  
  table <- copy(table)
  c.name <- paste0(col1,col2,col3)
  
  colOrderOriginal <- colnames(table)
  
  colPos <- min(which(colOrderOriginal %in% c(col1,col2,col3)))
  
  colOrder <- c(colOrderOriginal[1:colPos-1],c.name,colOrderOriginal[colPos:length(colOrderOriginal)])
  colOrder <- colOrder[!colOrder %in% c(col1,col2,col3)]
  
  
  setnames(table,c(col1,col2,col3),c("col1","col2","col3"))
  
  # DAP_tables2 <- table[,':='(N1PER1=paste0(get(amount),"(",get(perc),")"))]
  # [col1 != '' & col2 != ''& col3 != '',eval(c.name):=paste0(col1,"(",col2,",",col3,")")]
  # [col1 != '' & is.na(col2)& is.na(col3),eval(c.name):=col1]
  # [col1 != '' & col2 != ''& is.na(col3),eval(c.name):=paste0(col1,"(",col2,")")]
  DAP_tables2 <- table[col1 != '' & is.na(col2)& is.na(col3),eval(c.name):=col1]
  DAP_tables2 <- DAP_tables2[col1 != '' & col2 != ''& is.na(col3),eval(c.name):=paste0(col1," (",col2,")")]
  DAP_tables2 <- DAP_tables2[col1 != '' & col2 != ''& col3 != '',eval(c.name):=paste0(col1," (",col2,", ",col3,")")]
  DAP_tables2 <- DAP_tables2[,col1:=NULL][,col2:=NULL][,col3:=NULL]
  setcolorder(DAP_tables2,colOrder)
  
  return(DAP_tables2)
}

format_strnr <- function(x){
  
# This function takes values in string format and evaluate if the value is a real string, an integer or an decimal 
# The format will be changed according to the value that is decided upon.
# @@todo  check what happens when the value is not in string format() and adjust function according to that
  # browser()
  y <-suppressWarnings(as.numeric(x)) 
  pattern <- "\\b0\\.00\\b"
  if(grepl(pattern,x)){
    
    replacement <- "< 0.01"
    output_string <- gsub(pattern, replacement, x)
    return(output_string)
    
  }

  if (!is.na(y) && is.numeric(y) ){ 
  
  
    if (y %% 1 == 0) {
    
      y <- formatC(y, digits = 0, big.mark = ",", format= "f")
    
      } else if (y != round(y)){
    
       y <- formatC(y, digits = 2, decimal.mark = ".",big.mark = ",", format= "f")
      }
      x <- y

  }
  return(x)
}

rewind_to_0_with_an_n_of_zero <- function(intermediate_folder,RDS_filename,CheckVariable,UpdateVariable){
  # browser()
  # Read .rds dataset
  dataset <- readRDS(paste0(intermediate_folder,"/",RDS_filename))
  
  # Define the variables to check and modify
  variable1 <- CheckVariable
  variable2 <- UpdateVariable
  
  pattern <- "^\\s*<\\s*0\\.01$"
  
  
  # Process the dataset line by line
  for (i in 1:nrow(dataset)) {
    if (dataset[[variable1]][i] == 0 && grepl(pattern,dataset[[variable2]][i])) {
      dataset[[variable2]][i] <- 0
    }
  }
  saveRDS(dataset, paste0(intermediate_folder,"/",RDS_filename))
}

mask_value_when_n_less_than_5 <- function(intermediate_folder,RDS_filename,CheckandUpdateVariable){
  # browser()
  # Read .rds dataset
  dataset <- readRDS(paste0(intermediate_folder,"/",RDS_filename))
  
  # Define the variables to check and modify
  variable1 <- CheckandUpdateVariable
  # Process the dataset line by line
  for (i in 1:nrow(dataset)) {
    # browser()
    value <- suppressWarnings(as.numeric(dataset[[variable1]][i]))
    if (!is.na(value)  && is.numeric(value) && (value < 5) && (value>0) ) {
      # Your code here
      dataset[[variable1]][i] <- "< 5"
    }
  }
  saveRDS(dataset, paste0(intermediate_folder,"/",RDS_filename))  
  
  
}


# ----------------------------------------------------------------------------------------------------------------------------------
# forrestplot function made by roel
# 
# ----------------------------------------------------------------------------------------------------------------------------------

# d = as.data.frame(event_tmp)
# 
# cn.var.ylab = c("group") 
# cn.var.ylab2 = c("band") 
# 
# 
# cn.sufc = c('IR',"lb","ub")
# cn.sufc.l = c('IR','LL','UL')
# cn.IR = 'IR' 
# cn.LL = 'lb'	
# cn.UL = 'ub' 
# title = paste0(scheme[z,][["Event_name"]]," - ", scheme[z,][["DAP"]])
# log.scale = F
# log.xaxis = T
# x.label = 'Incidence rate/10.000 PY (+ 95% CI)'
# y.group = "group"
# sep = NULL


my_forestplot2 <- function(
    
  ## Parameters
  d,                                                         ## Data object
  cn.var.ylab,                      ## Column names (string) of columns which make up the label (can be a vector, see example below)
  cn.var.ylab2 = NULL, 
  #suffix columns
  cn.sufc,    ## Vector of columns names of the columns you want R to show behind the graph
  cn.IR,                                  ## Column name (string) of IR
  cn.LL,                                  ## Column name (string) of CI LL of IR
  cn.UL,      ## Column name (string) of CI UL of IR
  title = 'Forest plot',
  log.scale = F,
  log.xaxis = F,
  cn.sufc.l, ## column text
  x.label = NULL,
  y.group = NULL,
  sep = NULL
){
  
  #x="IR"
  #d = as.data.frame(event_tmp)
  #ifelse(is.na(d[[x]]), d[paste0("l.",x)] <- "NA", d[paste0("l.",x)] <- as.character(format(d[[x]],digits = 2)))
  #ifelse(is.na(d[[x]]), d[paste0("v.",x)] <- as.numeric(0.0), d[paste0("v.",x)] <- as.numeric(d[[x]]))
  
  
  d <- as.data.table(d)
  for(x in cn.sufc){
    d <- d[,eval(paste0("l.",x)) := fifelse(is.na(get(x)), "NA" , as.character(format(get(x),digits = 2)))]
    
    if(log.xaxis | log.scale){
      d <- d[,eval(x) := fifelse(is.na(get(x)), 0.0000001 , get(x))]
      d <- d[,eval(x) := fifelse(get(x) == 0, 0.0000001 , get(x))]
    }else{
      d <- d[,eval(x) := fifelse(is.na(get(x)), 0 , get(x))]  
    }
  }
  d <- as.data.frame(d)
  
  
  
  
  ## Set to character
  for(i in 1:length(cn.var.ylab)) d[,cn.var.ylab[i]] <- as.character(d[,cn.var.ylab[i]])
  if(!is.null(cn.var.ylab2)) for(i in 1:length(cn.var.ylab2)) d[,cn.var.ylab2[i]] <- as.character(d[,cn.var.ylab2[i]])
  
  #### Here the log scale is applied if input says so. There is a separation of data in plot and numbers in suffix columns
  if (log.scale) {
    
    d[[paste0(cn.IR,"2")]] <- log(d[[cn.IR]])
    d[[paste0(cn.LL,"2")]] <- log(d[[cn.LL]])
    d[[paste0(cn.UL,"2")]] <- log(d[[cn.UL]])
    
    
    
  } else{
    
    d[[paste0(cn.IR,"2")]] <- d[[cn.IR]]
    d[[paste0(cn.LL,"2")]] <- d[[cn.LL]]
    d[[paste0(cn.UL,"2")]] <- d[[cn.UL]]
    
  }
  
  
  #for(i in 1:length(cn.sufc)){
  #  if (max(nchar(as.character(round(d[[cn.sufc[[i]]]],digits = 2)))) > 8) d[[cn.sufc[[i]]]] <-  format(d[[cn.sufc[[i]]]], scientific = T, digits = 2) else{
  #    d[[cn.sufc[[i]]]] <- as.character(format(round(d[[cn.sufc[i]]],2)),width=3)
  #  }
  #}
  
  ## Number of rows
  n <- nrow(d)
  
  ## X and  Y limits of plot
  xl <- range(c((d[[paste0(cn.IR,"2")]]+0.0000001), (d[[paste0(cn.LL,"2")]]+0.0000001), (d[[paste0(cn.UL,"2")]]+0.0000001),na.rm = TRUE))
  yl <- c(0, 1+n)
  
  d <- d[n:1,]
  
  ## Create plot
  
  if (log.xaxis) xlabel <- 'Log incidence rate (+ 95% CI)' else xlabel <- 'Incidence rate (+ 95% CI)'
  if (!is.null(x.label)) xlabel <- x.label 
  
  
  if(log.xaxis == F) plot(NULL, xlim = xl, ylim = yl, main = title, xlab = xlabel, ylab = '', axes = F, yaxs = 'i', cex=1)
  if(log.xaxis == T) plot(NULL, xlim = xl, ylim = yl, main = title, xlab = xlabel, ylab = '', axes = F, yaxs = 'i', cex=1, log = "x")
  grid(ny = NA, nx = NULL)                           
  
  abline(h = 1:n, col = 'lightgrey', lty = 'dotted', lwd=2)
  
  ## Add IR and CI bounds
  
  if(is.null(y.group)){
    points(x = d[[paste0(cn.IR,"2")]], y = c(1:n), pch = 16, cex = 1)
    segments(x0 = d[[paste0(cn.LL,"2")]], y0 = c(1:n), x1 = d[[paste0(cn.UL,"2")]])
    
  }else{
    
    d$row <- c(1:n)
    sym <- c(16, 17,21, 3)
    seg <- c("black","black","darkgray", "azure4" )
    #j=1
    
    vec <- unique(d[[y.group]])
    
    for(j in 1:length(vec)){
      
      d_tmp <- d[d[[y.group]] == vec[j],]  
      points(x = d_tmp[[paste0(cn.IR,"2")]], y = d_tmp[["row"]], pch = sym[j], cex = 1)
      segments(x0 = d_tmp[[paste0(cn.LL,"2")]], y0 = d_tmp[["row"]], x1 = d_tmp[[paste0(cn.UL,"2")]], col = seg[j])
      rm(d_tmp)
      
      
      
    }
    rm(sym, vec)
    
    
    
    #Add separator line
    ###
    
    if(!is.null(sep)){
      tmp <- as.data.table(d)[, tmp := seq_len(.N), by = sep][, tmp2 := .N , by = sep][tmp == tmp2,][["row"]]
      tmp <- tmp[1:length(tmp) - 1] + 0.5
      abline(h = tmp, col = "darkgrey", lwd = 1.3)
      rm(tmp)
    }
    
    ###
    
    
    #abline(h = d[d[["row"]] != nrow(d) & d[["Ageband"]] == "80+" & d[["Group"]] == "Vaccinated",][["row"]] - 0.5, col = "darkgrey", lwd = 1.3)
  }
  
  points(x = d[d[[paste0("l.",cn.IR)]] == "NA",][[paste0(cn.IR,"2")]], y = d[d[[paste0("l.",cn.IR)]] == "NA",][["row"]], pch = 4, cex = 1, col = "red")
  
  ## Add axes
  axis(1, las = 1, cex.axis = 1)
  
  box()
  
  ## Add label
  cn.var.ylab <- cn.var.ylab[!is.null(cn.var.ylab) & !is.na(cn.var.ylab)]
  d$lab <- do.call(paste, c(d[cn.var.ylab], sep = ' - '))
  
  if(!is.null(cn.var.ylab2)) cn.var.ylab2 <- cn.var.ylab2[!is.null(cn.var.ylab2) & !is.na(cn.var.ylab2)]
  if(!is.null(cn.var.ylab2)) d$lab2 <- do.call(paste, c(d[cn.var.ylab2], sep = ' - '))
  
  #############
  
  mtext(d$lab, side = 2, line = 1, outer = F, at = c(1:n), las = 2, cex = 0.6, adj = 1, font = 2)
  if(!is.null(cn.var.ylab2)) mtext(d$lab2, side = 2, line = 7, outer = F, at = c(1:n) - 0.5, las = 2, cex = 1, adj = 1, font = 1)
  
  mtext("x-axis is generated automatically", side = 1, line = 5, outer = F, cex = 0.5)
  
  for (i in 1:length(cn.sufc)){
    mtext(cn.sufc.l[i], side = 4, line = 2+((i-1)*3), outer = F, at = n + 1, cex = 0.6, las=2, font = 2)
    
    #mtext(format(d[[cn.sufc[i]]],digits=2), side = 4, line = 2+((i-1)*3), outer = F, at = c(1:n), cex = 0.6, las=2)
    mtext(d[[paste0("l.",cn.sufc[i])]], side = 4, line = 2+((i-1)*3), outer = F, at = c(1:n), cex = 0.6, las=2)
    
  }
  
  
  
  ## End function
}


