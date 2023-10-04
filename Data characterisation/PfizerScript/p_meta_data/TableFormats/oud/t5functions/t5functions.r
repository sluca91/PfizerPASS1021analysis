
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

                                      if(is.integer(temp[,i])){temp[,i] <- formatC(temp[,i], digits = 0, big.mark = ",", format= "f")}                                      
                                      if(is.numeric(temp[,i])){temp[,i] <- formatC(temp[,i], digits = 2, decimal.mark = ".",big.mark = ",", format= "f")}

                                      
                                    }
                                    
                                    
                                    temp <- data.table(temp)
                                    
                                    
                                    
                                    
                                    # Temp[['DAP']] daarmee maak je een nieuwe kolom aan in temp
                                    # @@ todo  Dap wordt ook toegevoegd als waarde aan de eerste twee rijen moet dat wel of niet?
                                    temp[['DAP']] <- DAP
                                    
                                    if(file.exists(fileLocation)) {
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
                                    
                                    if(!file.exists(fileLocation)) saveRDS(temp,fileLocation)

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

  for(i in 1:length(tablelist)){
    #----------------------------------------------------------------------------------------------------------
    # start the debugger
    # browser()
    #----------------------------------------------------------------------------------------------------------

    
    tabletitle <- tabletitles[i]
    
    TableFormats <- fread(paste0(projectFolder,"/FormatTable",as.integer(pull(tabletitle, tableNumber)),".csv"), na.strings = NULL ,colClasses = "character")

  
    #nr_daps_on_one_page <- 2
    DAP_tables_Location <- paste0(IntermediateFolder,'/',tablelist[i],'.rds')
    
    if(file.exists(DAP_tables_Location)){
          
      
      
      av_daps <- unique(readRDS(DAP_tables_Location)[["DAP"]])
      
      nr_daps_on_one_page <- pull(tabletitle, ndaps)
      if(nr_daps_on_one_page == "ALL"){nr_daps_on_one_page <- length(av_daps)}else{nr_daps_on_one_page <- as.integer(nr_daps_on_one_page)}
      
      needed_pages <- ceiling(length(av_daps)/nr_daps_on_one_page)
      
      print(tabletitle[["tableName"]])
      

      for(z in 1:needed_pages){
      page <- z
      print(z)
      
      st <- 1 + ((page - 1) * nr_daps_on_one_page)
      en <- nr_daps_on_one_page + ((page - 1) * nr_daps_on_one_page)
      
      selection_daps <- av_daps[st:en]
      
          #Get table
          DAP_tables <- readRDS(DAP_tables_Location)[DAP %in% selection_daps,]
          by_vars <- colnames(DAP_tables[,1:as.integer(pull(tabletitle, nby_vars))])
          
          test <- unique(DAP_tables[, .(SUM = .N) , by = "DAP"][["SUM"]])
          
          if(length(test) != 1){warning("Tables are not similar in number of rows  between DAPS and not processed")}else{
            DAP_tables[["id"]] <- rep(c(1:test), length(unique(DAP_tables[["DAP"]])))
            if(nrow(unique(DAP_tables[, c(by_vars ,"id"), with = F])) != test){stop("Order is not similar between DAPS")}
            }
          
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
          
          rm(test)
          
          #Provide needed variables
          #storeOrder <- unique(DAP_tables[DAP == selection_daps[1], by_vars, with = F][, order := seq_len(.N)])
        
          #Provide an id to test
          #setorderv(DAP_tables, c(by_vars, "DAP"))
          #DAP_tables <- DAP_tables[,id := seq_len(.N), by = c(by_vars, "DAP") ]
          by_vars <- c(by_vars, "id")
          
        
          data_vars <- colnames(DAP_tables)[!colnames(DAP_tables) %in% c(by_vars, "DAP")] 
          cast_expr <- formula(paste0(paste0(by_vars, collapse = " + "), " ~ DAP"))
          
          #Make new table
          order <- lapply(selection_daps, function(x) paste0(data_vars,"_", x))
          order2 <- NULL 
          for(k in 1:length(order)){order2 <- c(order2, unlist(order[k]))}
          rm(order)
          
          TableFormats_new <-  do.call(cbind, lapply(c(1:nr_daps_on_one_page), function(x) TableFormats[,c(data_vars)  , with = F]))
          colnames(TableFormats_new) <- order2 
          
          label2 <- as.data.frame(rbind(rep(selection_daps, each = length(data_vars))))
          colnames(label2) <- order2
          
          TableFormats_new <- rbind(label2, TableFormats_new)
          
          DAP_tables2 <- data.table::dcast(DAP_tables, formula = cast_expr , value.var =  data_vars )
          
          #Restore original order that changes by dcast
          
          setorder(DAP_tables2, "id")[, id := NULL]
          #[, id := NULL]
          
          DAP_tables2 <-  rbindlist(list(TableFormats_new, DAP_tables2), use.names = , fill = T)
          
          setcolorder(DAP_tables2, c(by_vars[!by_vars %in% "id"], order2))
          
          DAP_tables2[is.na(DAP_tables2)] <- ""
          
          d <- DAP_tables2
          
          # browser()
          
          header_rows <- nrow(TableFormats_new)
          
          ft <- flextable(d[c((header_rows + 1):nrow(d)),]) ## Flextable of the data (excl. header(s))
          ft <- delete_part(ft, part = "header")  ## Remove the header
          
          
          for(k in header_rows:1){
          ft <- add_header_row(x = ft, values = fun1(as.character(d[k,]))[[1]], colwidths = fun1(as.character(d[k,]))[[2]]) ## Adds second row
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
          ft <- set_caption(ft, caption = pull(tabletitle, tabletitle), style = "Table Caption")
          ft <- font(ft, fontname = "Times New Roman", part = "all")          
      
          TemplatePath <- paste0(projectFolder,"/t5functions/wordTemplates/template_word.docx")
          # C:\Users\jmaaskan\GitHub\C4591021_PfizerUMC\Data characterisation\PfizerScript\p_meta_data\TableFormats\t5functions\wordTemplates
          
          doc_section_1 <- read_docx(path = TemplatePath)
          
          # Add flextable to doc body
          doc_section_1 <- body_add_flextable( doc_section_1, value = ft, align = "left", keepnext = F,split=T)
          
          # create a prop_section to orient the page in a landscape format and make it an oddPage(?)
          ps <- prop_section( page_size = page_size(orient = pull(tabletitle, orient)), type = "oddPage")
          
          # Add the doc_section_1 to itself using a body_end_block_section with the property value of the block_section equal to the previous prop_section ps
          doc_section_1 <- body_end_block_section( x = doc_section_1,  value = block_section(property = ps))
          
          # print doc_section_ to its target
          print(doc_section_1, target = paste0(InterMediateFolder,"/temp_docx_output/",tablelist[i],"_instance_",z,".docx"))
          
          
          rm(d); rm(ft)
          rm(page, st,en, selection_daps, by_vars, data_vars, cast_expr, order2, TableFormats_new, DAP_tables2, header_rows, TemplatePath, doc_section_1  )

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










