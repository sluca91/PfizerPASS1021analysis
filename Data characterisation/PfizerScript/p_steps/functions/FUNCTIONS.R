
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

# 
# 
# CreateBands <- function(bands, NEWBORNS = T){
#   
#   length0 <- nchar(as.character(max(bands)))
#   
#   if(NEWBORNS & bands[1] == 0 & bands[2] != 1){bands[1] <- 1}
#   if(NEWBORNS & bands[1] == 0 & bands[2] == 1){bands <- bands[2:length(bands)]}
#   
#   bands_list <- list()
#   
#   for (k in 1:length(bands)){
#     if(!k== length(bands)){
#       bands_list[["band"]][k] <- paste0(bands[k],"-",bands[k+1]-1)
#       bands_list[["ST"]][k] <- bands[k]
#       bands_list[["END"]][k] <- bands[k+1]-1
#       bands_list[["band0"]][k] <- paste0(formatC(bands[k], width = length0, format = "d", flag = "0"),"-",formatC(bands[k+1]-1, width = length0, format = "d", flag = "0"))}
#     
#   }
#   
#   
#   bands <- as.data.frame(cbind("band" = bands_list[["band"]],"ST" = bands_list[["ST"]],"END" = bands_list[["END"]], "band0" = bands_list[["band0"]]))
#   
#   bands[,1:ncol(bands)] <- lapply(bands[,1:ncol(bands)], as.character)
#   
#   new <- list()
#   
#   for(i in 1:nrow(bands)){
#     if(length(strsplit(bands[i,"band"],"-")[[1]])==2) {
#       ST <- as.integer(strsplit(bands[i,"band"],"-")[[1]][1])
#       EN <- as.integer(strsplit(bands[i,"band"],"-")[[1]][2])
#       vec <- c(ST:EN)
#       new[[i]] <-cbind(bands[i,][rep(seq_len(nrow(bands[i,])), each = length(vec)),],"new" = vec)
#       
#     } else new[[i]] <- bands[i,]
#     
#   }
#   
#   bands <- do.call(rbind,new)
#   bands <- as.data.table(bands)[,INT := as.numeric(new)][,.(band,INT,band0)]
#   
#   setorder(bands,INT)
#   ORDER <- as.data.table(cbind("band" = unique(bands[,band]),"Order" = seq(from = 1, to = length(unique(bands[,band])),by = 1)))[,Order := as.integer(Order)]
#   bands <- merge(x= bands, y= ORDER, by = "band", all.x = T) 
#   
#   if(NEWBORNS){bands <- rbindlist(list(list("0",0,formatC(0, width = length0, format = "d", flag = "0"),0),bands))}
#   
#   setorder(bands,Order)
#   
#   
# }



# IMPORT_PATTERN <- function(pat,dir, colls = NULL, colls.new = NULL, exprss = NULL, date.colls = NULL, append = T){
#   obs_files<-list.files(dir, pattern=pat)
#   if(!append & length(obs_files) > 1){stop("Several files meet pattern while append is FALSE")}
#   
#   if(length(obs_files) > 0){
#     temp <- list()
#     for(i in 1:length(obs_files)){
#       
#       if(!is.null(colls)){TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, select = colls, na.strings = c("", NA))}else{
#       TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, na.strings = c("" , NA))
#       }
#       
#       #if(!is.null(colls)){ TEMP <- TEMP[, ..colls] }
#       
#       
#       if(!is.null(colls) & !is.null(colls.new)){setnames(TEMP, eval(colls),eval(colls.new))}
#       
#           
#       #TEMP[, (colnames(TEMP)) := lapply(.SD, as.character), .SDcols = colnames(TEMP)]
#       invisible(lapply(colnames(TEMP), function (x) if (class(TEMP[[x]]) != "character") TEMP[, eval(x) := as.character(get(x)) ]))
#       
#       #Add correction for spaces to prevent misinterpretaion of NA's
#       invisible(lapply(colnames(TEMP), function(x) TEMP <- TEMP[, eval(x) := trimws(get(x), "b")]))
#       TEMP[TEMP == ""] <- NA
#       
#       
#       
#       if(!is.null(date.colls)){lapply(date.colls, function (x)  TEMP[, eval(x) := as.Date(get(x),"%Y%m%d") ])}
#       
#       if(!is.null(exprss)){
#         
#         rem1 <- nrow(TEMP) 
#         TEMP <- TEMP[eval(exprss),]
#         rem2 <- nrow(TEMP)
#         print(paste0(rem2," selected rows from ",rem1," rows after evaluation of exprss"))
#       }
#       
#       if(i == 1) {FILE <- TEMP}
#       if(i > 1) {FILE <- rbindlist(list(TEMP,FILE),fill = T, use.names = T)}
#       rm(TEMP)
#       gc()
#       
#       
#     }
#   }else FILE <- NULL 
#   
#   return(FILE)
#   rm(FILE,obs_files)
#   gc()
# }


# SeparateRanges <- function(codesheet,code_col,separator,vars){
#   
#   new <- list()
#   
#   for(i in 1:nrow(codesheet)){
#     if(length(strsplit(codesheet[i,code_col],separator)[[1]])==2) {
#       
#       ST <- strsplit(codesheet[i,code_col],"-")[[1]][1]
#       EN <- strsplit(codesheet[i,code_col],"-")[[1]][2]
#       
#       EN_char <- gsub("^([[:alpha:]]*).*$","\\1",EN)
#       ST_char <- gsub("^([[:alpha:]]*).*$","\\1",ST)
#       if(EN_char != ST_char) warning("Chars differ")
#       
#       EN_num <- floor(as.numeric(gsub("[^[:digit:].]", "\\1",EN)))
#       ST_num <- floor(as.numeric(gsub("[^[:digit:].]", "\\1",ST)))
#       
#       vec <- paste0(ST_char,formatC(c(ST_num:EN_num), width = 2, format = "d", flag = "0"))
#       
#       
#       
#       
#       new[[i]] <-cbind(codesheet[i,][rep(seq_len(nrow(codesheet[i,])), each = length(vec)),],"Code_new" = vec)
#       
#       
#       
#       
#     } else new[[i]] <- codesheet[i,]
#     
#   }
#   
#   file <- do.call(rbind,new)
#   
# }


INPUTMATRIX  <- function(d,value,type,var,var.v,cat = NULL,cat.v = NULL, per = F, perdir = "row", mask = T, output = "matrix" ){
  
  d <- copy(d)
  
  if(is.null(cat)){
    d[,cat := "ALL"]
    cat <- "cat"
    cat.v <- "ALL"
  }
  
  if(!class(d[[cat]]) == "character") d[,eval(cat) := as.character(get(cat)) ]
  if(!class(d[[var]]) == "character") d[,eval(var) := as.character(get(var)) ]
  
  if(type == "sum")graph <- aggregate(d[[value]],list(d[[var]],d[[cat]] ), FUN = sum,na.rm = TRUE)
  if(type == "count"){graph <- aggregate(!is.na(d[[value]]),list(d[[var]],d[[cat]] ), FUN = sum,na.rm = TRUE)}
  if(type == "none"){
    tmp.v <- c(var,cat,value)
    #graph <- d[,.(get(var),get(cat),get(value))]
    graph <- d[,..tmp.v]
    rm(tmp.v)
  }
  colnames(graph) <- c("Var","Cat","Val")
  
  graph <- graph[!is.na(graph[["Val"]]),]
  
  if(any(duplicated(graph))) stop("Duplicated values in INPUMATRIX") 
  graph[["Val"]] <- as.numeric(graph[["Val"]])
  
  temp <- matrix(NA, ncol = length(var.v),nrow = length(cat.v))
  colnames(temp) <- as.character(var.v)
  row.names(temp) <- as.character(cat.v)
  
  
  for (i in as.character(cat.v)){
    
    for(j in as.character(var.v)){temp[i,j] <- ifelse(any(graph$Cat == i & graph$Var == j),graph$Val[graph$Cat == i & graph$Var == j] ,0)}
    
  }
  
  if(per){
    if(perdir == "row") temp <- round((temp/rowSums(temp))*100,1)
    if(perdir == "col") temp <- round(prop.table(temp, 2)*100,1)
    
    
  } 
  
  
  if(mask & !per){
    temp[temp > 0 & temp < 5] <- 5
    
  }
  
  if(output == "long"){
    temp <- as.data.table(temp, keep.rownames = cat)
    temp <- melt(temp,id.vars = cat ,measure.vars =  colnames(temp)[!colnames(temp) %in% cat], variable.name = var, value.name = value )
    #temp[, ':=' (eval(cat) = as.character(get(cat)), eval(var) = as.character(get(var)), eval(value) = as.numeric(get(value)))]  
    temp[, eval(cat) := as.character(get(cat))]  
    temp[, eval(var) := as.character(get(var))] 
    temp[, eval(value) := as.numeric(get(value))] 
    
  }
  
  
  
  return(temp)
  rm(d,graph,temp)
  gc()
}


# POP_TREE <- function(m, xlabel = 'Percentage', offset = 1, linewidth = 15, cols = c('pink', 'lightblue'), x.axis.steps = 2, cex.percentage = 0.7){
#   xlimits <- c(-max(m) - offset + (0.1*-max(m)), max(m) + offset + (0.1*max(m)))    
#   ylimits <- c(1, ncol(m))
#   x.lab <- seq(0, ceiling(max(xlimits)), x.axis.steps)
#   x.at <- seq(offset, ceiling(max(xlimits) + offset), x.axis.steps)
#   plot(NULL, xlim = xlimits, ylim = ylimits, main = '', xlab = xlabel, axes = F, ylab = '')
#   abline(v = c(-x.at, x.at), col = 'lightgrey', lty = 'dotted')
#   segments(x0 = -offset, y0 = 1:ncol(m), x1 = -1 * m[1,] - offset, lwd = linewidth, lend = 1, col = cols[1])
#   segments(x0 = offset, y0 = 1:ncol(m), x1 = m[2,] + offset, lwd = linewidth, lend = 1, col = cols[2])
#   text(colnames(m), x = 0, y = 1:ncol(m), cex = 0.8)
#   text(as.character(round(m[1,], 1)), x = -m[1,] - offset, y = 1:ncol(m), pos = 2, cex = cex.percentage)
#   text(as.character(round(m[2,], 1)), x = m[2,] + offset, y = 1:ncol(m), pos = 4, cex = cex.percentage)    
#   axis(1, x.lab, at = x.at, cex.axis = 0.8)
#   axis(1, rev(x.lab), at = -rev(x.at), cex.axis = 0.8)
#   #legend('topright', legend = row.names(m), ncol = 1, cex = 0.8, fill = cols, bty = 'n')
#   mtext(rownames(m)[1], at = -offset, side = 3, adj = 1, line = 1)
#   mtext(rownames(m)[2], at = offset, side = 3, adj = 0, line = 1)
# }






# Line_plot2 <- function(MATRIX, title, x.l, y.l, y.axis = F, color = NULL, leg = T, y.labels = NULL, y.thicks = NULL, y.las = 2){
#   
#   
#   #if(is.null(color)) color <- rainbow(nrow(MATRIX))
#   if(is.null(color)) color <- c(2:(nrow(MATRIX)+1))
#   plot(NULL,type="l",xlab = x.l,ylab = y.l,cex.lab = 0.8,lwd=3,xlim = c(1,length(colnames(MATRIX))), ylim = c(0,max(MATRIX)), main= title, axes = F)
#   
#   #X-axis
#   if(!y.axis) {axis(1,at=1:length(colnames(MATRIX)),labels = colnames(MATRIX), las=2,cex.axis =0.6)}else{
#   axis(1,at=y.thicks,labels = y.labels, las = y.las,cex.axis = 0.6)}
#       
#   if(ceiling(max(MATRIX)) < 10) t <- format(seq(from = 0,to = ceiling(max(MATRIX)),length.out = 11),digits = 1)
#   if(ceiling(max(MATRIX)) >= 10)t <- format(seq(from = 0,to = ceiling(max(MATRIX)),length.out = 11),digits = 0)
#   #axis(2, at = t , labels = as.character(t),cex.axis =0.6 )
#   
#   #axis(2, at = t , labels = as.character(t),cex.axis =0.6 )
#   axis(2, cex.axis =0.6 )
#   
#   #mtext(y.l, side=2, line=2, cex=0.8,las=0, col="black")
#   #mtext(x.l, side=1, line=2, cex=0.8,las=0, col="black")
#   
#   
#   #temp <-MATRIX[,which(colSums(temp)>0),drop=F]
#   for(i in 1:nrow(MATRIX)){
#     #if(max(temp[i,]) > 0){
#     lines(MATRIX[i,],lwd=2.3,type = 'l',col = color[i])
#     #}
#   }
#   if(leg)legend("right",legend =  rownames(MATRIX), col = color, cex = 0.5,pch=10, box.col = "white",inset = c(-0.1, 0))
#   
# }



# MergeGaps <- function(file, start_date, end_date, id, max_gap){
#   
#   file <- copy(file)
#   order <- colnames(file)
#   
#   x1 <- nrow(file)
#   file <- file[get(end_date) > get(start_date),]
#   x2 <- nrow(file)
#   
#   if (x1 != x2) print(paste0(x1 - x2, " rows are removed because start start and end date were not in the correct order"))
#   
#   setkeyv(file, c(id, start_date)) 
#   file <- file[, row := rownames(file)]
#   file <- file[, shift := shift(get(end_date),n = 1), by = id][, gap := get(start_date) - shift ]
#   
#   #old
#   #file <- file[shift >= get(start_date)-1  , overlapping := "overlap"  ]
#   #file <- file[which(file[,overlapping] == "overlap")-1  , overlapping := "overlap" ]
#   
#   
#   file <- file[gap < max_gap, divide := T]
#   file <- file[, shift2 := shift(divide,n = -1), by = id][divide | shift2, group := T]
#   file <- file[is.na(divide) & shift2, group_id := row]
#   
#   file <- file[group == T, forward_fill := group_id[1], .(cumsum(!is.na(group_id)))]
#   file <- file[group == T, eval(start_date) := min(get(start_date)), by = forward_fill ]
#   file <- file[group == T, eval(end_date) := max(get(end_date)), by = forward_fill ]
#   
#   order <- c(id,start_date)
#   setorderv(file, order)
#   cols <- c(id,start_date,end_date)
#   OBSERVATION_PERIODS2 <- unique(file[,..cols])[, num_spell := cumsum(!is.na(get(start_date))), by = eval(id)]
#   setcolorder(OBSERVATION_PERIODS2, order)
#   
#   return(OBSERVATION_PERIODS2)
#   
#   rm(file)
#   gc()
#   
# }









#Outcomes moet als events geinterepreteerd worden. Kan zijn exposures of conditions
# CleanOutcomes <- function(Dataset = NULL, Person_id, Rec_period = NULL, Outcomes = NULL, Name_event = NULL, Date_event = NULL){
#   
#   if(length(Outcomes) != length(Rec_period)) stop("Specifiy the same number of Rec_periods as Outcomes")
#   
#   Dataset  <- copy(Dataset)[get(Name_event) %in% Outcomes]
#   tmp <- copy(Dataset[0])
#   
#   for (i in 1:length(Outcomes)){
#     
#     
#     print(paste("Remove ",Outcomes[i], "outcomes (conditions or vaccines) within a the Rec_period distance of ",Rec_period[i]," days"))
#     Dataset_temp  <- copy(Dataset)[get(Name_event) == Outcomes[i],]
#     
#     it=1
#     while(nrow(Dataset_temp) > 0){ 
#       
#       setorderv(Dataset_temp,c(Person_id,Name_event,Date_event))
#       Dataset_temp <- Dataset_temp[,D := shift(get(Date_event)),by = c(Person_id,Name_event) ]
#       Dataset_temp <- Dataset_temp[,dif := get(Date_event) - D]
#       Dataset_temp <- Dataset_temp[is.na(dif), dif := 0 ][,dif := as.numeric(dif)]
#       Dataset_temp <- Dataset_temp[,cumdif := cumsum(dif),by = c(Person_id,Name_event)]
#       
#       Dataset_temp2 <- Dataset_temp[ cumdif <= Rec_period[i],]
#       setorderv(Dataset_temp2,c(Person_id,Name_event,Date_event))
#       Dataset_temp2 <- Dataset_temp2[,.SD[1],by = c(Person_id,Name_event)][,Iteration := it]
#       
#       tmp <- rbindlist(list(tmp, Dataset_temp2),fill=T)
#       rm(Dataset_temp2)
#       
#       Dataset_temp <- Dataset_temp[cumdif > Rec_period[i],]
#       
#       lapply(c("dif","cumdif","D"), function(x){Dataset_temp <-Dataset_temp[,eval(x) := NULL]})
#       print(paste0("Cycle ",it))
#       it=it+1
#       gc()
#     }
#     
#     
#     
#     rm(Dataset_temp, it)
#     gc()
#     
#     
#     
#     
#     
#     
#   }  
#   lapply(c("dif","cumdif","D"), function(x){tmp <- tmp[,eval(x) := NULL]})
#   setorderv(tmp,c(Person_id,Name_event,Date_event))
#   return(tmp)
#   
# } 
# 


remove_files <- function(dir,names = NULL, file.type = NULL){
  
  if(is.null(names)){
    x <- list.files(paste0(dir))
    names <-x[substr(x,nchar(x) - (nchar(file.type) - 1) , nchar(x)) == file.type]
  }
  
  for(i in names){
    if(file.exists(paste0(dir,i))) file.remove(paste0(dir,i))
  }
}  



Flowchart <- function(file, expr_list, id = NULL){    
  
  FlowChart <- list()
  
  file <- copy(file)
  
  for (j in 1:length(expr_list)){
    
    before <- nrow(file)
    if(!is.null(id)) before2 <- length(unique(file[[id]]))
    
    file <- file[eval(expr_list[[j]]),]
    
    after <- nrow(file)
    if(!is.null(id)) after2 <- length(unique(file[[id]]))
    
    FlowChart[[paste("Step_",j)]]$step <- names(expr_list)[j]
    FlowChart[[paste("Step_",j)]]$before_rows <- before
    FlowChart[[paste("Step_",j)]]$after_rows <- after
    
    if(!is.null(id)) FlowChart[[paste("Step_",j)]]$before_subjects <- before2
    if(!is.null(id)) FlowChart[[paste("Step_",j)]]$after_subjects <- after2
    
    rm(before,after,before2,after2)
    gc()
  } 
  
  
  FlowChart <- as.data.table(do.call(rbind,FlowChart))
  
  return(list(file = file, FlowChart = FlowChart))
  
  
}
# 
# 
# CountHistorical <- function(file, c.date, lookback, id, lookback_function = "sum"){
#   
#   file <- file[, month := month(get(c.date))][, year := year(get(c.date))]
#   file <- file[,label := paste0(sprintf("%02d",month),"-",year)]
#   
#   comb <- as.data.table(expand.grid(1:12,c((year(start_study_date) - lookback):year(end_study_date))))[,label := paste0(sprintf("%02d",Var1),"-",Var2)]
#   comb <- comb[, order := as.numeric(row.names(comb))]
#   comb <- comb[Var1 == month(start_study_date) & Var2 == year(start_study_date), TIME := 0]
#   comb <- comb[, TIME2 := order - comb[TIME == 0, order]]
#   comb <- comb[TIME2 > - (12 * lookback) - 1, ]
#   
#   file <- merge(x = comb[,.(label,TIME2)], y = file, by = "label", all.x = T)
#   if(lookback_function == "sum"){file <- file[, .(NB = sum(!is.na(eval(Date)))), by = c(id, "label", "TIME2")]}
#   if(lookback_function == "min"){file <- file[, .(NB = min(eval(Date))), by = c(id, "label", "TIME2")]}
#   setorder(file,TIME2)
#   setnames(file, eval(id), "id")
#   
#   
#   
#   file <- as.matrix(data.table::dcast(file, id  ~  TIME2, value.var = "NB")[!is.na(id),], rownames = "id")
#   
#   if(lookback_function == "sum") file[is.na(file)] <- 0
#   
#   TEMP <- matrix(NA,nrow = nrow(file), ncol = ncol(file) - (12 * lookback))
#   colnames(TEMP) <- comb[TIME2 >= 0,label]
#   rownames(TEMP) <- row.names(file)
#   
#   if(lookback_function == "sum") for(i in ((12 * lookback) + 1):max(as.numeric(ncol(file)))) TEMP[,i - (12 * lookback)] <- rowSums(file[, (i- (12 * lookback)):(i-1)])
#   if(lookback_function == "min") for(i in ((12 * lookback) + 1):max(as.numeric(ncol(file)))){ 
#     
#     #TEMP[,i - (12 * lookback)] <- do.call(pmin, c(as.list(as.data.table(file[, (i- (12 * lookback)):(i-1)])), na.rm = TRUE))
#     TEMP[,i - (12 * lookback)] <- do.call(pmin, c(as.list(as.data.table(file[, (i- (12 * lookback)):(i)])), na.rm = TRUE))
#     
#     
#   }
#   
#   
#   
#   #TEMP <- as.data.table(TEMP, keep.rownames = T)
#   #TEMP <- melt(TEMP, id.vars = "rn", measure.vars =  colnames(TEMP)[!colnames(TEMP) %in% "rn"], variable.name = "month", value.name = "SUM_year")
#   #TEMP <- TEMP[SUM_year > 0,]
#   #setnames(TEMP, "rn", eval(id))
#   
#   return(TEMP)
#   
#   rm(TEMP,file)
#   gc()
#   
# } 
# 


### FUNCTIONS FOR TABLE SHELLS
Add_Results_To_Template <- function(template,results){
  dimTemplate <- dim(template)
  dimResults <- dim(results)
  origin <- dimTemplate-dimResults+1
  template[c(origin[1]:dimTemplate[1]),c(origin[2]:dimTemplate[2])] <- results
  return(template)
}

## Helper function to remove direct duplicates (e.g. 'A', 'A', 'B', 'B', 'A' becomes 'A', 'B', 'A') and count
## v should not contain missing values
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

Create_FlexTable <- function(filledTemplate,head.num,footer.text){
  ## head.num <- Indicate how many of the first rows are the header
  
  ## Actual table
  ft <- flextable(filledTemplate[(1+head.num):nrow(filledTemplate),]) ## Flextable of the data (excl. header(s))
  ft <- delete_part(ft, part = "header")  ## Remove the header
  for (headNum in head.num){
    ft <- add_header_row(x = ft, values = fun1(as.character(filledTemplate[headNum,]))[[1]], 
                         colwidths = fun1(as.character(filledTemplate[headNum,]))[[2]]) ## Adds first row
  }
  
  ft <- merge_v(ft, j = ~ HEADER)          ## Merge similar values of AECAT 
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- border_inner(ft)
  ft <- border_outer(ft)
  ## ft <- set_caption(ft, caption = "Caption",  style = "Table Caption", autonum = run_autonum(start_at = 4)) ## I cant find a way to manually set the caption. So I now use the section header for this purpose.
  ft <- add_footer(ft, Caption = footer.text ) ## Add footer information
  ft <- merge_at(ft, j = 1:ncol(filledTemplate), part = "footer")
  
  return(ft)
  
}



RUN_LOG <-  function(output.loc){
  
  
  library(logr)
  
  start_file <- rstudioapi::getSourceEditorContext()$path
  
  loc <- gregexpr(pattern ='/', start_file)[[1]][length(gregexpr(pattern ='/', start_file)[[1]])]
  start_file <- substr(start_file, loc + 1, nchar(start_file) - 2)
  
  log.loc <- file.path(paste0(output.loc,start_file,".log"))
  log_open(log.loc, logdir = F)
  
  if(exists("DAP")) logr::log_print(c('DAP Name:',DAP))
  if(exists("StudyName")) logr::log_print(c('StudyName Name:',StudyName))
  if(exists("parallel_method")) logr::log_print(c('Parallel Method:',parallel_method))
  if(exists("MATCHING_METHOD")) logr::log_print(c('MATCHING_METHOD Method:',MATCHING_METHOD))
  if(exists("start_study_date")) logr::log_print(c('Start Study Period:',as.character(start_study_date)))
  if(exists("end_study_date")) logr::log_print(c('End Study Period:',as.character(end_study_date)))
  if(exists("lookback_period")) logr::log_print(c('LookBack Period:',lookback_period))
  if(exists("max_spells_gap")) logr::log_print(c('Maximum Spell Gap:',max_spells_gap))
  
  log_close()
  
}

