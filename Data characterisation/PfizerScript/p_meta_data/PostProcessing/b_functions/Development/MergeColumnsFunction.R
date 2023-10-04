# saveRDS(DAP_tables,'C:/Temp/test.rds')

test <- readRDS('C:/Temp/test.rds')[,N1PER1:=NULL]


amount <- 'N1'
perc <- 'PER1'
table <- test

MergeColumns <- function(amount,perc,table) {
 
  
  
  
  
  
  table <- copy(table)
  c.name <- paste0(amount,perc)
  
  colOrderOriginal <- colnames(table)
  
  colPos <- min(which(colOrderOriginal %in% c(amount,perc)))
  
  colOrder <- c(colOrderOriginal[1:colPos-1],c.name,colOrderOriginal[colPos:length(colOrderOriginal)])
  colOrder <- colOrder[!colOrder %in% c(amount,perc)]
  
  
  setnames(table,c(amount,perc),c("amount","perc"))
  
  # DAP_tables2 <- table[,':='(N1PER1=paste0(get(amount),"(",get(perc),")"))]
  DAP_tables2 <- table[amount != '' & perc != '',eval(c.name):=paste0(amount,"(",perc,")")]
  DAP_tables2 <- DAP_tables2[,amount:=NULL][,perc:=NULL]
  setcolorder(DAP_tables2,colOrder)
  
  return(DAP_tables2)
}