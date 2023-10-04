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