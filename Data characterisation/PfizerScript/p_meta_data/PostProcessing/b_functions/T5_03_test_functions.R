DAPTablesTest1 <- function(test, DAP_tables, by_vars) {
  
  if(length(test) != 1){warning("Tables are not similar in number of rows  between DAPS and not processed")}else{
    # browser()
    DAP_tables[["id"]] <- rep(c(1:test), length(unique(DAP_tables[["DAP"]])))
    #if(nrow(unique(DAP_tables[, c(by_vars ,"id"), with = F])) != test){stop("Order is not similar between DAPS")}
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
