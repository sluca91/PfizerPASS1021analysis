


CountCodes <- function(
  Concept,
  db 
  
  
){
  #browser()
  library(data.table)
  library(RSQLite)
  library(DBI)
  library(Rcpp)
  
  db <- dbConnect(RSQLite::SQLite(), db)
  RSQLite:: sqliteSetBusyHandler(db, 10000)
  
  print(Concept)
  colls <- colnames(dbGetQuery(db ,paste0("SELECT * FROM ", Concept," LIMIT 1")))
  
  if(any(colls %in% c("Voc", "Value"))){
    code_select <- c("Voc", "Value")[c("Voc", "Value") %in% colls]
  }
  else{
    code_select <- NULL
    }

  if(!is.null(code_select)){
    
    if(length(code_select) > 1) 
      code_select <- paste0(code_select, collapse = ",")
    
    TEMP <- as.data.table(dbGetQuery(db,
                                     
                                     paste0(
                                       "
             SELECT ",code_select,", COUNT(*) AS NB FROM (SELECT DISTINCT * FROM ",Concept,") GROUP BY ",code_select,"
             
             "
                                       
                                     )))
    
    TEMP <- TEMP[, Outcome := Concept ]
    return(TEMP)
    rm(TEMP)
    gc()
    
  }
  else if(any(colls %in% c("Date"))){
    TEMP <- as.data.table(dbGetQuery(db,
                                     
                                     paste0(
                                       "
             SELECT COUNT(*) AS NB FROM (SELECT DISTINCT * FROM ",Concept,")
             
             "
                                       
                                     )))
    
    TEMP <- TEMP[, c("Voc","Value","Outcome") := list("","",Concept) ]
    return(TEMP)
    rm(TEMP)
    gc()
    
  }
  else{
    return(NULL)
    }
  
  
}
