

MATCHING_SQL_spells_loop <- function(db, scheme){
  
  library("data.table")
  library("RSQLite")
  library("DBI")
  library("Rcpp")
  
  
  pathdb <- dbGetInfo(db)$dbname
  db <- dbConnect(RSQLite::SQLite(), pathdb)
  RSQLite:: sqliteSetBusyHandler(db, 10000)
  group2 <- copy(scheme)
  group2 <- as.data.table(group2)[, `:=` (FIRST_PFIZER = NULL, YEAR_BIRTH = NULL)]
  
  CODE0 <- paste0(paste0(colnames(group2), " = " ,group2[1,]), collapse = " AND ") 
  CODE1 <- paste0(scheme[1,][["FIRST_PFIZER"]]," BETWEEN ST2 AND EN2 AND ")
  CODE2 <- paste0("(",scheme[1,][["FIRST_PFIZER"]]," < VAC_DATE1 OR VAC_DATE1 IS NULL)")
  CODE3 <- paste0(scheme[1,][["YEAR_BIRTH"]]," BETWEEN YEAR_BIRTH - 1 AND YEAR_BIRTH + 1")
  
  CODE_WHERE2 <- paste0(CODE0," AND  " , CODE1 ,CODE2," AND  ", CODE3, collapse = " AND  ")
  
  Controls <- dbGetQuery(db, paste0("SELECT person_id AS Control FROM Controls WHERE ", CODE_WHERE2))
  
  if(nrow(Controls) > 0){
    
    group1 <- scheme
    
    CODE_WHERE1 <- paste0(paste0(colnames(group1), " = " ,scheme[1,]), collapse = " AND ") 
    
    Exposed <- dbGetQuery(db, paste0("SELECT person_id AS Exposed FROM Exposed WHERE ", CODE_WHERE1))
    
    Control <- as.integer(sample(as.character(Controls[["Control"]]), nrow(Exposed), replace = T))
    nb_match <- rep(length(Controls[["Control"]]), nrow(Exposed))
    T0 <- rep(scheme[1,][["FIRST_PFIZER"]], nrow(Exposed))
    dbDisconnect(db)
    return(as.data.table(cbind(Exposed, nb_match, Control, T0)))
  }else{
    dbDisconnect(db)
    return(data.table(Exposed = as.integer(), nb_match = as.integer(), Control = as.integer(), T0= as.integer() ))
  }
  
  
}



