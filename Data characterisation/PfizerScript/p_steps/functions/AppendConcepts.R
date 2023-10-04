#' Aim
#'
#'Append multiple concepts to a new concept. This can be used in situations where the question is if you have concept 1 OR
#'concept 2. In the meta data this is announced as an OR algorithm. (With this function the additional concepts are generated
#'prior to the query that concepts the concept to T0. in theory, this can also be done post.) This functions assumes equal columns
#'names and column order. When the table column structure is not exact equal among the needed tables, the needed columns can be filled in colls.  
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name AppendConcepts
#' @keywords ??
#' @import data.table

NULL


#' @param DB required | A DBIConnection object | Specifies the database where the input concepts are stored.
#' @param CONCEPTS required | vector of strings | the concepts that you want to append to a new concept
#' @param NAME required | string | name of the new concept
#' @param DT.coll default = "Date"  | string | column name where de date is stored in the old concepts  
#' @param colls default = "*" (select ALL) | SQL code in string | here you can specify which columns you want to retain. 
#' 
#' @return A data.table data frame. If no input concept is found NULL is returned

#' @export

AppendConcepts <- function(DB, CONCEPTS, NAME, DT.coll = "Date", colls = "*"){
  
  #Determine which concepts needed are not in the database. If missing concepts inform the user
  MISSING <-  CONCEPTS[!CONCEPTS %in% dbListTables(DB)]  
  if(length(MISSING) > 0) print(paste0(paste0(MISSING, collapse = " "), " not in database"))  
  
  #Only if any of the needed concepts in in the database a table is appended.
  if(any(CONCEPTS %in% dbListTables(DB))){
    
    #Make a variable that only has the needed concepts that are in the database. If you do not do this an error will
    #Occur when executing the SQL query.
    CONCEPTS <- CONCEPTS[CONCEPTS %in% dbListTables(DB)]
    
    #Write the SQL query to grab all concepts to 1 new table. This can be writen more elegant preventing duplication of code.
    if(length(CONCEPTS) > 1) Query <- paste0("SELECT DISTINCT ",colls," FROM ", CONCEPTS[1],paste0(paste0(" UNION SELECT ",colls," FROM ",  CONCEPTS[2:length(CONCEPTS)]), collapse = " "))
    if(length(CONCEPTS) == 1) Query <- paste0("SELECT DISTINCT ",colls," FROM ", CONCEPTS[1])
    
    #As an extra check input concepts that contain 0 rows, are feeded back to the user.
    for(i in CONCEPTS) if(dbGetQuery(DB, paste0("SELECT count(*) FROM ", i)) == 0) warning(paste0("0 cases in ",NAME," for ",i))
    
    #return the new table. Note that it is not writen directly to the database. This is flexible because it can used in 2 
    #manners but because the table is loaded to the working memory may give memory errors when the table becomes to big.
    #The latter did not occur in Pfizer yet.
    return(as.data.table(dbGetQuery(DB, Query))[, eval(DT.coll) := as.Date(get(DT.coll), origin="1970-01-01")])
  }else{
    warning(paste0("0 tables for ",NAME," in database"))
    return(NULL)
  }
}
