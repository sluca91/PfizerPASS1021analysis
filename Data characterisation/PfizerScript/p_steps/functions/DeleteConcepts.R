#' Aim: in a sql database errors occur when a table already exists, or in steps where appending is needed you may want to start 
#' with a situation where the start table is not yet in the database to prevent duplication of data or mixture with data
#' from another instance
#'
#'
#'          
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name deleteConcepts
#' @keywords ??
#' @import DBI RSQLite 

NULL


#' @param db default = mydb | A dbi database connection object
#' @param concepts required | vector of strings | tables that you want to delete if they exists

#' @return No output. It only executes a query on the database that deletes tables
#' @export



DeleteConcepts <- function(db = mydb, concepts){
    
    #Check which concepts are available in the database to create a vector
    conceptsDelete <- dbListTables(mydb)[dbListTables(mydb) %in% concepts]
    
    #Only continue if there are any tables to remove
    if(length(conceptsDelete) > 0){
      
      for(i in conceptsDelete){
        
        #Delete the table
        q <- dbSendStatement(db, paste0("DROP TABLE IF EXISTS ", i))
        dbClearResult(q)
        
      }
      
    }

}