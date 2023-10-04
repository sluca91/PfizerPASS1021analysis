
#' Aim
#'
#'Create a new concept from multiple concepts using an append and a join. This function assumes 1 starting concept that is
#'joined to 1 or multiple appended concepts. A join does represent an AND and an append does represent an OR. This functions
#'was primary developed to create covid severity concepts. 
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CovidSeverity
#' @keywords ??
#' @import data.table

NULL


#' @param db.path required | string | the path to a SQLite database file (.db) 
#' @param outcome.name required | string | the name of the new concept which is the name of the written table in the database.
#' @param or.cols required | vector of strings | name(s) of the concepts/tables that need to be appended
#' @param and.col required  | string | name of the concept/table that is joined to the appended tables as specified in or.cols
#' @param t.post required | +/- number | specify the upper border of the time range in days for the join related to the date of and.col. 0 is on the date, before is minus.
#' @param t.prior required | +/- number | specify the lower border of the time range in days for the join related to the date of and.col. 0 is on the date, before is minus.
#' 
#' @return A table written to a SQLite database.

#' @export


CovidSeverity <- function(or.cols, and.col, db.path , outcome.name, t.post, t.prior){ 
  
  #Open connection with database
  db <- dbConnect(RSQLite::SQLite(), db.path)
  
  #Make a temp intermediate table for the query that needs to be joined
  dbWriteTable(db, "TEMP" , AppendConcepts(DB = db, CONCEPTS = or.cols, NAME = outcome.name, colls = " person_id, Date "), overwrite = T, append = F)
  
  #Check if variable is already in database. If so delete
  if(outcome.name %in% dbListTables(db)){
    p <- dbSendStatement(db ,paste0("DROP TABLE ",outcome.name ))
    dbClearResult(p)
  } 
  
  #Do query which is a inner join taking into account a time window from the and.col
  p <- dbSendStatement(db,paste0(
    "
                     CREATE TABLE ",outcome.name," AS
                     
                     SELECT t1.Date, t1.person_id, t1.COVID_NB, t1.NB FROM (SELECT * FROM ",and.col," WHERE NB = 1) t1
                     
                     INNER JOIN (SELECT DISTINCT person_id, Date FROM TEMP) t2 
                     
                     ON (
                          t1.person_id = t2.person_id AND
                          (t2.Date - t1.Date) < ",t.post," AND
                          (t2.Date - t1.Date) >= ",t.prior," 
                          )
                     
                     "
    
  )
  )
  
  dbClearResult(p)
  
  #Delete the temp table
  p <- dbSendStatement(db ,"DROP TABLE TEMP")
  dbClearResult(p)
  
  dbDisconnect(db)
  
  
}
