MATCHING_SQL_spells <- function(db,colls, ids = NULL, group = NULL , print = F){
  
  library("data.table")
  library("RSQLite")
  library("DBI")
  
  if(!is.null(ids) & !is.null(group)) stop("Ids and group cannot be executed at the same time")
  
  if(!is.null(ids) | !is.null(group)){
    
    pathdb <- dbGetInfo(db)$dbname
    
    dbtmp <- dbConnect(RSQLite::SQLite(), "")
    
    p <- dbSendStatement(dbtmp, paste0("ATTACH DATABASE '",pathdb,"' AS tempdb"))
    dbClearResult(p)
    
    if(!is.null(ids)){
      
      if(print) print(paste0("Matching of ",length(gregexpr(",", ids, fixed = TRUE)[[1]]) , " subject(s) is started"))
        
    p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Exposed AS SELECT * FROM tempdb.Exposed WHERE person_id IN(",ids,")"))
    dbClearResult(p)
    
    p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Controls AS SELECT * FROM tempdb.Controls "))
    dbClearResult(p)
    }
    
    if(!is.null(group)){
      
      CODE_WHERE <- paste0(paste0(colnames(group), " = " ,group[1,]), collapse = " AND ") 
      
      p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Exposed AS SELECT *  FROM tempdb.Exposed WHERE ", CODE_WHERE ))
      dbClearResult(p)
      
      p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Controls AS SELECT *  FROM tempdb.Controls WHERE ", CODE_WHERE ))
      dbClearResult(p)
      
      rm(CODE_WHERE)
    }
    ### ORIGINAL
    # p <- dbSendStatement(dbtmp, paste0("CREATE INDEX Exposed_index ON Exposed(person_id, ST, EN, YEAR_BIRTH, FIRST_PFIZER,",paste0(colls, collapse = ","),")"))
    # dbClearResult(p)
    # 
    # p <- dbSendStatement(dbtmp, paste0("CREATE INDEX Controls_index ON Controls(person_id, ST2, EN2, YEAR_BIRTH, VAC_DATE1,",paste0(colls, collapse = ","),")"))
    # dbClearResult(p)
    ###
    
    p <- dbSendStatement(dbtmp, paste0("CREATE INDEX Exposed_index_V2 ON Exposed(",paste0(colls, collapse = ","),", sex_at_instance_creation, YEAR_BIRTH, FIRST_PFIZER)"))
    dbClearResult(p)
    p <- dbSendStatement(dbtmp, paste0("CREATE INDEX Controls_index_V2 ON Controls(",paste0(colls, collapse = ","),", sex_at_instance_creation, YEAR_BIRTH, VAC_DATE1, ST2, EN2)"))
    dbClearResult(p)
    
    db <- dbtmp
    
    ###
    
    
  }
  
  
  
  CODE_JOIN2 <- paste0("t1.",colls," = t2.",colls, collapse =  " AND ")
  CODE_SELECT2 <- paste0("t1.",colls, collapse = ",")
  # consider remove DISTINCT for better performance, results are identifical usin dev data
  # consider update VAC_DATE where is null to 99999999, so it will use
  p <- dbSendStatement(db, paste0(
    "         
              CREATE TABLE POS_MATCH AS
              SELECT DISTINCT
              t1.person_id as Exposed,
              t1.FIRST_PFIZER as T0,
              t2.person_id as Control
              
              FROM Exposed t1 
              
              inner join Controls t2
              
              on(
                
                    t1.FIRST_PFIZER BETWEEN ST2 AND EN2
                    
                    AND
                    
                    (t1.FIRST_PFIZER < t2.VAC_DATE1 OR t2.VAC_DATE1 IS NULL)
                    
                    
                    AND
                    
                    (t1.YEAR_BIRTH BETWEEN t2.YEAR_BIRTH - 1 AND t2.YEAR_BIRTH + 1 )
                    
                    
                    AND
                    
                    (",CODE_JOIN2,")
                
                
              )
            
            
           "
  ))
  #print(p)
  db <- dbtmp
  dbClearResult(p)
  
# # detect any case of duplicate results (uncomment to test if removing DISTINCT from query has duplicates)
#   duplicates <- dbGetQuery(db , "
#                               SELECT Exposed, T0, Control, COUNT(*) as num_occurrences
#                               FROM POS_MATCH
#                               GROUP BY Exposed, T0, Control
#                               HAVING num_occurrences > 1
#                               LIMIT 1
#                               ")
#   
  # p <- dbSendStatement(dbtmp, paste0("CREATE INDEX POS_MATCH_V2 ON POS_MATCH(Exposed)"))
  # dbClearResult(p)
  
  # Check and print the results
  # if (nrow(duplicates) > 0) {
  #   print("Matching Error: Found duplicates!")
  # } else {
  #   print("No matching duplicates error found.")
  # }
  
TEMP1 <- dbGetQuery(db ,
    
    "
          SELECT * FROM(
          SELECT * , ROW_NUMBER () OVER ( 
                  PARTITION BY Exposed
                  ORDER BY Exposed, random()
          		
              ) NB  
          
          FROM POS_MATCH
          )
          
          WHERE NB = 1
          
          "                    
    
  )
  
TEMP2 <- dbGetQuery(db , "SELECT Exposed, COUNT(Control) as nb_match FROM POS_MATCH GROUP BY Exposed")
  
TEMP <- as.data.table(merge(TEMP1, TEMP2, by = "Exposed"))[, NB := NULL] 
  
#any(duplicated(TEMP$Exposed))
  
p <- dbSendStatement(db, "DROP TABLE POS_MATCH")
dbClearResult(p)

dbDisconnect(dbtmp)
  
return(TEMP)
  
  
rm(TEMP,TEMP1,TEMP2)
gc()
  
  
}




