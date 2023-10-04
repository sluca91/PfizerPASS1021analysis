
#' Package UMCUtrechtRWE
#'
#'aim: Extract concepts from the CDM that are stored via a coding system and code combination. Secondary, is is possible store the concepts in a standardized table.
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CreateConceptDatasets
#' @keywords ??
#' @import data.table Rcpp RSQLite rlist DBI

NULL


#' @param codesheet required | date.table data frame object with all character columns | table that contains all information regarding the codes and its connected outcomes/concepts. 
#' @param c.voc required | string | column name referring to the column where the coding system is stored.
#' @param c.concept required | string | column name referring to the column where the label is stored.
#' @param c.codes required | string | column name referring to the column where the code is stored.
#' @param c.startwith optional | vector of strings | coding systems stored in c.voc and f.voc that you want to match using a wildcard.
#' @param file optional | date.table data frame object with all character columns | table that contains the data from a events/medicines/vaccines table.
#' @param db.f.name optional | string | path to a sqlite database where the data from a events/medicines/vaccines table is stored. 
#' @param db.f.path optional | string | name of the table in the database defined by db.f.path that you are interested in. 
#' @param f.code required | string | column name referring to the column where the code is stored.
#' @param f.voc required | string | column name referring to the column where the coding system is stored.
#' @param f.date optional | string | column name referring to the column where the date is stored. Note that this column needs to be in data format.
#' @param f.id optional | string | column name referring to the column where the person identifier is stored.
#' @param db optional | string | path to the sqlite database where you would store the table(s) 
#' @param path optional | string | path where you would store the output in rds data.table dataframe(s) 
#' @param group default = T | boolean | if T for every concept/outcome defined in c.concept, a separate table is produced. If F 1 table with a column concept is created. 
#' @param f.name default = NULL if group = T required | string | name of the output table
#' @param standardized.cols default = T | boolean | if T, columns of the output table have standardized names. (Voc, Value, Date, Outcome) Needed for appending in a database

#' @return One table or multiple tables per outcome in rds or in a sqlite database file. When storing in a sqllite database tables are appended.

#' @export


CreateConceptDatasets <- function(
                                  #Define data.table dataframe name of the codesheet
                                  codesheet,
                                  #Define the column names of the codesheet needed
                                  c.voc, #coding system 
                                  c.concept, #label of the concept that you want to add
                                  c.codes, 
                                  c.startwith = NULL,#coding systems for which a start with approach is needed
                                  
                                  #Define the data.table data frame of the data OR define the sqlite db path and table name in the database 
                                  file = NULL, 
                                  db.f.name = NULL,
                                  db.f.path = NULL,
                                  #Define the column names of the data needed
                                  f.code, #code
                                  f.voc, #coding system
                                  f.date, #date or the outcome/concept
                                  f.id, #person identifier
                                  
                                  #Define how you want to store the output table(s)
                                  db = NULL, #If you want to store in a sqlite database define the path of the database
                                  path = NULL, #If you want rds files set a path of the folder
                                  group = T, #1 table or in separated tables per concept name
                                  f.name = NULL, #If group = F give a name of the output table 
                                  standardized.cols = T 
                                  
                                  ){
  
  
  
  library("rlist")
  library("data.table")
  library("DBI")
  library("Rcpp")
  library("RSQLite")
  
  #Not all columns are required. Therefore, check which are available and work with them throughout the rest of the script
  availableCols <- !sapply(list(f.id, f.voc,f.code, f.date), is.null)
  cols <-  c(f.id, f.voc,f.code, f.date)
  standardizedCols <- c("person_id", "coding_system","code", "date")[availableCols]

  #Prepare temporary database
  mydb <- dbConnect(RSQLite::SQLite(), "")
  
  #Get codesheet an prepare codesheet (standardize column names, remove dots from code and determine which rows should be analysed with a wildcard)
  codesheet <- copy(codesheet)
  setnames(codesheet,c(c.voc,c.concept,c.codes),c("coding_system","concept","code"))
  codesheet[,code_no_dot := gsub("\\.","",codesheet[,code])]
  codesheet[,start_with := fifelse(substr(code,nchar(code),nchar(code) + 1) == "." | coding_system %in% c.startwith ,"T","F")]
  dbWriteTable(mydb, "codesheet" , codesheet[,.(concept, coding_system, code, code_no_dot, start_with)], overwrite = T)
  
  #Same for the file. Also add an ID for the join procedure in the database
  if(!is.null(file)){
  file <- copy(file)
  setnames(file, cols, standardizedCols)
  file[,code_no_dot := gsub("\\.","",file[,code])]
  file[, id := seq_len(.N)]
  dbWriteTable(mydb, "cdm_table" , file, overwrite = T) #[id < 10000,]
  }else{
    p <- dbSendStatement(mydb, paste0("ATTACH DATABASE '",db.f.path,"' AS tempdb"))
    dbClearResult(p)
    
    selectFile <- paste0(
      paste0(paste0(cols, " AS ", standardizedCols), collapse = ", "),
      paste0(", REPLACE(",f.code,", '.', '') AS code_no_dot, "),
      paste0("ROW_NUMBER() OVER (ORDER BY NULL DESC) ID")
    )
    
    if(!standardized.cols){
      addCols <- paste0(dbListFields(db, db.f.name)[!dbListFields(db, db.f.name) %in% cols], collapse = ",")
      if(length(addCols) > 0) selectFile <- paste0(selectFile, ", ", addCols)
      rm(addCols)
    }
    
    p <- dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS ", "cdm_table"))
    dbClearResult(p)
    
    p <- dbSendStatement(mydb, paste0("CREATE TABLE cdm_table AS SELECT ",selectFile," FROM tempdb.", db.f.name))
    dbClearResult(p)
    
    }
  
    #Add dummy table to enlarge the events table for the start with component
    dbWriteTable(mydb, "spt_values" , data.table(number = 0:max(nchar(codesheet[["code_no_dot"]]))), overwrite = T)
    
    q <- dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS  start_with_codes"))
    dbClearResult(q)
    
    #To improve performence of the start with matching, the file is strethed out so all possible lengths of the code have a separate row.
    q <- dbSendStatement(mydb, paste0("CREATE TABLE start_with_codes AS
                                      SELECT substr(cdm.code_no_dot, 1, spt.number + 1) AS code, cdm.coding_system, cdm.id
                                      FROM (SELECT code_no_dot, id, coding_system FROM cdm_table) cdm
                                      INNER JOIN spt_values spt on spt.number < length(cdm.code_no_dot)"))
    dbClearResult(q)
    
    #add indexes to imporve speed of the join
    q <- dbSendStatement(mydb, "CREATE INDEX cdm_table_index ON cdm_table(code, coding_system, code_no_dot, id)")
    dbClearResult(q)  
    
    q <- dbSendStatement(mydb, "CREATE INDEX start_with_codes_index ON start_with_codes(code, coding_system, id)")
    dbClearResult(q)
    
    q <- dbSendStatement(mydb, "CREATE INDEX codesheet_index ON codesheet(concept, coding_system, code, code_no_dot, start_with)")
    dbClearResult(q)
    
    
    #Do the join with the columns that are available and needed
    if(standardized.cols) colsSelect <- paste0(paste0("cdm.", standardizedCols), collapse = ", ")
    if(!standardized.cols){
                        colsSelect <- c(colnames(file)[!colnames(file) %in%  c("id",standardizedCols)], standardizedCols)
                        colsSelect <- paste0(paste0("cdm.", colsSelect), collapse = ", ")
                        }
                           
    table <- as.data.table(dbGetQuery(mydb, paste0("SELECT DISTINCT ",colsSelect,", concept.concept  FROM cdm_table cdm
                                      INNER JOIN
                                      	(SELECT DISTINCT c.concept, st.id 
                                      		FROM start_with_codes st 
                                      			INNER JOIN (SELECT *  FROM codesheet WHERE start_with = 'T' ) c 
                                      				ON (st.code = c.code_no_dot AND st.coding_system = c.coding_system)) concept
                                      
                                      on (concept.id = cdm.id)
                                      
                                      UNION
                                      	SELECT DISTINCT ",colsSelect,", c.concept
                                      		FROM cdm_table cdm 
                                      			INNER JOIN (SELECT *  FROM codesheet WHERE start_with = 'F' ) c 
                                      				ON (cdm.code_no_dot = c.code_no_dot AND cdm.coding_system = c.coding_system)
  

                                    ")))
    
    
    
    
    dbDisconnect(mydb)
    
    if(!is.null(path)) table[, date := as.Date(date, origin = "1970-01-01") ]
    
    #standardize the column names so the output can be appended to other tables also coming form functions in this package
    if(!standardized.cols){
      setnames(table, c(standardizedCols, "concept"), c(cols, c.concept))
      }
    
    #To fit into pfizer...fix good for V3
    ###
    
    if(standardized.cols){
      if("coding_system" %in% colnames(table)) setnames(table, "coding_system", "Voc")  
      if("code" %in% colnames(table)) setnames(table, "code", "Value") 
      if("date" %in% colnames(table)) setnames(table, "date", "Date")
      if("concept" %in% colnames(table)) setnames(table, "concept", "Outcome")
      order <- c("person_id", "Outcome", "Value", "Voc", "Date")
      order <- order[order %in% colnames(table)]
      setcolorder(table, order)
      rm(order)
    }
    
    if(standardized.cols){
      vectorConcepts <- unique(table[["Outcome"]])
      colConcept <- "Outcome"
    }
    
    if(!standardized.cols){
      vectorConcepts <- unique(table[[c.concept]])
      colConcept <- c.concept
    }
    
    ###
    
    #Write tables to rds and/or sqlite tables in 1 table or multiple tables
    if(!is.null(db)) mydb <- dbConnect(RSQLite::SQLite(), db)
    
    if(group){
    for(i in vectorConcepts){
      
      if(!is.null(db)) dbWriteTable(mydb, i, table[get(colConcept) == i,], append = T, overwrite = F)
      if(!is.null(path)){
          if(file.exists(paste0(path,"/",i,".rds"))) warning(paste0("File for ",i," already exsist and is overwriten. Use db if you want to append"))
          saveRDS(table[get(colConcept) == i,], paste0(path,"/",i,".rds"))
         }
        }
      }else{
        if(!is.null(db)) dbWriteTable(mydb, f.name, table, append = F, overwrite = T)
        if(!is.null(path))saveRDS(table, paste0(path,"/",f.name,".rds"))
    }
    
    #if(!is.null(db)) dbListTables(mydb)
    if(!is.null(db)) dbDisconnect(mydb)
    
 
} 













  
  