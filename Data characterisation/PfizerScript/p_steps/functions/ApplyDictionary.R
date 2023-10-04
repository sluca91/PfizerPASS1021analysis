#' Aim
#' Give an integer value to the original values of a categorical study variable. Note that this function only relevant for the Pfizer study. This is because
#' in Pfizer all the concepts are stored in a separate table instead of 1 table. Therefore, the updating process is done loop wise concept per concept. This may
#' not be optimal. Instead, storing all concepts in 1 table will give the opportunity to execute only 1 query which will be more efficient. The aim of setting
#' the column with categorical information to integer is not to reduce memory in the concepts database. Instead, the aim was to minimize memory in the eventually
#' covariate tables in the d4. Because many adverse events need to be analysed using the same covariates it was needed to make that table as small as possible.  
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name ApplyDictionary
#' @keywords ??
#' @import data.table DBI RSQLite

NULL


#' @param Table required | string | name of a table in the SQLite database
#' @param Dictionary default = Dic | data.table data frame object | table that contains the information for assigning the integer value to a DAP specific value.
#' @param db required | a DBI database object | specifies the database that contains the tables to set to integer. 
#' @param StudyVar required | string | name of the study variable in the dictionary that should be used.
#' @param StudyVar.col default = "VarName" | string | column in the dictionary where the study variable is stored
#' @param WHEN.col default = "Value" | string | column in the sqlite table where the value that needs to be set to integer is stored
#' @param WHEN.value default = "oriVal" | string | column in the dictionary where the original value is stored
#' @param THEN.value default = "integerVal" | string | column in the dictionary where the integer is stored. this integer is connected to the desired value that is stored in
#' in category

#' @return An update statement executed on a SQLite database table and an updated dictionary.

#' @export




ApplyDictionary <- function(
  Table,
  Dictionary = Dic,
  db = mydb,
  StudyVar,
  StudyVar.col = "VarName",
  WHEN.col = "Value",
  WHEN.value = "oriVal",
  THEN.value = "integerVal"
){
  
  ###
  #For tractability of the process the original value is first stored.
  #To make the process repeatable this original value overwrites the value if the original value columns does already exist.  
  
  
  
  if(!paste0(WHEN.col, "_original") %in% dbListFields(db, Table)){
    # store the original value is a new columns so it can be seen in the database what was the origin.
    #First create the new column
    p <- dbSendStatement(db, paste0("ALTER TABLE ",Table," ADD ",paste0(WHEN.col, "_original")," TEXT")) 
    dbClearResult(p)
    #Then fill with the original value
    p <- dbSendStatement(db, paste0("UPDATE ",Table," SET ",paste0(WHEN.col, "_original")," = ",WHEN.col)) 
    dbClearResult(p)
    
    
  }else{
    
    #Remove the index on table you going to set back to original state. Otherwise you cannot delete a column that is part of the index table
    ###
    indexes <- dbGetQuery(mydb,paste0("SELECT name, tbl_name  FROM sqlite_master WHERE type = 'index' AND tbl_name = '",Table,"'"))[["name"]]
    if(length(indexes) > 0){
      for(k in indexes){
        p <- dbSendStatement(mydb, paste0("DROP INDEX IF EXISTS ",k)) 
        dbClearResult(p)
      }
    }
    rm(indexes)
    ###
    
    #remove the value which was already set to integer
    p <- dbSendStatement(db, paste0("ALTER TABLE ",Table," DROP ",WHEN.col)) 
    dbClearResult(p)
    
    #make a new value column to restore the original value
    p <- dbSendStatement(db, paste0("ALTER TABLE ",Table," ADD ",WHEN.col," TEXT")) 
    dbClearResult(p)
    
    #fill the new value column with the original value
    p <- dbSendStatement(db, paste0("UPDATE ",Table," SET ",WHEN.col," = ",paste0(WHEN.col, "_original"))) 
    dbClearResult(p)
    
  }
  ###    
  
  
  #Get information for relevant study variable. This function is made to loop over several study variables stored in separate tables. 
  Dictmp <- Dictionary[get(StudyVar.col) == StudyVar,]
  
  #Retrieve the values available in the data. This is needed to determine which procedure is demanded 
  check <- as.data.table(dbGetQuery(db, paste0("SELECT DISTINCT ",WHEN.col," FROM ", Table)))[!is.na(get(WHEN.col)),]
  
  # The following procedures can be executed:
  # NO_ACTION: if the variable is already filled with only integer values no action is needed
  # DIC_USED: if all values in the data (as in variable check) are known in the dictionary, the integer from the dictionary is used.
  # RANDOM: if some values in the data (as in variable check) are now known in the dictionary the dictionary is not used. Instead a random integer is added
  # and the dictionary is updated with this random assigned integer. 
  
  if((nrow(check) > 0 & sum(grepl('^-?[0-9]+$', check[[WHEN.col]])) < length(check[[WHEN.col]])) | 
     (nrow(check) == 1 & any(is.na(Dictmp[[WHEN.value]]))) 
     ){
      
      if(any(!check[[WHEN.col]] %in% Dictmp[[WHEN.value]])){
        print(paste0(StudyVar," is set to integer randomly because values where found that are not in the dictionary. New dictionary is created"))
        #Because there are values not known by the dictionary or there is not dictionary information at all a dictionary is created and added.
        setorderv(check, WHEN.col)
        Dictmp <- check[, eval(THEN.value) :=  as.integer(seq_len(.N))][, eval(WHEN.value) := get(WHEN.col)][, eval(WHEN.col) := NULL][, eval(StudyVar.col) := StudyVar]
        cat <- "RANDOM"
      }else{cat <- "DIC_USED"}
      
      #Create query. Note that a case when expression is created to update the value column. Update the table using a join may be more efficient
      #Moreover, the format of the changed column is not yet set to integer, while all values are an integer. This can be done using a cast statement
      #in sqlite. However, the disadvantage of cast in sqlite is that it gives not warnings if a value is truncated if it is not fitting in integer.
      # therefore, this format change is done at step 12 after the need rows are extracted. This is done in R which  gives a warning when na's as produced.
      ###
      code <-list()
      
      for(i in unique(Dictmp[[THEN.value]])){
        code_in <- paste0(Dictmp[get(THEN.value) == as.character(i),][[WHEN.value]], collapse = "','")
        code[[as.character(i)]] <- paste0("WHEN ",WHEN.col," IN ('",code_in,"') THEN '",i,"'" )
        rm(code_in)
      }
      
      code <- do.call(paste0, list(code, collapse = " "))
      code <- paste0( "UPDATE ",Table," SET ",WHEN.col," = CASE  ",code," ELSE ",WHEN.col," END")
      ###
      
      #Execute the query
      q <- dbSendStatement(db, code)
      dbClearResult(q)
  
  }else{
    cat <- "NO_ACTION"
  }  
  
  Dictmp <- Dictmp[, action := cat]
  return(Dictmp)
  
}
