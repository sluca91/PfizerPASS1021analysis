
#' Package UMCUtrechtRWE
#'
#'aim: Extract concepts form the CDM and store/append them in a standardized table. This function is a spin off from CreateConceptDatasets 
#'
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CreateConceptDatasetsMultipleVars
#' @keywords ??
#' @import data.table Rcpp RSQLite rlist DBI

NULL


#' @param codesheet required | date.table data frame object with all character columns | table that contains the coordinates of a concepts in the CDM. 
#' @param file required | date.table data frame object with all character columns | table that originates from the CDM.
#' @param f.id required | string| column in the file that specifies the person id.   
#' @param db required | A DBIConnection object representing the database connection
#' @param c.keep required | string | The column in the code sheet that specifies the CDM column name where a relevant value is stored per concept. This is needed for categorical values
#' @param c.columns required | vector of strings | The columns in the code sheet that specify the CDM column names for the WHERE statement.  
#' @param c.values required | vector of strings | The columns in the code sheet that specify the values in the CDM column names (c.columns) for the WHERE statement.
#' This vector need to be of the same length as c.columns. Positions in c.values are connected with c.columns. 
#' @param c.date required| string | The column in the code sheet that specifies the CDM column name where a relevant date is stored per concept
#' @param c.outcome required | string | The column in the code sheet that specifies the concept name
#' 
#' 
#' @return Appends a table to a SQLlite table with a standardized format (person_id, Outcome, Value, Voc, Date ). This standardized format is equal to the output of
#' CreateConceptDatasets when standardized.cols = T and group = T

#' @export


CreateConceptDatasetsMultipleVars <- function(codesheet, file , f.id, c.keep, c.columns, c.values, c.date, c.outcome, db){
  
  #Make a copy so the input data frame is not changing in the global environment. 
  codesheet <- copy(codesheet)
  
  
  #Get data. If not use copy input data set may be affected (see data.table properties)
  file <- copy(file) 
  file <- unique(file)
  
  #Continue only with the needed columns in the input data
  colls <- unique(c(c.columns, c.values, c.outcome, c.date, c.keep))
  #colls2 <- c(f.id, c.keep, "Outcome", "Voc", "Date" )
  codesheet <- codesheet[,..colls]
  
  #Create the expressions in R language. This is needed for filtering the rows like a WHERE statement in SQL
  for(i in 1:length(c.columns)){
    #Make expression. This is stored in var. Not sure why I took that name..
    if(i == 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), var := paste0(get(c.columns[i])," == '", get(c.values[i]),"'")]
    if(i > 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), var := paste0(var ," & ",paste0(get(c.columns[i])," == '", get(c.values[i]),"'"))]
    
    #Log where the information is extracted. In the Voc column the where statement is written down using column.value separated by |
    if(i == 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), Voc := paste0(get(c.columns[i]),".", get(c.values[i]))]
    if(i > 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), Voc := paste0(Voc ," | ",paste0(get(c.columns[i]),".", get(c.values[i])))]
  }
  
  #Fill the value column. If c.keep is filled that column is used. Otherwise the last part of the Voc is put in the value. This is not relevant in the latter situation
  keepcolls <- unique(c(c.outcome, c.date, c.keep, "var", "Voc"))
  codesheet <- codesheet[, ..keepcolls]
  codesheet <- codesheet[, Val := fifelse(grepl("[|]", Voc), trimws(substr(Voc, max(unlist(gregexpr("[|]", Voc)))+ 1, nchar(Voc)), "b"), Voc), by = list(row.names(codesheet))]
  codesheet <- codesheet[, Voc2 := fifelse(grepl("[|]", Voc), trimws(substr(Voc,1 , max(unlist(gregexpr("[|]", Voc))) - 1), "b"), "NO_VOC"), by = list(row.names(codesheet))]
  
  for(j in 1:nrow(codesheet)){
    
    #Get the expression generated in the code sheet and execute the filtering using this expression
    x <- codesheet[j,][["var"]]
    TEMP <- file[eval(parse(text = x)),]
    
    #Only continue if anything is found for the particular concept
    if(nrow(TEMP) > 0){
    
    #Create the standardized columns so appending is possible and the rest of the script can assume this table format  
    setnames(TEMP, codesheet[j,][[c.date]], "Date" )
    TEMP <- TEMP[, Voc := codesheet[j,][["Voc2"]]]
    TEMP <- TEMP[, Outcome := codesheet[j,][[c.outcome]]]
    
    #If the study variable is a binary variable it is not needed to fill the value. However, a part of the vocabulary is written to value to reduce the string length
    #per cell and for readability. Not sure if this is logical to do but it is also irrelevant since nothing is done with this cell in binary variables. 
    TEMP <- TEMP[, ':=' (Value = codesheet[j,][["Val"]])] 
    
    if(!is.na(codesheet[j,][[c.keep]])){
      
      #If the variable is not binary it the result is written to value. Therefore, first the Voc is again created and Value is removed. 
      TEMP <- TEMP[, Voc := paste0(Voc,"|", Value)][, Value := NULL]
      #Make the Value column
      setnames(TEMP, codesheet[j,][[c.keep]], "Value" )
      }
    
    #Select the needed columns and set them in the correct order so it is appendable  
    colls2 <- c(f.id, "Outcome", "Value", "Voc", "Date" )
    TEMP <- TEMP[, ..colls2]
     
    #Append to the database. Note that old tables from a previous run need to be removed outside the function.
    dbWriteTable(db, codesheet[j,][[c.outcome]] ,TEMP, overwrite = F, append = T)
    
    rm(TEMP, x, colls2)
    gc()
    }else{print(paste0("0 rows found for ", codesheet[j,][["Voc"]]))}
  
    
  }
  
  
  
}
