
#' Aim
#'
#'This function aims to import (groups of) csv's in a standardized manner and apply some basic formatting (Date, NA,leading/tailing spaces, all character) and filtering.  
#'The function is able append multiple the csv's to 1 file.
#'By default all columns are set to character format and from all columns leading and trailing spaces are removed and if "" cells are set to NA 
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name IMPORT_PATTERN
#' @keywords ??
#' @import data.table

NULL


#' @param pat Required | A string that serves as a search pattern (contain) for the needed files in a folder specified with dir.
#' @param dir Required | The path of the folder you want to search in for your csv files. End with /
#' @param colls Optional | A vector of strings to indicate which column you want to import.
#' @param colls.new Optional | A vector of strings to indicate how to rename the columns in colls. This vector needs to have the same length as colls.
#' @param exprss Optional | A data.table expression filled within expression(). With this expression you can subset the rows that are needed. If colls.new is filled use these names.
#' @param date.colls Optional | vector of strings to indicate which variables are dates with 'YYYYMMDD' format. These columns are set to date format. If colls.new is filled use these names.
#' @param append If FALSE it is checked that only 1 file is found with the pat in dir. If not the function is stopped. Default is TRUE, meaning if multiple csv files contain pat, these are appended.

#' @return A data.table data frame.

#' @export



IMPORT_PATTERN <- function(pat,dir, colls = NULL, colls.new = NULL, exprss = NULL, date.colls = NULL, append = T){
  
  library("data.table")
  
  #Find all files that contain the pat which will become the input for the for loop. 
  obs_files<-list.files(dir, pattern = pat)
  
  #If more than 1 file is found while append is F then stop the function
  if(!append & length(obs_files) > 1){stop("Several files meet pattern (pat) while append is FALSE")}
  
  #Import every file and append loop wise. If no files found the function returns NULL. Formatting and selection actions are taken per file.
  if(length(obs_files) > 0){
    
    for(i in 1:length(obs_files)){
      
      #Import csv and force all columns to character to provide a starting point that is always met. If you let an import function take decisions unexpected
      #things happen which may give warnings and errors.
      if(!is.null(colls)){TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, select = colls, na.strings = c("", NA), colClasses=c("character"), encoding = 'UTF-8')}else{
        TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, na.strings = c("" , NA), colClasses=c("character"), encoding = 'UTF-8')
      }
      
      #Rename. Note that actions after the renaming of the columns require the new names
      if(!is.null(colls) & !is.null(colls.new)){setnames(TEMP, eval(colls),eval(colls.new))}
      
      #Add correction for spaces to prevent misinterpretation of NA's and avoiding leading and tailing spaces.When filtering/sub-setting data spaces may
      #cause that rows are not found while they should.
      invisible(lapply(colnames(TEMP), function(x) TEMP <- TEMP[, eval(x) := trimws(get(x), "b", whitespace = "[\\h\\v]")]))
      
      #After all leading and tailing spaces are removed, "" will be the only analogy of missing in the cells (" ", "   " this is then converted to ""). 
      #This can be converted to NA 
      TEMP[TEMP == ""] <- NA
      
      #Set specified date columns in data.colls to date format. Dates are essential in epidemiological pipelines.
      if(!is.null(date.colls)){lapply(date.colls, function (x)  TEMP[, eval(x) := as.Date(get(x),"%Y%m%d") ])}
      
      #Apply the sub-setting with the specified exprss. In this manner unneeded data is removes as early as possible enhancing performence. 
      if(!is.null(exprss)){
        #To feedback the removed rows due to the expression, the differnce in number of rows is calculated 
        rem1 <- nrow(TEMP) 
        TEMP <- TEMP[eval(exprss),]
        rem2 <- nrow(TEMP)
        print(paste0(rem2," selected rows from ",rem1," rows after evaluation of exprss"))
      }
      
      #Append the data frames if needed.
      if(i == 1) {FILE <- TEMP}
      if(i > 1) {FILE <- rbindlist(list(TEMP,FILE),fill = T, use.names = T)}
      rm(TEMP)
      gc()
      
      
    }
  }else FILE <- NULL 
  
  return(FILE)
  rm(FILE,obs_files)
  gc()
}
