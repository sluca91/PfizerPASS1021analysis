
#' Package UMCUtrechtRWE
#'@aim To transform a concept date to a file with a start and an end date representing the time where a categorical value is applicable .It is assuming that a single date represents a start date 
#'of the value and that that this value is the best estimate until the following date. Because this is used in the matching, and for the matching it is assumed that over the whole study period an unique
#'value is in the data, a greedy method is used in which all time that is before the first date is set to the value of this first date. If this is not desired
#'a parameters can be added like greedy = T/F. Note that when making a code change like this, that the time between the start study date and the first date needs to be captured and that this then
#'should be filled as 'UNK'. Note that persons that are not in the input file are not added. This is done outside this function in contrast to DefideTF in which this is done inside. It can be considered
#'to add this to this function to be consequent. 
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#' 
#'
#' @docType package
#' @name MakeSpells
#' @keywords ??
#' @import data.table Rcpp RSQLite DBI

NULL

#' @param Data required | string | data.table data frame
#' @param c.id required | string | column name that represent the person identifier
#' @param c.value required | string | column name that represent the value. 
#' @param c.date required | string | column name in the input table where the start of the concept date is stored
#' @param start_study_date required | date | start date of the study or period of interest.
#' @param end_study_date required | date | end date of the study or period of interest.

#' @return A data.table data frame with a start end end date. For the whole study population the whole study period need to be captured once.
#' The output columns of the table are ST and EN.

#' @export 

#It assumes no NA's in the input. You need to specify a database and table to collect from an to store to as for all the functions. No appending or overwriting allowed.



MakeSpells <- function(Data, c.id, c.value, c.date, start_study_date, end_study_date){
        
        #Monitor the content of the file that is used at the end to compare and do checks
        check1 <- length(unique(Data[[c.id]]))
        id1 <- unique(Data[[c.id]])
        
        #Standardize the column names within the function so get() and eval() is not needed in the coding. Before the return of the output table the column names are set back.
        setnames(Data, c(c.id, c.value, c.date), c("id", "value", "date"))
        
        #Remove double dates with different region. This cannot be true and a warning is needed then
        ###
        #Sort on date. value is added for QC process so that recoding gives comparable results
        setorder(Data, id, -date, value)
        #Add a follow number that can be use to keep only 1 value if 2 values are on the same date
        Data[, seq := seq_len(.N), by = c("id", "date")]
        
        #keep only 1 value per date and set a warning if this lead to removal of rows.
        invalid <- unique(Data[seq > 1,][["id"]])
        Data <- Data[seq == 1,]
        if(length(invalid) > 0) print(paste0(paste0(invalid, collapse = ","), " have more values on 1 date. 1 observations for this date are removed"))
        rm(invalid)
        ###
        
        #Determine start and end point
        ###
        #Make a shift of the data column so the first following date comes in the same rownext to the date.
        setorder(Data, id, date)
        Data[, Date1 := shift(date, -1), by =  "id"]
        #Add an order number and a number of rows per subject. This is used later to distinct starting and ending spells form intermediate spells
        Data[, seq := seq_len(.N), by = "id"]
        Data[, N := .N, by = "id"]
        
        #Set a spell with a start and an end date based in the date and the sifted date which is the first following date for the specific subject.
        Data[, ST := date]
        Data[, EN := Date1 - 1]
        
        #In case where the date is the last the shift will give an NA and so the end_study_date is filled so the assumption of full coverage of the study period is met.
        Data[is.na(EN), EN := end_study_date]
        ###
        
        #Extend/inpute range add the first and last spell so so the assumption of full coverage of the study period is met.
        Data[seq == 1 & ST > start_study_date, ST := start_study_date ]
        Data[seq == .N & EN < end_study_date, EN := end_study_date ]
        
        
        #Removal of information out off scope of study time, which are the spells that have no overlap with the study period
        Data <- Data[!(EN < start_study_date |  ST > end_study_date) ,]
        
        
        #Do checks. 
        Data[, test := EN - ST]
        check2 <- length(unique(Data$id))
        id2 <- unique(Data[["id"]])
        
        if(check1 != check2) warning(paste0(i,": Unequal subjects probably because NA in Date. Missing ", paste0(id1[!id1 %in% id2], collapse = ", ")))
        if(any(Data$test < 0)) warning(paste0(i, ": Spells not correct. Check spells for: ", paste0(Data[test < 0,][["id"]], collapse = ", ")))
        
        #Restore the original column names
        setnames(Data, c("id", "value", "date"), c(c.id, c.value, c.date))
        

        return(Data[, c(c.id, "ST", "EN", "seq", c.value), with = F])


}



  