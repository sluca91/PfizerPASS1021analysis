#' Aim
#'Calculate a property (on subject level) based on the last number of years for every month within a time period.
#'  
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CountHistorical
#' @keywords ??
#' @import data.table lubridate

NULL


#' @param file The name of a data.table object (data frame)
#' @param c.date A string representing a column name that contains the date
#' @param lookback An integer representing the number of years to look back
#' @param start_date A string representing a column name that contains the start date of the period of interest
#' @param end_date A string representing a column name that contains the end date of the period of interest
#' @param id String representing the column name of the person identifier
#' @param lookback_function Representing the type of calculation over the period of interest prior to the month. 'sum' or 'min'. 'sum' by default

#' @return A data.table data frame with an age band (bands/string), ages that are within the band (INT/integer), order for sorting (integer/Order) and an ageband with leading 0 (string/band0)

#' @export



CountHistorical <- function(file, c.date, lookback, start_date, end_date, id, lookback_function = "sum"){
  
  file <- file[, month := month(get(c.date))][, year := year(get(c.date))]
  file <- file[,label := paste0(sprintf("%02d",month),"-",year)]
  
  comb <- as.data.table(expand.grid(1:12,c((year(start_date) - lookback):year(end_date))))[,label := paste0(sprintf("%02d",Var1),"-",Var2)]
  
  ###
  comb <- comb[as.Date(paste0("01-", label), format = "%d-%m-%Y") < end_study_date  ,]
  ###
  
  
  comb <- comb[, order := as.numeric(row.names(comb))]
  comb <- comb[Var1 == month(start_date) & Var2 == year(start_date), TIME := 0]
  comb <- comb[, TIME2 := order - comb[TIME == 0, order]]
  comb <- comb[TIME2 > - (12 * lookback) - 1, ]
  
  file <- merge(x = comb[,.(label,TIME2)], y = file, by = "label", all.x = T)
  if(lookback_function == "sum"){file <- file[, .(NB = sum(!is.na(eval(Date)))), by = c(id, "label", "TIME2")]}
  if(lookback_function == "min"){file <- file[, .(NB = min(eval(Date))), by = c(id, "label", "TIME2")]}
  setorder(file,TIME2)
  setnames(file, eval(id), "id")
  
  
  
  file <- as.matrix(data.table::dcast(file, id  ~  TIME2, value.var = "NB")[!is.na(id),], rownames = "id")
  
  if(lookback_function == "sum") file[is.na(file)] <- 0
  
  TEMP <- matrix(NA,nrow = nrow(file), ncol = ncol(file) - (12 * lookback))
  colnames(TEMP) <- comb[TIME2 >= 0,label]
  rownames(TEMP) <- row.names(file)
  
  if(lookback_function == "sum") for(i in ((12 * lookback) + 1):max(as.numeric(ncol(file)))) TEMP[,i - (12 * lookback)] <- rowSums(file[, (i- (12 * lookback)):(i-1)])
  if(lookback_function == "min") for(i in ((12 * lookback) + 1):max(as.numeric(ncol(file)))){ 
    
    
  TEMP[,i - (12 * lookback)] <- do.call(pmin, c(as.list(as.data.table(file[, (i- (12 * lookback)):(i)])), na.rm = TRUE))
    
    
  }
  

  
  return(TEMP)
  
  rm(TEMP,file)
  gc()
  
} 

