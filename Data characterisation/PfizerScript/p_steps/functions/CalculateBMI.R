
#' Aim
#'Create a new study variable BMI using the study variables weight, length and BMI. This function is applied after the extraction of from the concepts based on T0.
#'This in contrast to other algorithms that are applied before this extraction.This means that the function determines the bmi based on 1 occurrence of weight and height
#'and/or 1 occurrence of BMI. Which height, weight and/or bmi is delivered to the function is outside the scope of this function. In Pfizer that is determined in the extractions
#'in relation to T0. 
#'
#'Assumptions
#'  BMI can be calculated if weights and heights are available
#'  -weights are with the unit 'kg' 
#'  -heights are with the unit 'm'
#'  -the date of the calculated bmi is the latest date from height and weight
#'  
#'  BMI can be also available directly
#'  -BMI's are with the unit kg/m2
#'  
#'  If there is a calculated and extracted BMI the latest in time is selected.
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CalculateBMI
#' @keywords ??
#' @import data.table

NULL


#' @param file required | data.table data frame object | a long file that contains the continuous variables
#' @param Result required | string | the column in file where the continuous values for weight, height and bmi are stored   
#' @param Date required | string | the column in file where the date is stored
#' @param filevoc optional | data.table data frame object | the place where the units are stored. 
#' @param Voc optional | string | the column in file with the unit which should be m/kg/kg/m2 
#' @param id required | vector of strings | columns in file that make a row unique and by which aggregation and sorting is performed.
#' @param StudyVar required | string | the column in file where the study variable is stored. Values that are filterd out are specified in weight.var, height.var and bmi.var
#' @param weight.var required | string | the name if the study variable that represents weight
#' @param height.var required | string | the name if the study variable that represents height
#' @param bmi.var required | string | the name if the study variable that represents bmi
#' 
#' @return a Data.table data frame in the same format as the input table specified in file with a BMI in the Result column. 

#' @export



CalculateBMI <- function(
  file,
  Result = "Result",
  Date = "Date",
  Voc = "Voc",
  id = c("person_id", "id"),
  StudyVar = "col",
  weight.var,
  height.var,
  bmi.var
){
  
  #Get weight information 
  WEIGHT <- copy(file)[get(StudyVar) == weight.var,]
  setnames(WEIGHT, Voc, "unit")
  
  #Check if the weight is in the supported unit.
  if(any(is.na(WEIGHT[["unit"]]))) print("Missing units for WEIGHT")
  if(any(WEIGHT[["unit"]] != "kg")) print("Wrong units for WEIGHT. Make sure all units are kg")
  WEIGHT <- WEIGHT[unit == "kg",]
  
  #Check if the weight is a positive number needed to calculate with. The highest measured weight is according to google lower than 700 kg
  if(any(as.numeric(WEIGHT[["Result"]]) <= 0 | as.numeric(WEIGHT[["Result"]]) > 700 )) print("WEIGTHS from 0 or negatief or above 700 kg. These rows are deleted. Please check if the input table.")
  WEIGHT <- WEIGHT[as.numeric(Result) > 0 | as.numeric(Result) <= 700,]
  
  #Prepare column names for merge to height
  WEIGHT <- WEIGHT[, WEIGHT := as.numeric(eval(Result))][, WEIGHT_DT := eval(Date)][, c(id, "WEIGHT", "WEIGHT_DT", StudyVar), with = F]
  
  #Get height information
  HEIGHT <- copy(file)[get(StudyVar) == height.var,]
  setnames(HEIGHT, Voc, "unit")
  
  #Check if the height is in the supported unit.
  if(any(is.na(HEIGHT[["unit"]]))) print("Missing units for HEIGHT")
  if(any(HEIGHT[["unit"]] != "m")) print("Wrong units for WEIGHT. Make sure all units are m")
  HEIGHT <- HEIGHT[unit == "m",]
  
  #Check if the weight is a positive number needed to calculate with. The highest measured weight is according to google lower than 3 meter
  if(any(as.numeric(HEIGHT[["Result"]]) <= 0 | as.numeric(HEIGHT[["Result"]]) > 3  )) print("HEIGHTS from 0 or negatief or above 3 meter. These rows are deleted. Please check the input table")
  HEIGHT <- HEIGHT[as.numeric(Result) > 0 | as.numeric(Result) <= 3,]
  
  #Prepare column names for merge to weight
  HEIGHT <- HEIGHT[, HEIGHT := as.numeric(eval(Result))][, HEIGHT_DT := eval(Date)][, c(id, "HEIGHT", "HEIGHT_DT", StudyVar), with = F]
  
  #Calculate BMI
  BMI2 <- merge(x = WEIGHT, y = HEIGHT, by = c(id), all = F)
  rm(WEIGHT, HEIGHT)
  gc()
  
  #Make calculation and get the latest date in time as date for the calculated BMI
  BMI2 <- BMI2[, Result := WEIGHT/(HEIGHT^2)][, Date := pmax(WEIGHT_DT, HEIGHT_DT)][, c(id, "Result", "Date" ), with = F]
  
  #Get directly available BMI
  BMI1 <- copy(file)[get(StudyVar) == bmi.var,]
  setnames(BMI1, Voc, "unit")
  
  #Check if the BMI is in the supported unit.
  if(any(is.na(BMI1[["unit"]]))) print("Missing units for BMI")
  if(any(BMI1[["unit"]] != "kg/m2")) print("Wrong units for BMI. Make sure all units are kg/m2")
  BMI1 <- BMI1[unit == "kg/m2",]
  
  #Check if the weight is a positive number needed to calculate with. The highest measured bmi is according to google lower than 300 kg/m2
  if(any(as.numeric(BMI1[["Result"]]) <= 0 | as.numeric(BMI1[["Result"]]) > 300)) print("BMI'S from 0 or negatief or above 300 kg/m2. These rows are deleted. Please check the input table")
  BMI1 <- BMI1[as.numeric(Result) > 0 | as.numeric(Result) >= 300,]
  
  #Prepare column names so appending to the calculated bmi is possible.
  BMI1 <- BMI1[, Result := round(as.numeric(eval(Result)), digits = 0)][, Date := eval(Date)][, c(id, "Result", "Date" ), with = F]
  
  #Append so the latest in time prior to T0 can be captured by sorting and taking the first.
  BMI <- rbindlist(list(BMI1, BMI2), use.names = T, fill = F)
  
  #Take the bmi latest in time in case there is a bmi calculated via length and weight and also a directly available bmi
  setorderv(BMI, c(id, "Date"), order = c(rep(1, length(id)),-1))
  BMI <- BMI[, order := seq_len(.N), by = c(id)][order == 1,][, order := NULL]
  
  return(BMI)
  
  
}
