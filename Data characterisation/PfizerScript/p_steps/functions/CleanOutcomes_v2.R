
#' Aim
#'
#'Removing rows within a number of days from the last occurrence. So for a person that had an outcome on 1/1/2022, 2/1/2022 and 3/1/2022 you can remove the row with 2/1/2022 using
#'a wash out period (rec.period) from 2 days.     
#' @section ??
#'
#' @author
#' Roel Elbers
#' Albert Cid Royo (edited v281022)
#' 
#' @docType package
#' @name CleanOutcome
#' @keywords ??
#' @import data.table

NULL


#' @param Dataset A data.table date frame
#' @param Person_id A string with the column name where the person identifier is stored in Dataset
#' @param rec.period A number or a vector of numbers representing the number of days within rows/outcomes are removed. If a vector the number of outcomes per person will be equal to the length of the vector.
#' if a number, the output file will give back the maximum number of outcomes per person.
#' @param c.date A string with the column name where the date of the outcome is stored in Dataset
#' @param outcome An optional string representing the name of the outcome of interest. 
#' @param c.outcome If outcome is filled, A string with the column name where the name of the outcome is stored in Dataset

#' @param c.hierarchy column name with rank values. The numbers define the order of priority of each of the records e.g c.hierarchy <- 'quality' and recordsQuality <- as.data.table(list(meaning_of_vx_record = toupper(c('GP_medication','GP_INVOICE','GP_Journal','GP_episode')), quality = c(1,1,2,2)))

#' @return A data.table data frame with the outcomes cleaned from outcomes within the specified rec.period or washout period.
#' @export


CleanOutcomes_v2 <- function(
    Dataset, 
    Person_id, 
    rec.period, 
    c.date,
    outcome = NULL,
    c.outcome = NULL,
    c.hierarchy = NULL
){
  
  #Copy over the data frame to prevent that the input dataset is changed outside the function
  Dataset_temp  <- copy(Dataset)
  
  #Take only the outcome of interest as specified in outcome and c.outcome
  if(!is.null(outcome)) Dataset_temp <- Dataset_temp[get(c.outcome) == outcome,] 
  
  #Retrieve the column names the file where the table as appended to
  tmp <- copy(Dataset_temp[0])[, Iteration := as.integer()]
  cols <- colnames(tmp)
  
  #Set a variable to 1. This variable serves as a counter for the while loop. 
  it=1
  #Make a boolean varaible that serves a check to go into the next loop
  check <- T
  
  #Execute the process. Start with the first outcome per person_id and append them to the tmp file. Per loop go to the next outcome after removing rows to close
  #To the previous event
  while(check){ 
    
    #Distinct between 1 rec.period or differing rec.periods per iteration
    if(length(rec.period) == 1) rec.periodTmp <- rec.period
    if(length(rec.period) > 1) rec.periodTmp <- rec.period[it]
    
    #Determine the difference to the first occurrence in days
    setorderv(Dataset_temp,c(Person_id,c.date))
    Dataset_temp <- Dataset_temp[,D := data.table::shift(get(c.date)),by = c(Person_id) ]
    Dataset_temp <- Dataset_temp[,dif := get(c.date) - D]
    Dataset_temp <- Dataset_temp[is.na(dif), dif := 0 ][,dif := as.numeric(dif)]
    Dataset_temp <- Dataset_temp[,cumdif := cumsum(dif),by = c(Person_id)]
    
    #Collect the first outcome represting the first if in loop 1, the second if in loop 2 and so on.
    Dataset_temp2 <- Dataset_temp[ cumdif <= rec.periodTmp,]
    if (is.null(c.hierarchy)){
      setorderv(Dataset_temp2,c(Person_id,c.date))
      #Dataset_temp2 <- Dataset_temp2[, N := .N , by = c(Person_id,c.date) ]
      Dataset_temp2 <- Dataset_temp2[ ,manu_count := length(unique(na.omit(vx_manufacturer))) , by = c(Person_id,c.date)]
      Dataset_temp2 <- Dataset_temp2[manu_count > 1, vx_manufacturer := NA][,manu_count := NULL]
      Dataset_temp2 <- Dataset_temp2[, order := frank(.I), by = c(Person_id) ]
      # Dataset_temp2[,vx_manufacturer_old := vx_manufacturer]
      # Dataset_temp2[person_id %in% '10000001889' & order == 3, vx_manufacturer := 'test1']
      Dataset_temp2[, 
                    vx_manufacturer := vx_manufacturer[which.max(!is.na(vx_manufacturer))], 
                    by = Person_id]
      Dataset_temp2 <- Dataset_temp2[,.SD[1],by = c(Person_id)][,Iteration := it][, cols, with = F]
      
    }else{
      dateHierarchyDataset <- copy(Dataset_temp2)
      setorderv(dateHierarchyDataset,c(Person_id,c.date))
      
      Dataset_temp2 <- Dataset_temp2[ ,manu_count := length(unique(na.omit(vx_manufacturer))) , by = c(Person_id,c.date,c.hierarchy)]
      Dataset_temp2 <- Dataset_temp2[manu_count > 1, vx_manufacturer := NA][,manu_count := NULL]
      
      manuHierarchyDataset <- copy(Dataset_temp2)[!is.na(vx_manufacturer)]
      setorderv(manuHierarchyDataset,c(Person_id,c.hierarchy))
      
      dateHierarchyDataset <- dateHierarchyDataset[,.SD[1],by = c(Person_id)][,Iteration := it][, cols, with = F]
      manuHierarchyDataset <- manuHierarchyDataset[,.SD[1],by = c(Person_id)][,Iteration := it][, cols, with = F]
      
      dateHierarchyDataset[,vx_manufacturer := NULL]
      Dataset_temp2 <- merge(dateHierarchyDataset,manuHierarchyDataset[,c('person_id','vx_manufacturer')], by = 'person_id', all.x = TRUE)
      rm(manuHierarchyDataset,dateHierarchyDataset)
    }
    
    
    #Add the collected outcomes to the main file. also the iteration is stored representing the following number of the event.
    tmp <- rbindlist(list(tmp, Dataset_temp2),fill = F, use.names = T)
    rm(Dataset_temp2)
    
    #Remove the rows that where witin the rec.period.
    Dataset_temp <- Dataset_temp[cumdif > rec.periodTmp,]
    
    #Remove the temporary columns that will be recreated the next round
    lapply(c("dif","cumdif","D"), function(x){Dataset_temp <-Dataset_temp[,eval(x) := NULL]})
    print(paste0("Cycle ",it))
    it=it+1
    gc()
    
    if(length(rec.period) == 1) check <- nrow(Dataset_temp) > 0
    if(length(rec.period) > 1) check <- it <= length(rec.period)
  }
  
  rm(Dataset_temp, it)
  gc()
  
  
  #lapply(c("dif","cumdif","D"), function(x){tmp <- tmp[,eval(x) := NULL]})
  #if(!is.null(outcome)) tmp <- tmp[, eval(c.outcome) := outcome]
  if (is.null(c.hierarchy)){
    setorderv(tmp,c(Person_id,c.date))
  }else{
    setorderv(tmp,c(Person_id,c.date, c.hierarchy))
  }
  
  return(tmp)
  
} 


#Example how to fill
# load("D:/CleanOutcomes.RData")
# setorder(Dataset,"person_id")
# 
# 
# Dataset <- Dataset[1:10000,]
# 
#  check1 <- CleanOutcome(
#    Dataset = Dataset,
#    Person_id = Person_id,
#    rec.period = 14,
#    c.date = Date_event,
#    c.outcome = Name_event,
#    outcome = "COV"
#  )
# 
# 
# rm(Date_event, Name_event, Outcomes, Rec_period)

