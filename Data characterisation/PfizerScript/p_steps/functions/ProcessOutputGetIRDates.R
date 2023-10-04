#Get a long file for all the concepts. For some concepts the unit should be valid for further calculations. To double check this this information is also stored. 
#Long file is more efficient to perform calculation on and at the end it is transposed to a wide table.
#This step takes to much time and is to much a puzzle.
#Things that are handled here are: greedy method (look prior, if nothing found, look post T0), identify values for not binary variables, ensure numeric values, write the units to a separate file, harmonize the column names.
#This step was done in R and not directly SQL because writing to the database from multiple cores is not possible.  

#While reviewing and commenting I noticed that the long format result column is in character format because of a mistake in the processing of file 3.
#this was without notice or consequence because rbindlist is setting everything to character.
###

ProcessOutputGetIRDates <- function(COV2, i){
  
  #Go over the list. The list contains data frame's that need to be combined to 1 long file
    #Prior information is stored in file1. So first check if this file in in the list instance. If it is there it is known that there is prior T0 information.
    if(exists("file1", where = COV2)){
      
      #If there is a value this is written to the long file, if not the date is written instead so this column can still be used for the transposing of the long file.
      if(nchar(scheme[i,]$coll) > 0){#x1 <- unlist(strsplit(scheme[i,]$coll, ",")[[1]])[1]
        if("Value" %in% unlist(strsplit(scheme[i,]$coll, ",")[[1]])){x1 <- "Value"}else{x1 <- "Date"}}else{x1 <- "Date"}
      
      #For some variables a greedy method is taken. So if no information is found in the past, the information is taken from the future if available. This post information is
      #Stored in file2. In pfizer, for the covariates, file2 is only there if file1 is also available.   
      if(exists("file2", where = COV2)){
        #Combine file1 (prior) with file2 (post), with the aim to determine the eventually value that is taking following the greedy method.
        temp2 <- merge(x = as.data.table(COV2$file1), y = as.data.table(COV2$file2), all = T, allow.cartesian = F, by = c("person_id", "id"))
        
        #Check if all results are numeric. This because a numeric/integer format is more lean and good for performance when it will end up in a very wide file.
        #Moreover, some calculation need to be performed for BMI in which numeric information is needed.
        ###
        if(
          any(!suppressWarnings(!is.na(as.numeric(na.omit(unique(temp2[[paste0(x1,".x")]])))))) |
          any(!suppressWarnings(!is.na(as.numeric(na.omit(unique(temp2[[paste0(x1,".y")]]))))))
        ){
          warning(paste0("Covariate ",scheme[i,]$c.name," has values that do not fit in numeric format"))
        }
        ###
        #Collect the needed info from prior and if not available from the future.
        temp2 <- temp2[, Result := as.numeric(fifelse(is.na(get(paste0(x1,".x"))), get(paste0(x1,".y")), get(paste0(x1,".x"))))  ]
        temp2 <- temp2[, REFDT := as.integer(fifelse(is.na(get(paste0(x1,".x"))), REFDT.y, REFDT.x))  ]
        temp2 <- temp2[, Date := as.integer(fifelse(is.na(get(paste0(x1,".x"))), Date.y, Date.x))  ]
        
      }else{
        #Independent from if it is a greedy method or not, the loop continues with temp2 as the object name.
        temp2 <- as.data.table(COV2$file1)
        
        #The sqlite database gives dates back as integers. So check if this is correct.
        if(
          any(!suppressWarnings(!is.na(as.numeric(na.omit(unique(temp2[[x1]]))))))
        ){
          warning(paste0("Covariate ",scheme[i,]$c.name," has values that do not fit in integer format"))
        }
        
        #Set all to integer. Thing that this is not needed.
        temp2 <- temp2[, Result := as.numeric(get(x1)) ][,Date := as.integer(Date)][,REFDT := as.integer(REFDT)]
      }
      
      #The name of the study variable is stored in the list and needs to be in a column so you can filter or aggregate on it.  
      temp2 <-  temp2[, col := paste0(COV2$Concept,"")]
      
      #If a unit is available then store this in a separate file. This can be used for the calculation of the BMI 
      if("Unit" %in% colnames(temp2)){
        tempvoc <- copy(temp2)[, Voc := Unit][,.(person_id, id, Voc, col, REFDT)]
      }else{tempvoc <- NULL}  
      
      #Standardize column order in preparation of appending.  
      temp2 <- temp2[,.(person_id, id, Result, col, REFDT, Date)]
      rm(x1)
      gc()
      
    }
    
    #There is also an option to extract concepts with a start and an and date(BETWEEN). This cannot in combination with prior or post.
    if(exists("file3", where = COV2)){
      temp2 <- as.data.table(COV2$file3)[, col := paste0(COV2$Concept,"")]
      #Distinct between binary and categorical. Not tested yet because in Pfizer only binary is used.
      if(nchar(scheme[i,]$coll) > 0){if("Value" %in% unlist(strsplit(scheme[i,]$coll, ",")[[1]])){x1 <- "Value"}else{x1 <- "ST"}}else{x1 <- "ST"}
      temp2 <- copy(temp2)[, Result := as.numeric(get(x1))][, Date := as.Date(NA)][,.(person_id, id, Result, col, REFDT,Date)]
      rm(x1)
      tempvoc <- NULL
    }
    
  return(list(result = temp2, voc = tempvoc))  
  
}



