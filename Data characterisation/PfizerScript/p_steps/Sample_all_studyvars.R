
#Aim: sample all codes and additional concepts needed for this study to CDM files. 
#Due to bad performance, this is only useful for small sample or a subset of a larger sample.

#Note that this was work in progress. During the project I did have several scripts to sample ad-hoc which I pasted in this script.
#Never finished this and there was set up a working group to sample data. Pleas ask them

#Input1
#PER <- unique(fread(paste0(path_dir,"PERSONS.csv"), nrows = 100)[["person_id"]])
PER <- readRDS(paste0(populations_dir, "MATCH_PAIRS.rds"))
id <- PER[1,1]
date <- gsub("-", "", as.character(as.Date(PER[1,3][["T0"]], origin = "1970-01-01") - 5))
#id <- PER[2]


#Input 2
codesheetEvents <- "20221208_ALL_full_codelist.csv"

#Input 3 
codesheetMed <- "20221208_ALL_drug_proxies_codelist.csv"

#Input 4
codesheetVac <- "Pfizer_vaccines_codelist.csv"

#Input 5
codesheetAdditional <- "Pfizer_additional_concepts.csv"

#id <- "#ID-00247543#"
#date <- "20210603"
#suffix <- "1"

#id <- "#ID-00209595#"
#date <- "20211228"
#probability.day <- 1

SampleAllCodes <- function(id, probability.day, date = NULL){

  #Load the observation periods which is the time in which occurrences are being sampled
  OBS <- IMPORT_PATTERN(dir = path_dir, pat = "OBSERVATION_PERIODS.csv")[person_id == id,][is.na(op_end_date), op_end_date := gsub("-","",Sys.Date()) ]
  OBS <- OBS[is.na(op_end_date), op_end_date := gsub("-","",Sys.Date()) ]
  
  #Only continue when there is a op_start_date
  if(!all(is.na(OBS[["op_start_date"]]))){
      
      #This code samples maximal 1 occurrence. Therefore a probability is multiplied with the number of days
      dateStart <- as.Date(as.character(max(as.numeric(OBS[["op_end_date"]]), na.rm = T)), format = "%Y%m%d") 
      dateEnd <- as.Date(as.character(min(as.numeric(OBS[["op_start_date"]]), na.rm = T)), format = "%Y%m%d")
      probability <- probability.day * (dateStart - dateEnd) 
      
      if(is.null(date)){date <- gsub("-", "", as.character(as.Date(sample(c(dateStart: dateEnd), 1, replace = T), origin = "1970-01-01")))}else{
        date <- gsub("-", "", as.character(as.Date(date, format = "%Y%m%d")))
      }
      
      #Higher then 1 is not possible
      if(probability > 1) probability <- 1
      
      #Fill events
      ###
        #Get event column names for the instance  
        EVENTS <- fread(paste0(path_dir,"EVENTS.csv"), nrows = 1, colClasses = "character")[0]
        #Get the codes needed for this study 
        Codes <- fread(paste0(meta_dir,codesheetEvents ))
        Codes <- unique(Codes[,.(coding_system, code)])
        #Determine which codes are needed to be included
        Codes$prob <- rbinom(n = nrow(Codes),size = 1, prob = probability)
        Codes <- Codes[prob == 1,][, prob := NULL]
        #Give a date to the occurence
        #Prepare the file
        setnames(Codes, c("code","coding_system"), c("event_code","event_record_vocabulary"))
        Codes <- Codes[, person_id := id][, start_date_record := date]
        EVENTS <- rbindlist(list(EVENTS, Codes), fill = T, use.names = T)
        rm(Codes)
      ###

      #Fill medicines
      ###
        MED <- fread(paste0(path_dir,"MEDICINES.csv"), nrows = 1)[0]
        Codes <- fread(paste0(meta_dir,codesheetMed))
        Codes <- unique(Codes[,.(atc_codes)])
        Codes$prob <- rbinom(n = nrow(Codes),size = 1, prob = probability)
        Codes <- Codes[prob == 1,][, prob := NULL]
        setnames(Codes, c("atc_codes"), c("medicinal_product_atc_code"))
        Codes <- Codes[, person_id := id][, date_dispensing := date]
        MEDICINES <- rbindlist(list(MED, Codes), fill = T, use.names = T)
        rm(Codes, MED)
      ###

      #Fill vaccines
      ###
        VAC <- fread(paste0(path_dir,"VACCINES.csv"), nrows = 1)[0]
        Codes <- fread(paste0(meta_dir,codesheetVac))[!StudyVar %in% c("CoV"),]
        Codes <- unique(Codes)
        Codes$prob <- rbinom(n = nrow(Codes),size = 1, prob = probability)
        Codes <- Codes[prob == 1,][, prob := NULL]
        setnames(Codes, c("atc_codes"), c("vx_atc"))
        Codes <- Codes[, person_id := id][, vx_admin_date := date][,StudyVar := NULL]
        
        VACCINES <- rbindlist(list(VAC, Codes), fill = T, use.names = T)
        rm(Codes, VAC)
      ###


      #Print to tables. If there are already rows then append 
      ###  
      for(i in c("EVENTS", "MEDICINES", "VACCINES")){
      filePath <- paste0(path_dir,i,"_sample_all.csv")
          if(!file.exists(filePath)){ fwrite(get(i), filePath)}else{
            fwrite(
            rbind(
              IMPORT_PATTERN(dir = path_dir, paste0(i,"_sample_all.csv") ),
              get(i)
              )
            , filePath)
          }
      }
      ###
      
      #Add the additional concepts
      ###
      #Get all additional concepts in the study
      baseFile <- IMPORT_PATTERN(dir = meta_dir, pat = codesheetAdditional)[DAP_NAME == DAP,]
      
      #Determine which rows need to be added
      baseFile$prob <- rbinom(n = nrow(baseFile),size = 1, prob = probability)
      baseFile <- baseFile[prob == 1,]
      
      #Create already the csv files to append to if they are not already prepared in the previous steps
      if(nrow(baseFile) > 0){
      
      for(i in unique(baseFile$table)){
        filePrint <- paste0(path_dir,i,"_sample_all.csv")
        #Check first if a base file is available and if any other version of the basefile is available. If the table is known and there is not yet
        #a base file to append to make a base file based on the already available file
        if(!file.exists(filePrint) & length(list.files(path =  path_dir, pattern = i)) > 0)(
          fwrite(fread(list.files(path = path_dir, pattern = i, full.names = T)[1])[0], filePrint)
        )
        rm(filePrint)
      }

      ###


      #Get the columns that contain the coordinates
      cols <- colnames(baseFile)[substr(colnames(baseFile),1,3) == "col" & nchar(colnames(baseFile)) < 6]
      vals <- colnames(baseFile)[substr(colnames(baseFile),1,3) == "val"]
      
      #file the table with the values in the correct columns
      for(i in 1:nrow(baseFile)){
        tempFile <- baseFile[i,]
        filePrint <- paste0(path_dir,tempFile[["table"]],"_sample_all.csv")
        fileCsv <- IMPORT_PATTERN(dir = path_dir, paste0(tempFile[["table"]],"_sample_all.csv"))
        if(!is.null(fileCsv)){
        
        #Create an empty row to fill and append  
        add <- fileCsv[0][nrow(fileCsv) + 1,] 
        
        keepCol <- tempFile[["keep"]]
        for(j in 1:length(cols)){
          col <- tempFile[[cols[j]]]
          val <- tempFile[[vals[j]]]
          
          if(!is.na(col) & !is.na(val)){
            if(j == 1) addTemp <- copy(add)
            addTemp[nrow(addTemp), which(colnames(addTemp) == col)] <- val
          }
          rm(col, val)
        }
        
        #Add the date
        dateCol <- tempFile[["date_column"]]
        
        #vaccines is an exception, here you do not need to fill the date in the additional concepts file
        if(tempFile[["table"]] == "VACCINES") dateCol <- "vx_record_date"
        
        addTemp[, person_id := id][, eval(dateCol) := date]
        
        #Add the filled row to the table
        if(!is.na(keepCol)) if(is.na(addTemp[[keepCol]])) addTemp[nrow(addTemp), which(colnames(addTemp) == keepCol)] <- "50"
        fileCsv <- rbind(fileCsv,addTemp)
        
        #Write the table back
        fwrite(fileCsv ,filePrint)
        
        rm(dateCol, add, keepCol)
        }
        rm(tempFile, filePrint, fileCsv)
        gc()
      }
}
}
}


unlink(list.files(path = path_dir, pattern = "_sample_all", full.names = T), recursive = T)
SampleAllCodes(id = id,probability.day =  1, date = date )


rm(PER, id, date, codesheetEvents, codesheetMed, codesheetVac, codesheetAdditional)
gc()


#if(tempFile[["StudyVar"]] == "H_EMERG_COV"){}

# count = 1
# for(i in PER[1:2]){
#   
#   print(count)
#   
#   count <- count + 1
# }


