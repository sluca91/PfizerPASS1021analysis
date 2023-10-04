

CombineConceptsFunctions <- function(t.interest, additional, expr, concepts){
      
      #Because column names throughout the conception CDM are not equal renaming is needed to 
      #provide a standardized starting point. In SDTM/CDISC column names are more homogeneous and logical.
      #When using such a data model this step would have been not needed.
      
      #Store all renaming amd format info in a list
      scheme <- list(
        
        "VACCINES" = list(
          col1 = c("person_id","vx_record_date","vx_admin_date","vx_atc","vx_type"),
          col2 = c("person_id","vx_record_date","vx_admin_date","vx_atc","vx_type"),
          Date = c("vx_record_date","vx_admin_date"),
          Filter = expr
        )
        
        
        ,
        
        
        "MEDICINES" = list(
          #col1 = c("person_id",	"medicinal_product_atc_code","date_dispensing","date_prescription"),
          #From AZ study for CPRD
          col1 = if (DAP %in% 'CPRD'){
            c("person_id", "medicinal_product_id", "date_dispensing", "date_prescription")} 
          else{
            c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription")},
          col2 = c("person_id","Code","date_dispensing","date_prescription"),
          Date = c("date_dispensing","date_prescription"),
          Filter = expr
        )
        
        ,
        "EVENTS" = list(
          col1 = c("person_id", "start_date_record","event_code","event_record_vocabulary", "event_free_text"),
          col2 = c("person_id","Date","Code","CodingSystem", "Free_text"),
          Date = c("Date"),
          Filter = expr
        
        )
      )
      
      #Only load the needed CDM tables
      scheme <- scheme[names(scheme) %in% t.interest]
      
      for(j in names(scheme)){
        
        #The inputted codesheets are messy and not standardized correctly in Pfizer. This is corrected
        #Per table CDM type.
        if(j == "EVENTS"){
          FILE <- readRDS(file = paste0(tmp, "CODES_EVENTS.rds"))[toupper(Outcome) %in% concepts, ]
          setnames(FILE,c("coding_system","code"),c("CodeSystem","Code"))
        }
        
        if(j == "MEDICINES"){
          FILE <- readRDS(file = paste0(tmp, "CODES_MEDICINES.rds"))[toupper(Outcome) %in% concepts, ]
        }
        
        if(j == "VACCINES"){
          FILE <- readRDS(file = paste0(tmp, "CODES_VACCINES.rds"))[toupper(Outcome) %in% concepts, ]
        }
        
        files <- list.files(path_dir, pattern = j)
        
        for(i in files){
          #browser()
          
          print(paste0("File ",i," is imported"))
          TEMP <- IMPORT_PATTERN(
            append = F,
            dir = path_dir, 
            pat = i,
            colls = scheme[[j]][["col1"]],
            colls.new = scheme[[j]][["col2"]], 
            date.colls = scheme[[j]][["Date"]],
            exprss = scheme[[j]][["Filter"]]
          )
          
          #Corrections due to deviations within CDM tables in structure. (long/wide approach deviates)
          if(j == "VACCINES") TEMP <- TEMP[,Date := fifelse(is.na(vx_admin_date),vx_record_date,vx_admin_date)][,vx_admin_date := NULL][,vx_record_date := NULL]
          if(j == "VACCINES") TEMP <- TEMP[, Code := fifelse(is.na(vx_atc) | vx_atc == "", vx_type, vx_atc)]
          if(j == "VACCINES") TEMP <- TEMP[, CodingSystem := fifelse(is.na(vx_atc) | vx_atc == "", "vx_type", "vx_atc")]
          
          if(j == "MEDICINES"){
            if (!DAP %in% 'CPRD'){
            TEMP <- TEMP[, CodingSystem := "ATC"]
            }else{
              TEMP <- TEMP[, CodingSystem := "PRODCODEID"]
            }
            }
          if(j == "MEDICINES"){TEMP <- TEMP[,Date := fifelse(is.na(date_dispensing),date_prescription,date_dispensing)][,date_dispensing := NULL][,date_prescription := NULL]}
          
          if(j == "EVENTS" & (DAP == "PHARMO" | DAP == "PEDIANET")){
            TEMP <- TEMP[toupper(CodingSystem) == "FREE_TEXT", Code := Free_text ]
            TEMP <- TEMP[toupper(CodingSystem) == "FREE_TEXT", CodingSystem := "Free_text" ]
            }
          if(j == "EVENTS") TEMP <- TEMP[, Free_text := NULL]
          
          #Clean rows with no valid date
          ###
          check1 <- nrow(TEMP)
          TEMP <- TEMP[!is.na(Date),]
          TEMP <- TEMP[Date < end_study_date,]
          check2 <- nrow(TEMP)
          
          if(check1 != check2) print(paste0("In file ", i," ", check1 - check2, " row(s) with a NA for date are found or the date is after end of study date. These rows are excluded"))
          rm(check1, check2)
          ###
          
          CreateConceptDatasets(
            codesheet = FILE,
            c.voc = "CodeSystem",
            c.concept = "Outcome",
            c.codes = "Code",
            file = TEMP,
            f.code = "Code",
            f.voc =  "CodingSystem",
            f.date = "Date",
            f.id = "person_id",
            c.startwith = start_with_colls,
            db = dbGetInfo(mydb)$dbname,
            group = T,
            f.name = i,
            standardized.cols = T
          )
          
          rm(TEMP)
          gc()
          
        }
        rm(FILE, files)
        
      }
      
      
      #Importing DAP specific concepts.
      ###
      
      if(additional){
      
                  FILE <- readRDS(file = paste0(tmp, "CODES_ADDITIONAL.rds"))[Outcome %in% concepts,]
                  
                  if(nrow(FILE) > 0){
                    scheme <- unique(FILE[["table"]])
                    
                    needed_colls <- unique(c(colnames(FILE)[substr(colnames(FILE),1,3) == "col"], "date_column", "keep"))
                  
                    for(j in 1:length(scheme)){
                      
                      files <- list.files(path_dir, pattern = scheme[j])
                      
                      FILE_TEMP <- FILE[table == scheme[j] ,]
                      
                      for(i in files){
                        print(paste0("File ",i," is imported"))
                        TEMP <- IMPORT_PATTERN(
                          dir = path_dir, 
                          pat = i,
                          colls = na.omit(unique(c("person_id", unique(do.call(list.append, lapply(needed_colls, function(x) unique(FILE_TEMP[!is.na(x)][[x]]) )))))),
                          date.colls = unique(FILE_TEMP[["date_column"]]),
                          exprss = expr,
                          append = F
                        )
                        
                        #Clean rows with no valid date
                        ###
                        check1 <- nrow(TEMP)
                        TEMP <- TEMP[!is.na(get(unique(FILE_TEMP[["date_column"]]))),]
                        check2 <- nrow(TEMP)
                        
                        if(check1 != check2) print(paste0("In file ", i," ", check1 - check2, " row(s) with a NA for date are found. These rows are excluded"))
                        rm(check1, check2)
                        
                        ###
                        
                        
                        CreateConceptDatasetsMultipleVars(
                          codesheet = FILE_TEMP,
                          file = TEMP,
                          f.id = "person_id",
                          c.columns = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "col"]),
                          c.values = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "val"]),
                          c.date = "date_column",
                          c.outcome = "Outcome",
                          c.keep = "keep",
                          db = mydb
                        ) 
                        
                        
                        rm(TEMP)
                        gc()
                        
                      }
                      
                      rm(files, FILE_TEMP)
                      gc()
                      
                    }              
                  }            
                  rm(FILE)   
                  gc()
      
      
      }

}
