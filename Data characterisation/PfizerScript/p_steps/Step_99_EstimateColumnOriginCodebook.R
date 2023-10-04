



tables <- unique(PROGRAM[TYPE == "OUTPUT" & FORMAT %in% c("rds") ,][["FILE"]])

ORIGIN <- data.table(table = as.character(), colls = as.character(), format_file = as.character(), col = as.character())

for(i in 1:length(tables)){
  
  if(file.exists(unique(unlist(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT" & FORMAT == "rds",][["result"]])))){
    colls_output <- colnames(readRDS(unique(unlist(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT" & FORMAT == "rds",][["result"]]))))
  }
  
  x0 <- unique(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT", ][["PROGRAM"]])
  if(length(x0) == 1){x <-PROGRAM[PROGRAM == x0 & TYPE == "INPUT" & FORMAT %in% c("rds", "CDM", "db"), ][["FILE"]]}else{
    x <- NULL
  }
  
  if(length(x) > 0){
    for(j in 1: length(x)){
      
      
      type <- unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT"][["FORMAT"]]))
      
      if(type == "rds"){
        if(file.exists(unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "rds",][["result"]])))){
          colls_input <- colnames(readRDS(unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "rds",][["result"]]))))
        }
      }
      
      if(type == "CDM"){ 
        if(file.exists(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1])))){
          colls_input <-colnames(fread(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1]))))
          
        }
      }
      
      if(type == "db"){ 
        #PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT"][["FOLDER_VAR"]]
        #PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "db",]
        #if(file.exists(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1])))){
        #  colls_input <-colnames(fread(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1]))))
          
        #}
        print(tables[i])
        print(i)
      }
      
      if(exists("colls_input") & exists("colls_output")){  
        colls <- colls_output[colls_output %in% colls_input]
        
        if(length(colls) > 1){
          col <- colls
          colls <- paste0(x[j],".", colls)
          if(type == "CDM") colls <- paste0("CDM.", colls)
          table <- rep(tables[i], length(colls))
          format_file <- rep(type, length(colls))
          ORIGIN <- rbind(ORIGIN, as.data.table(cbind(table, colls, format_file, col)))
          rm(table, col, format_file)
        }
        rm(colls, colls_input)
      }
    }
  }
  
  if(exists("colls_output")) rm(colls_output)
  rm(x, x0)
  
}







