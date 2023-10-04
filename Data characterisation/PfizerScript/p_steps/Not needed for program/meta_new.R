



tables <- unique(PROGRAM[TYPE == "OUTPUT" & FORMAT %in% c("rds") ,][["FILE"]])

TEMP <- data.table(table = as.character(), colls = as.character(), col = as.character())

for(i in 1:length(tables)){
  
  colls_output <- colnames(readRDS(unique(unlist(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT" & FORMAT == "rds",][["result"]]))))
  
  x0 <- unique(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT", ][["PROGRAM"]])
  x <-PROGRAM[PROGRAM == x0 & TYPE == "INPUT" & FORMAT %in% c("rds", "CDM"), ][["FILE"]]
  
  if(length(x) > 0){
  for(j in 1: length(x)){
    
    type <- unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT"][["FORMAT"]]))
    if(type == "rds") colls_input <- colnames(readRDS(unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "rds",][["result"]]))))
    if(type == "CDM") colls_input <-colnames(fread(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1]))))
    
    colls <- colls_output[colls_output %in% colls_input]
    
    if(length(colls) > 1){
    colls <- paste0(x[j],".", colls)
    if(type == "CDM") colls <- paste0("CDM.", colls)
    table <- rep(tables[i], length(colls))
    TEMP <- rbind(TEMP, as.data.table(cbind(table, colls)))
    rm(table)
    }
    rm(colls, colls_input, table, col)
    
  }
  }
  
  rm(x, x0, colls_output)
  
}




