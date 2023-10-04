format_strnr <- function(x){
  
  # This function takes values in string format and evaluate if the value is a real string, an integer or an decimal 
  # The format will be changed according to the value that is decided upon.
  # @@todo  check what happens when the value is not in string format() and adjust function according to that
  # browser()
  y <-suppressWarnings(as.numeric(x)) 
  pattern <- "\\b0\\.00\\b"
  if(grepl(pattern,x)){
    
    replacement <- "< 0.01"
    output_string <- gsub(pattern, replacement, x)
    return(output_string)
    
  }
  
  if (!is.na(y) && is.numeric(y) ){ 
    
    
    if (y %% 1 == 0) {
      
      y <- formatC(y, digits = 0, big.mark = ",", format= "f")
      
    } else if (y != round(y)){
      
      y <- formatC(y, digits = 2, decimal.mark = ".",big.mark = ",", format= "f")
    }
    x <- y
    
  }
  return(x)
}

rewind_to_0_with_an_n_of_zero <- function(OutputFolder,RDS_filename,CheckVariable,UpdateVariable){
  # browser()
  # Read .rds dataset
  dataset <- readRDS(paste0(OutputFolder,"/",RDS_filename))
  
  # Define the variables to check and modify
  variable1 <- CheckVariable
  variable2 <- UpdateVariable
  
  pattern <- "^\\s*<\\s*0\\.01$"
  
  
  # Process the dataset line by line
  for (i in 1:nrow(dataset)) {
    if (dataset[[variable1]][i] == 0 && grepl(pattern,dataset[[variable2]][i])) {
      dataset[[variable2]][i] <- 0
    }
  }
  saveRDS(dataset, paste0(OutputFolder,"/",RDS_filename))
}

mask_value_when_n_less_than_5 <- function(OutputFolder,RDS_filename,CheckandUpdateVariable){
  # browser()
  # Read .rds dataset
  dataset <- readRDS(paste0(OutputFolder,"/",RDS_filename))
  
  # Define the variables to check and modify
  variable1 <- CheckandUpdateVariable
  # Process the dataset line by line
  for (i in 1:nrow(dataset)) {
    # browser()
    value <- suppressWarnings(as.numeric(dataset[[variable1]][i]))
    if (!is.na(value)  && is.numeric(value) && (value < 5) && (value>0) ) {
      # Your code here
      dataset[[variable1]][i] <- "< 5"
    }
  }
  saveRDS(dataset, paste0(OutputFolder,"/",RDS_filename))  
  
  
}
