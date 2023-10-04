


# RUN_SCRIPT <- function(name){
#   
#   temp <- PROGRAM[PROGRAM == name,]
#   
#   vars <- list()
#   
#   for(i in 1:nrow(temp)){
#     
#     vars[temp[i,][["VAR"]]] <- temp[i,][["result"]]
#     
#     
#   }
#   
#   source(paste0(pre_dir,name,".R"), local = T)
#   #print(vars[[1]])
#   #return(vars)
#   
# }

RUN_SCRIPT <- function(name = NULL){
  
  if(is.null(name)){
    name <- getActiveDocumentContext()$path
    
    loc <- gregexpr(pattern ='/', name)[[1]][length(gregexpr(pattern ='/', name)[[1]])]
    name <- substr(name, loc + 1, nchar(name))
    
    
    
  }
  
  if(toupper(substr(name, nchar(name) - 1, nchar(name))) == ".R"){ name <- substr(name, 1, nchar(name) - 2)}
  
  temp <- PROGRAM[PROGRAM == name,]
  
  vars <- list()
  
  for(i in 1:nrow(temp)){
    
    vars[[temp[i,][["VAR"]]]][["folder"]] <- temp[i,][["FOLDER"]]
    vars[[temp[i,][["VAR"]]]][["name"]] <- temp[i,][["FILE"]]
    vars[[temp[i,][["VAR"]]]][["format"]] <- temp[i,][["FORMAT"]]
    vars[[temp[i,][["VAR"]]]][["path"]] <- temp[i,][["result"]]
    vars[[temp[i,][["VAR"]]]][["object"]] <- temp[i,][["OBJECT"]]
    vars[[temp[i,][["VAR"]]]][["key"]] <- temp[i,][["KEY"]]
    vars[[temp[i,][["VAR"]]]][["script"]] <- temp[i,][["PROGRAM"]]
    
  }
  
  #source(paste0(pre_dir,name,".R"), local = T)
  #print(vars[[1]])
  return(vars)
  
}
