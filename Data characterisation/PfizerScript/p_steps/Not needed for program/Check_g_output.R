



path1 <- "C:/Users/relbers/Documents/GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_output/OutputDRE_20220303_old/"
path2 <- "C:/Users/relbers/Documents/GitHub/C4591021_PfizerUMC/Data characterisation/PfizerScript/g_output/OutputDRE_20220303/"  

comp1 <- list.files(path1, pattern = ".csv")[!list.files(path1, pattern = ".csv") %in% list.files(path2, pattern = ".csv")]
if(length(comp1) > 0) print(paste0(comp1," in path 1 but not in path 2"))

comp2 <- list.files(path2, pattern = ".csv")[!list.files(path2, pattern = ".csv") %in% list.files(path1, pattern = ".csv")]
if(length(comp2) > 0) print(paste0(comp2," in path 2 but not in path 1"))


x <- list.files(path1, pattern = ".csv")

#i = x[17]
for(i in x ){
  
  
  
  if(!file.exists(paste0(path2,i))){
    print(paste0(i," is not in path 2"))
    next
    }
  
  x1 <- fread(paste0(path1,i), stringsAsFactors = F, na.strings = c("" , NA), colClasses=c("character"))
  x2 <- fread(paste0(path2,i), stringsAsFactors = F, na.strings = c("" , NA), colClasses=c("character"))
  
  if(nrow(x1) != nrow(x2)){print(paste0(i," has different number of rows"))}
  if(ncol(x1) != ncol(x2)){print(paste0(i," has different number of columns"))}
  
  if(nrow(x1) == nrow(x2) & ncol(x1) == ncol(x2)){
  x1[is.na(x1)] <- "EMPTY"
  x2[is.na(x2)] <- "EMPTY"
  
  if(any(!x1 == x2)){print(paste0(i,"_has different results"))}else{print("EQUAL")}
  }
  
  rm(x1,x2)
  gc()
}
  


#test <- fread(paste0(path1,i), na.strings = "")
