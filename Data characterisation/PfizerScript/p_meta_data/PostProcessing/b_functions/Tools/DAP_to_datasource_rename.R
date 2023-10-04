# start definitions DAP_to_datasource_rename function
library(tidyverse)

DAP_to_datasource_rename <- function(path01,DAPname,datasourcename) {

file_list <- list.files(path = path01,recursive=FALSE)
dir_list <- list.dirs(path = path01,recursive=FALSE, full.names=FALSE)
file_list <- file_list[!(file_list %in% dir_list)]
is.character(file_list)
DAP <- paste0(DAPname,"_")
datasource <- paste0(datasourcename,"_")

for(j in 1:length(file_list)){
  file01 <- file_list[j]
  path01_file01 <- paste0(path01,"/",file01)
  if(file.exists(path01_file01)){
    file02 <- str_replace(file01,DAP,datasource)
    path01_file02 <- paste0(path01,"/",file02)
    file.rename(path01_file01,path01_file02)
  }
}
}
#  End definitions DAP_to_datasource_rename function
#  First run the lines of code from line 1 to line 23 to make the function available.



# Run function DAP_to_datasource_rename to change the prefix from the name of a DAP into the name of a Datasource
# set the parameters,uncomment the lines below and run the lines of code and the function.

 path01 <- "C:/Temp/Pfizer/NHR"
 DAPname <- "UOSL"
 datasourcename <- "NHR"
# 
 DAP_to_datasource_rename(path01=path01,DAPname=DAPname,datasourcename=datasourcename)

#! the name of the Directory should also be changed. In this case from UOSL to NHR this needs to be done manually