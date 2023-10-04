


library(data.table)
M_Studycohort <- readRDS(paste0(populations_dir,"M_Studycohort.rds"))

x <- unique(M_Studycohort[["person_id"]])

nb_per_extraction <- 900000
#nb_per_extraction <- 9
path <- "D:/test/"


for(i in 1:ceiling(length(x)/nb_per_extraction)){
  
  ID <- x[(((i-1)*nb_per_extraction) + 1):(i*nb_per_extraction)]
  fwrite(x=as.data.table(ID), file = paste0(path,"BATCH",i,".csv"))
  rm(ID)
  
}



test <- as.data.table(x)

