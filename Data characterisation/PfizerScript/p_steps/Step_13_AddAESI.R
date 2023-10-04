

#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Get the dates for the AESI's from the SQLite database. 

##in/output
#Input 1: M_Studycohort.rds
#Input 2: Pfizer_full_codelist.csv/Stored in parameters.Data
#Input 3: database.db
#Output 1: M_Studycohort2.rds

SCRIPT <- RUN_SCRIPT(name = "Step_13_AddAESI.R")

#Set up database connection and add M_Studycohort file
###
mydb <- dbConnect(RSQLite::SQLite(), db = dbconcepts)


#Get relevant AESI's in the study. This is the information from Pfizer_full_codelist.csv combined with what is in the database
####

load(file = store_dir)
##########

if(length(Available_AESI) > 0){

scheme <- rbind(
                cbind(Concept = AESI, FILE = rep("M_Studycohort",length(AESI)), Start_date = rep("T0",length(AESI)), type = rep("AESI",length(AESI))),            
                cbind(Concept = AESI, FILE =rep("EXPOSED",length(AESI)),Start_date =rep("SECOND_PFIZER",length(AESI)), type = rep("AESI",length(AESI))),
                cbind(Concept = AESI, FILE =rep("EXPOSED",length(AESI)),Start_date =rep("THIRD_PFIZER",length(AESI)),  type = rep("AESI",length(AESI)))
                )

#colnames(scheme) <- c("CONCEPT","FILE","Start_date")

scheme <- as.data.table(scheme)
scheme <- scheme[Start_date == "T0" & FILE == "M_Studycohort", c.name := paste0(Concept,"_T0")]
scheme <- scheme[Start_date == "SECOND_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D2")]
scheme <- scheme[Start_date == "THIRD_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D3")]
scheme <- scheme[type == "AESI", lookback := 5][Concept == "E_DM1_AESI", lookback := 999]

needed_cols <- unique(scheme[type == "AESI",][["c.name"]])
needed_cols <- c(paste0(needed_cols,"_HIST"),paste0(needed_cols,"_COUNT"))

scheme <- scheme[!Concept %in% c(Missing_AESI, Empty_AESI),]

if(parallel_method %in% c("NONE")){
  AESI2 <- lapply(1:nrow(scheme), FUN =  function(i) GetDatesIR(
                                                              Concept = scheme[i,][["Concept"]], 
                                                              Start_date = scheme[i,][["Start_date"]], 
                                                              FILE = scheme[i,][["FILE"]], 
                                                              c.name = scheme[i,][["c.name"]],
                                                              lookback = scheme[i,][["lookback"]],
                                                              db = dbconcepts,
                                                              pathrds = aesi_dir
                                                              ))
}

if(!parallel_method %in% c("NONE")){
    

    if(parallel_method %in% c("WINDOWS")) { 
      clust <- makeCluster(nb_cores, setup_timeout = 5000, outfile=paste0(populations_dir,"log3.txt"))
      clusterExport(clust, varlist =  c("mydb","tmp","GetDatesIR","scheme", "dbconcepts", "aesi_dir"))
      
      AESI2 <- parLapply(cl = clust, 1:nrow(scheme), function(i) GetDatesIR(
                                                                      Concept = scheme[i,][["Concept"]], 
                                                                      Start_date = scheme[i,][["Start_date"]], 
                                                                      FILE = scheme[i,][["FILE"]], 
                                                                      c.name = scheme[i,][["c.name"]],
                                                                      lookback = scheme[i,][["lookback"]],
                                                                      db = dbconcepts,
                                                                      pathrds = aesi_dir
                                                                      ))
    
      stopCluster(clust)
      rm(clust)
    }
    
    
    
    
    #dbDisconnect(mydb)

}

dbDisconnect(mydb)



#file <- file[, Date := as.Date(Date, origin = "1970-01-01")]
#file <- data.table::dcast(file , person_id + id ~ col, value.var = "Date")

#missing_cols <- needed_cols[!needed_cols %in% colnames(file)]





}else{
  dbDisconnect(mydb)
  print("No AESI availble in data")
  #invisible(lapply(needed_cols, function(x) M_Studycohort <- M_Studycohort[, eval(x) := as.Date(x = integer(0), origin = "1970-01-01")] ))
  
}

rm(needed_cols)


#Make file with additional columns needed for sencoring
#M_Studycohort <- readRDS(SCRIPT[["INPUT1"]][["path"]])[.()]
#setnames(M_Studycohort, "group", "S_Group")

#saveRDS(as.data.table(M_Studycohort),SCRIPT[["OUTPUT1"]][["path"]])
#rm(M_Studycohort,mydb)
#gc()



rm(AESI2, Available_AESI, Available_cov, Missing_AESI, Missing_cov, Empty_AESI, Empty_cov, SCRIPT, scheme)
gc()




