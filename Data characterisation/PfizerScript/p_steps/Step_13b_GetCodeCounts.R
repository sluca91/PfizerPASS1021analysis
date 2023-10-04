

SCRIPT <- RUN_SCRIPT(name = "Step_13b_GetCodeCounts.R")

mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#FILE <- readRDS(file = paste0(tmp,"CODES_EVENTS.rds"))

concepts <- dbListTables(mydb)[!dbListTables(mydb) %in% c("M_Studycohort", "EXPOSED")]
concepts <- concepts[!concepts %in% c("L_BMI_COV", "L_WEIGHT_COV", "L_HEIGHT_COV")]

#x = "O_DEATHANY_AESI"
if(parallel_method %in% c("NONE")){
    COUNTS <- lapply(concepts, FUN =  function(x) CountCodes(Concept = x, db = dbconcepts))
}

if(parallel_method %in% c("WINDOWS")){
  
  clust <- makeCluster(nb_cores, setup_timeout = 5000)
  clusterExport(clust, varlist =  c("concepts", "CountCodes", "dbconcepts" ))
  
  COUNTS <- parLapply(cl = clust, concepts, function(x) CountCodes(Concept = x, db = dbconcepts))
  stopCluster(clust)
  rm(clust)
}

COUNTS <- do.call(rbindlist, list(l = COUNTS, fill = T, use.names = T))[!is.na(Voc),]

fwrite(COUNTS, SCRIPT[["OUTPUT1"]][["path"]])

dbDisconnect(mydb)
rm(COUNTS, mydb)
gc()



