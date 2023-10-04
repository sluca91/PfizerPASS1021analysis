


AnalyseMatching <- function(
  db = dbmatching, 
  match_cols = time_dep_match[sapply(time_dep_match, function(x) file.exists(paste0(matching_dir, x, "_SPELLS.rds")))],
  sample = 100
){
  
  library("peakRAM")
  mydb <- dbConnect(RSQLite::SQLite(), dbmatching)
  
  dbnametmp <- paste0(substr(dbmatching,0, nchar(dbmatching) - 3),"temp.db")
  dbtmp <- dbConnect(RSQLite::SQLite(), dbnametmp)
  
  p <- dbSendStatement(dbtmp, paste0("ATTACH DATABASE '",db,"' AS tempdb"))
  dbClearResult(p)
  
  p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Exposed AS SELECT * FROM tempdb.Exposed"))
  dbClearResult(p)
  
  STATS <- list()
  
  STATS[["size"]] <- as.numeric(file.info(dbnametmp)$size) *(1 * 10^-9)
  STATS[["size"]] <- as.numeric(file.info(dbnametmp)$size) *(1 * 10^-9)
  
  dbDisconnect(dbtmp)
  
  if(file.exists(dbnametmp)) file.remove(dbnametmp)
  ids <- unique(sample(dbGetQuery(mydb, "SELECT person_id FROM Exposed")$person_id, 100, replace = T))
  STATS[["peakram"]] <- peakRAM(STATS[["mean_nb"]] <- mean(MATCHING_SQL_spells(db = mydb, colls =  c(match_cols, "sex_at_instance_creation"), ids = paste0(ids, collapse = ","))$nb_match))
  
  dbDisconnect(mydb)
  
  
  return(STATS)
  
}