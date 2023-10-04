
#Attempt to run the query of exercise 1 in parallel. Reduction of running time from +/- 21 seconds to 13 seconds using 6 cores.


rm(list=ls())

#Load needed packages
library("data.table")
library(DBI)
library(RSQLite)
library(glue)
library(parallel)

#Locations of the input file
dir <- "C:/Users/relbers/Documents/GitHub/Pfizer/Documentation/Functions_instruction/Input files"

functionsDir <- "C:/Users/relbers/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_steps/functions"

#Get the persons table 
lapply("IMPORT_PATTERN.r", function(x){print(x); 
  source(paste0(functionsDir,"/", x))}
)

persons <- IMPORT_PATTERN(
  pat = "PERSONS.csv",
  dir = dir,
  date.colls = NULL,
  colls = c("person_id", "sex_at_instance_creation", "year_of_birth")
  
)


#Store this in a database
mydb <- dbConnect(RSQLite::SQLite(), "D:/test6.db")
dbWriteTable(mydb, "persons", persons, overwrite = T)

#Match in parallel
###
clust <- makeCluster(ceiling(detectCores()/2), setup_timeout = 5000)

#clusterExport(clust, varlist =  c(""))  

clusterEvalQ(clust, {
  library("data.table")
  library(DBI)
  library(RSQLite)
  library(glue)
  library(parallel)
  
})

personsGroeps <- dbGetQuery(mydb,"SELECT DISTINCT person_id FROM persons")$person_id
personsGroeps <- split(personsGroeps, sort(seq_along(personsGroeps))%%(ceiling(detectCores()/2)))

system.time(TEMP <- parLapply(cl = clust,personsGroeps, function(group){
  
  unlink(paste0("D:/",gsub("#|-","",group[1]),".db"), recursive = T, force = T)
  
  mydb <- dbConnect(RSQLite::SQLite(), paste0("D:/",gsub("#|-","",group[1]),".db"))
  
  p <- dbSendStatement(mydb, paste0("ATTACH DATABASE 'D:/test6' AS tempdb"))
  dbClearResult(p)
  
  p <- dbSendStatement(mydb, paste0("CREATE TABLE exposed AS SELECT person_id, sex_at_instance_creation, year_of_birth  FROM persons WHERE person_id IN('",paste0(group, collapse = "','"),"')"))
  dbClearResult(p)
  
  p <- dbSendStatement(mydb, paste0("CREATE TABLE control AS SELECT person_id, sex_at_instance_creation, year_of_birth FROM tempdb.persons"))
  dbClearResult(p)
  
  q <- dbSendStatement(mydb, "CREATE INDEX exposed_index ON exposed(person_id, sex_at_instance_creation, year_of_birth)")
  dbClearResult(q) 
  
  q <- dbSendStatement(mydb, "CREATE INDEX control_index ON control(person_id, sex_at_instance_creation, year_of_birth)")
  dbClearResult(q) 
  
  
  query2 <- glue(paste0(
    "
           CREATE TABLE RESULT AS
           SELECT  
           t1.sex_at_instance_creation,
           t1.year_of_birth,
           t1.person_id AS exposed,
           t2.person_id AS control
           
           
           FROM exposed t1
           
           LEFT JOIN control t2 on(t1.sex_at_instance_creation = t2.sex_at_instance_creation AND t1.year_of_birth - t2.year_of_birth < 3)
           
           
           
           "
    
  ))
  
  
  p <- dbSendQuery(mydb, query2)
  dbClearResult(p)
  
  return(query2)
  dbDisconnect(mydb)
}
))

#Always terminate the parallel process. This to prevent warnings.
stopCluster(clust)
rm(clust)
###



#Put results in 1 table in the main database in serie
###

p <- dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS MATCHES"))
dbClearResult(p)
                   
system.time(lapply(personsGroeps, function(group){
  
  mydb <- dbConnect(RSQLite::SQLite(), 'D:/test6.db')
  
  p <- dbSendStatement(mydb, paste0("ATTACH DATABASE '",paste0("D:/",gsub("#|-","",group[1]),".db"),"' AS tempdb"))
  dbClearResult(p)
  
  if("MATCHES" %in%  dbListTables(mydb)){
    
    p <- dbSendStatement(mydb, paste0("INSERT INTO MATCHES SELECT * FROM tempdb.RESULT"))
    dbClearResult(p)
  }
  
  p <- dbSendStatement(mydb, paste0("CREATE TABLE IF NOT EXISTS MATCHES AS  SELECT * FROM tempdb.RESULT"))
  dbClearResult(p)
  
  
  
  
  dbDisconnect(mydb)
  
  unlink(paste0("D:/",gsub("#|-","",group[1]),".db"), recursive = T, force = T)
}
))

###
dbDisconnect(mydb)

