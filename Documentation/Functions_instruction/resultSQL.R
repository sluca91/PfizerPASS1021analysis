
rm(list=ls())

library("data.table")
library(DBI)
library(RSQLite)
library(glue)
library(parallel)

dir <- "C:/Users/relbers/Documents/GitHub/Pfizer/Documentation/Functions_instruction/Input files"

functionsDir <- "C:/Users/relbers/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_steps/functions"

lapply("IMPORT_PATTERN.r", function(x){print(x); 
  source(paste0(functionsDir,"/", x))}
)

persons <- IMPORT_PATTERN(
  pat = "PERSONS.csv",
  dir = dir,
  date.colls = NULL
  #colls = c("person_id", "sex_at_instance_creation", "year_of_birth")
  
)

setDTthreads(6)
getDTthreads()
system.time(aatest <- merge(
  x = copy(persons)[, exposed := person_id ][, person_id := NULL] , 
  y = copy(persons)[,.(person_id, sex_at_instance_creation, year_of_birth)][, control := person_id ][, person_id := NULL], 
  by = c("sex_at_instance_creation"),
  allow.cartesian = T,
  all.x = T
  )[as.numeric(year_of_birth.x) - as.numeric(year_of_birth.y) < 3,])

mydb <- dbConnect(RSQLite::SQLite(), "D:/test6")

dbWriteTable(mydb, "persons", persons, overwrite = T)

query <- glue(
  "
           
           SELECT  
           t1.sex_at_instance_creation,
           t1.year_of_birth,
           t1.person_id AS exposed,
           t2.person_id AS control
           
           
           FROM persons t1
           
           LEFT JOIN persons t2 on(t1.sex_at_instance_creation = t2.sex_at_instance_creation AND t1.year_of_birth - t2.year_of_birth < 3)
           
           
           
           "
  
)

system.time(aatest <- dbGetQuery(mydb, query))
p <- dbSendQuery(mydb, "DROP TABLE IF EXISTS RESULT ")
dbClearResult(p)

system.time(p <- dbSendQuery(mydb, glue(paste0("CREATE TABLE RESULT AS ", query))))
dbClearResult(p)

q <- dbSendStatement(mydb, "CREATE INDEX persons_index ON persons(person_id, sex_at_instance_creation, year_of_birth)")
dbClearResult(q) 

system.time(dbGetQuery(mydb, query))
p <- dbSendQuery(mydb, "DROP TABLE IF EXISTS RESULT ")
dbClearResult(p)

system.time(p <- dbSendQuery(mydb, glue(paste0("CREATE TABLE RESULT AS ", query))))
dbClearResult(p)


clust <- makeCluster(ceiling(detectCores()/2), setup_timeout = 5000)

#clusterExport(clust, varlist =  c(""))  

clusterEvalQ(clust, {
  library("data.table")
  library(DBI)
  library(RSQLite)
  library(glue)
  library(parallel)
  
})

x <- dbGetQuery(mydb,"SELECT DISTINCT person_id FROM persons")$person_id
x <- split(x, sort(seq_along(x))%%(ceiling(detectCores()/2)))
group <- x[[1]]



system.time(TEMP <- parLapply(cl = clust,x, function(group){
  
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

p <- dbSendStatement(mydb, paste0("DROP TABLE IF EXISTS MATCHES"))
dbClearResult(p)
                   
system.time(lapply(x, function(group){
  
  mydb <- dbConnect(RSQLite::SQLite(), 'D:/test6')
  
  p <- dbSendStatement(mydb, paste0("ATTACH DATABASE '",paste0("D:/",gsub("#|-","",group[1]),".db"),"' AS tempdb"))
  dbClearResult(p)
  
  if("MATCHES" %in%  dbListTables(mydb)){
    
    p <- dbSendStatement(mydb, paste0("INSERT INTO MATCHES SELECT * FROM tempdb.RESULT"))
    dbClearResult(p)
  }
  
  p <- dbSendStatement(mydb, paste0("CREATE TABLE IF NOT EXISTS MATCHES AS  SELECT * FROM tempdb.RESULT"))
  dbClearResult(p)
  
  
  
  
  dbDisconnect(mydb)
}
))

dbReadTable(mydb, )



dbDisconnect(mydb)



events <- IMPORT_PATTERN(
  pat = "EVENTS.csv",
  dir = dir,
  date.colls = NULL
  
  
)

mydb <- dbConnect(RSQLite::SQLite(), "")

for(i in 1:100){
  dbWriteTable(mydb, "events", events[, id := as.character(paste0("INSTANCE",i))], append = T)
  print(i)
  }

#q <- dbSendQuery(mydb, paste0("DROP TABLE IF EXISTS EVENTS_BIG"))
#dbClearResult(q)

#q <- dbSendQuery(mydb, paste0("CREATE TABLE EVENTS_BIG AS ", paste0(paste0(" SELECT *, ",c(1:2)," AS R1 FROM EVENTS "), collapse = " UNION ")))
#dbClearResult(q)


system.time(aatest <- dbGetQuery(mydb, "SELECT person_id, event_code FROM EVENTS WHERE event_code = '748.3'  "))
system.time(aatest <- dbGetQuery(mydb, "SELECT person_id, count(event_code) AS count FROM EVENTS GROUP BY person_id  "))
system.time(aatest <- dbGetQuery(mydb, "SELECT * FROM EVENTS WHERE event_code = '748.3'  "))
system.time(aatest <- dbGetQuery(mydb, "SELECT *, count(event_code) AS count FROM EVENTS GROUP BY person_id  "))

system.time(q <- dbSendStatement(mydb, "CREATE INDEX EVENTS_index ON EVENTS(person_id, event_code)"))
dbClearResult(q) 

system.time(aatest <- dbGetQuery(mydb, "SELECT person_id, event_code FROM EVENTS WHERE event_code = '748.3'  "))
system.time(aatest <- dbGetQuery(mydb, "SELECT person_id, count(event_code) AS count FROM EVENTS GROUP BY person_id  "))
system.time(aatest <- dbGetQuery(mydb, "SELECT * FROM EVENTS WHERE event_code = '748.3'  "))
system.time(aatest <- dbGetQuery(mydb, "SELECT *, count(event_code) AS count FROM EVENTS GROUP BY person_id  "))

dbDisconnect(mydb)

mydb <- dbConnect(RSQLite::SQLite(), "")

for(i in 1:100){
  dbWriteTable(mydb, "events", events[, id := as.character(paste0("INSTANCE",i))], append = T)
  print(i)
}

system.time(aatest1 <- dbGetQuery(mydb, "EXPLAIN QUERY PLAN SELECT * FROM EVENTS WHERE event_code = '748.3'  "))
system.time(aatest2 <- dbGetQuery(mydb, "EXPLAIN QUERY PLAN SELECT *, count(event_code) AS count FROM EVENTS GROUP BY person_id  "))

system.time(q <- dbSendStatement(mydb, "CREATE INDEX EVENTS_index ON EVENTS(person_id, event_code)"))
dbClearResult(q) 

system.time(aatest3 <- dbGetQuery(mydb, "EXPLAIN QUERY PLAN SELECT * FROM EVENTS WHERE event_code = '748.3'  "))
system.time(aatest4 <- dbGetQuery(mydb, "EXPLAIN QUERY PLAN SELECT *, count(event_code) AS count FROM EVENTS GROUP BY person_id  "))

dbDisconnect(mydb)

















