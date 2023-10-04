

#dbGetQuery(mydb, paste0("SELECT DISTINCT * FROM( SELECT DISTINCT ", paste0(files, collapse = ","), " FROM exposed UNION SELECT DISTINCT ",paste0(files, collapse = ","), " FROM controls)"))

files <- time_dep_match[sapply(time_dep_match, function(x) file.exists(paste0(matching_dir, x, "_SPELLS.rds")))]
mydb <- dbConnect(RSQLite::SQLite(), dbmatching)


p <- dbSendStatement(mydb, "DROP TABLE IF EXISTS id_table" )
dbClearResult(p)

p <- dbSendStatement(mydb, paste0("CREATE TABLE id_table AS SELECT *, ROW_NUMBER () OVER () status FROM(SELECT DISTINCT sex_at_instance_creation,", paste0(files, collapse = ","), "    FROM exposed)"))
dbClearResult(p)

p <- dbSendStatement(mydb, "DROP TABLE IF EXISTS exposed_lean" )
dbClearResult(p)

p <- dbSendStatement(mydb, 
           glue(paste0(
              "
              CREATE TABLE exposed_lean AS 
               SELECT 
               t1.person_id, 
               t1.ST, 
               t1.EN, 
               t1.YEAR_BIRTH,
               t1.FIRST_PFIZER,
               t2.status
               
               FROM exposed t1
               
               INNER JOIN id_table t2 ON(",paste0("t1.",c("sex_at_instance_creation", files)," = t2.",c("sex_at_instance_creation", files), collapse =  " AND "),")
               
               
               
               
               "
           ))
)

dbClearResult(p)

p <- dbSendStatement(mydb, "DROP TABLE IF EXISTS controls_lean" )
dbClearResult(p)

p <- dbSendStatement(mydb, 
                     glue(paste0(
                       "
                        CREATE TABLE controls_lean AS 
                         SELECT 
                         t1.person_id, 
                         t1.ST2, 
                         t1.EN2, 
                         t1.YEAR_BIRTH,
                         t1.FIRST_PFIZER,
                         t1.VAC_DATE1,
                         t2.status
                         
                         FROM controls t1
                         
                         INNER JOIN id_table t2 ON(",paste0("t1.",c("sex_at_instance_creation", files)," = t2.",c("sex_at_instance_creation", files), collapse =  " AND "),")
           
           
           
           
           "
                     ))
)

dbClearResult(p)

p <- dbSendStatement(mydb, "DROP TABLE IF EXISTS controls" )
dbClearResult(p)

p <- dbSendStatement(mydb,"ALTER TABLE controls_lean RENAME TO controls")
dbClearResult(p)

p <- dbSendStatement(mydb, "DROP TABLE IF EXISTS exposed" )
dbClearResult(p)

p <- dbSendStatement(mydb,"ALTER TABLE exposed_lean RENAME TO exposed")
dbClearResult(p)

dbDisconnect(mydb)












