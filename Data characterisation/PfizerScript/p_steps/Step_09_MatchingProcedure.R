
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Join Exposed and Controls table and pick 1 match per exposed subject.

##in/output
#Input 1: Exposed.db
#Input 2: Controls.db
#Input 3: DIC_PER
#Input 4: DIC_REG
#Output 1: MATCH_PAIRS.rds

#MATCHING_METHOD = "SQL"
#parallel_method = "WINDOWS"

SCRIPT <- RUN_SCRIPT(name = "Step_09_MatchingProcedureV2.R")
files <- time_dep_match[sapply(time_dep_match, function(x) file.exists(paste0(matching_dir, x, "_SPELLS.rds")))]
mydb <- dbConnect(RSQLite::SQLite(), dbmatching)

#In this script there are 2 distinctions:
###
# - Pick a match via a looped procedure or via a SQL join.
# - Utilize parallel execution when it is a windows system or no parallel execution if no windows.

#For running parallel the package parallel is used and a socket is created. Note that in this socket a new R session is launched and that all objects needed for 
#the execution need to by loaded into the sockets. After the sockets are established, the parLapply function is used which is analog to lapply(). lapply is a looping
#procedure and by using parLapply every instance of the loop is executed on a separate core instead of in serie.

#When running parallel using a SQL database, note problems can occur when multiple cores try to write to the same database. To prevent this 2 solutions are proposed:
# - Use the setup_timeout input of the makeCluster() function and set that to 5000.
# - Create a new temporary database and read from the central database using the ATTACH SQL statement. 

#See also: https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html

#Loop
###


if(MATCHING_METHOD == "LOOP"){

#Determine all the instances for the loop. Note that the looping is not per exposed subject. However, it is done by subjects that are alike based on the 
#matching factors. 
scheme <- dbGetQuery(mydb, paste0(" SELECT DISTINCT ", paste0(c(files, "sex_at_instance_creation", "FIRST_PFIZER", "YEAR_BIRTH"), collapse = ",")," FROM Exposed" ))

x <- 1:nrow(scheme)
      
      if(parallel_method == "NONE"){ 
        TEMP <- lapply(x, function(x){
        print(paste0(x," from ", nrow(scheme), " method is loop not parallel"));
        MATCHING_SQL_spells_loop(db = mydb, scheme = scheme[x,])})
        }
      
      if(parallel_method == "WINDOWS") { 
        #Create the sockets/cluster using the number of cores based on total cores/2. A logging is written to the disk because printed information is not visible 
        #any more in the console when using parLapply.
        clust <- makeCluster(nb_cores, setup_timeout = 5000, outfile = paste0(populations_dir,"log.txt"))
        
        #Copy all needed object to the cores. Note that the packages needed are loaded within the function and therefore, it is not needed to copy them to the core.
        #If not doing this objects are not found and errors will return who not always are very clear.
        clusterExport(clust, varlist =  c("mydb", "x", "scheme", "MATCHING_SQL_spells_loop"))  
        
        #Run the matching function in parallel.
        TEMP <- parLapply(cl = clust,x, function(x){
          print(paste0(x," from ", nrow(scheme), " method is loop parallel"));
          TEMP <-  MATCHING_SQL_spells_loop(db = mydb, scheme = scheme[x,])
          }
          )
        
        #Always terminate the parallel process. This to prevent warnings.
        stopCluster(clust)
        rm(clust)
        }                                              

rm(scheme)
}

###


#SQL
###
if(MATCHING_METHOD == "SQL"){
  
  #If parallel running is applied the exposed subjects need to be divided along the number of cores used.
  #TODO, I see that I setted the cores to 1 and then make 1 vector of all subjects which is later on used in the where expression. This is not needed and can
  #Be removed.
  
  if(parallel_method == "NONE") nb_cores <- 1
  x <- dbGetQuery(mydb,"SELECT DISTINCT person_id FROM Exposed")$person_id
  x <- split(x, sort(seq_along(x))%%(nb_cores * ceiling(match_batch)))
  
  if(parallel_method == "NONE"){
    TEMP <- lapply(x, function(x){
    print("method = SQL not parallel") ;
    MATCHING_SQL_spells(print = T, db = mydb, colls =  c(files, "sex_at_instance_creation"), ids = paste0(x, collapse = ","))})

  }
  
  if(parallel_method == "WINDOWS"){
  
    #Create the sockets/cluster using the number of cores based on total cores/2. A logging is written to the disk because printed information is not visible 
    #any more in the console when using parLapply.
    clust <- makeCluster(nb_cores, setup_timeout = 5000, outfile = paste0(populations_dir,"log.txt"))

    #Copy all needed object to the cores. Note that the packages needed are loaded within the function and therefore, it is not needed to copy them to the core.
    #If not doing this objects are not found and errors will return who not always are very clear.
    clusterExport(clust, varlist =  c("mydb", "x", "files", "MATCHING_SQL_spells"))  
    
    #Run the matching function in parallel.
    TEMP <- parLapply(cl = clust,x ,function(x){
                                                print("method = SQL parallel") ;
                                                MATCHING_SQL_spells(print = T, db = mydb, colls =  c(files, "sex_at_instance_creation"), ids = paste0(x, collapse = ","))})
    
    #Always terminate the parallel process. This to prevent warnings.
    stopCluster(clust)
    rm(clust)          
               
    }
  }
###  
  


TEMP <- as.data.table(do.call(rbind, TEMP)) 
rm(x)



CODE_SELECT2 <- paste0("t1.",c(files, "sex_at_instance_creation", "YEAR_BIRTH"), collapse = ",")

TEMP2 <- dbGetQuery(mydb ,paste0("SELECT DISTINCT person_id, FIRST_PFIZER AS T0, ST,EN, ",CODE_SELECT2, " FROM Exposed t1"))

dbDisconnect(mydb)


MATCHED <- as.data.table(sqldf(paste0("SELECT DISTINCT 
              t1.person_id AS Exposed,
              t1.T0,
              ",CODE_SELECT2,",
              t2.Control,
              t2.nb_match
              
              FROM TEMP2 t1
              
              LEFT JOIN TEMP t2 on(t2.Exposed = t1.person_id AND t2.T0 BETWEEN t1.ST AND t1.EN)
              
              
              ")))[is.na(nb_match),nb_match := 0]

rm(TEMP, TEMP2)
gc()
#MATCHED <- MATCHED[is.na(Control) & is.na(Exposed),`:=` (Exposed = person_id, nb_match = 0) ][, person_id := NULL]

lapply(MATCH_TF[MATCH_TF %in% files], function(x) MATCHED <- MATCHED[, eval(x) := as.logical(get(x))] )


Dictionary <- readRDS(SCRIPT[["INPUT3"]][["path"]])


Dictionary$Exposed <- Dictionary$person_id
colnames(Dictionary$Exposed) <- c("Exposed", "ID")
Dictionary$Control <- Dictionary$person_id
colnames(Dictionary$Control) <- c("Control", "ID")
Dictionary$person_id <- NULL
MATCHED <- RenameId(Data = MATCHED, Dictionary = Dictionary, colls = names(Dictionary), AddId = F)

#i=MATCH_CAT[2]
for(i in MATCH_CAT){
  
  if(file.exists(paste0(matching_dir,"DIC_",i,".rds"))){Dictionary_tmp <- readRDS((paste0(matching_dir,"DIC_",i,".rds")))
  MATCHED <- RenameId(Data = MATCHED, Dictionary = Dictionary_tmp, colls = names(Dictionary_tmp), AddId = F)
  rm(Dictionary_tmp)
  }
}  

setorder(MATCHED, T0, Exposed)
MATCHED <- MATCHED[, id := row.names(MATCHED)]
MATCHED[, T0 := as.Date(T0, origin = "1970-01-01")]


setnames(MATCHED, time_indep_match_name, time_indep_match)
setcolorder(MATCHED, c("Exposed","Control","T0","id","nb_match", c(time_indep_match ,time_dep_match)[sapply(c(time_indep_match ,time_dep_match), function(x) x %in% colnames(MATCHED))]))

saveRDS(MATCHED,SCRIPT[["OUTPUT1"]][["path"]])

rm(Dictionary, CODE_SELECT2, mydb, files, SCRIPT, MATCHED)
gc()




