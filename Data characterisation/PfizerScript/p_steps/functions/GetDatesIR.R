

  
  
 
  


GetDatesIR <- function(
                        
                        Concept,
                        Start_date, 
                        FILE, 
                        c.name, 
                        
                        prior = T, 
                        prior.order = "DESC",
                        prior.col = "Date",
                        prior.sum = F,
                        
                        post = T, 
                        post.order = "ASC",
                        post.col = "Date",
                        
                        between = F,
                        
                        lookback, 
                        endpoint = "t1.op_end_date", 
                        coll = "", 
                        db, 
                        
                        keep_start_date = F,
                        
                        pathrds = NULL
                        
                        ){
  
  library(data.table)
  library(RSQLite)
  library(DBI)
  #########
  
    if(between & (prior | post)){stop("If between is TRUE, prior and post can only be FALSE")}  
  
    mydb <- dbConnect(RSQLite::SQLite(), db)
    RSQLite:: sqliteSetBusyHandler(mydb, 10000)    
    
    if(nchar(coll) > 0){
      coll <- unlist(strsplit(coll, ",")[[1]])
      coll0 <- paste0(",",coll, collapse = " ")
      coll2 <- paste0(",t2.",coll, collapse = " ")  
      
      }else{
        coll0 <- ""
        coll2 <- ""
      }
    
    #coll0 <- if(nchar(coll) > 0){paste0(",",coll, collapse = " ")}else{coll0 <- ""}
    #coll2 <- if(nchar(coll) > 0){paste0(",t2.",coll, collapse = " ")}else{coll0 <- ""}
    
    if(keep_start_date){
      start0 <- paste0(",", Start_date, " as REFDT")
      start2 <- paste0(",t1.", Start_date)
    }else{
      start0 <- ""
      start2 <- "" 
      }
    
    if(post){
    temp2 <- as.data.table(dbGetQuery(mydb,paste0(
     "

     SELECT person_id, Date, id ",coll0, start0,"  FROM(
     SELECT
     * ,
     ROW_NUMBER () OVER (
                     PARTITION BY person_id, id
                     ORDER BY ",post.col," ",post.order,"
                     )  NB


     FROM(

     SELECT DISTINCT
     t1.person_id,
     t1.id,
     t2.Date
     ",coll2,"
     ",start2,"

     FROM ",FILE," t1

     inner join ",Concept," t2

     ON (t1.person_id = t2.person_id AND (t2.Date BETWEEN (t1.",Start_date," - 0) AND (",endpoint,")))

 	  )
    )
   WHERE NB = 1

     "

   )
   )
   )
    }  
  
  #browser() 
    
  if(prior){
  
      if(prior.sum == F){
         temp1 <- as.data.table(dbGetQuery(mydb,paste0(
           "
      
           SELECT person_id, Date, id ",coll0, start0,"  FROM(
           SELECT
           * ,
           ROW_NUMBER () OVER (
                           PARTITION BY person_id, id
                           ORDER BY ",prior.col," ",prior.order,"
                           )  NB
      
           FROM(
      
           SELECT DISTINCT
           t1.person_id,
           t1.id,
           t2.Date
           ",coll2,"
           ",start2,"
      
           FROM ",FILE,"  t1
      
           inner join ",Concept," t2
      
           ON (t1.person_id = t2.person_id AND (t2.Date BETWEEN (t1.",Start_date," - (",lookback,"*365.25)) AND (t1.",Start_date," - 1)))
      
       	  )
         )
         WHERE NB = 1
      
          "
      
         )
         )
         )
      }
    
    if(prior.sum == T){
      temp1 <- as.data.table(dbGetQuery(mydb,paste0(
        "
        
             SELECT person_id, cast(NULL as INT) AS Date, id , COUNT(*) AS ",coll, start0,"  
        
             FROM(
        
             SELECT DISTINCT
             t1.person_id,
             t1.id,
             t2.Date,
             t2.Value
             ",start2,"
        
             FROM ",FILE,"  t1
        
             inner join ",Concept," t2
        
             ON (t1.person_id = t2.person_id AND (t2.Date BETWEEN (t1.",Start_date," - (",lookback,"*365.25)) AND (t1.",Start_date," - 1)))
        
         	  )
           
           GROUP BY person_id, id
            "
        
      )
      )
      )
    }    
    
    
  }

  
    
  if(between){
    
    temp3 <- as.data.table(dbGetQuery(mydb,paste0(
      "

     SELECT DISTINCT person_id, id, ST, EN ",coll0,start0," FROM(
     SELECT DISTINCT
     
     ROW_NUMBER () OVER (
                     PARTITION BY t1.person_id, t1.id
                     ORDER BY t1.person_id, t1.id, ST
                     )  NB,
     t1.person_id,
     t1.id,
     t2.ST,
     t2.EN
     ",coll2,"
     ",start2,"
    
     FROM ",FILE," t1

     inner join ",Concept," t2

     ON (t1.person_id = t2.person_id AND t1.",Start_date," BETWEEN t2.ST AND t2.EN)
     ) WHERE NB = 1
     "
    )))
    
    
  }      
      
      
    
    
    
  if(is.null(pathrds)){
    if(post & prior) temp <- list(file1 = temp1, file2 = temp2, Concept = c.name)
    if(post & !prior) temp <- list(file2 = temp2, Concept = c.name) 
    if(!post & prior) temp <- list(file1 = temp1, Concept = c.name)
    if(between) temp <- list(file3 = temp3, Concept = c.name)
    return(temp)
  }
  
  if(!is.null(pathrds)){
    if(prior){
      if("Date" %in% colnames(temp1)) temp1 <- temp1[, Date := as.Date(Date, origin = "1970-01-01")]
      setnames(temp1, colnames(temp1)[!colnames(temp1) %in% c("person_id", "id")], paste0(colnames(temp1)[!colnames(temp1) %in% c("person_id", "id")], "_HIST"))
      }
    
    if(post){
      if("Date" %in% colnames(temp2)) temp2 <- temp2[, Date := as.Date(Date, origin = "1970-01-01")]
      setnames(temp2, colnames(temp2)[!colnames(temp2) %in% c("person_id", "id")], paste0(colnames(temp2)[!colnames(temp2) %in% c("person_id", "id")], "_COUNT"))
    }
    
    if(between){
      if("ST" %in% colnames(temp3)) temp3 <- temp3[, ST := as.Date(ST, origin = "1970-01-01")]
      if("EN" %in% colnames(temp3)) temp3 <- temp3[, EN := as.Date(EN, origin = "1970-01-01")]
      if("REFDT" %in% colnames(temp3)) temp3 <- temp3[, REFDT := as.Date(REFDT, origin = "1970-01-01")]
    }
      
    if(post & prior) temp <- merge(x = temp1, y = temp2, by = c("person_id", "id"), all = T)
    if(post & !prior) temp <- temp2
    if(!post & prior) temp <- temp1
    if(between) temp <- temp3
    
    if(nrow(temp) > 0) saveRDS(temp ,paste0(pathrds,"/",c.name,".rds"))
    return(c.name)
    
  }
      
  print(paste0("Dates for column ",c.name," are collected"))
  
  dbDisconnect(mydb)

 
}
  
