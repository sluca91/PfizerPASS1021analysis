
SCRIPT <- RUN_SCRIPT(name = "Step_09_MatchingProcedureV2.R")
files <- time_dep_match[sapply(time_dep_match, function(x) file.exists(paste0(matching_dir, x, "_SPELLS.rds")))]
mydb <- dbConnect(RSQLite::SQLite(), dbmatching)


Controls_tmp <- as.data.table(dbGetQuery(mydb, "SELECT *  FROM Controls"))
Exposed_tmp <- as.data.table(dbGetQuery(mydb, "SELECT *  FROM Exposed"))

MATCHING_SQL_spells_loop_nosqliite <- function(scheme, Controls = Controls_tmp, Exposed = Exposed_tmp){
  
  library("data.table")
  
  group2 <- copy(scheme)
  group2 <- as.data.table(group2)[, `:=` (FIRST_PFIZER = NULL, YEAR_BIRTH = NULL)]
  
  CODE0 <- paste0(paste0(colnames(group2), " == " ,group2[1,]), collapse = " & ") 
  CODE1a <- paste0(scheme[1,][["FIRST_PFIZER"]]," >= ST2  ")
  CODE1b <- paste0(scheme[1,][["FIRST_PFIZER"]]," <= EN2  ")
  #lower<=x & x<=upper
  
  CODE2 <- paste0("(",scheme[1,][["FIRST_PFIZER"]]," < VAC_DATE1 | is.na(VAC_DATE1) )")
  CODE3a <- paste0(scheme[1,][["YEAR_BIRTH"]]," >= (YEAR_BIRTH - 1) ")
  CODE3b <- paste0(scheme[1,][["YEAR_BIRTH"]]," <= (YEAR_BIRTH + 1)")
  
  CODE_WHERE2 <- paste0(CODE0," &  " , CODE1a, " &  " ,CODE1b, " &  ", CODE2, " &  ", CODE3a, " &  ", CODE3b)
  
  Controls <- Controls[eval(parse(text = CODE_WHERE2)),][["person_id"]]
  
  
  if(length(Controls) > 0){
    
    group1 <- scheme
    
    CODE_WHERE1 <- paste0(paste0(colnames(group1), " == " ,scheme[1,]), collapse = " & ") 
    
    Exposed <- Exposed[eval(parse(text = CODE_WHERE1)),][["person_id"]]
    
    Control <- sample(Controls, length(Exposed), replace = T)
    nb_match <- rep(length(Controls), length(Exposed))
    T0 <- rep(scheme[1,][["FIRST_PFIZER"]], length(Exposed))
    
    return(as.data.table(cbind(Exposed, nb_match, Control, T0)))
  }else{
    
    return(data.table(Exposed = as.integer(), nb_match = as.integer(), Control = as.integer(), T0= as.integer() ))
  }
  
  
}


scheme <- dbGetQuery(mydb, paste0(" SELECT DISTINCT ", paste0(c(files, "sex_at_instance_creation", "FIRST_PFIZER", "YEAR_BIRTH"), collapse = ",")," FROM Exposed" ))

x <- 1:nrow(scheme)

clust <- makeCluster(nb_cores, setup_timeout = 5000, outfile = paste0(populations_dir,"log.txt"))
clusterExport(clust, varlist =  c("mydb", "x", "scheme", "MATCHING_SQL_spells_loop_nosqliite", "Exposed_tmp", "Controls_tmp"))  

TEMP <- parLapply(cl = clust,x, function(x){
    print(paste0(x," from ", nrow(scheme), " method is loop parallel"));
    TEMP <-  MATCHING_SQL_spells_loop_nosqliite( scheme = scheme[x,])
  }
)
stopCluster(clust)
rm(clust)





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

i=MATCH_CAT[2]
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

rm(Dictionary, CODE_SELECT2, mydb, files, SCRIPT, MATCHED, Exposed_tmp, Controls_tmp)
gc()















