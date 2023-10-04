#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#Get the needed dates for the covariates from the SQLite database based on T0. 

##Remarks
#I would redevelop this step. In my first attempt I was focusing on the different types of covariates and not on efficiency.    
#Note that the matching process does a similar thing in a more sophisticated manner.
#To tackle performance issues spells where created per status.So for the matching variables the matching procedure and this script are calculating the same thing in a
#different and independent manner. This gave the opportunity to test/validate the process by comparing the results from the matching with the covariates in the matching
#cohort. However, for the future, it could be considered to use the method for the matching also for the creation of the rest of the covariates. This would make the total
#pipeline less complicated because only 1 system needs to be understood. Moreover, at the beginning of this project the matching was the most time consuming, after optimizing
#that the loading of the concepts emerged as an time consuming step. After optimizing that a bit this step appears to be time consuming. So optimizing this script would 
#be task to consider. 
#Pfizer is build with concepts in a database for every concepts a table. If by example put all binary variables in 1 table, and if the cohort tables
#would be appended, then it is not needed to use a loop wise method for extracting the variables per concept. Instead 1 query can be executed.

##in/output
#Input 1: M_Studycohort.rds
#Input 2: Pfizer_full_codelist.csv/Stored in parameters.Data
#Input 3: database.db
#Output 1: M_Studycohort_COHORT.rds

SCRIPT <- RUN_SCRIPT(name = "Step_12_AddCoVariates.R")

#Set up database connection 
###
mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)

#Get the 
load(file = store_dir)
##########

if(length(Available_cov) > 0){
  
  #Create data frame with all concepts and there settings for the function that creates the queries. (GetDatesIR) Every row in the scheme contains the settings
  #for the query
  ###
  
  #It is only needed to extract the available concepts. An extra variable is used for if things need to be added.
  cols_scheme <- Available_cov
  
  #Build the framework for the scheme based on the needed covariates for the T0, D2 and D3 cohort.
  scheme <- as.data.table(rbind(
    cbind(Concept = cols_scheme, FILE =rep("M_Studycohort",length(cols_scheme)),Start_date =rep("T0",length(cols_scheme)),  type = rep("COV",length(cols_scheme))),            
    cbind(Concept = cols_scheme, FILE =rep("EXPOSED",length(cols_scheme)),Start_date =rep("SECOND_PFIZER",length(cols_scheme)), type = rep("COV",length(cols_scheme))),
    cbind(Concept = cols_scheme, FILE =rep("EXPOSED",length(cols_scheme)),Start_date =rep("THIRD_PFIZER",length(cols_scheme)),  type = rep("COV",length(cols_scheme)))
  ))
  
  rm(cols_scheme)
  
  #Create the column names that will be used in the wide format end table's. There will be 3 separate tables (T0, D2, D3) with a column per covariate.
  ###
  scheme <- scheme[Start_date == "T0" & FILE == "M_Studycohort", c.name := paste0(Concept,"_T0")]
  scheme <- scheme[Start_date == "SECOND_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D2")]
  scheme <- scheme[Start_date == "THIRD_PFIZER" & FILE == "EXPOSED", c.name := paste0(Concept,"_D3")]
  ###
  
  #Most of the covariates are extracted with no lookback time (999 year). This is done because some concepts are part of an algorithm calculated after 
  #the execution of the query. So the lookback period per covariate is handled at the end of this script after all algorithm variables are created.
  scheme <- scheme[type == "COV", lookback := 999]
  
  #The base file is created but there are still some exceptions that need to be included in the scheme.
  ###
  #For non binary concepts also the value/result needs to be extracted and so be a part of the SELECT statement.
  scheme <- scheme[ , coll :=  fifelse(Concept %in% c(MATCH_CAT, NMATCH_CAT, NMATCH_NUM, NMATCH_ALG_CONCEPTS_CAT, MATCH_ALG_CONCEPTS_CAT)
                                      & !Concept %in% MATCH_SCORE,
                                      "Value", "")]
  
  #For some categorical variables, those used in the matching in Pfizer, a different assumptions is taken. Normally, occurrences before T0 are extracted. But in
  #this exception if no occurrences are found then also the instances after T0 are extracted. So on this point prior and post need to be extracted  
  scheme <- scheme[ , post :=  fifelse(Concept %in% COV_CLOSE_PRIOR_POST, T, F) ]
  #The end point after T0 is set to always as 999 year. endpoint is only used when post is TRUE so t1.op_end_date if false is irrelevant
  scheme <- scheme[ , endpoint :=  fifelse(post, paste0("t1.",Start_date," + (999 * 365.25)"), "t1.op_end_date") ]
  
  #ARS is delivering the pregnancy status with a start and end data meaning that extraction needs to be done via a between WHERE statement.
  ###
  scheme <- scheme[ , between :=  fifelse(Concept %in% COV_BETWEEN , T, F) ]
  #If between only that query is needed.
  scheme <- scheme[ , prior :=  fifelse(between, F,T) ]
  scheme <- scheme[between == T , `:=` (endpoint = "", prior.col = "") ] #So prior.col and endpoint are not relevant. 
  ###
  
  #HARDCODED. For most covariates the most recent in recent is time is taken. Height is an exception as decided in some meeting at some point. Taking the nearest
  #is done by means of sorting and then take the first. Therefore, for height the sorting variable needs to be changed to Value instead of Date. The highest date
  #is equal nearest, and for the height we want to have the highest value.
  scheme <- scheme[, prior.col :=  fifelse(Concept == "L_HEIGHT_COV", "Value", "Date")]
  
  #Some covariates need to be give a sum instead of 1/0. These are extracted by a different query
  scheme <- scheme[ , prior.sum :=  fifelse(Concept %in% NMATCH_SUM , T, F) ]
  #Not sure why the value needs to be extracted but it will not harm so I leave it in.
  scheme <- scheme[prior.sum == T , coll :=  "Value" ]
  
  #Within the calculation/query of the summing the lookback period is relevant. So this needs to be added in contrast to the other concepts in which this can be done later.
  scheme <- merge(x = scheme, y = unique(LOOKBACK_COV), by.x = "Concept", by.y =  "VarName", all.x = T, allow.cartesian = F)
  scheme <- scheme[prior.sum == T , lookback := CAT_PRIOR][, CAT_PRIOR := NULL]
  
  #HARDCODED: for some concepts the unit is relevant. This is stored in the Voc column. An alternative manner of doing this is making study variables per unit. 
  #So by example L_WEIGHT_COV_KG/L_WEIGHT_COV_G. Then those can be included in the algorithm.
  #Note that the first in the vector must contain the value. If you make it Unit,Value it goes wrong later in the script
  scheme <- scheme[Concept %in% c("L_BMI_COV","L_WEIGHT_COV","L_HEIGHT_COV"), coll := "Value,Unit"  ]
  
  ###
  #Get all relevant rows from database by running GetDatesIR. Within in this function several query types are stored which can be executed via the stored parameters in scheme.  
  #Note that the object scheme can be seen as a sort of documentation which can be reviewed when there emerge problems with a specific covariate.
  #Because over 400 queries need to be executed, parallelism is used in windows machines.
  #It is only possible to read from database parallel. Writing in parallel gives error. Therefore, first the occurrences are extracted and stored in a list in parallel. 
  #Then after that the list is created a long format table is created.
  #For future development: over 400 queries can be reduced to 4 queries which would optimize and simplify the process.  
  
  
  #First all relevant rows are extracted in parallel if in windows. 
  #The lookback preiods are not applied yet. This needs to be done after scoring because some #concepts are involved in the scoring and as an individual concept. 
  #The lookback periods may differ between those 2 situations.  
  
  if(parallel_method %in% c("NONE")){
    COV2 <- lapply(1:nrow(scheme), FUN =  function(i) GetDatesIR(
                                                                    Concept = scheme[i,][["Concept"]], 
                                                                    Start_date = scheme[i,][["Start_date"]], 
                                                                    FILE = scheme[i,][["FILE"]], 
                                                                    c.name = scheme[i,][["c.name"]],
                                                                    lookback = scheme[i,][["lookback"]],
                                                                    prior = scheme[i,][["prior"]],
                                                                    post = scheme[i,][["post"]],
                                                                    between = scheme[i,][["between"]],
                                                                    endpoint = scheme[i,][["endpoint"]],
                                                                    coll = scheme[i,][["coll"]],
                                                                    prior.col = scheme[i,][["prior.col"]],
                                                                    db = dbconcepts,
                                                                    keep_start_date = T,
                                                                    prior.sum = scheme[i,][["prior.sum"]]
    ))
  }
  
  
    
    
    if(parallel_method %in% c("WINDOWS")) { 
      clust <- makeCluster(nb_cores, setup_timeout = 5000, outfile=paste0(populations_dir,"log3.txt"))
      clusterExport(clust, varlist =  c("mydb","tmp","GetDatesIR","scheme", "dbconcepts"))
      
      COV2 <- parLapply(cl = clust, 1:nrow(scheme), function(i) GetDatesIR(
                                                                    Concept = scheme[i,][["Concept"]], 
                                                                    Start_date = scheme[i,][["Start_date"]], 
                                                                    FILE = scheme[i,][["FILE"]], 
                                                                    c.name = scheme[i,][["c.name"]],
                                                                    lookback = scheme[i,][["lookback"]],
                                                                    prior = scheme[i,][["prior"]],
                                                                    post = scheme[i,][["post"]],
                                                                    between = scheme[i,][["between"]],
                                                                    endpoint = scheme[i,][["endpoint"]],
                                                                    coll = scheme[i,][["coll"]],
                                                                    prior.col = scheme[i,][["prior.col"]],
                                                                    db = dbconcepts,
                                                                    keep_start_date = T,
                                                                    prior.sum = scheme[i,][["prior.sum"]]
      ))
      
      stopCluster(clust)
      rm(clust)
    }
    
    
   
###
  

#Get a long file for all the concepts. For some concepts the unit should be valid for further calculations. To double check this this information is also stored. 
#Long file is more efficient to perform calculation on and at the end it is transposed to a wide table.
#This step takes to much time and is to much a puzzle.
#Things that are handled here are: greedy method (look prior, if nothing found, look post T0), identify values for not binary variables, ensure numeric values, write the units to a separate file, harmonize the column names.
#This step was done in R and not directly SQL because writing to the database from multiple cores is not possible.  

#While reviewing and commenting I noticed that the long format result column is in character format because of a mistake in the processing of file 3.
#this was without notice or consequence because rbindlist is setting everything to character.
###

  #Because it is a long script during development I may want to rerun parts of the code. Therefore, remove some created objects before creating them.
  if(exists("filevoc")) rm(filevoc)
  if(exists("fileresult")) rm(fileresult)
  
  #Go over the list. The list contains data frame's that need to be combined to 1 long file
  for (i in 1:length(COV2)) {
    
    #Prior information is stored in file1. So first check if this file in in the list instance. If it is there it is known that there is prior T0 information.
    if(exists("file1", where = COV2[[i]])){
    
    #If there is a value this is written to the long file, if not the date is written instead so this column can still be used for the transposing of the long file.
    if(nchar(scheme[i,]$coll) > 0){#x1 <- unlist(strsplit(scheme[i,]$coll, ",")[[1]])[1]
                                    if("Value" %in% unlist(strsplit(scheme[i,]$coll, ",")[[1]])){x1 <- "Value"}else{x1 <- "Date"}}else{x1 <- "Date"}
    
    #For some variables a greedy method is taken. So if no information is found in the past, the information is taken from the future if available. This post information is
    #Stored in file2. In pfizer, for the covariates, file2 is only there if file1 is also available.   
    if(exists("file2", where = COV2[[i]])){
      #Combine file1 (prior) with file2 (post), with the aim to determine the eventually value that is taking following the greedy method.
      temp2 <- merge(x = as.data.table(COV2[[i]]$file1), y = as.data.table(COV2[[i]]$file2), all = T, allow.cartesian = F, by = c("person_id", "id"))
      
      #Check if all results are numeric. This because a numeric/integer format is more lean and good for performance when it will end up in a very wide file.
      #Moreover, some calculation need to be performed for BMI in which numeric information is needed.
      ###
      if(
        any(!suppressWarnings(!is.na(as.numeric(na.omit(unique(temp2[[paste0(x1,".x")]])))))) |
        any(!suppressWarnings(!is.na(as.numeric(na.omit(unique(temp2[[paste0(x1,".y")]]))))))
        ){
        warning(paste0("Covariate ",scheme[i,]$c.name," has values that do not fit in numeric format"))
      }
      ###
      #Collect the needed info from prior and if not available from the future.
      temp2 <- temp2[, Result := as.numeric(fifelse(is.na(get(paste0(x1,".x"))), get(paste0(x1,".y")), get(paste0(x1,".x"))))  ]
      temp2 <- temp2[, REFDT := as.integer(fifelse(is.na(get(paste0(x1,".x"))), REFDT.y, REFDT.x))  ]
      temp2 <- temp2[, Date := as.integer(fifelse(is.na(get(paste0(x1,".x"))), Date.y, Date.x))  ]
      
    }else{
      #Independent from if it is a greedy method or not, the loop continues with temp2 as the object name.
      temp2 <- as.data.table(COV2[[i]]$file1)
      
      #The sqlite database gives dates back as integers. So check if this is correct.
      if(
        any(!suppressWarnings(!is.na(as.numeric(na.omit(unique(temp2[[x1]]))))))
      ){
        warning(paste0("Covariate ",scheme[i,]$c.name," has values that do not fit in integer format"))
      }
      
      #Set all to integer. Thing that this is not needed.
      temp2 <- temp2[, Result := as.numeric(get(x1)) ][,Date := as.integer(Date)][,REFDT := as.integer(REFDT)]
    }
    
    #The name of the study variable is stored in the list and needs to be in a column so you can filter or aggregate on it.  
    temp2 <-  temp2[, col := paste0(COV2[[i]]$Concept,"")]
     
    #If a unit is available then store this in a separate file. This can be used for the calculation of the BMI 
    if("Unit" %in% colnames(temp2)){
        tempvoc <- copy(temp2)[, Voc := Unit][,.(person_id, id, Voc, col, REFDT)]
        if(exists("filevoc")) filevoc <- rbindlist(list(filevoc, tempvoc), use.names = T)
        if(!exists("filevoc")) filevoc <- tempvoc
        
    }  
    
    #Standardize column order in preparation of appending.  
    temp2 <- temp2[,.(person_id, id, Result, col, REFDT, Date)]
    
    #Append
    if(exists("fileresult")) fileresult <- rbindlist(list(fileresult, temp2), use.names = T, fill = T)
    if(!exists("fileresult")) fileresult <- temp2
    
    
    rm(temp2, x1)
    gc()
    
    }
    
    #There is also an option to extract concepts with a start and an and date(BETWEEN). This cannot in combination with prior or post.
    if(exists("file3", where = COV2[[i]])){
      temp2 <- as.data.table(COV2[[i]]$file3)[, col := paste0(COV2[[i]]$Concept,"")]
      #Distinct between binary and categorical. Not tested yet because in Pfizer only binary is used.
      if(nchar(scheme[i,]$coll) > 0){if("Value" %in% unlist(strsplit(scheme[i,]$coll, ",")[[1]])){x1 <- "Value"}else{x1 <- "ST"}}else{x1 <- "ST"}
      temp2 <- copy(temp2)[, Result := as.numeric(get(x1))][,.(person_id, id, Result, col, REFDT)]
      if(exists("fileresult")) fileresult <- rbindlist(list(fileresult, temp2), use.names = T, fill = T)
      if(!exists("fileresult")) fileresult <- temp2
      rm(temp2, x1)
    }
    
  }
    
  rm(COV2)
  gc()
  
  #Split variable and cohort information so it is easier to perform filter and aggregation actions.
  fileresult <- fileresult[, Time := substr(col, nchar(col) - 1, nchar(col))]
  fileresult <- fileresult[, Var := substr(col, 1, nchar(col) - 3)]
  #fileresult <- fileresult[, Result := as.numeric(Result)]
  ###
  
  
  #Create time to pregnancy 
  ###
  
  filepreg <- fileresult[Var == "L_PREGNSTATUS_COV",]
  if(nrow(filepreg) > 0){
  filepreg <- filepreg[, Result := as.integer(REFDT) - as.integer(Result)][, Var := "L_PREGTIMETO_DES"][, col := paste0("L_PREGTIMETO_DES_",Time)]
  saveRDS(filepreg, paste0(populations_dir,"DESCRIPTIVE_VARS.rds"))
  rm(filepreg)
  gc()
  }
  ###

  ###Construct BMI
  if(any(fileresult[["Var"]] %in% c("L_WEIGHT_COV", "L_HEIGHT_COV","L_BMI_COV"  ))){
    
    #Add also the Time and Var column on the file with the units to make it possible to merge
    filevoc <- filevoc[, Time := substr(col, nchar(col) - 1, nchar(col)) ]
    filevoc <- filevoc[, Var := substr(col, 1, nchar(col) - 3) ]  
    
    #Run the BMI function which is a simple function to give back 1 bmi out of 2 potential bmi's
    BMI <- CalculateBMI(
    #I separated the units from the values because otherwise I needed to have a column in the long file that would be almost entirely empty. So I stored separate and merged back.
    file = merge(x = copy(fileresult)[Var %in% c("L_WEIGHT_COV", "L_HEIGHT_COV","L_BMI_COV"  ),][, col := NULL], 
                 y = copy(filevoc), 
                 by = c("person_id", "id", "Time", "REFDT", "Var"),
                 all.x = T
                 ),
    Result = "Result",
    Voc = "Voc",
    Date = "Date",
    id = c("person_id", "id", "Time", "REFDT"),
    StudyVar = "Var",
    weight.var = "L_WEIGHT_COV",
    height.var = "L_HEIGHT_COV",
    bmi.var = "L_BMI_COV"
    
  )[, col := paste0("L_BMICALC_COV_", Time)][, Var := "L_BMICALC_COV"]
  
  #Append back to the long file and in the same time remove the input concepts form the long file. So weight, height and bmi are replaced by bmi
  #TODO: apparently rbindlist is appending tables without an error, or problem when a numeric column is added to a character...???
  fileresult <- rbindlist(list(copy(fileresult)[!grep(paste0(c("L_WEIGHT_COV","L_HEIGHT_COV","L_BMI_COV"), collapse = "|"), col)], BMI), use.names = T, fill = T)
  
  rm(BMI, filevoc)
  gc()
  
  }
  
  
  ###

  #Add scores
  ###
  
  #Import needed weights per sub concept
  #ALG <- readRDS(paste0(tmp, "Algorithms.rds"))
  ALG <- readRDS(SCRIPT[["INPUT3"]][["path"]])
  
  ALG <- ALG[NEW_CONCEPT %in% c(MATCH_WEIGHT, NMATCH_WEIGHT) & !is.na(WEIGHT),]
  
  #copy the file with needed subconcepts
  filescore <- copy(fileresult)
  
  #Merge the weights to the sub concepts by an inner join. Because of the innner join also a filtereing is done
  filescore <- merge(x = filescore[, .(person_id, id, Var, Time, REFDT, Result, Date)], y = unique(ALG[, .(WEIGHT, CONCEPT, NEW_CONCEPT)]), by.x = "Var", by.y = "CONCEPT", all = F, allow.cartesian = T)
  
  if(nrow(filescore) > 0){
  #Add also the lookback time for the scoring
  filescore <- merge(x = filescore, y = LOOKBACK_COV[,.(VarName, DAYS)], by.x = "NEW_CONCEPT", by.y =  "VarName", all.x = T, allow.cartesian = F)
  
  #Remove the rows that are outside the scope of the lookback time
  #filescore <- filescore[, Check := (as.numeric(REFDT) - as.numeric(Date))/365.25 ]
  filescore <- filescore[, Check := (as.numeric(REFDT) - as.numeric(Date)) ]
  filescore <- filescore[Check <= DAYS, ]
  #filescore3 <- filescore[Check <= CAT_PRIOR, ]
  
  
  #Score by summing the weights that are joined to the results
  filescore <- filescore[, .(Result = sum(as.numeric(WEIGHT))  ), by = c("person_id", "id", "Time", "REFDT", "NEW_CONCEPT")]
  
  #Give the column name. This name is needed for the casting later on.
  filescore <- filescore[, col := paste0(NEW_CONCEPT,"_",Time)]
  setnames(filescore, "NEW_CONCEPT", "Var")
  
  #Add the new study varaibles to the total file with covaraiates
  fileresult <- rbindlist(list(fileresult, filescore), use.names = T, fill = T)
  
  }
  
  rm(ALG, filescore)
  gc()
  

  
  
  ###
  
  
  #Remove cases outside lookback time
  ###
  
  fileresult <- fileresult[Var %in% COV,]
  fileresult <- merge(x = fileresult, y = unique(LOOKBACK_COV[,.(VarName, DAYS)]), by.x = "Var", by.y =  "VarName", all.x = T, allow.cartesian = F)
  #fileresult <- fileresult[!is.na(REFDT) & !is.na(Date) , Check := (as.numeric(REFDT) - as.numeric(Date))/365.25 ]
  fileresult <- fileresult[!is.na(REFDT) & !is.na(Date) , Check := (as.numeric(REFDT) - as.numeric(Date)) ]
  
  
  #BMI and score algoritms are already checked on lookback time. these have empty REFDT's and are not excluded. 
  #fileresult <-fileresult[Check <= CAT_PRIOR | (is.na(CAT_PRIOR) |  is.na(Date)), ]
  fileresult <-fileresult[Check <= DAYS | (is.na(DAYS) |  is.na(Date)), ]
  
  ###
  
  #Set TF vars to T
  ###
  fileresult <- fileresult[Var %in% c(COV_TF) , Result := 1 ]
  
  ###
  
  #Create per cohort a file with all the Covariates. 
  ###
  
  
  
  #Correct for naming. For intirim 3 change THIRD_PFIZER to t3 at the beginning.
  scheme <- scheme[, `:=` (Start_date = substr(c.name, nchar(c.name) - 1, nchar(c.name)))]

  #Import scores for variables that need scoring
  SCORE <-  readRDS(SCRIPT[["INPUT4"]][["path"]])[, c("CONCEPT","INTEGER_CODE","LOWER","UPPER"), with = F]
  
  filelabel <- copy(fileresult)[Var %in% unique(SCORE[["CONCEPT"]]),][, ST := as.numeric(Result)][,EN := as.numeric(Result)]
  setkeyv(filelabel, c("Var", "ST", "EN"))
  
  checkNa <- filelabel[is.na(Result) | is.nan(Result),][["col"]]
  
  if(length(checkNa) > 0){
    filelabel <- filelabel[!is.na(Result) & !is.nan(Result),]
    warning(paste0(paste0(checkNa, collapse = ", "), " has values with NA or NaS. These rows are deleted. Please check if original data is all numeric"))
  }  
  rm(checkNa)
  
  filelabel <- foverlaps(x = SCORE, 
                         y = filelabel, 
                         by.x = c("CONCEPT","LOWER","UPPER"), 
                         nomatch = 0L, 
                         type = "any")[, `:=` (ST = NULL, EN = NULL, LOWER = NULL, UPPER = NULL)][, Result := NULL]
    
  setnames(filelabel, c("INTEGER_CODE", "CONCEPT"), c("Result", "Var"))
  
  fileresult <- rbindlist(list(copy(fileresult)[!Var %in% unique(SCORE[["CONCEPT"]]),], filelabel), use.names = T, fill = T)
  
    
    
    rm(filelabel, SCORE)
    gc()
  
  #Check if all results are integer
  ###
  if(!any(!grepl('^-?[0-9]+$', unique(fileresult[["Result"]])))){
    
    fileresult <- fileresult[, Result := as.integer(Result)]}else{
    warning("Covariated that do not fit in integer format")
  }
  ###
  
  
  #i = "T0"
  for(i in unique(scheme$Start_date)){
        
        #Get all the avaiable covariates rows per cohort   
        colls_temp <- unique(fileresult[Time == i,][["col"]])  
        temp <- copy(fileresult)[(col %in% colls_temp) , ]   
        
        #temp <- rbind(temp, temp[1,])[0]
        
        testCast <- temp[, .(count = .N), by = c("person_id", "id", "col")][count >= 2 ,]
        
        if(nrow(testCast) > 0){
          tempError <- copy(temp)[, count := .N, by = c("person_id", "id", "col")][count >= 2 ,]
          saveRDS(tempError, paste0(tmp, "errorResultCastInvalidStep12.rds"))
          print(testCast )
          stop("Error because duplicated datapoints. Provide the following file when making an issue: '/Pfizer/Data characterisation/PfizerScript/g_intermediate/tmp/errorResultCastInvalidStep12.R'")
        }
        rm(testCast)
        
        #Make a wide with per person_id and match pair id only 1 row with all the covariates
        if(nrow(temp) > 0){
        temp <- data.table::dcast(temp , person_id + id ~ col, value.var = "Result", fill = 0)
        }else{temp <- temp[, .(person_id, id)]}
        
        #Check which covariates have 0 cases and create columns for them. Covariates form persons table are added later
        new_person <- c(time_indep_match, time_indep_nmatch)[c(time_indep_match, time_indep_nmatch) %in% COV]
        needed_cols_temp <- paste0(COV[!COV %in% new_person],"_",i)
        missing_cols <- needed_cols_temp[!needed_cols_temp %in% colnames(temp)]
        invisible(lapply(missing_cols, function(x) temp <- temp[, eval(x) := as.integer()] ))
        
        #Get person level information/covariates and make a file with all the subjects. Also those with no covariate occurences 
        new_person_code <- paste0(",",paste0(new_person, collapse = ","))
        
        #get correct cohort
        from_code <- unique(scheme[Start_date == i,][["FILE"]])
        
        #If not T0 than matches are not included HARD coded. Improve in intirim 3
        if(i == "D3"){
          where_code <- "WHERE THIRD_PFIZER IS NOT NULL" 
          
        }else if(i == "D2"){
          where_code <- "WHERE SECOND_PFIZER IS NOT NULL" 
           
        }else{where_code <- ""}
        
        temp <- merge(
                      x = as.data.table(dbGetQuery(mydb, paste0("SELECT person_id, id ",new_person_code ,"  FROM ", from_code," ", where_code))),
                      y = temp,
                      by = c("person_id", "id"),
                      all.x = T,
                      allow.cartesian = F
        )
      
        temp[is.na(temp)] <- 0
      
        if(i == "T0"){
          #Get categorical variables that sould be deleted if missing. This is socioeconomic status and region. Note that extra are added that they are automatically be added
          deleteExpression <- paste0(MATCH_CAT[!MATCH_CAT %in% MATCH_SCORE], "_T0")
          
          #Check if these columns are in the file and not empty
          deleteExpression <- deleteExpression[deleteExpression %in% colnames(temp)]
          deleteExpression <- deleteExpression[sapply(deleteExpression , function(x) sum(temp[[x]]) > 0)]
          
          if(length(deleteExpression) > 0){
          
          #Create base expression based on this variables to check for missingness in any of the varaibles
          deleteExpression <-paste0(paste0(deleteExpression, " != 0 "), collapse = " & ")
          
          #Store the subjects that are going to be deleted because of missingness in socio economic status or region
          #temp2 <- temp[!(L_GEOREGION_COV_T0 != 0 &  L_SOCIOECO_COV_T0 != 0), ]
          temp2 <- temp[eval(parse(text = paste0("!(",deleteExpression,")"))), ]
          saveRDS(as.data.table(temp2), paste0(populations_dir, "/M_Studycohort_Covariates_",i,"_deleted.rds"))
          
          
          #Continue with the subjects that do not have any missings in region and socioeconomic status
          #temp1 <- temp[L_GEOREGION_COV_T0 != 0 &  L_SOCIOECO_COV_T0 != 0, ]
          temp <- temp[eval(parse(text = deleteExpression)), ]
          
          #As an extra check assure that it not happens that from a pair one is in the deleted while the other not. This may be possible in the situation of contrasting information on 1 day.
          checkPair <- sum(temp$id %in% temp2$id)
          if(checkPair > 0) warning(paste0("A categorical matcing variable is not equal within a pair in ",checkPair," pairs. This may happen when contrasing information on 1 date is in the CDM. Pleas check this."))
          
          rm(temp2, checkPair)
          
          }
          
          rm(deleteExpression)
          
        }
        
        saveRDS(as.data.table(temp), paste0(populations_dir, "/M_Studycohort_Covariates_",i,".rds"))
        
    
        
        rm(colls_temp, temp, missing_cols, needed_cols_temp, new_person, new_person_code,  from_code, where_code)
        gc()
  }
  
  ####
  
  rm(fileresult, scheme)
  gc()
  dbDisconnect(mydb)
  
}else{
  dbDisconnect(mydb)
  print("No Covariates availble in data")
  
  for(i in c("T0", "D3")){
    
    coll_temp <- c("person_id", "id", paste0(COV,"_",i,"_COV"))
    temp <- matrix(NA, nrow = 0, ncol = length(coll_temp))
    colnames(temp) <- coll_temp
    saveRDS(as.data.table(temp), paste0(populations_dir, "/M_Studycohort_Covariates_",i,".rds"))
    rm(temp, coll_temp)
    
  }
  
  

}


rm(mydb, Available_AESI, Available_cov, Missing_AESI, Missing_cov, Empty_AESI, Empty_cov, SCRIPT, COV_check)
gc()





