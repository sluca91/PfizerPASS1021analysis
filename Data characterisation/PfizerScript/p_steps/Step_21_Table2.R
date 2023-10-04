#Author: Albert Cid Royo MSc.
#email: a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/01/2022

##Aim
#Generate Table 2

##in/output
#Input 1: PERSONS3.rds

#Output 1: TABLE1.rds
### Create Immunocompmised and Fraility


CreateNewCovariates <- function(){
  ALG <- as.data.table(read.csv(paste0(meta_dir,'/Pfizer_algorithms_additional.csv'),sep = ';'))
  mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)
  Algoritms <- unique(ALG$NEW_CONCEPT) 
  
  #i <- Algoritms[1]
  
  db_tables <- dbListTables(mydb)
  
  tablesToCreate <- Algoritms[!Algoritms %in% db_tables]
  
  for(i in tablesToCreate){
    
    to_append <- toupper(unique(ALG[NEW_CONCEPT == i,]$CONCEPT))
    
    TEMP <- AppendConcepts(DB = mydb, CONCEPTS = to_append, NAME = i)
    
    if(!is.null(TEMP)){
      dbWriteTable(mydb, i, TEMP)
    }
    
    rm(TEMP, to_append)
    gc()
    
  }
  
  dbDisconnect(mydb)
}

CreateNewCovariates()

GetDatesNewCovariates <- function(){
  additionalStudyVar <- as.data.table(read.csv(paste0(meta_dir,'/Pfizer_study_variables_additional.csv'),sep = ';'))
  ALG <- as.data.table(read.csv(paste0(meta_dir,'/Pfizer_algorithms_additional.csv'),sep = ';'))
  
  
  cols_scheme <- unique(ALG$NEW_CONCEPT) 
  additionalStudyVar <- additionalStudyVar[VarName %in% cols_scheme]
  scheme <- cbind(Concept = cols_scheme, FILE =rep("M_Studycohort",length(cols_scheme)),Start_date =rep("T0",length(cols_scheme)),  type = rep("COV",length(cols_scheme)))            
  scheme <- as.data.table(scheme)
  scheme <- scheme[,c.name := paste0(Concept,"_T0")]
  scheme <- scheme[, lookback := 999]
  scheme <- scheme[ ,CAT := FALSE]
  scheme <- scheme[, post := FALSE]
  scheme <- scheme[, prior := TRUE]
  scheme <- scheme[ ,between := FALSE]
  scheme <- scheme[ , endpoint := "t1.op_end_date" ]
  scheme <- scheme[ , coll := "" ]
  scheme <- scheme[ , prior.col := "Date" ]
  
 # statement <- dbSendStatement(mydb,'CREATE TABLE M_Studycohort2 AS SELECT * FROM M_Studycohort;')

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
    keep_start_date = T
  ))
  
  #mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)
  M_Studycohort <<- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort.rds')))
  
  availableNames <- NULL
  for (i in COV2){
    availableNames <- c(availableNames,i$Concept)
    concept_file <- as.data.table(i$file)
    M_Studycohort <<- merge(x = M_Studycohort, y = concept_file[,c('person_id','id','Date')], all = T, allow.cartesian = F, by = c("person_id", "id"))
    setnames(M_Studycohort,'Date',i$Concept)
  }
  
  if(length(availableNames) != length(cols_scheme)){
    missing_cols <- cols_scheme[!cols_scheme %in% availableNames]
    lapply(missing_cols, function(x)  M_Studycohort[,get(x) := NA])
  }
    
  colNames <- c('person_id','id',paste0(cols_scheme,sep = '_T0'))
  M_Studycohort2 <- as.data.table(M_Studycohort)[,..colNames]
  saveRDS(M_Studycohort2,paste0(populations_dir,'M_Studycohort2.rds'))
}

GetDatesNewCovariates()

table2_content_grouping <- function(data,var){
  #This function uses the table_content funciton within a loop for each of the unique values within a column.
  #This functions is used for generating the table 2 of subpopulations based in a covariate such as sex or age_band
 
  data[,label:=.GRP, by = var]
  data[,name_extension:= do.call(paste, c(.SD, sep = "_")), .SDcols= var]
  #combn(letters[1:4], 2) 
  
  for (current_group in unique(data$label)){
    df_filtered <- data[label == current_group]
    table2_content(df_filtered, unique(df_filtered$name_extension))
  }
}



table2_content <- function(data, name_extension = NULL ){
  #This funciton generates the results defined within the Pfizer SAP
  # See the table shells document for further details of the table
  
    Two_Dose_Population <- data[!is.na(FIRST_PFIZER) & !is.na(SECOND_PFIZER),]
    Three_Dose_Population <- data[!is.na(FIRST_PFIZER) & !is.na(SECOND_PFIZER) & !is.na(THIRD_PFIZER),]
    Four_Dose_Population <- data[!is.na(FIRST_PFIZER) & !is.na(SECOND_PFIZER) & !is.na(THIRD_PFIZER) &!is.na(FOURTH_PFIZER),]
    
    Two_Dose_Population <- Two_Dose_Population[, ":=" ( 
      diffVac_12 = as.numeric(difftime(SECOND_PFIZER,FIRST_PFIZER, units = 'days'))
    )]
    
    Three_Dose_Population <- Three_Dose_Population[, ":=" (
      diffVac_12 = as.numeric(difftime(SECOND_PFIZER,FIRST_PFIZER, units = 'days')),
      diffVac_23 = as.numeric(difftime(THIRD_PFIZER,SECOND_PFIZER, units = 'days'))
    )]
    
    Four_Dose_Population <- Four_Dose_Population[, ":=" (
      diffVac_12 = as.numeric(difftime(SECOND_PFIZER,FIRST_PFIZER, units = 'days')),
      diffVac_23 = as.numeric(difftime(THIRD_PFIZER,SECOND_PFIZER, units = 'days')),
      diffVac_34 = as.numeric(difftime(FOURTH_PFIZER,THIRD_PFIZER, units = 'days'))
    )]
    
    Distribution_diffVac12 <- summary(Two_Dose_Population$diffVac_12)
    Distribution_diffVac23 <- summary(Three_Dose_Population$diffVac_23)
    Distribution_diffVac34 <- summary(Four_Dose_Population$diffVac_34)
    
    col1 = list(n1 = data[!is.na(FIRST_PFIZER), .N],#'First dose COVID-19 received, n',
                n20 = Two_Dose_Population[diffVac_12 < 7*7,.N],# 'Second dose COVID-19 received within 6 weeks (completion rate) after 1st dose, n (%)',
                n21 = Two_Dose_Population[diffVac_12 >= 7*7,.N],# 'Second dose COVID-19 received later than 6 weeks after 1st dose',
                n22 = Two_Dose_Population[diffVac_12 < 7*7,.N] + Two_Dose_Population[diffVac_12 >= 7*7,.N], #Total second dose received (n)
                '',#Interval between first and second dose COVID-19 (weeks)
                media_1 = format(round(Distribution_diffVac12[[3]]/7, 2), nsmall = 2),
                min_1 = format(round(Distribution_diffVac12[[1]]/7, 2), nsmall = 2),
                n0_2 = Two_Dose_Population[diffVac_12 < 7*2,.N],
                n2_4 = Two_Dose_Population[diffVac_12 >= 7*2 & diffVac_12 < 7*5,.N],
                n5_6 = Two_Dose_Population[diffVac_12 >= 7*5 & diffVac_12 < 7*7,.N],
                n7_8 = Two_Dose_Population[diffVac_12 >= 7*7 & diffVac_12 < 7*9,.N],
                n9_13 = Two_Dose_Population[diffVac_12 >= 7*9 & diffVac_12 < 7*13,.N],
                n13_18 = Two_Dose_Population[diffVac_12 >= 7*13 & diffVac_12 < 7*18,.N],
                n18_ = Two_Dose_Population[diffVac_12 >= 18*7,.N],
                n3 = Three_Dose_Population[,.N],#'Third dose COVID-19 received'
                '',#'Interval between second and third dose COVID-19 (weeks)'
                media_2 = format(round(Distribution_diffVac23[[3]]/7, 2), nsmall = 2),
                min_2 = format(round(Distribution_diffVac23[[1]]/7, 2), nsmall = 2),
                n0_12 = Three_Dose_Population[diffVac_23 < 7*12,.N],
                n12_24 = Three_Dose_Population[diffVac_23 >= 7*12 & diffVac_23 < 7*25,.N],
                n25_37 = Three_Dose_Population[diffVac_23 >= 7*25 & diffVac_23 < 7*38,.N],
                n38_50 = Three_Dose_Population[diffVac_23 >= 7*38 & diffVac_23 < 7*51,.N],
                n50_ = Three_Dose_Population[diffVac_23 >= 7*51,.N],
                n4 = Four_Dose_Population[,.N],#Fourth COVID-19 received
                '',
                media_3 = format(round(Distribution_diffVac34[[3]]/7, 2), nsmall = 2),
                min_3 = format(round(Distribution_diffVac34[[1]]/7, 2), nsmall = 2),
                n0_12_3 = Four_Dose_Population[diffVac_34 < 7*12,.N],
                n12_24_3 = Four_Dose_Population[diffVac_34 >= 7*12 & diffVac_34 < 7*25,.N],
                n25_37_3 = Four_Dose_Population[diffVac_34 >= 7*25 & diffVac_34 < 7*38,.N],
                n38_50_3 = Four_Dose_Population[diffVac_34 >= 7*38 & diffVac_34 < 7*51,.N],
                n50_3 = Four_Dose_Population[diffVac_34 >= 7*51,.N] 
    )
    
    
    col2 <- list( n1_2 = format(round(100*col1$n1/col1$n1, 2), nsmall = 2),#'First dose COVID-19 received, n',
                  n20_1 = format(round(100*col1$n20/col1$n1, 2), nsmall = 2),# 'Second dose COVID-19 received within 6 weeks (completion rate) after 1st dose, n (%)',
                  n21_1 = format(round(100*col1$n21/col1$n1, 2), nsmall = 2),# 'Second dose COVID-19 received later than 6 weeks after 1st dose',
                  n22_1 = format(round(100*col1$n22/col1$n1, 2), nsmall = 2),#Total second dose received (n)
                  '',
                  q1_1 = paste0(format(round(Distribution_diffVac12[[2]]/7, 2), nsmall = 2),'-',format(round(Distribution_diffVac12[[5]]/7, 2), nsmall = 2)),
                  max_1 = format(round(Distribution_diffVac12[[6]]/7, 2), nsmall = 2),
                  per0_2 =  format(round(100*col1$n0_2/(col1$n21+col1$n20), 2), nsmall = 2),
                  per2_4 =  format(round(100*col1$n2_4/(col1$n21+col1$n20), 2), nsmall = 2),
                  per5_6 = format(round(100*col1$n5_6/(col1$n21+col1$n20), 2), nsmall = 2),
                  per7_8 =  format(round(100*col1$n7_8/(col1$n21+col1$n20), 2), nsmall = 2), 
                  per9_13 =  format(round(100*col1$n9_13/(col1$n21+col1$n20), 2), nsmall = 2),
                  per13_18 =  format(round(100*col1$n13_18/(col1$n21+col1$n20), 2), nsmall = 2),
                  per18_ =  format(round(100*col1$n18_/(col1$n21+col1$n20), 2), nsmall = 2),
                  n3_1 =  format(round(100*col1$n3/col1$n1, 2), nsmall = 2),
                  '',
                  q1_2 = paste0(format(round(Distribution_diffVac23[[2]]/7, 2), nsmall = 2),'-',format(round(Distribution_diffVac23[[5]]/7, 2), nsmall = 2)),
                  max_2 = format(round(Distribution_diffVac23[[6]]/7, 2), nsmall = 2),
                  per0_12 =  format(round(100*col1$n0_12/col1$n3, 2), nsmall = 2),
                  per12_24 =  format(round(100*col1$n12_24/col1$n3, 2), nsmall = 2), 
                  per25_37 =  format(round(100*col1$n25_37/col1$n3, 2), nsmall = 2),
                  per38_50 =  format(round(100*col1$n38_50/col1$n3, 2), nsmall = 2),
                  per50_ =  format(round(100*col1$n50_/col1$n3, 2), nsmall = 2),
                  n4_1 =  format(round(100*col1$n4/col1$n1, 2), nsmall = 2),
                  '',
                  q1_2_3 = paste0(format(round(Distribution_diffVac34[[2]]/7, 2), nsmall = 2),'-',format(round(Distribution_diffVac34[[5]]/7, 2), nsmall = 2)),
                  max_2_3 = format(round(Distribution_diffVac34[[6]]/7, 2), nsmall = 2),
                  per0_12_3 =  format(round(100*col1$n0_12_3/col1$n4, 2), nsmall = 2),
                  per12_24_3 =  format(round(100*col1$n12_24_3/col1$n4, 2), nsmall = 2), 
                  per25_37_3 =  format(round(100*col1$n25_37_3/col1$n4, 2), nsmall = 2),
                  per38_50_3 =  format(round(100*col1$n38_50_3/col1$n4, 2), nsmall = 2),
                  per50_3 =  format(round(100*col1$n50_3/col1$n4, 2), nsmall = 2)
                  
    )
    
    table2 <- data.table(cbind(col1,col2))
    setnames(table2, c("N", "PER"))
    table2 <- cbind(setnames(as.data.table(template_table2),'LABEL'),table2)
    if (is.null(name_extension)){
      fwrite(table2, file = paste0(output_dir,DAP,'_Table2.csv'))
    }else{
      fwrite(table2, file = paste0(output_dir,DAP,'_Table2_',name_extension,'.csv'))
    }
   
}

Main <- function(){
  #M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort.rds')))
  M_Studycohort_Covariates <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort_Covariates_T0.rds')))
  ExposedPersons <- M_Studycohort[group == 'EXPOSED' | group == 'UNMATCHED' & !is.na(FIRST_PFIZER),][,band := NULL]
  
  #Count Per Age band
  bands1 <- as.data.table(CreateBands(c(0,2,5,12,16,18,30,40,50,60,70,80), NEWBORNS = F))
  bands2 <- as.data.table(CreateBands(c(80,140), NEWBORNS = F))[band0 == "080-139", band0 := "80+"]
  bands <- rbind(bands1, bands2)[,.(INT, band0)]
  setnames(bands, "band0", "band")
  ExposedPersons <- merge(ExposedPersons, bands, by.x="AGE_T0", by.y="INT")
  
  
  Exposed_M_Studycohort_Covariates <- M_Studycohort_Covariates[ExposedPersons, on = c("person_id","id")]
  
  
  table2_content_grouping(ExposedPersons,'band') #Per age band
  table2_content_grouping(ExposedPersons,'L_SEX_COV') #Per Sex
  table2_content_grouping(ExposedPersons,c('band','L_SEX_COV')) #Per age Band and Sex
  table2_content(ExposedPersons) #All Exposd
  table2_content(ExposedPersons[AGE_T0 > 64],'Elderly')
  table2_content(ExposedPersons[!is.na(Im_IMMCOMPMATCHPFIZER_COV_T0) == TRUE],'Immunocompromised')#Immunicompromised
  table2_content(ExposedPersons[!is.na(Im_FRAILCOMORB_COV_T0) == TRUE],'FrailComorbid') #Frail comorbid population

}

# template_table2 =  c(
#                              'Individuals receiving a first dose of the Pfizer-BioNTech COVID‑19 vaccine.',
#                              'Individuals receiving a second dose of the Pfizer-BioNTech COVID‑19 vaccine within 6 weeks (completion rate). ',
#                              'Individuals receiving a second dose of the Pfizer-BioNTech COVID‑19 vaccine outside of the Pfizer-BioNTech recommended COVID-19 vaccination schedule',
#                              'Distribution of days between the first and second dose of the Pfizer-BioNTech COVID‑19 vaccine',
#                              'Median (Q1, Q3)',
#                              '[Minimum, maximum]',
#                              '   < 2 weeks',
#                              '   2-4 weeks',
#                              '   5-6 weeks',
#                              '   7-8 weeks',
#                              '   9-12 weeks',
#                              '   13-18 weeks',
#                              '   >18 weeks',
#                              'Individuals receiving a third dose of the Pfizer-BioNTech COVID‑19 vaccine (after 2 consequtive doses of the same vaccine). ',
#                              'Distribution of days between the second and third dose of the Pfizer-BioNTech COVID‑19 vaccine',
#                              'Median (Q1, Q3)',
#                              '[Minimum, maximum]',
#                              '<12 weeks',
#                              '12-24 weeks',
#                              '25-37 weeks',
#                              '38-50 weeks',
#                              '>50 weeks')

template_table2 <- c('Total first dose Pfizer-BioNTech COVID-19 vaccine received, N',
                      'Second dose PfizerBioNTech COVID-19 vaccine received ≤6 weeks (completion rate) after 1st dose, n (%) ',
                      'Second dose PfizerBioNTech COVID-19 vaccine received >6 weeks after 1st dose',
                      'Total second dose received (n)',
                      '   Interval between first and second dose (weeks)',
                        'Median (Q1, Q3)',
                        '[Minimum, maximum]',
                        '< 2 weeks, n (%)',
                        '2-4 weeks, n (%)',
                        '5-6 weeks, n (%)',
                        '7-8 weeks, n (%)',
                        '9-12 weeks, n (%)',
                        '13-18 weeks, n (%)',
                        '>18 weeks, n (%)',
                      'Total third dose received (n)',
                      'Interval between second and third dose (weeks)',
                        'Median (Q1, Q3)',
                        '[Minimum, maximum]',
                        '<12 weeks, n (%)',
                        '12-24 weeks, n (%)',
                        '25-37 weeks, n (%)',
                        '38-50 weeks, n (%)',
                        '>50 weeks, n (%)',
                      'Total fourth dose received (n)',
                      'Interval between third and fourth dose (weeks)',
                        'Median (Q1, Q3)',
                        '[Minimum, maximum]',
                        '<12 weeks, n (%)',
                        '12-24 weeks, n (%)',
                        '25-37 weeks, n (%)',
                        '38-50 weeks, n (%)',
                        '>50 weeks, n (%)')

cols <- c('birth_date','FIRST_PFIZER','SECOND_PFIZER','THIRD_PFIZER','FOURTH_PFIZER','FIRST_OTHER','SECOND_OTHER','THIRD_OTHER','FOURTH_OTHER','T0','Im_FRAILCOMORB_COV_T0','Im_IMMCOMPMATCHPFIZER_COV_T0')
M_Studycohort <<- M_Studycohort[ ,                        
                              (cols) := lapply(.SD, as.Date, origin = "1970-01-01" ),
                              .SDcols = cols]

M_Studycohort <<- M_Studycohort[,op_start_date := NULL][,op_end_date := NULL]

Main()
rm(M_Studycohort,cols)
