#Author: Albert Cid Royo MSc.
#email: a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/01/2022

##Aim
#Generate Table 4

##in/output
#Input 1: 

#Output 1:

library(data.table)

labels <- as.data.table(c(
  'Total, n (%)',
  'Comedications, n (%)',
'  Analgesics',
  'Antibiotics',
  'Antiviral medications',
  'Corticosteroids',
  'Non-steroidal anti-inflammatory drugs',
  'Psychotropics',
  'Statins',
  'Novel oral anticoagulants',
  'Warfarin',
  'Immunosuppressant medication use',
  'Other vaccines, n (%)',
  'Influenza',
  'Pneumococcal',
  'DTP (diphtheria, tetanus, and pertussis)',
  'TPV (polio)',
  'TV (MMR) (measles, mumps and rubella)',
  'Hib (Haemophilus influenzae type b)',
  'HB (hepatitis B virus)',
  'VZV (varicella-zoster virus)',
  'HPV (human papillomavirus)',
  'Meningitis',
  'Rotavirus'))
setnames(labels,c("LABELS"))

table4_content <- function(data,eventList){
  
  col1 <- as.numeric()
  i <- 0
  for (event in eventList$variable_name){
    i <- i +1
    if ((!is.na(event) | !is.null(event)) & event %in% names(data)){
      
      if (is.na(eventList[i,subcategory_value]) | is.null(eventList[i,subcategory_value])){
        col1 <- c(col1,
                  data[event == TRUE,.N])
      }else{
        col1 <- c(col1,
                  data[event == eventList[i,subcategory_value],.N])
      }
    }
    else{
      col1 <- c(col1,
                NA)
    }
  }
  
  col2 <- col1/nrow(data)
  table4 <- data.table(cbind(col1,col2))
  
}

f <- function(e, .SD) {
  eval(parse(text=e[1]), envir=.SD)
}

table4_content2 <- function(subjectList, eventList,CovariatesInformation,eventFileList,referenceExtension){
  #Version of the table4_conten for the interim 2
  eventFileList <- gsub('.rds',replacement = '',eventFileList)
  col1 <- as.numeric()
  N <- length(subjectList)
  col1 <- c(col1, N)
  i <- 0
  for (event in eventList$variable_name){
    i <- i +1
    if (!is.na(event) & !is.null(event)){
      eventColName <- toupper(paste0(event,referenceExtension))
      if (eventColName %in% names(CovariatesInformation) & !eventColName %in% eventFileList){
        #When the event is a covariate
        if (is.na(eventList[i,subcategory_value]) | is.null(eventList[i,subcategory_value]) | eventList[i,subcategory_value] == ''){
          print(paste0(event,' is counted as COVARIATE without subcategory '))
          col1 <- c(col1,
                    CovariatesInformation[get(eventColName) == TRUE,.N])
        }else{
          print(paste0(event,' is counted as COVARIATE with subcategory ',eventList[i,subcategory_value]))
          if (!is.na(as.numeric(eventList[i,subcategory_value]))){
            subcategoryVal <- as.numeric(eventList[i,subcategory_value])
            col1 <- c(col1,
                      CovariatesInformation[get(eventColName) == subcategoryVal,.N])
          }else{
            col1 <- c(col1,
                      CovariatesInformation[eval(parse(text = paste0(eventColName,eventList[i,subcategory_value]))),.N])
          }
          
        }
      }else if(eventColName %in% eventFileList){
        #When the event is a aesi
        print(paste0(event,' is counted as AESI'))
        aesi <- as.data.table(readRDS(paste0(aesi_dir,eventColName,'.rds')))[person_id %in% subjectList]
        col1 <- c(col1,
                  aesi[,.N])
      }else{
        if (!event %in% 'NA'){
          print(paste0(eventColName,' not found either as AESI or COVARIATE'))
        }
        col1 <- c(col1,
                  NA)
      }
    }else{
      col1 <- c(col1,
                NA)
    }
  }
  
  N_col <- col1
  table4 <- data.table(N_col)[,'PER_col' := format(round((100*N_col/N), 2), nsmall = 2)]
  table4[N_col %between% list(1,4), N_col := -5]
  table4[N_col == -5 , PER_col := NA]
}

Main <- function(){
  
  Pfizer_Table4_metadata <- setorder(fread(paste0(meta_dir,'/Pfizer_Table5_metadata.csv')),cols='index')
  M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort.rds')))
  M_Studycohort_Covariates_T0 <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort_Covariates_T0.rds')))
  M_Studycohort_Covariates_D3 <- as.data.table(readRDS(paste0(populations_dir,'/M_Studycohort_Covariates_D3.rds')))
  
  names_t0 <- str_remove(colnames(M_Studycohort_Covariates_T0),'_T0')
  names_D3 <- str_remove(colnames(M_Studycohort_Covariates_D3),'_D3')
  load(paste0(dir_base,"/Data characterisation/PfizerScript/g_intermediate/tmp/parameters.RData"))
  
  missingAvailable_T0 <- Available_cov[!Available_cov %in% names_t0]
  invisible(lapply(missingAvailable_T0, function(x) M_Studycohort_Covariates_T0[,eval(x) := 0]))
  
  missingAvailable_D3 <- Available_cov[!Available_cov %in% names_D3]
  invisible(lapply(missingAvailable_D3, function(x) M_Studycohort_Covariates_D3[,eval(x) := 0]))
  
  AESIS_list_T0 <- list.files(aesi_dir, pattern = '_T0.rds')
  setnames(M_Studycohort,'group','S_Group')
  First_Dose_Population <- M_Studycohort[!is.na('FIRST_PFIZER') & (S_Group == 'EXPOSED'| S_Group == 'UNMATCHED'),][,month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),'-',sprintf("%04d",year(FIRST_PFIZER)))]
  M_Studycohort_Covariates_T0 <- M_Studycohort_Covariates_T0[First_Dose_Population[,.(person_id,id)], on= c('person_id','id')]
  First_Dose_Population_subject_list <- First_Dose_Population$person_id
  t5_first <- table4_content2(First_Dose_Population_subject_list, Pfizer_Table4_metadata,M_Studycohort_Covariates_T0,AESIS_list_T0, referenceExtension = '_T0')
  setnames(t5_first, c("N1","PER1"))
  rm(First_Dose_Population,M_Studycohort_Covariates_T0,AESIS_list_T0,First_Dose_Population_subject_list)
  
  
  AESIS_list_D3 <- list.files(aesi_dir, pattern = '_D3.rds')
 
  Three_Dose_Population <- M_Studycohort[!is.na(FIRST_PFIZER) & !is.na(SECOND_PFIZER) & !is.na(THIRD_PFIZER) & (S_Group == 'EXPOSED' | S_Group == 'UNMATCHED'),][,month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),'-',sprintf("%04d",year(FIRST_PFIZER)))]
  M_Studycohort_Covariates_D3 <- M_Studycohort_Covariates_D3[Three_Dose_Population[,person_id], on= 'person_id']
  Three_Dose_Population_subject_list <- Three_Dose_Population$person_id
  t5_third <- table4_content2(Three_Dose_Population_subject_list, Pfizer_Table4_metadata,M_Studycohort_Covariates_D3,AESIS_list_D3, referenceExtension = '_D3')
  setnames(t5_third, c("N3","PER3"))
  rm(M_Studycohort_Covariates_D3,AESIS_list_D3,Three_Dose_Population,Three_Dose_Population_subject_list)
  
  t5 <- cbind(t5_first,t5_third)
  table5 <- cbind(labels,t5)
  table5[ table5 == -5] <- '<5'
  
  fwrite(table5, file = paste0(output_dir,DAP,'_Table5.csv'))
  
}
Main()

