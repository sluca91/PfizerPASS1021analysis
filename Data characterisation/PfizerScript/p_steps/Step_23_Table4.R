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
  'COVID‑19 history',
  'Previous diagnosis of COVID‑19',
  'Positive test result for COVID‑19',
  'Comorbidities',
  'History of anaphylaxis',
  'History of allergies',
  'Diabetes mellitus (types 1 and 2)',
  'Hypertension',
  'Cardiovascular disease',
#  'Cerebrovascular disease',
  'Chronic respiratory disease',
  'Chronic kidney disease',
  'Chronic liver disease',
  'Cancer',
  'Autoimmune disorders',
  'Influenza infection or other respiratory infections',
  'Charlson Comorbidity Index',
  'Score 0 or 1',
  'Score 2',
  'Score 3 or more',
  'Myocardial infarct ',
  'Congestive heart failure', 
  'Cerebrovascular disease',
  'Peripheral vascular disease',
  #'Chronic respiratory disease',
  'Mild to moderate kidney disease',
  'Severe kidney disease',
  'Mild liver disease',
  'Moderate or severe liver disease',
  'Malignant tumor',
  'Metastatic solid tumor',
  'HIV/AIDS', 
  'Diabetes complications',
  'Diabetes no complications',
  'Dementia',
  'Skin ulcer',
  'Hemiplegia',
  'Connective tissue disease',
  'CDC at-risk groups',
  'Group 0 (no conditions)',
  'Group 1 (1 condition)',
  'Group 2 (>1 condition)',
  'Immunocompromising conditions',
  'Surrogates of frailty',
  'Wheelchair use',
  'Home hospital bed',
  'Paralysis',
  'Parkinson’s disease',
  'Weakness',
  'Stroke/brain injury',
  'Ambulance transport',
  'Difficulty walking',
  'Home oxygen',
  'Rehabilitation care',
  'Psychiatric illness',
  'Sepsis',
  'Podiatric care',
  'Bladder incontinence',
  'Arthritis',
  'Coagulation deficiencies',
  'Vertigo',
  'Lipid abnormalities'))
setnames(labels,c("LABELS"))

#labels2 <- fread(paste0(meta_dir,'/Pfizer_Table4_header.csv'),sep = ',')

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

table4_content2 <- function(subjectList, eventList, CovariatesInformation, eventFileList, referenceExtension) {
  # Define a function called `table4_content2` with the following input arguments:
  # - `subjectList`: a vector of subject IDs
  # - `eventList`: a data frame containing information about the events
  # - `CovariatesInformation`: a data frame containing information about covariates
  # - `eventFileList`: a vector of file names
  # - `referenceExtension`: a character string specifying the reference extension
  
  # Remove ".rds" from file names in `eventFileList`
  eventFileList <- gsub('.rds', replacement = '', eventFileList)
  
  # Create an empty numeric vector called `col1`
  col1 <- as.numeric()
  
  # Get the number of subjects
  N <- length(subjectList)
  
  # Add the number of subjects to `col1`
  col1 <- c(col1, N)
  
  # Initialize a counter variable `i` to 0
  i <- 0
  
  # Loop through each event in `eventList`
  for (event in eventList$variable_name) {
    i <- i + 1
    
    # Check if the event is not NA or NULL
    if (!is.na(event) & !is.null(event)) {
      
      # Create the event column name by converting the event name to uppercase and appending the `referenceExtension`
      eventColName <- toupper(paste0(event, referenceExtension))
      
      # Check if the event is a covariate and is present in `CovariatesInformation` but not in `eventFileList`
      if (eventColName %in% names(CovariatesInformation) & !eventColName %in% eventFileList) {
        
        # When the event is a covariate
        
        # Check if the event has a subcategory value
        if (is.na(eventList[i, subcategory_value]) | is.null(eventList[i, subcategory_value]) | eventList[i, subcategory_value] == '') {
          # If the event does not have a subcategory value, count the number of subjects with the covariate
          print(paste0(event, ' is counted as COVARIATE without subcategory '))
          col1 <- c(col1, CovariatesInformation[get(eventColName) == TRUE, .N])
        } else {
          # If the event has a subcategory value, count the number of subjects with the covariate and the specified subcategory
          print(paste0(event, ' is counted as COVARIATE with subcategory ', eventList[i, subcategory_value]))
          if (!is.na(as.numeric(eventList[i, subcategory_value]))) {
            # If the subcategory value is numeric, use it to subset the data
            subcategoryVal <- as.numeric(eventList[i, subcategory_value])
            col1 <- c(col1, CovariatesInformation[get(eventColName) == subcategoryVal, .N])
          } else {
            # If the subcategory value is not numeric, evaluate the expression using `eval(parse(text = ...))` and count the number of subjects
            col1 <- c(col1, CovariatesInformation[eval(parse(text = paste0(eventColName, eventList[i, subcategory_value]))) == 1, .N])
          }
        }
      } else if (eventColName %in% eventFileList) {
        # When the event is an AESI
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

  Pfizer_Table4_metadata <- setorder(fread(paste0(meta_dir,'/Pfizer_Table4_metadata.csv')),cols='index')
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
  t4_first <- table4_content2(First_Dose_Population_subject_list, Pfizer_Table4_metadata,M_Studycohort_Covariates_T0,AESIS_list_T0, referenceExtension = '_T0')
  #N_total1 <- length(First_Dose_Population_subject_list) #Total number subject
  #t4_first <- as.data.table(c(N_total1,t4_first))
  setnames(t4_first, c("N1","PER1"))
  #t4_first <- t4_first[,'PER1' := N1/N_total1]
  rm(First_Dose_Population,M_Studycohort_Covariates_T0,AESIS_list_T0,First_Dose_Population_subject_list)
 
  
  AESIS_list_D3 <- list.files(aesi_dir, pattern = '_D3.rds')
 
  Three_Dose_Population <- M_Studycohort[!is.na(FIRST_PFIZER) & !is.na(SECOND_PFIZER) & !is.na(THIRD_PFIZER) & (S_Group == 'EXPOSED' | S_Group == 'UNMATCHED'),][,month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),'-',sprintf("%04d",year(FIRST_PFIZER)))]
  M_Studycohort_Covariates_D3 <- M_Studycohort_Covariates_D3[Three_Dose_Population[,.(person_id,id)], on= c('person_id','id')]
  Three_Dose_Population_subject_list <- M_Studycohort_Covariates_D3$person_id
  t4_third <- table4_content2(Three_Dose_Population_subject_list, Pfizer_Table4_metadata,M_Studycohort_Covariates_D3,AESIS_list_D3, referenceExtension = '_D3')
  #N_total3 <- length(Third_Dose_Population_subject_list)
  #t4_third <- as.data.table(c(N_total3,t4_third))
  setnames(t4_third, c("N3","PER3"))
  #t4_third <- t4_third[,'PER3' := N3/N_total3]
  rm(M_Studycohort_Covariates_D3,AESIS_list_D3,Three_Dose_Population,Three_Dose_Population_subject_list)
  
  t4 <- cbind(t4_first,t4_third)
  table4 <- cbind(labels,t4)
  table4[ table4 == -5] <- '<5'
  fwrite(table4, file = paste0(output_dir,DAP,'_Table4.csv'))
  
}
Main()

