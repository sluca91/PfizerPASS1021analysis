#Author: Albert Cid Royo MSc.
#email: a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/01/2022

##Aim
#Generate Table 3

##in/output
#Input 1: 

#Output 1:

library(data.table)



table3_11_content <- function(data,name,name_extension = NULL, t0_name){
  Distribution_age <- summary(data$AGE_T0)
  
  #In case we don't have all the population we still need to generate the same amount of bands
  #THis is a automatic way to do it.
  # Create the fixed bands (code above) and then merge with the ones we calculate from the data
  
  bands1 <- as.data.table(CreateBands(c(0,2,5,12,16,18,30,40,50,60,65,70,80), NEWBORNS = F))
  bands2 <- as.data.table(CreateBands(c(80,140), NEWBORNS = F))[band0 == "080-139", band0 := "80+"]
  bands <- rbind(bands1, bands2)[,.(INT, band0)]
  
  uniqueBands <- data.frame('band' = unique(bands$band))
  countBand <- data[, .N, by = band][order(band)]
  results<-merge(x=uniqueBands,y=countBand,by='band',all.x=TRUE)
  listCountBand <- results$N
  listCountBand[is.na(listCountBand)] <- 0
  names(listCountBand) <- results$band
  
  # #Count Per Region band
  countRegion <- data[, .N, by = eval(paste0('L_GEOREGION_COV',t0_name))][order(get(paste0('L_GEOREGION_COV',t0_name)))]
  
  
  if(paste0('L_GEOREGION_COV',t0_name) %in% names(data)){
    countRegion <- data[, .N, by = eval(paste0('L_GEOREGION_COV',t0_name))][order(get(paste0('L_GEOREGION_COV',t0_name)))]
    dictionaryCategoricalVars_reg <- as.data.table(readRDS(paste0(tmp,'Dictionary_result.rds')))[VarName %in% 'L_GEOREGION_COV']
    missingCase_reg <- as.data.table(list('integerVal' = 0, 'oriVal' = 'Unknown', 'VarName' = 'L_GEOREGION_COV'))
    dictionaryCategoricalVars_reg <- rbindlist(list(dictionaryCategoricalVars_reg,missingCase_reg),fill = TRUE)
    listCountRegion <- countRegion$N
    if (nrow(dictionaryCategoricalVars_reg) > 1){
      countRegion <- merge(countRegion,dictionaryCategoricalVars_reg, by.x = paste0('L_GEOREGION_COV',t0_name), by.y = 'integerVal')
      names(listCountRegion) <- countRegion$oriVal
    }else{
      names(listCountRegion) <- countRegion[,get(paste0('L_GEOREGION_COV',t0_name))]
    }
  } else{
    listCountRegion <- NA
    names(listCountRegion) <- 'empty'
  } 
  
  
  # if(paste0('L_SOCIOECO_COV',t0_name) %in% names(data)){
  #   countSocio <- data[, .N, by = eval(paste0('L_SOCIOECO_COV',t0_name))][order(get(paste0('L_SOCIOECO_COV',t0_name)))]
  #   dictionaryCategoricalVars <- as.data.table(readRDS(paste0(tmp,'Dictionary_result.rds')))[VarName %in% 'L_SOCIOECO_COV']
  #   missingCase <- as.data.table(list('integerVal' = 0, 'oriVal' = 'Unknown', 'VarName' = 'L_SOCIOECO_COV'))
  #   dictionaryCategoricalVars <- rbindlist(list(dictionaryCategoricalVars,missingCase),fill = TRUE)
  #   listCountSocio <- countSocio$N
  #   if (nrow(dictionaryCategoricalVars) > 1){
  #     countSocio <- merge(countSocio,dictionaryCategoricalVars, by.x = paste0('L_SOCIOECO_COV',t0_name), by.y = 'integerVal')
  #     names(listCountSocio) <- countSocio$oriVal
  #   }else{
  #     names(listCountSocio) <- countSocio[,get(paste0('L_SOCIOECO_COV',t0_name))]
  #   }
  # } else{
  #   listCountSocio <- NA
  #   names(listCountSocio) <- 'empty'
  # } 
  
  #concept_to_numeric <- colnames(data)[!colnames(data) %in% c("person_id", "band")]
  concept_to_numeric <- c('H_HOSPNUM_COV','H_EMERG_COV','H_NURSING_COV','H_NURSING_COV','H_PRIMARYHCUSE_COV','H_PREVENTIVE_COV','L_BMICALC_COV_T0','TP_CANCERSCREEN_COV')
  concept_to_numeric <- paste0(concept_to_numeric,t0_name)
  lapply(concept_to_numeric, function(x) if(x %in% names(data)) data[,eval(x) := as.numeric(get(x))])
  possible_preg_pop <- data[L_SEX_COV == 1 & AGE_T0 %between% list(15,50),.N]
  
  col1 = as.list(c('N' = data[,.N],
                   'n', #AGE
                   '',
                   'age_mean' = format(round(Distribution_age[[4]], 2)),
                   'age_median' = format(round(Distribution_age[[3]], 2)),
                   '',#AGE BANDS
                   listCountBand,
                   'female' = data[L_SEX_COV == 1,.N], #FEMALES
                   'possible_preg' = possible_preg_pop, 
                   'Pregnancy_status_n' =  if(paste0('L_PREGNSTATUS_COV',t0_name) %in% names(data)) data[get(paste0('L_PREGNSTATUS_COV',t0_name)) >= 1,.N] else NA,
                   'First_trimester' =  if(paste0('L_PREGTIMETO_DES',t0_name) %in% names(data)) data[get(paste0('L_PREGTIMETO_DES',t0_name)) == 1,.N] else NA, 
                   'Second_trimester' = if(paste0('L_PREGTIMETO_DES',t0_name) %in% names(data)) data[get(paste0('L_PREGTIMETO_DES',t0_name)) == 2,.N] else NA, 
                   'Third_trimester' = if(paste0('L_PREGTIMETO_DES',t0_name) %in% names(data)) data[get(paste0('L_PREGTIMETO_DES',t0_name)) == 3,.N] else NA,
                   '',
                   # listCountSocio,
                   # '',#Geograohic region
                   listCountRegion,
                   'residency' = if(paste0('L_LCF_COV',t0_name) %in% names(data)) data[is.na(get(paste0('L_LCF_COV',t0_name))),.N] else NA,#'Residency in a long-term care facility, n (%)',
                   #'healthcareWorker' = if(paste0('H_HCW_COV',t0_name) %in% names(data)) data[is.na(get(paste0('H_HCW_COV',t0_name))),.N] else NA,#'Healthcare worker or essential worker status, n (%)',
                   '', #Date of vaccination, n (%) 
                   '4_2020' = data[get(paste0('quarter',t0_name)) == 4 & (get(paste0('year',t0_name)) %in% '2020' | get(paste0('year',t0_name)) == 2020),.N],
                   '1_2021' = data[get(paste0('quarter',t0_name)) == 1 & (get(paste0('year',t0_name)) %in% '2021' | get(paste0('year',t0_name)) == 2021),.N],
                   '2_2021' = data[get(paste0('quarter',t0_name)) == 2 & (get(paste0('year',t0_name)) %in% '2021' | get(paste0('year',t0_name)) == 2021),.N],
                   '3_2021' = data[get(paste0('quarter',t0_name)) == 3 & (get(paste0('year',t0_name)) %in% '2021' | get(paste0('year',t0_name)) == 2021),.N],
                   '4_2021' = data[get(paste0('quarter',t0_name)) == 4 & (get(paste0('year',t0_name)) %in% '2021' | get(paste0('year',t0_name)) == 2021),.N],
                   '1_2022' = data[get(paste0('quarter',t0_name)) == 1 & (get(paste0('year',t0_name)) %in% '2022' | get(paste0('year',t0_name)) == 2022),.N],
                   '2_2022' = data[get(paste0('quarter',t0_name)) == 2 & (get(paste0('year',t0_name)) %in% '2022' | get(paste0('year',t0_name)) == 2022),.N],
                   '3_2022' = data[get(paste0('quarter',t0_name)) == 3 & (get(paste0('year',t0_name)) %in% '2022' | get(paste0('year',t0_name)) == 2022),.N],
                   '', #Personal lifestyle characteristics
                   '', #SMOKE_STATUS
                   'Current' = if(paste0('L_SMOKESTATUSALG_COV',t0_name) %in% names(data)) data[get(paste0('L_SMOKESTATUSALG_COV',t0_name)) == 3,.N] else NA,
                   'Former' = if(paste0('L_SMOKESTATUSALG_COV',t0_name) %in% names(data)) data[get(paste0('L_SMOKESTATUSALG_COV',t0_name)) == 2,.N] else NA,
                   'Never'= if(paste0('L_SMOKESTATUSALG_COV',t0_name) %in% names(data)) data[get(paste0('L_SMOKESTATUSALG_COV',t0_name)) == 1,.N] else NA,
                   'Never_or_Former' =  if(paste0('L_SMOKESTATUSALG_COV',t0_name) %in% names(data)) data[get(paste0('L_SMOKESTATUSALG_COV',t0_name)) == 5,.N] else NA,
                   'Unknown' =  if(paste0('L_SMOKESTATUSALG_COV',t0_name) %in% names(data)) data[get(paste0('L_SMOKESTATUSALG_COV',t0_name)) == 0 | get(paste0('L_SMOKESTATUSALG_COV',t0_name)) == 4,.N] else NA,
                   '', #BMI
                   'underBMI' = if(paste0('L_BMICALC_COV',t0_name) %in% names(data)) data[get(paste0('L_BMICALC_COV',t0_name)) == 1,.N] else NA,#'Underweight',
                   'normalBMI' = if(paste0('L_BMICALC_COV',t0_name) %in% names(data)) data[get(paste0('L_BMICALC_COV',t0_name)) == 2,.N] else NA,#'Normal ',
                   'overBMI' = if(paste0('L_BMICALC_COV',t0_name) %in% names(data)) data[get(paste0('L_BMICALC_COV',t0_name)) == 3,.N] else NA,#'Overweight',
                   'obseBMI' = if(paste0('L_BMICALC_COV',t0_name) %in% names(data)) data[get(paste0('L_BMICALC_COV',t0_name)) == 4,.N] else NA,#'Obese',
                   'missinBMI' = if(paste0('L_BMICALC_COV',t0_name) %in% names(data)) data[get(paste0('L_BMICALC_COV',t0_name)) == 0,.N] else NA,#'BMI_missing',
                   'obeseDiagnosis' = if(paste0('L_OBESITY_COV',t0_name) %in% names(data)) data[get(paste0('L_OBESITY_COV',t0_name)) == 1 ,.N] else NA,#'Obesity diagnosis or obesity surgery',
                   '' ,#'Healthcare utilisation',
                   '',#'Number of hospitalisations, n (%)',
                   'hosp0' = if(paste0('H_HOSPNUM_COV',t0_name) %in% names(data)) data[get(paste0('H_HOSPNUM_COV',t0_name)) == 0,.N] else NA,#0',
                   'hosp1' = if(paste0('H_HOSPNUM_COV',t0_name) %in% names(data)) data[get(paste0('H_HOSPNUM_COV',t0_name)) == 1,.N] else NA,#'1',
                   'hosp2+' = if(paste0('H_HOSPNUM_COV',t0_name) %in% names(data)) data[get(paste0('H_HOSPNUM_COV',t0_name)) > 1,.N] else NA,#'2+',
                   '', #Number of emergency department visits, n (%)',
                   'emergency0' = if(paste0('H_EMERG_COV',t0_name) %in% names(data)) data[get(paste0('H_EMERG_COV',t0_name)) == 0,.N] else NA, #'0',
                   'emergency1' = if(paste0('H_EMERG_COV',t0_name) %in% names(data)) data[get(paste0('H_EMERG_COV',t0_name)) == 1,.N] else NA ,#'1',
                   'emergency2+' = if(paste0('H_EMERG_COV',t0_name) %in% names(data)) data[get(paste0('H_EMERG_COV',t0_name)) > 1,.N] else NA, #'2+',
                   '', #'Skilled nursing facility, nursing home, or extended care facility stay, n (%)',
                   'nursing0' = if(paste0('H_NURSING_COV',t0_name) %in% names(data))data[get(paste0('H_NURSING_COV',t0_name)) == 0,.N] else NA, #'0',
                   'nursing1' = if(paste0('H_NURSING_COV',t0_name) %in% names(data)) data[get(paste0('H_NURSING_COV',t0_name)) == 1,.N] else NA, #'1',
                   'nursing2+' = if(paste0('H_NURSING_COV',t0_name) %in% names(data)) data[get(paste0('H_NURSING_COV',t0_name)) >1,.N] else NA, #'2+',
                   '',#'Primary care utilisation, n (%)',
                   'primaryCare0' = if(paste0('H_PRIMARYHCUSE_COV',t0_name) %in% names(data)) data[get(paste0('H_PRIMARYHCUSE_COV',t0_name)) == 0,.N] else NA, #'0',
                   'primaryCare1' = if(paste0('H_PRIMARYHCUSE_COV',t0_name) %in% names(data)) data[get(paste0('H_PRIMARYHCUSE_COV',t0_name)) == 1,.N] else NA, #'1',
                   'primaryCare2+' = if(paste0('H_PRIMARYHCUSE_COV',t0_name) %in% names(data)) data[get(paste0('H_PRIMARYHCUSE_COV',t0_name)) ==2,.N] else NA, #'2+',
                   '',#'Cancer screening, n (%)',
                   'cancer0' = if(paste0('TP_CANCERSCREEN_COV',t0_name) %in% names(data)) data[get(paste0('TP_CANCERSCREEN_COV',t0_name)) == 0,.N] else NA, #'0',
                   'cancer1' = if(paste0('TP_CANCERSCREEN_COV',t0_name) %in% names(data)) data[get(paste0('TP_CANCERSCREEN_COV',t0_name)) == 1,.N] else NA ,#'1',
                   'cancer2+' = if(paste0('TP_CANCERSCREEN_COV',t0_name) %in% names(data)) data[get(paste0('TP_CANCERSCREEN_COV',t0_name)) == 2,.N] else NA, #'2+',
                   'otherPreventives' = if(paste0('H_PREVENTIVE_COV',t0_name) %in% names(data)) data[get(paste0('H_PREVENTIVE_COV',t0_name)) == 1,.N] else NA, #'Other preventive health services, n (%)',
                   '',#COVID19 tests, n (%)',
                   'covidtest0' = if(paste0('TP_COVID19TEST_COV',t0_name) %in% names(data)) data[get(paste0('TP_COVID19TEST_COV',t0_name)) == 0,.N] else NA,#'0',
                   'covidtest1_2' = if(paste0('TP_COVID19TEST_COV',t0_name) %in% names(data)) data[get(paste0('TP_COVID19TEST_COV',t0_name)) == 1,.N] else NA,#'1-2',
                   'covidtest3_4' = if(paste0('TP_COVID19TEST_COV',t0_name) %in% names(data)) data[get(paste0('TP_COVID19TEST_COV',t0_name)) == 2,.N] else NA,#'3-4',
                   'covidtest5' = if(paste0('TP_COVID19TEST_COV',t0_name) %in% names(data)) data[get(paste0('TP_COVID19TEST_COV',t0_name)) == 3,.N] else NA#"5+",
                   #'PriorInfection' = data[I_COVID19DX_COV_T0 == TRUE,.N],
                   #'PriorInfluenzaVac' = data[INF_T0 == TRUE,.N],
  )) 
  
  
  
  col2 = as.list(c('',#n1
                   '%',
                   '',#Demographics
                   'std' = format(round(sd(data$AGE_T0), 2)),
                   'q1_q3' = paste0('(',format(round(Distribution_age[[2]], 2)),',',format(round(Distribution_age[[5]], 2)),')'),
                   '',#age
                   format(round(listCountBand/data[,.N]*100, 2)),
                   'female' =  format(round(data[L_SEX_COV == 1,.N]/data[,.N]*100, 2)),
                   'possible_preg' = format(round(possible_preg_pop/as.numeric(col1$female)*100,2)), 
                   'Per_Preg_N' = format(round(as.numeric(col1['Pregnancy_status_n'])/possible_preg_pop*100, 2)),#Pregstatus
                   'Per_First_trimester' = format(round(as.numeric(col1['First_trimester'])/possible_preg_pop*100, 2)), #TODO need the date start and end of pregnancy
                   'Per_Second_trimester' = format(round(as.numeric(col1['Second_trimester'])/possible_preg_pop*100, 2)),
                   'Per_Third_trimester' = format(round(as.numeric(col1['Third_trimester'])/possible_preg_pop*100, 2)),
                   # '',#SocioEco
                   # format(round(listCountSocio/data[,.N]*100, 2)),
                   '',#Geographic region
                   format(round(listCountRegion/data[,.N]*100, 2)),
                   'perResidency' = format(round(as.numeric(col1$residency)/data[,.N]*100, 2)),
                   'perHealthcare' = format(round(as.numeric(col1$healthcareWorker)/data[,.N]*100, 2)),
                   '',#Date of vacinateion
                   '4_2020per' = format(round(as.numeric(col1['4_2020'])/data[,.N]*100, 2)),
                   '1_2021per' = format(round(as.numeric(col1['1_2021'])/data[,.N]*100, 2)),
                   '2_2021per' = format(round(as.numeric(col1['2_2021'])/data[,.N]*100, 2)),
                   '3_2021per' = format(round(as.numeric(col1['3_2021'])/data[,.N]*100, 2)),
                   '4_2021per' = format(round(as.numeric(col1['4_2021'])/data[,.N]*100, 2)),
                   '1_2022per' = format(round(as.numeric(col1['1_2022'])/data[,.N]*100, 2)),
                   '2_2022per' = format(round(as.numeric(col1['2_2022'])/data[,.N]*100, 2)),
                   '3_2022per' = format(round(as.numeric(col1['3_2022'])/data[,.N]*100, 2)),
                   '',#Personal Lifestyle
                   '',#Smoking Status
                   'Currentper' = format(round(as.numeric(col1['Current'])/data[,.N]*100, 2)),
                   'Formerper' = format(round(as.numeric(col1['Former'])/data[,.N]*100, 2)),
                   'Neverper'= format(round(as.numeric(col1['Never'])/data[,.N]*100, 2)),
                   'PER_Never_or_Former'= format(round(as.numeric(col1['Never_or_Former'])/data[,.N]*100, 2)),
                   'Unknowper' =  format(round(as.numeric(col1['Unknown'])/data[,.N]*100, 2)),
                   '', #BMI
                   'underBMI' = format(round(as.numeric(col1['underBMI'])/data[,.N]*100, 2)) ,#'Underweight',
                   'normalBMI' = format(round(as.numeric(col1['normalBMI'])/data[,.N]*100, 2)),#'Normal ',
                   'overBMI' = format(round(as.numeric(col1['overBMI'])/data[,.N]*100, 2)),#'Overweight',
                   'obseBMI' =format(round(as.numeric(col1['obseBMI'])/data[,.N]*100, 2)),#'Obese',
                   'missinBMI' = format(round(as.numeric(col1['missinBMI'])/data[,.N]*100, 2)),#'BMI_missing',
                   'obeseDiagnosis' = format(round(as.numeric(col1['obeseDiagnosis'])/data[,.N]*100, 2)),#'Obesity diagnosis or obesity surgery',
                   '' ,#'Healthcare utilisation',
                   '',#'Number of hospitalisations, n (%)',
                   'hosp0' = format(round(as.numeric(col1['hosp0'])/data[,.N]*100, 2)),#0',
                   'hosp1' = format(round(as.numeric(col1['hosp1'])/data[,.N]*100, 2)),#'1',
                   'hosp2+' = format(round(as.numeric(col1['hosp2+'])/data[,.N]*100, 2)),#'2+',
                   '', #Number of emergency department visits, n (%)',
                   'emergency0' = format(round(as.numeric(col1['emergency0'])/data[,.N]*100, 2)),#'0',
                   'emergency1' = format(round(as.numeric(col1['emergency1'])/data[,.N]*100, 2)),#'1',
                   'emergency2+' = format(round(as.numeric(col1['emergency2+'])/data[,.N]*100, 2)),#'2+',
                   '', #'Skilled nursing facility, nursing home, or extended care facility stay, n ',
                   'nursing0' = format(round(as.numeric(col1['nursing0'])/data[,.N]*100, 2)),#'0',
                   'nursing1' = format(round(as.numeric(col1['nursing1'])/data[,.N]*100, 2)),#'1',
                   'nursing2+' = format(round(as.numeric(col1['nursing2+'])/data[,.N]*100, 2)),#'2+',
                   '',#'Primary care utilisation, n (%)',
                   'primaryCare0' = format(round(as.numeric(col1['primaryCare0'])/data[,.N]*100, 2)),#'0',
                   'primaryCare1' = format(round(as.numeric(col1['primaryCare1'])/data[,.N]*100, 2)),#'1',
                   'primaryCare2+' = format(round(as.numeric(col1['primaryCare2+'])/data[,.N]*100, 2)),#'2+',
                   '',#'Cancer screening, n (%)',
                   'cancer0' = format(round(as.numeric(col1['cancer0'])/data[,.N]*100, 2)),#'0',
                   'cancer1' = format(round(as.numeric(col1['cancer1'])/data[,.N]*100, 2)),#'1',
                   'cancer2+' = format(round(as.numeric(col1['cancer2+'])/data[,.N]*100, 2)),#'2+',
                   'otherPreventives' = format(round(as.numeric(col1['otherPreventives'])/data[,.N]*100, 2)),#'Other preventive health services, n ',
                   '',#COVID19 tests, n (%)',
                   'covidtest0' = format(round(as.numeric(col1['covidtest0'])/data[,.N]*100, 2)),#'0',
                   'covidtest1_2' = format(round(as.numeric(col1['covidtest1_2'])/data[,.N]*100, 2)),#'1-2',
                   'covidtest3_4' = format(round(as.numeric(col1['covidtest3_4'])/data[,.N]*100, 2)),#'3-4',
                   'covidtest5' = format(round(as.numeric(col1['covidtest5'])/data[,.N]*100, 2))
  )#"5+",
  
  )
  
  template_table3_11 =  c('Total, N (%)',
                          'Demographics',
                          'Age (years)',
                          'Mean (SD)',
                          'Median (Q1, Q3)',
                          'Age groups (years), n (%)',
                          '0-1',
                          '2-4',  
                          '5-11',
                          '12-15',
                          '16-17',
                          '18-29',
                          '30-39',
                          '40-49',
                          '50-59',
                          '60-64',
                          '65-69',
                          '70-79',
                          '80+',
                          'Female, n (%)',
                          'Females aged 14 to 50 years, n (%)',
                          'Pregnancy status, n (%)',
                          'First trimester',
                          'Second trimester',
                          'Third trimester',
                          # 'Socio economic status, n (%)',
                          # names(listCountSocio),
                          'Geographic region, n (%)',
                          names(listCountRegion),
                          'Residency in a long-term care facility, n (%)',
                          #'Healthcare worker or essential worker status, n (%)',
                          'Date of vaccination (year or month), n (%)' ,
                          '   1 Oct–31 Dec 2020',
                          '   1 Jan–31 Mar 20211st quarter 2021',
                          '   1 Apr–30 Jun 2021',
                          '   1 Jul 2021–30 Sep 2021',
                          '   1 Oct 2021–31 Dec 2021',
                          '   1 Jan 2022–31 Mar 2022',
                          '   1 Apr–30 Jun 2022',
                          '   1 Jul 2022–30 Sep 2022',
                          'Personal lifestyle characteristics',
                          'Smoking status, n (%)',
                          '  Current',
                          '  Former',
                          '  Never',
                          '  Never or Former',
                          '  Unknown',
                          'BMI',
                          '  Underweight (BMI <20kg/m2)',
                          '  Normal weight (BMI 20 to < 25kg/m2)',
                          '  Overweight (BMI 25 to < 30kg/m2)',
                          '  Obese (BMI ≥ 30kg/m2)',
                          '  BMI missing',
                          '  Obesity diagnosis or obesity surgery',
                          'Healthcare utilisation',
                          '  Number of hospitalisations, n (%)',
                          '     0',
                          '     1',
                          '     2+',
                          '  Number of emergency department visits, n (%)',
                          '     0',
                          '     1',
                          '     2+',
                          '  Skilled nursing facility, nursing home, or extended care facility stay, n (%)',
                          '     0',
                          '     1',
                          '     2+',
                          '  Primary care utilisation, n (%)',
                          '    0',
                          '    1',
                          '    2+',
                          '  Cancer screening, n (%)',
                          '    0',
                          '    1',
                          '    2+',
                          '  Other preventive health services, n (%)',
                          'COVID-19 tests, n (%)',
                          '    0',
                          '    1-2',
                          '    3-4',
                          "    5+"
  )
  
  maskingVector <- c(rep(0,6),rep(1,length(col1)-6))
  
  #Masking values under between 1 to 4
  col1_num <- as.numeric(col1)
  col1[!is.na(col1_num) & maskingVector == 1 & col1_num %between% list(1,4) ] <- '<5'
  col2[!is.na(col1_num) & maskingVector == 1 & col1_num %between% list(1,4) ] <- 'NA'
  table <- data.table(cbind(col1,col2))
  
  setnames(table, c("N", "PER"))
  table <- cbind(setnames(as.data.table(template_table3_11),'LABEL'),table)
  
  if (is.null(name_extension)){
    fwrite(table, file = paste0(output_dir,DAP,'_',name,'.csv'))
    if(length(listCountRegion) != 0) {
      fwrite(as.data.frame(names(listCountRegion)), file = paste0(output_dir,DAP,'_',name,'_namesRegions.csv'))
    }
  }else{
    fwrite(table, file = paste0(output_dir,DAP,'_',name,'_', name_extension,'.csv'))
    if(length(listCountRegion) != 0) {
      fwrite(as.data.frame(names(listCountRegion)), file = paste0(output_dir,DAP,'_',name,'_', name_extension,'_namesRegions.csv'))
    }
  }
  
  return(table)
}

smd <- function(CovariatesInformation,metadata,referenceExtension){
  #Version of the table4_conten for the interim 2
  
  lapply(names(CovariatesInformation),function(x) 
    if(! ( is.integer(CovariatesInformation[,get(x)]) == TRUE | is.numeric(CovariatesInformation[,get(x)]) == TRUE) ){
      CovariatesInformation[,eval(x) := as.factor(get(x))]
    }
  )
  
  CovariatesInformation <- as.data.frame(CovariatesInformation)
  col1 <- as.numeric()
  i <- 0
  for (var in metadata$var){
    i <- i +1
    if (!is.na(var) & !is.null(var)){
      if (var %in% names(CovariatesInformation)){
        indexGroup <- grep("group", colnames(CovariatesInformation))
        indexCol <-  grep(var, colnames(CovariatesInformation))
        if (dim(table(CovariatesInformation[,indexCol])) > 1){
          if (metadata[i]$type == "NUM"){
            col1 <- c(col1, abs(CovariatesInformation,indexGroup,indexCol,met = metadata[i]$type)[1,7])
          }else{
            col1 <- c(col1, abs(CovariatesInformation,indexGroup,indexCol,met = metadata[i]$type)[1,5])
          }
          
        }else{
          col1 <- c(col1,
                    0)
        }
      }else{
        print(paste0(var,' not found either in M_studyCohort_Covariats'))
        col1 <- c(col1,
                  NA)
      }
    }else{
      col1 <- c(col1,
                NA)
    }
  }
  return(col1)
}


if(!require("stddiff")){install.packages("stddiff")}
suppressPackageStartupMessages(library("stddiff"))

abs<-function(indf,y,x,met="num"){
  if (met=="NUM") {outdf<-stddiff.numeric(data=indf,gcol=y,vcol=x)}
  if (met=="BIN") {outdf<-stddiff.binary(data=indf,gcol=y,vcol=x)}
  if (met=="CAT") {outdf<-stddiff.category(data=indf,gcol=y,vcol=x)}
  return(outdf)
}
# 
# library('stddiff')
# 
# {outdf<-stddiff.binary(data=indf,gcol=y,vcol=x)}
# 
# calculateSMD <- function(){
# 
#   
# }


Main1 <- function(){
  
  #Reading pregnancy
  #preg_info <- readRDS(paste0(populations_dir,"DESCRIPTIVE_VARS.rds"))
  
  M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,'M_Studycohort.rds')))[, band := NULL]
  M_Studycohort_Covariates_T0 <-  as.data.table(readRDS(paste0(populations_dir,'M_Studycohort_Covariates_T0.rds')))
  M_Studycohort_Covariates_D3 <-  as.data.table(readRDS(paste0(populations_dir,'M_Studycohort_Covariates_D3.rds')))
  
  id <- c("person_id", "id")
  names_t0 <- str_remove(colnames(M_Studycohort_Covariates_T0),'_T0')
  names_D3 <- str_remove(colnames(M_Studycohort_Covariates_D3),'_D3')
  load(paste0(dir_base,"/Data characterisation/PfizerScript/g_intermediate/tmp/parameters.RData"))
  
  missingAvailable_T0 <- Available_cov[!Available_cov %in% names_t0]
  invisible(lapply(missingAvailable_T0, function(x) M_Studycohort_Covariates_T0[,eval(x) := 0]))

  missingAvailable_D3 <- Available_cov[!Available_cov %in% names_D3]
  invisible(lapply(missingAvailable_D3, function(x) M_Studycohort_Covariates_D3[,eval(x) := 0]))

  #Count Per Age band
  bands1 <- as.data.table(CreateBands(c(0,2,5,12,16,18,30,40,50,60,65,70,80), NEWBORNS = F))
  bands2 <- as.data.table(CreateBands(c(80,140), NEWBORNS = F))[band0 == "080-139", band0 := "80+"]
  bands <- rbind(bands1, bands2)[,.(INT, band0)]
  setnames(bands, "band0", "band")
  M_Studycohort <- merge(M_Studycohort, bands, by.x="AGE_T0", by.y="INT")
  
  descriptive_var_path <- paste0(populations_dir,"DESCRIPTIVE_VARS.rds")
  if(file.exists(descriptive_var_path)){
    pregInfo <- as.data.table(readRDS(paste0(populations_dir,"DESCRIPTIVE_VARS.rds")))
   
    pregInfo <- pregInfo[Result < 93 &  Result >= 0 , Result_trim := 1]
    pregInfo <- pregInfo[Result >= 93 & Result < 186 , Result_trim := 2]
    pregInfo <- pregInfo[Result >= 186, Result_trim := 3]
    pregInfo <- pregInfo[Result > 280 , Result_trim := NA]
    pregInfo2 <- data.table::dcast(pregInfo, person_id + id ~ col, value.var = 'Result_trim')
    M_Studycohort_Covariates_T0 <- merge(M_Studycohort_Covariates_T0,pregInfo2, by = c('person_id','id'), all.x = T)
    M_Studycohort_Covariates_D3 <- merge(M_Studycohort_Covariates_D3,pregInfo2, by = c('person_id','id'), all.x = T)
    
    
  }
  
  load(paste0(dir_base,"/Data characterisation/PfizerScript/g_intermediate/tmp/parameters.RData"))
 
  
  
  #First dose calculation Table 3
  First_Dose_Population <- M_Studycohort[!is.na(FIRST_PFIZER) & (group == 'EXPOSED'| group == 'UNMATCHED'),][,year_T0 := sprintf("%04d",year(FIRST_PFIZER))][,quarter_T0 := quarter(as.Date(FIRST_PFIZER))]#[,month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),'-',sprintf("%04d",year(FIRST_PFIZER)))]
  First_Dose_Population_Covariate_T0 <- M_Studycohort_Covariates_T0[First_Dose_Population[,c("person_id", "id","band","year_T0","quarter_T0","AGE_T0")], on = c("person_id", "id")]
  table3_11_content(First_Dose_Population_Covariate_T0,'Table3','FirstDose', t0_name = '_T0')
  
  #Third dose calculation Table 3
  Three_Dose_Population <- M_Studycohort[!is.na(FIRST_PFIZER) & !is.na(SECOND_PFIZER) & !is.na(THIRD_PFIZER) & (group == 'EXPOSED' | group == 'UNMATCHED'),][,year_D3 := sprintf("%04d",year(THIRD_PFIZER))][,quarter_D3 := quarter(as.Date(THIRD_PFIZER))]#[,month_t2 := paste0(sprintf("%02d",month(THIRD_PFIZER)),'-',sprintf("%04d",year(THIRD_PFIZER)))]
  Three_Dose_Population_Covariate_D3 <- M_Studycohort_Covariates_D3[Three_Dose_Population[,c("person_id", "id","band","year_D3","quarter_D3","AGE_T0")], on = c("person_id", "id")]
  table3_11_content(Three_Dose_Population_Covariate_D3,'Table3','ThirdDose', t0_name = '_D3')
  
  #fwrite(paste0(output_dir,DAP,'_',name,'_', name_extension,'_namesRegions.csv'))
  # frstDoseTable <- fwrite(paste0(output_dir,DAP,'_',name,'_', name_extension,'_namesRegions.csv'))
  # fread()
  
  M_Studycohort2 <- copy(M_Studycohort)
  M_Studycohort_Covariates_T0_2 <- copy(M_Studycohort_Covariates_T0)
  
  # Exposed Matched Group Table 11
  Vaccinated <- M_Studycohort2[group == 'EXPOSED'][,year_T0 := sprintf("%04d",year(FIRST_PFIZER))][,quarter_T0 := quarter(as.Date(FIRST_PFIZER))]
  Vaccinated_Population_Covariate_T0 <- M_Studycohort_Covariates_T0_2[Vaccinated[,c("person_id", "id","band","year_T0","quarter_T0","AGE_T0")], on = c("person_id", "id")]
  table11_vaccinated <- table3_11_content(Vaccinated_Population_Covariate_T0,'Table11','Vaccinated', t0_name = '_T0')
  
  # Control Matched Group Table 11
  UnVaccinated <- M_Studycohort2[group == 'CONTROL',][,year_T0 := sprintf("%04d",year(T0))][,quarter_T0 := quarter(as.Date(T0))]
  UnVaccinated_Population_Covariate_T0 <- M_Studycohort_Covariates_T0_2[UnVaccinated[,c("person_id", "id","band","year_T0","quarter_T0","AGE_T0")], on = c("person_id", "id")]
  table11_unvaccinated <-table3_11_content(UnVaccinated_Population_Covariate_T0,'Table11','Unvaccinated', t0_name = '_T0')
  
  
  Pfizer_Table11_metadata <- fread(paste0(meta_dir,'/Pfizer_Table11_metadata.csv'))
  numberRegions <- length(unique(UnVaccinated_Population_Covariate_T0$L_GEOREGION_COV_T0))
  #numberSocioEco <- length(unique(UnVaccinated_Population_Covariate_T0$L_SOCIOECO_COV_T0))
  Pfizer_Table11_metadata <- rbind(Pfizer_Table11_metadata[seq(25)],
             #cbind(rep(NA,numberSocioEco),rep(NA,numberSocioEco),rep(NA,numberSocioEco)),
             cbind(rep(NA,numberRegions),rep(NA,numberRegions),rep(NA,numberRegions)),
             Pfizer_Table11_metadata[seq(from = 26, to = nrow(Pfizer_Table11_metadata))], use.names = FALSE)
  
  Matched <- M_Studycohort[group == 'EXPOSED' | group == 'CONTROL'][,quarter_year_T0 := paste0(quarter(as.Date(FIRST_PFIZER)),'_',sprintf("%04d",year(FIRST_PFIZER)))]
  Matched_Population_Covariate_T0 <- M_Studycohort_Covariates_T0[Matched[,c("person_id", "id","band","quarter_year_T0","AGE_T0","group")], on = c("person_id", "id")]
  smd_col <- smd(Matched_Population_Covariate_T0,Pfizer_Table11_metadata,'')
  
  
  
  smd_col <- as.data.table(smd_col)
  table11 <- cbind(table11_vaccinated,table11_unvaccinated[,-1],smd_col)
  
  fwrite(as.data.table(table11), file = paste0(output_dir,DAP,'_Table11.csv'))
  
  
}
Main1()


# 
# ###Third DOSE POPULATION  
# 
# 
# countBand <- Three_Dose_Population[, .N, by = band][order(band)]
# 
# results<-merge(x=uniqueBands,y=countBand,by='band',all.x=TRUE)
# 
# 
# listCountBand <- results$N.y
# names(listCountBand) <- results$band
# 
# 
# # #Count Per Region band
# countRegion <- Three_Dose_Population[, .N, by = REGION][order(REGION)]
# listCountRegion <- countRegion$N
# names(listCountRegion) <- countRegion$REGION
# 
# # Count Per Month band
# countMonth <- Three_Dose_Population[, .N, by = month_t0][order(month_t0)]
# listCountMonth <- countMonth$N
# names(listCountMonth) <- countMonth$month_t0
# 
# 
# col1 = as.list(c('N' = Three_Dose_Population[,.N],
#                  '',
#                  '',
#                  'age_mean' = Distribution3_age[[4]],
#                  'age_median' = Distribution3_age[[3]],
#                  'N',
#                  listCountBand,
#                  'female' = Three_Dose_Population[sex_at_instance_creation == 'F',.N],
#                  '',
#                  listCountRegion,
#                  '',
#                  listCountMonth,
#                  'PriorInfection' = Three_Dose_Population[FIRST_COV_INF == TRUE,.N],
#                  'PriorInfluenzaVac' = Three_Dose_Population[INFP5 == TRUE,.N]
# )) 
# col2 = as.list(c('N' = Three_Dose_Population[,.N],
#                  '','',
#                  'std' = sd(Three_Dose_Population$AGE_T0),
#                  'q1_q3' = paste0('(',Distribution3_age[[2]],',',Distribution_age[[5]],')'),
#                  '%',
#                  listCountBand/Three_Dose_Population[,.N],
#                  'female' = Three_Dose_Population[sex_at_instance_creation == 'F',.N]/Three_Dose_Population[,.N],
#                  '',
#                  listCountRegion/Three_Dose_Population[,.N],
#                  '',
#                  listCountMonth/Three_Dose_Population[,.N],
#                  'PriorInfection_per' = Three_Dose_Population[FIRST_COV_INF == TRUE,.N]/Three_Dose_Population[,.N],
#                  'PriorInfluenzaVac_per' = Three_Dose_Population[INFP5 == TRUE,.N]/Three_Dose_Population[,.N]
# ))




# 
# table3 <- data.table(cbind(col1,col2))
# 
# 
# 
# fwrite(table3, file = paste0(output_dir,DAP,'_Table3_ThirdDose.csv'))



#filledTemplate3 <- Add_Results_To_Template(template_table3,table3)
# N <- length(col2)+1  # total number of rows to preallocate--possibly an overestimate
# template_table3 <- data.frame(HEADER=rep('', N), col1=rep('', N),col2=rep('', N),  # as many cols as you need
#                               stringsAsFactors=FALSE)
# template_table3$HEADER =  c( '',
#                              'Baseline Characteristics',
#                              'Demographics',
#                              'Age (years)',
#                              'Mean (SD)',
#                              'Median (Q1, Q3)',
#                              'Age groups (years), n (%)',
#                              '0-5 ',
#                              '6-11',
#                              '12-17 ',
#                              '18-29 ',
#                              '30-39',
#                              '40-49',
#                              '50-59',
#                              '60-69',
#                              '70-79',
#                              '80+',
#                              'Female, n (%)',
#                              'Geographic region, n (%)',
#                              names(listCountRegion),
#                              'Date of vaccination (year or month), n (%)' ,
#                              names(listCountMonth),
#                              'Prior COVID‑19 infection, n (%)',
#                              'Prior Influenza vaccination, n (%)'
# )
# 
# template_table3[1,c(2:3)] <- c('First dose')
# N <- length(col2)+1  # total number of rows to preallocate--possibly an overestimate
# template_table3 <- data.frame(HEADER=rep('', N), col1=rep('', N),col2=rep('', N),  # as many cols as you need
#                               stringsAsFactors=FALSE)
# template_table3$HEADER =  c( '',
#                              'Baseline Characteristics',
#                              'Demographics',
#                              'Age (years)',
#                              'Mean (SD)',
#                              'Median (Q1, Q3)',
#                              'Age groups (years), n (%)',
#                              '0-5 ',
#                              '6-11',
#                              '12-17 ',
#                              '18-29 ',
#                              '30-39',
#                              '40-49',
#                              '50-59',
#                              '60-69',
#                              '70-79',
#                              '80+',
#                              'Female, n (%)',
#                              'Geographic region, n (%)',
#                              names(listCountRegion),
#                              'Date of vaccination (year or month), n (%)' ,
#                              names(listCountMonth),
#                              'Prior COVID‑19 infection, n (%)',
#                              'Prior Influenza vaccination, n (%)'
# )
# 
# template_table3[1,c(2:3)] <- c('Third dose')



