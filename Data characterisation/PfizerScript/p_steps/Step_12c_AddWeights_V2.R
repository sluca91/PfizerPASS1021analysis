#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/01/2023

##Aim
#Calculate Weights (IPW) that can be used to correct in the statistical methods later on. The script also monitors the choices for covariate selection that are the input for table
#18 and 19 later on.

##in/output
#Input 1: M_Studycohort_Covariates_T0.rds
#Input 1: M_Studycohort.rds
#Output 1: weights.rds
#Output 2: weights_distribution.rds 
#Output 3: OR_PS.rds
#output 4: IPW_covariates.rds


#Needed functions to merge named lists to base file. This is done throughout the whole script by statistical functions within a sapply returning a named list
###
MergeToNamedList <- function(named.list, file = overviewFile, by = "var", c.name = NULL){
  
  if(is.null(c.name)) c.name <- "col"
  
  tmp <- merge(
    x = file,
    y = as.data.table(cbind(var = names(named.list), col =  named.list)),
    by = by,
    all.x = T
  )
  
  setnames(tmp, "col", c.name)
  return(tmp)
}

###

#Function for calculating prevalence and OR for binary and categorical. If more then 3 categories all combiations are given back
###
GetORPrev <- function(file, c.group, var ){
  
  file <- copy(file)[, c("group", var ), with = F]
  #JR Version - I updated the funtion to allow numeric variables. 
  if (!var %in% finally_num){levels <- levels(factor(file[[var]]))
  
  #if(length(levels) > 2){
  for(j in 1:(length(levels) - 1)){
    fileRelevel <- copy(file)[,eval(var) := as.factor(get(var)) ]
    fileRelevel[[var]] <- relevel(fileRelevel[[var]], ref = levels[j])
    resultsTmp <- glm(f.build("group", var), data = fileRelevel[,eval(var) := get(var) ],  family = binomial(link=logit))
    
    orValues <- exp(coef(resultsTmp))
    orValues[1] <- NA
    orFactors <-unname(unlist(resultsTmp$xlevels))
    resultsTmp <- as.data.table(cbind("OR" = orValues, "factor" = orFactors), keep.rownames = T)[, ref := levels[j]][!is.na(OR),]
    
    
    if(j == 1) result <-  resultsTmp
    if(j > 1) result <- rbind(result,  resultsTmp)
    rm(resultsTmp, orValues, orFactors)
    
  }

  
  prevResults <- table(file[[var]])/length(file[[var]]) * 100
  result <- merge(
    x = result,
    y = as.data.table(cbind(factor = names(prevResults), prevelence =   prevResults)),
    by = "factor",
    all.x = T
  )[, variable := var][, OR := as.numeric(OR)][, prevelence := as.numeric(prevelence)]}else{
    fileNUM <- copy(file)[,eval(var) := as.numeric(get(var)) ]
    resultsTmp <- glm(f.build("group", var), data = fileNUM[,eval(var) := get(var) ],  family = binomial(link=logit))
    orValues <- exp(coef(resultsTmp))
    orValues[1] <- NA
    result <- as.data.table(cbind("OR" = orValues, "factor" = "1"), keep.rownames = T)[, ref := "99",]
    prevResults <- 99
    result <- merge(
      x = result,
      y = as.data.table(cbind(factor = "1", prevelence =   prevResults)),
      by = "factor",
      all.x = T
    )[, variable := var][, OR := as.numeric(OR)][, prevelence := as.numeric(prevelence)][rn!='(Intercept)']
  }
  
  

  
  return(result)
  
}
###


##########################################
#
#Load variables needed for this script
#
##########################################

cohort <- "T0"
weightingMethod <- "ATE" #Define ATT or ATE
stabalizeWeights <- T #T/F if T weight is multiplied with 0.5
smdLB <- 0.1 #set to 0.1 if the standardize mean difference needs to be above 0.1 for inclusion in the PS model
missingCutOf <- 30  #Variables wit a missingCutOf% of missing values has to be excluded from the PS calculation
###

#Get needed columns in 1 file and set exposure to integer. 
###

#JR Version - I deleted band and added AGE_T0, because we decided to force the lineal and quadratic effect of AGE_T0
#             instead of band. 

fileCov <-merge(
  x = as.data.table(readRDS(paste0(populations_dir, "M_Studycohort_Covariates_",cohort,".rds"))),
  y = as.data.table(readRDS(paste0(populations_dir, "M_Studycohort.rds")))[, .(group, id, person_id, AGE_T0)],
  by = c("id", "person_id")
)[group %in% c("EXPOSED", "CONTROL") ,][, group :=  fifelse(group == "EXPOSED", 1, 0)]
###
#Remove region.
#JR Version - All matching variables will be forced in the PS, including GEOREGION_COV_T0. We excluded it from the PS in the IR3.
fileCov[,L_GEOREGION_COV_T0 := NULL]

#Extract the variables that need to be possible used per type of data
binaryCov <- colnames(fileCov)[colnames(fileCov) %in% paste0(COV_TF,"_", cohort)]
#JR Version - I deleted 'band' from the catCov and I create numCov including AGE_T0
catCov <- c(colnames(fileCov)[colnames(fileCov) %in% paste0(unique(c(COV_CAT, COV_SCORE)),"_", cohort)], paste0("L_SEX_COV_", cohort))
numCov <- c("AGE_T0")
setnames(fileCov, "L_SEX_COV", paste0("L_SEX_COV_", cohort))

#Build an overview file to build the program on and include a check on mistiness of the whole column to prevent errors in later steps
#This file is the starting point serving as an overview file of all the steps
###
overviewFile <- as.data.table(cbind(
  c(binaryCov, catCov,numCov), #, numCov JR Version - I included numCOV with only one variable AGE_T0. 
  unname(sapply(c(binaryCov, catCov,numCov), function(x) length(unique(fileCov[[x]])))), #, numCov JR Version - I put the number of different values for AGE_T0
  c(rep("bin", length(binaryCov)), rep("cat", length(catCov)), rep("num", length(numCov))) #, rep("num", length(numCov)) JR Version - I included "num"
)) 

colnames(overviewFile) <- c("var", "nb_values", "type")

###

#Determine ASD function that loads the function of Rutger vd Bor and creates a named list that can be merged to the overview file
###
ASDNamedLIst <- function(file = fileCov, bin, cat, num, c.group = "group", c.weights = NULL){
        
        if(!is.null(c.weights)){weights <- file[[c.weights]]}else{weights = NULL}     
    
        tmpList <-append(
          sapply(bin,function(x) ASD.binary(  file[[c.group]], file[[x]], weights)),
          sapply(cat,function(x) ASD.category(file[[c.group]], file[[x]], weights))
        )
        #JR Version - I added the ASD.numeric for the numeric variables. 
        tmpList<-append(tmpList,sapply(num,function(x) ASD.numeric( file[[c.group]], file[[x]], weights)))
        return(tmpList)
}


#JR Version - I added the next 5 lines to classify all covariates regarding the number of values. It will be used on the ASD estimation.
finally_binary<-overviewFile[as.numeric(nb_values) == 2,][["var"]]
finally_cat<-overviewFile[as.numeric(nb_values) > 2,][["var"]]
finally_cat<-finally_cat[!finally_cat %in% c('AGE_T0')]
finally_num<-numCov
if(dim(overviewFile)[1] != sum(length(finally_binary)+length(finally_cat)+length(finally_num)+length(overviewFile[as.numeric(nb_values)==1]$var))) print(paste0("Not all variables have been correctly classified regarding the number of values"))


asdResults <- ASDNamedLIst(bin = finally_binary, # JR Version - I deleted:  overviewFile[as.numeric(nb_values) == 2,][["var"]] and added <finally_binary>
                           cat = finally_cat,# JR Version - I deleted: overviewFile[as.numeric(nb_values) > 2,][["var"]] and added finally_cat 
                           num = finally_num # jR version - I added and initialized the parameter num = finally_num
                           )
overviewFile <- MergeToNamedList(named.list = asdResults, c.name = "SMD")
rm(asdResults)
###

#

#Determine for which variables 0 in the file means missingnes
varsMissing <- COV_CAT[!COV_CAT %in% c("L_CHARLSON_COV", "V_CDC_COV",MATCH_CAT)]
missingness <- sapply(paste0(varsMissing,"_", cohort), function(x) sum(fileCov[[x]] == 0)/nrow(fileCov) *100 )
overviewFile <- MergeToNamedList(named.list = missingness, c.name = "missing")
rm(missingness)


#Determine correlation
###
#First asses which covariates are can be  included based on the other inclusion criteria. Therefore the OR is not needed
matchRelatedCov <- c(paste0(unique(c(time_dep_match, time_indep_match)),"_T0"),"band")
#JR Version - I deleted band and O_YEARBIRTH_COV_T0 and I added AGE_T0
matchRelatedCov<-append(matchRelatedCov[!matchRelatedCov %in% c('band','O_YEARBIRTH_COV_T0')],'AGE_T0')

#JR Version - The variables included have to be those described on Tables 11, 12, 13 and 14. Covariates in tables 12,13 and 14 
#              are listed on Pfizer_AESI_Variables (metadata folder). The others will be hand written. 
AESI_Cov_info <- fread(paste0(meta_dir,"Pfizer AESI_information.csv"), stringsAsFactors = F, na.strings = "")
  #JR Version - Table 11 doesn't have a list of covariates to be analyzed so I need to enter manually. 
  Tab11<-c("AGE_T0","L_SEX_COV_T0","L_PREGNSTATUS_COV_T0","L_LCF_COV_T0","H_HCW_COV_T0","L_SMOKESTATUSALG_COV_T0","L_BMICALC_COV_T0","L_OBESITY_COV_T0","H_HOSPNUM_COV_T0","H_EMERG_COV_T0","H_NURSING_COV_T0","H_PRIMARYHCUSE_COV_T0","TP_CANCERSCREEN_COV_T0","TP_COVID19TEST_COV_T0")
  if (length(Tab11[Tab11 %in% names(fileCov)])>0)  {COV_notmissing<-cbind(Tab11[Tab11 %in% names(fileCov)],"T11")}
  if (length(Tab11[!Tab11 %in% names(fileCov)])>0) {COV_missing<-cbind(Tab11[!Tab11 %in% names(fileCov)],"T11")}
  #JR Version - Table 12
  Tab12 <-toupper(unique(sapply(subset(AESI_Cov_info, Table == 12)[,1],as.character)))
    #JR Version There are multiple variable names, such as CDC and Charlson
  Tab12[grep("L_CHARLSON_COV", Tab12)] <- c("L_CHARLSON_COV", rep("",length(grep("L_CHARLSON_COV", Tab12)) - 1))
  Tab12[grep("V_CDC_COV", Tab12)] <- c("V_CDC_COV", rep("",length(grep("V_CDC_COV", Tab12)) - 1))
  Tab12 <- Tab12[-which(Tab12 == "" | Tab12 == "NA")]
  Tab12 <- str_trim(Tab12, side = "right")
  if (length(paste0(Tab12,"_T0")[paste0(Tab12,"_T0") %in% names(fileCov)])>0) {COV_notmissing<-rbind(COV_notmissing,cbind(paste0(Tab12,"_T0")[paste0(Tab12,"_T0") %in% names(fileCov)],"T12"))}
  if (length(paste0(Tab12,"_T0")[!paste0(Tab12,"_T0") %in% names(fileCov)])>0) {COV_missing<-rbind(COV_missing,cbind(paste0(Tab12,"_T0")[!paste0(Tab12,"_T0") %in% names(fileCov)],"T12"))}
  #JR Versioni - Table 13
  Tab13 <-toupper(unique(sapply(subset(AESI_Cov_info, Table == 13)[,1],as.character)))
  Tab13 <- str_trim(Tab13, side = "right")
  if (length(paste0(Tab13,"_T0")[paste0(Tab13,"_T0") %in% names(fileCov)])>0){COV_notmissing<-rbind(COV_notmissing,cbind(paste0(Tab13,"_T0")[paste0(Tab13,"_T0") %in% names(fileCov)],"T13"))}
  if (length(paste0(Tab13,"_T0")[!paste0(Tab13,"_T0") %in% names(fileCov)])>0){ COV_missing<-rbind(COV_missing,cbind(paste0(Tab13,"_T0")[!paste0(Tab13,"_T0") %in% names(fileCov)],"T13"))}
  #JR Versioni - Table 14
  ## Load a function to cater for different format of AESI_info due to myo and pericarditis requests
  source(paste0(pre_dir,"functions/", "reformat_mpc_AESI.R"))  
  Tab14 <-toupper(unique(reformat_mpc_AESI(subset(AESI_Cov_info, Table == "AESI tables"), opt = "uni")$Event_abbreviation)) 
  Tab14 <- Tab14[-which(Tab14 == "C_MYOPERICARD_AESI")]
  Tab14 <- Tab14[-which(Tab14 == "I_COVID19VAED_AESI")]
  if (length(paste0(Tab14,"_T0")[paste0(Tab14,"_T0") %in% names(fileCov)])>0){COV_notmissing<-rbind(COV_notmissing,cbind(paste0(Tab14,"_T0")[paste0(Tab14,"_T0") %in% names(fileCov)],"T14"))}
  if (length(paste0(Tab14,"_T0")[!paste0(Tab14,"_T0") %in% names(fileCov)])>0){ COV_missing<-rbind(COV_missing,cbind(paste0(Tab14,"_T0")[!paste0(Tab14,"_T0") %in% names(fileCov)],"T14"))}

  COV_notmissing<-data.frame(unique(COV_notmissing))
  COV_missing<-data.frame(unique(COV_missing))
  setnames(COV_notmissing,c('X1','X2'),c('var','Tab'))
  setnames(COV_missing,c('X1','X2'),c('var','Tab'))

#JR Version - The first covariates list is conformed by those described on T11-T14 with a SDM > 0.1 AND with a % of missing values < than missingCutOf
  overviewFile<-merge(x=overviewFile,y=COV_notmissing,by="var",all = T)
  overviewFile$matched<-ifelse(overviewFile$var %in% matchRelatedCov,1,0)
  overviewFile$missing<-ifelse(is.na(overviewFile$missing),0,overviewFile$missing)
  overviewFile$SMD<-as.numeric(overviewFile$SMD)
  overviewFile$list1<-ifelse((
              !is.na(overviewFile$Tab) & 
              (overviewFile$SMD >smdLB & !is.na(overviewFile$SMD)) & 
              (overviewFile$missing<missingCutOf )),
              1,0)
  overviewFile$list1<-ifelse(overviewFile$matched==1,1,overviewFile$list1)
#JR Version - The second covariates lists exclude from the first one those variables with an important OR, so I will modify the Roel 
#             version calculating the OR only for the covariates needed. Also I need to exclude those variables with only one category
overviewFile$calc_OR<-ifelse(overviewFile$list1==1 & overviewFile$nb_values>1,T,F)
#overviewFile[ , calc_OR := fifelse(
  #                                    (missing < missingCutOf | is.na(missing ) | var %in% matchRelatedCov) & #If more then 10 categories performance may hamper and a error for working memory may occur 
  #                                  as.numeric(nb_values) < 10 & #We start with only variables that have a low percentage of missingness
  #                                  as.numeric(nb_values) > 1 & #No variables with only 1 value 
  #                                  (SMD > smdLB  | var %in% matchRelatedCov  ) #Matching variable or not equal divided among exposure (prevalence > 0.02 | type != "bin"
  #                                , T, F)]



vars <- paste0(overviewFile[calc_OR == T][["var"]])
#Calculate the OR and prevalence for all variables that did passe the first tests.
###
for(i in 1:length(vars)){

  tmpResult <- GetORPrev(
    file = fileCov,
    c.group = "group",
    var = vars[i]
  )
  
  
  if(i == 1) overviewFile2 <- tmpResult
  if(i > 1) overviewFile2 <- rbind(overviewFile2, tmpResult)
  rm(tmpResult)
}
###

#Because for categorical varaibles multiple OR's per varaible are calculated it cannot be merged to overview file and is saved separately
saveRDS(overviewFile2, paste0(populations_dir,"IPW_covariates_cat.rds"))

#JR Version - Select those OR regarding binary variables or numeric ones. 
or_not_cat<-overviewFile2[variable %in% rbind(overviewFile[type == "bin" & nb_values == 2,],overviewFile[type=="num"])[["var"]] & !is.na(OR),]

#Merge all OR and prevalences for the binary results to the overview file
overviewFile <- merge(x = overviewFile, 
      y = or_not_cat[,.(variable, prevelence, OR)], 
      by.x = "var", 
      by.y = "variable",
      all.x = T
      )

#For the categorical values determine if there is at least 1 row per variable that does not match the assumptions to be within OR 0.1-10 if preve < 0.02
#Ther rsult of this can be merged to the overviewfile while more specific infor concerning the categorical OR's is found in the earlier saved overviewfile2
overviewFile2 <- overviewFile2[variable %in% overviewFile[type == "cat",][["var"]] & !is.na(OR),]

#JR Version - I define OR_EXC_CAT in a different way. 
overviewFile2$OR_EXC_CAT<-ifelse(((overviewFile2$OR>10 | overviewFile2$OR<0.1) & overviewFile2$prevelence>2),T,F)
overviewFile2<-sqldf('SELECT variable, max(OR_EXC_CAT) as OR_EXC_CAT FROM overviewFile2 GROUP BY variable')
#overviewFile2 <- overviewFile2[, OR_EXC_CAT :=  fifelse((!data.table::between(OR, 0.1, 10)) &
#                                                        prevelence > 0.02,
#                                                           T, F)][OR_EXC_CAT == T,]

overviewFile <- merge(x = overviewFile, 
                      #y = unique(overviewFile2[,.(variable, OR_EXC_CAT)]), 
                      #JR Version - I added directly overviewFile2 since the OR_EXC_CAT is there
                      y = overviewFile2, 
                      by.x = "var", 
                      by.y = "variable",
                      all.x = T
)

#JR Version - From the list1 I have to remove those variables with an extreme OR. Extreme OR is defined as (OR>10 or OR<1) AND prevalence > 2%

overviewFile$OR_EXC_CAT<-ifelse(is.na(overviewFile$OR_EXC_CAT),F,overviewFile$OR_EXC_CAT)
overviewFile$extreme_OR<-ifelse(
                                (overviewFile$calc_OR==T & (overviewFile$var %in% c(finally_binary,finally_num)) & ((overviewFile$OR>10 | overviewFile$OR<0.1) & overviewFile$prevelence>2)) |
                                (overviewFile$calc_OR==T & (overviewFile$var %in% finally_cat) & overviewFile$OR_EXC_CAT==T)
                                ,T,F)
overviewFile$extreme_OR<-ifelse(is.na(overviewFile$extreme_OR),F,overviewFile$extreme_OR)


#Determine which variables to add to the PS model by creating a variable use with T/F. This variable is the result that is used for the determination of the weights.
#overviewFile[ , use:= fifelse(
#                              (type == "bin" & data.table::between(OR, 0.1, 10) & prevelence > 0.02 ) | #Binary variables should have a OR between 0.1 and 10  
#                             (type == "bin" & prevelence <= 0.02 & calc_OR ) |
#                              (type == "cat" & calc_OR & is.na(OR_EXC_CAT))  #Categorical values are only included when the other inclusion criteria are met (calc_OR)
#
#                                                           , T, F, na = F)]

#JR Version - Easier way to define use is to select the covariates in list1 without extreme OR
overviewFile[,use:=fifelse(list1==T & extreme_OR==F & nb_values>1,T,F)]

#Check missingness per row
###
#Reduce file with only the needed columns
#fileCov <- fileCov[, c("id", "person_id","group", overviewFile[use == T,][["var"]]), with = F]
#gc()
varsMissingRow <- varsMissing[paste0(varsMissing, "_", cohort) %in% overviewFile[use == T,][["var"]]]
varsMissingRow <- paste0(varsMissingRow, "_", cohort)
#Matching variables are only taken into account for the complete case determination if not a matcging factor.
varsMissingRow <- varsMissingRow[!varsMissingRow %in% matchRelatedCov]

if(length(varsMissingRow) > 0){
fileCov$any_missing <- as.integer(rowSums(fileCov[, varsMissingRow, with = F] == 0) > 0)
}else{
  fileCov$any_missing <- 0
}

#Split dataset in subjects with any missing and without.
fileCovMissing <- fileCov[any_missing ==1 ,]
fileCov <- fileCov[any_missing == 0 ,]
###


#Run glm for PS
###

#Make a temporary copy of fileCov to set to factor. By setting to factor the original values are lost...?? Josep any advise here. Factors act strange...
fileCov2 <- copy(fileCov)[, c("id", "person_id","group", overviewFile[use == T,][["var"]]), with = F]


#JR Version - A list of variables to convert to factor
to_factor<-overviewFile[use == T,][["var"]][!overviewFile[use == T,][["var"]] %in% finally_num]
#Set to factor so all variables as calculated as categorical and not continuous 
lapply(#c(!overviewFile[use == T,][["var"]] %in% finally_num),
       #JR Version _ I added directly to_factor
  to_factor, function(x) fileCov2 <- fileCov2[, eval(x) := as.factor(get(x))])

#JR Version - I calculate AGE_T0_2 and I add to f.build
to_number<-names(fileCov2)[!names(fileCov2) %in%c("id", "person_id","group",to_factor)]
lapply(to_number,function(x) fileCov2<-fileCov2[,eval(x):=as.numeric((get(x)))])
fileCov2$AGE_T0_2<-fileCov2$AGE_T0*fileCov2$AGE_T0

#Run model to retrieve needed parameters for PS
glmOutcomes <- glm(f.build("group", c(to_factor,to_number,"AGE_T0_2")), #JR Version- I added AGE_T0_2
                   data = fileCov2, #[, c("group",  c(overviewFile[use == T,][["var"]],"AGE_T0_2")), with = F], , "matched_cohort"
                   family = "binomial"
)

rm(fileCov2)
gc()

#Make object to store the reference values for the OR's. This values need to be added to the table 18 later on.
levelsRef <-  glmOutcomes$xlevels

#The covaraites are not needed anymore so are removed from the file since we have all needed parameters in the object glmOutcomes
#fileCov <- fileCov[, .(id, person_id, group)]

#lapply(c( overviewFile[use == T,][["var"]]), function(x) fileCov <- fileCov[, eval(x) := as.numeric(get(x))])

#Add OR with 95% confidence interval to overview file based on PS. Also add reference value. This needs to be the input for table 18
###
ORps <- as.data.table(cbind(names(exp(coef(glmOutcomes))),exp(coef(glmOutcomes)) )) #, exp(confint(glmOutcomes))
colnames(ORps) <- c("var","ORPs") #, "ORPsLb", "ORPsUb"
for(i in 1:length(levelsRef)){ORps[grepl(names(levelsRef[i]), var) , ref := levelsRef[[i]][1] ]}
rm(levelsRef)
#overviewFile <- merge(x = overviewFile,y = ORps, all.x = T, by = "var")
saveRDS(ORps, paste0(populations_dir,"OR_PS.rds"))
rm(ORps)
###

#Calculate PS per subject and add to covariate file
p.score_full_cor <- glmOutcomes$fitted.values
fileCov$PS <- p.score_full_cor

#Calculate the weights based on the PS. Differentiate for effect on the treated or exposed and stabilized or not
###

#if(weightingMethod == "ATT"){
#  fileCov$IPTW_full_cor <- with(fileCov, group + (p.score_full_cor * (1 - group) / (1 - p.score_full_cor)))
#}

#if(weightingMethod == "ATE"){
#  fileCov$IPTW_full_cor <- with(fileCov, ((group/p.score_full_cor) + ((1-group)/(1-p.score_full_cor))))
#}

#if(stabalizeWeights){s.factor <- 0.5
#                    fileCov$IPTW_full_cor <- s.factor * fileCov$IPTW_full_cor 
                      

#}else{s.factor <- 1} 

#rm(s.factor)

#JR - Version. I calculate the IPWT
fileCov$IPTW_full_cor<-0.5*((as.numeric(fileCov$group)/fileCov$PS)+((1-as.numeric(fileCov$group))/(1-fileCov$PS)))

###
#Trim the values to correct for outliers
#There is an overlapping weight function whit the consequence of not trimming so therefore always specify the used package!!
fileCov$IPTW_full_cor_trim <- WeightIt::trim(fileCov$IPTW_full_cor, at = .99)

#min, p25, p50, p75, p99, max, mean, std
describeIPW <-  function(file, col){
                                    descriptives <- matrix(c(min(file[[col]]),
                                                                    max(file[[col]]),
                                                                    mean(file[[col]]),
                                                                    quantile(file[[col]]),
                                                                    sd(file[[col]])))
                                    rownames(descriptives) <- c("min", "max", "mean", "p0", "p25", "p50", "p75", "p99", "sd")
                                    colnames(descriptives) <- col
                                    
                                    return(descriptives) 
                                    }


descriptives <- as.data.table(
                        rbind(
                              describeIPW(fileCov[group == 1,], "IPTW_full_cor_trim"),
                              describeIPW(fileCov[group == 0,], "IPTW_full_cor_trim") 
                              
          ), keep.rownames = T)

descriptives[["group"]] <-  c(rep(1,9),rep(0,9))
      
saveRDS(descriptives, paste0(populations_dir,"weights_distribution.rds"))
rm(p.score_full_cor, descriptives)

#Test 
###


#Check balance by redoing the the SMD but then with the calculated weights. ()
# smdAfter <- ASDNamedLIst(
#                     bin = overviewFile[use == T & type == "bin",][["var"]], 
#                     cat = overviewFile[use == T & type == "cat",][["var"]],
#                     c.weights = "IPTW_full_cor_trim"
#                     )

#JR Version - I added num and update the selection of binaly and categorical variables
                            
smdAfter <- ASDNamedLIst(  bin = finally_binary, #overviewFile[as.numeric(nb_values) == 2,][["var"]], 
                           cat = finally_cat, # overviewFile[as.numeric(nb_values) > 2,][["var"]],
                           num = finally_num,
                           c.weights = "IPTW_full_cor_trim"
)

overviewFile <- MergeToNamedList(named.list = smdAfter, c.name = "SMD_balanced")
rm(smdAfter)

fileCov <- rbindlist(
                    list(fileCov[,c("id", "person_id", "group", "PS", "IPTW_full_cor", "IPTW_full_cor_trim"), with = F],
                    fileCovMissing[,c("id", "person_id", "group"), with = F]),
                    use.names = T,
                    fill = T
)



saveRDS(fileCov,paste0(populations_dir,"weights.rds"))
saveRDS(overviewFile, paste0(populations_dir,"IPW_covariates.rds"))

  
rm(overviewFile, overviewFile2, fileCov,fileCovMissing, weightingMethod, stabalizeWeights, vars, varsMissing, glmOutcomes, matchRelatedCov)
gc()

