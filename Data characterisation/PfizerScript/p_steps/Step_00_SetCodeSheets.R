
#Aim: In this script all available codesheets in p_meta are processed so they are ready for use.
#The outputs are rds files with information stored by CDM tables MEDICINES, EVENTS, VACCINES. Also an additional table is made storing information not merely for 
#1 specific CDM table

SCRIPT <- RUN_SCRIPT(name = "Step_00_SetCodeSheets.R")



#Prepare EVENTS codes
###
CODESHEET_EVENTS <- IMPORT_PATTERN(
  dir = unlist(SCRIPT[["INPUT1"]][["folder"]]), 
  pat = paste0(SCRIPT[["INPUT1"]][["name"]],".csv"),
  colls = c('coding_system','code','event_abbreviation', 'type', 'tags', 'system'),
  append = F
  
  
)[,.(coding_system,code,event_abbreviation, type, tags, system)][, Outcome := paste0(system,"_", event_abbreviation, "_", type)][, ':=' (event_abbreviation = NULL, type = NULL, system = NULL)]
#setnames(CODESHEET_EVENTS,'code','code_original')

#It was decided to only continue with narrow codes, with the exception of concepts that do not have any narrow code or for the concept M_ARTASEPTIC_AESI (HARDCODED) 
CODESHEET_EVENTS_TEMP <-  unique(CODESHEET_EVENTS[toupper(tags) == toupper("narrow"),][, any_narrow := T][,.(Outcome, any_narrow)])
CODESHEET_EVENTS <- merge(x = CODESHEET_EVENTS, y = CODESHEET_EVENTS_TEMP, by = "Outcome", all.x = T)
CODESHEET_EVENTS <- CODESHEET_EVENTS[toupper(tags) == toupper("narrow") |  Outcome == "M_ARTASEPTIC_AESI" | (is.na(any_narrow) & is.na(tags)),] #for the new approach this needs to be applied when excluded the possible code
rm(CODESHEET_EVENTS_TEMP)

###

#Prepare the file that contains the coordinates of the concepts that are in the CDM in a not standardized manner.
###
DAP_SPECIFIC_CONCEPTS <- IMPORT_PATTERN(
  dir = unlist(SCRIPT[["INPUT3"]][["folder"]]), 
  pat = paste0(SCRIPT[["INPUT3"]][["name"]],".csv"),
  append = F
  
)[DAP_NAME == DAP]

#Because the col/val combination columns can vary in the in the number of instances it is checked which are in the file for the selection of the needed columns. 
x <- colnames(DAP_SPECIFIC_CONCEPTS)
x <- x[substr(x, 1, 3) %in% c("col", "val") & nchar(gsub("[0-9]","", substr(x, 4,nchar(x)))) == 0]

#Keep only the needed columns.Only needed because of the x which in theory can be changed throughout the project.
DAP_SPECIFIC_CONCEPTS <- DAP_SPECIFIC_CONCEPTS[, c("DAP_NAME", "table", "StudyVar", "keep", "date_column", x) , with = F ][table != "VACCINES",] 
rm(x)

#Make the variable that contains the study variable homogeneous.This corrects bad design of the meta layer concerning naming
setnames(DAP_SPECIFIC_CONCEPTS, "StudyVar", "Outcome")

#HARDCODED: CPRD apparently has information in the MO table that is expected in the events tables. Please verify with CPRD. This is bad practice and will be going wrong
#over and over again. I ensure you. Please solve this by proper agreements concerning how to map information coming from the code sheets. If procedures and mo are also 
#allowed to contain coding system information in pre specified columns this needs to be added in step_05_11. I forgot that this was into the code since it was pragmaticly
#put in in interim 1
###
if(DAP == "CPRD"){
  
  TEMP <- CODESHEET_EVENTS[Outcome == "I_COVID19DX_COV" &  coding_system == "MEDCODEID",]
  setnames(TEMP, c("code"), c("val1"))
  TEMP <- TEMP[, `:=`
               
               (val2 = "positive",
                 
                 col1 = "mo_code",
                 col2 = "mo_unit",
                 #Please verify this which CRPD. Which concepts need to be extracted via MO.
                 Outcome = "COVID19DX2",
                 DAP_NAME = "CPRD",
                 date_column = "mo_date",
                 table = "MEDICAL_OBSERVATIONS"
                 
               )][,.(val1, val2, col1, col2, DAP_NAME, Outcome, date_column, table)]
  
  
  
  DAP_SPECIFIC_CONCEPTS <- rbindlist(list(DAP_SPECIFIC_CONCEPTS, TEMP), use.names = T, fill = T)
  rm(TEMP)
  gc()  
}


###




#Prepare Medicines codes
###
CODESHEET_MEDICINES <-  IMPORT_PATTERN(
  dir = unlist(SCRIPT[["INPUT4"]][["folder"]]), 
  pat = paste0(SCRIPT[["INPUT4"]][["name"]],".csv"),
  append = F
  
)

#Harmonies the column names with the other codesheets. In events there is a column with the coding system
#To process the event in the same manner using the same function a column is added with the coding system
CODESHEET_MEDICINES <- CODESHEET_MEDICINES[, Outcome := drug_abbreviation]
if(!DAP %in% 'CPRD'){
  CODESHEET_MEDICINES <- CODESHEET_MEDICINES[product_identifier %in% 'ATC']
}else{
  CODESHEET_MEDICINES <- CODESHEET_MEDICINES[product_identifier %in% 'PRODCODEID']
}
setnames(CODESHEET_MEDICINES,'product_identifier','CodeSystem')
setnames(CODESHEET_MEDICINES, c("code"), c("Code"))
CODESHEET_MEDICINES <- CODESHEET_MEDICINES[, .(Code, Outcome, CodeSystem, tags)]
CODESHEET_MEDICINES <- CODESHEET_MEDICINES[tags %in% 'narrow']

#Prepare VACCINES codes
#This incoming via ATC codes AND/OR DAP specific annotations in vx_type
###
#For the vx_type manner of importing the additional concepts is processed in a way that it can be added to the codelist with the atc codes.
#Eventually, it can be processed in the same manner as medicines and events.
DAP_SPECIFIC_VACCINES <- IMPORT_PATTERN(
  dir = unlist(SCRIPT[["INPUT3"]][["folder"]]), 
  pat = paste0(SCRIPT[["INPUT3"]][["name"]],".csv"),
  append = F
  
)[DAP_NAME == DAP][table == "VACCINES",]

#Reform to a table structure equal to the structure used for the event and medicines codelist which is demanding 2 columns (code and system)
lapply(colnames(DAP_SPECIFIC_VACCINES)[substr(colnames(DAP_SPECIFIC_VACCINES),1,3) == "col"], 
       function(x) DAP_SPECIFIC_VACCINES[get(x) == "vx_type", Code := get(paste0("val",substr(x,4,nchar(x))))]
)

DAP_SPECIFIC_VACCINES <- DAP_SPECIFIC_VACCINES[, `:=` (CodeSystem =  "vx_type", Outcome = StudyVar) ][, c("CodeSystem", "Code", "Outcome")  , with = F]


CODESHEET_VACCINES <-  IMPORT_PATTERN(
  dir = unlist(SCRIPT[["INPUT2"]][["folder"]]), 
  pat = paste0(SCRIPT[["INPUT2"]][["name"]],".csv"),
  append = F
  
)[, CodeSystem := "vx_atc"]
setnames(CODESHEET_VACCINES, c("StudyVar", "atc_codes"), c("Outcome", "Code"))

#Combine the atc and vx_type meta information to one file.
CODESHEET_VACCINES <- rbindlist(list(DAP_SPECIFIC_VACCINES, CODESHEET_VACCINES), fill = T, use.names = T)
rm(DAP_SPECIFIC_VACCINES)
###

#Prepare Algorithms and scoring: this consists of a file that specifies grouping and a file that specifies scoring
#These 2 components where 1 thing earlier in the project but are now 2 in depended features. 
###


 ALG <-  IMPORT_PATTERN(
   dir = unlist(SCRIPT[["INPUT5"]][["folder"]]), 
   pat = paste0(SCRIPT[["INPUT5"]][["name"]],".csv"),
   append = F
   
 )
 
 SCORE <-  IMPORT_PATTERN(
   dir = unlist(SCRIPT[["INPUT6"]][["folder"]]), 
   pat = paste0(SCRIPT[["INPUT6"]][["name"]],".csv"),
   append = F
   
 )[DAP_NAME == "ALL" | DAP_NAME == DAP,][, DAP_NAME := NULL]
 
 invisible(lapply(c("INTEGER_CODE","LOWER","UPPER"), function(x) SCORE <- SCORE[,eval(x) := as.numeric(get(x))]))
 
 ###
 
 #Prepare the dictionary needed to add standardized study specific labels for categorical variables.
 ###

 DICTIONARY <-  IMPORT_PATTERN(
   dir = unlist(SCRIPT[["INPUT7"]][["folder"]]), 
   pat = paste0(SCRIPT[["INPUT7"]][["name"]],".csv"),
   append = F
   
 )[,c("DAP_NAME","VarName","category","integerVal","oriVal"), with = F ]

 #Here there is an option to differentiate between DAP's but also make labels for all DAP's. This is not implemented in all the meta information yet which could be a consideration 
 #for further development.
 DICTIONARY <- DICTIONARY[DAP_NAME == DAP | DAP_NAME == "ALL",]
 DICTIONARY <- unique(DICTIONARY)[!is.na(oriVal) & !is.na(integerVal) & !is.na(category),]
 
 #Since the integer values and the connected categorical values need to be equal over the DAP's this checked.
 DICTIONARY <-  DICTIONARY[, check := seq_len(.N), by = "oriVal"]
 if(any(DICTIONARY$check > 1)) stop("Overlapping values in dictionary. Check dixtionary in p_meta")
 DICTIONARY <-  DICTIONARY[, check := NULL]
 invisible(lapply(c("integerVal"), function(x) DICTIONARY <- DICTIONARY[,eval(x) := as.integer(get(x))]))
###
 
 
 
#Handle ranges and unit meta information
###
 #For continuous variables it is assumed that the units are stored in a CDM column name that contains unit. This code collect all the units that are known
 ###
 checkUnits <- colnames(DAP_SPECIFIC_CONCEPTS)[substr(colnames(DAP_SPECIFIC_CONCEPTS), 1, 3) == "col"]
 #Make an empty variable where the collected units are appended to.
 foundUnits <- NULL
 
 for(i in checkUnits){
   foundUnits <- append(
     foundUnits,
     DAP_SPECIFIC_CONCEPTS[grepl(x = toupper(get(i)), pattern = "UNIT") ,c(i, paste0("val", substr(i,4,4))) , with = F][[paste0("val", substr(i,4,4))]]
   )
 }
 
 rm(checkUnits)
 ###
 
 if(any(!foundUnits %in% unname(unlist(supportedUnits)))){warning("Please check enviromental variable supportedUnits. Some found units from additional_concepts are not supported")}
 
 #Reform and safe the meta file in a separate script. Code is more extensive then the handling of the other meta files.
 source(paste0(pre_dir,"functions/", "ReformUnitsRangesSheet.R"))
  
### 

#To prevent merging and filtering problems all meta information is set to upper case.
###
CODESHEET_EVENTS <- CODESHEET_EVENTS[,Outcome := toupper(Outcome)]
CODESHEET_VACCINES <- CODESHEET_VACCINES[,Outcome := toupper(Outcome)]
DAP_SPECIFIC_CONCEPTS <- DAP_SPECIFIC_CONCEPTS[,Outcome := toupper(Outcome)]
CODESHEET_MEDICINES <- CODESHEET_MEDICINES[,Outcome := toupper(Outcome)]
ALG <- ALG[,NEW_CONCEPT := toupper(NEW_CONCEPT)]
ALG <- ALG[, CONCEPT := toupper(CONCEPT)]
SCORE <- SCORE[,CONCEPT := toupper(CONCEPT)]
DICTIONARY <- DICTIONARY[,VarName := toupper(VarName)]
###

#See which concepts that are in the main file are not specified in any way. 
notNeededInSheets <- c(time_indep_match, time_indep_nmatch)
missingInSheets <- ALL_CONCEPTS[!ALL_CONCEPTS %in% unique(c(CODESHEET_EVENTS$Outcome, CODESHEET_MEDICINES$Outcome, CODESHEET_VACCINES$Outcome, ALG$NEW_CONCEPT, DAP_SPECIFIC_CONCEPTS$Outcome ))]
missingInSheets <- missingInSheets[!missingInSheets %in% notNeededInSheets]

print(paste0(paste0(missingInSheets, collapse = ","), " are not specified for this DAP or in the codesheets"))

#Safe the meta files. Note that output 8 is stored in a separate script in row 202 of this script.
###
saveRDS(CODESHEET_EVENTS[Outcome %in% ALL_CONCEPTS_CODESHEETS,], file = SCRIPT[["OUTPUT1"]][["path"]])
saveRDS(CODESHEET_VACCINES[Outcome %in% ALL_CONCEPTS_CODESHEETS,], file = SCRIPT[["OUTPUT2"]][["path"]])
saveRDS(DAP_SPECIFIC_CONCEPTS[Outcome %in% ALL_CONCEPTS,], file = SCRIPT[["OUTPUT3"]][["path"]])
saveRDS(CODESHEET_MEDICINES[Outcome %in% ALL_CONCEPTS_CODESHEETS,], file = SCRIPT[["OUTPUT4"]][["path"]])
saveRDS(ALG[NEW_CONCEPT %in% ALL_CONCEPTS,], file = SCRIPT[["OUTPUT5"]][["path"]])
saveRDS(SCORE[CONCEPT %in% ALL_CONCEPTS,], file = SCRIPT[["OUTPUT6"]][["path"]])
saveRDS(DICTIONARY[VarName %in% ALL_CONCEPTS,], file = SCRIPT[["OUTPUT7"]][["path"]])

rm(CODESHEET_VACCINES,DAP_SPECIFIC_CONCEPTS, CODESHEET_MEDICINES, CODESHEET_EVENTS, SCORE, ALG, DICTIONARY)
gc()





