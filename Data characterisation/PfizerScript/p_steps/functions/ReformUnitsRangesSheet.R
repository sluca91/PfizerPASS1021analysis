#Aim: to reform the meta file that contains the information for the units and validation ranges in a way that it can be merged to the concepts.
#Note that this is not a function but a script which is a part of 00_SetCodeSheets. This because this piece of code contains many rows and would make 
#00_Set_codesheets less readable. 

#Input 1: Pfizer_unit_ranges.csv
#Output 1: Ranges.rds


#Step 1: Prepare meta file: this file contains all needed information to inpute and is joinable to the concepts tables
###

#Get meta file
units <- IMPORT_PATTERN(
                          dir = unlist(SCRIPT[["INPUT7"]][["folder"]]), 
                          pat = paste0(SCRIPT[["INPUT8"]][["name"]],".csv"),
                          append = F
)[, StudyVar := toupper(StudyVar)]

if(!all(unique(units$UNIT) %in% sapply(1:length(supportedUnits), FUN = function(x) supportedUnits[[x]][1]))) warning("Not all units in UNIT column are in supported units")

#Some columns need to be in numeric format
toNumeric <- c("MIN_AGE", "MAX_AGE", "MIN", "MAX", colnames(units)[grepl(x = colnames(units), pattern =  "FACTOR")])
lapply(toNumeric , function(x) units[, eval(x) := as.numeric(get(x))])

#Retrieve the number alternative units that can be related to the leading unit using a factor
###
#Get columns that contain UNIT but that are not equal to unit.
unitsFactor <- colnames(units)[grepl(x = colnames(units), pattern =  "UNIT")] 
unitsFactor <- unitsFactor[!unitsFactor %in% "UNIT"]
#Split the number behind the underscore and save this as a vector that is used as an input for 2 loop porcedures later.
stringsEnd <- stringr::str_split(string = unitsFactor, pattern = "_")
stringsEnd <- sapply(stringsEnd, tail, 1)
###

#Loop procedure 1: calculate the ranges for the alternative units and check if the ranges of the different units do overlap
###
lapply(stringsEnd , function(x){
  units[, eval(paste0("MIN_", x)) := MIN * get(paste0("FACTOR_", x))][, eval(paste0("MAX_", x)) := MAX * get(paste0("FACTOR_", x))]
  temp <- copy(units)[!is.na(get(paste0("MIN_", x))) & !is.na(get(paste0("MAX_", x))) ,]
  setkeyv(temp, c("StudyVar", "MIN_AGE", "MAX_AGE", "MIN", "MAX"))
  temp2 <- foverlaps(temp, temp, by.x = c("StudyVar", "MIN_AGE", "MAX_AGE", paste0("MIN_", x), paste0("MAX_", x)), nomatch = 0L, type = "any")
  
  if(nrow(temp2) > 0) warning("ranges for units do overlap")
  
}
)
###

#Loop procedure 2: Make a long file of the wide file that can be joined to add the estimated unit to the concept files
###
unitsBase <- copy(units)[, c("StudyVar", "MIN_AGE", "MAX_AGE", "MIN", "MAX", "UNIT"), with = F][, FACTOR := 1][, UNIT_FROM := UNIT]

for(x in stringsEnd){
  appendFactors <- copy(units)[, UNIT_FROM := UNIT][, c("StudyVar", "MIN_AGE", "MAX_AGE", paste0("MIN_", x), paste0("MAX_", x), paste0("UNIT_", x), paste0("FACTOR_", x), "UNIT_FROM"), with = F]
  setnames(appendFactors,
           c("StudyVar", "MIN_AGE", "MAX_AGE", paste0("MIN_", x), paste0("MAX_", x), paste0("UNIT_", x), paste0("FACTOR_", x)),
           c("StudyVar", "MIN_AGE", "MAX_AGE", "MIN", "MAX", "UNIT", "FACTOR")
  )
  
  unitsBase <- rbindlist(list(unitsBase, appendFactors))
  rm(appendFactors)
}

unitsBase <- unitsBase[!is.na(MIN) & !is.na(MAX) ,]

#Check the file on mistakes. These meta files are not normalized so be careful with filling. This check may detect mistakes.
#Note that the FACTOR columns are not checked. This can be added. 
###
if(
    any(copy(unitsBase)[,shift :=  MIN_AGE - shift(MAX_AGE, n = 1, fill = NA, type = "lag"), by = c("StudyVar", "UNIT")][!is.na(shift),][["shift"]] != 1)
){warning("Check if all the age ranges are filled correctly. There are gaps or overlaps between the ranges.")}

if(
    any(unitsBase[, .(sum = .N), by = c("StudyVar", "MIN_AGE", "MAX_AGE", "UNIT")][["sum"]] > 1) |
    any(unitsBase[, .(sum = .N), by = c("StudyVar", "MAX_AGE", "UNIT")][["sum"]] > 1) |
    any(unitsBase[, .(sum = .N), by = c("StudyVar", "MIN_AGE", "UNIT")][["sum"]] > 1) |
    any(unitsBase[, .(sum = .N), by = c("MIN_AGE", "MAX_AGE", "UNIT")][["sum"]] > 1)
){warning("Suspected mistake in the meta file Pfizer_unit_ranges.csv. Check if all the units are in the correct row")}
###

  
saveRDS(unitsBase,file = SCRIPT[["OUTPUT8"]][["path"]])
rm(unitsBase, units, toNumeric, unitsFactor, stringsEnd)
gc()

###