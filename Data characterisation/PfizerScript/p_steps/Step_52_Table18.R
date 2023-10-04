
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organization: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/01/2023

##Aim
#Create a table (table 18) that gives an overview of the covariate selection in the IPW model.

##in/output
#Input 1: OR_PS.rds
#Input 2: IPW_covariates.rds
#Input 3: Dictionary_result.rds
#Input 4: Scores.rds
#Output 1: DAP_Table18.csv


ORps <- readRDS( paste0(populations_dir,"OR_PS.rds"))
SMD <- readRDS(paste0(populations_dir,"IPW_covariates.rds"))[, .(var, SMD_balanced, use, nb_values)] #[use ==T,]

for(i in SMD[use == T,][["var"]]) ORps <- ORps[grepl(i,var), Covariate := i ]

ORps <- ORps[, Value := as.character(substr(var, nchar(Covariate) + 1, nchar(var)))][, OR := as.character(ORPs)] 
ORps2 <- unique(copy(ORps)[,   Value := as.character(ref)][, OR := "reference"][, .(Covariate, Value, OR )])

temp <- rbindlist(list(ORps[,.(Covariate, Value, OR )], ORps2[,.(Covariate, Value, OR )]))[!is.na(Covariate),]

temp <- merge(temp, SMD, by.x = "Covariate", by.y = "var", all.y = T)[OR != "reference", SMD_balanced := ""]
setorder(temp, Covariate, Value)
temp <- temp[, var := fifelse(Covariate != "band", substr(Covariate, 1, nchar(Covariate)-3), Covariate)] 

scores <- readRDS(paste0(tmp,"Scores.rds"))[, .(CONCEPT, INTEGER_CODE, LABEL)]
dic <- readRDS(paste0(tmp,"Dictionary_result.rds"))[, .(VarName, integerVal, oriVal)]

colnames(scores) <- c("var", "Value", "label")
colnames(dic) <- c("var", "Value", "label")

dic2 <- rbind(scores, dic)[var %in% temp[use == T,][["var"]],][, Value := as.character(Value)]

temp <- merge(temp, dic2, by = c("var", "Value"), all.x =T)
temp <- temp[var == "band" , label := Value ]
temp <- temp[is.na(label) & Value == 0 , label := paste0("No") ]
temp <- temp[is.na(label) & Value == 1 , label := paste0("Yes") ]

fwrite(temp, paste0(output_dir,DAP,'_Table18.csv'), sep = ";")

rm(ORps, ORps2, temp, scores, dic, dic2, SMD)
gc()
#lapply(colnames(MATCH_PAIRS)[!colnames(MATCH_PAIRS) %in% c("Exposed", "Control", "T0","id")], function(x) table(test[[x]]))