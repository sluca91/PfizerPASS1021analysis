
DAP <- "PEDIANET"
#i = "MEDICAL_OBSERVATIONS"

#add_con_path <- "Y:/Studies/ConcePTION/B_Documentation/2 Protocol_DSMB_Monitoring/WP 7.6 Data Characterization SAP/tmp_RE/AnalyseStudyVaribaleDAPs/output/ETL_SPECS2.csv"
add_con_path <- "Pfizer_additional_concepts.csv"
#concepts <- fread(add_con_path)
concepts <- IMPORT_PATTERN(pat = add_con_path, dir =  meta_dir )[DAP_NAME == DAP, ]

#concepts <- IMPORT_PATTERN(pat = "ETL_SPECS.csv", dir =  "Y:/Studies/ConcePTION/B_Documentation/2 Protocol_DSMB_Monitoring/WP 7.6 Data Characterization SAP/tmp_RE/AnalyseStudyVaribaleDAPs/output" )[DAP_NAME == DAP, ]


concepts <- concepts[, id := seq_len(.N)][, N := as.numeric()]
#[table == i,]

test <- data.table::melt(data = concepts, id.vars = c("StudyVar", "id", "table"), measure.vars = list(c("col1", "col2", "col3"), c("val1", "val2", "val3")) )[!is.na(value1),]

test <- data.table::dcast(data = test, formula = StudyVar + id + table  ~ value1, value.var = "value2")

path <- paste0("H:/Review_level1b/", DAP)
files <- list.files(path)

for(i in 1:nrow(test)){
  
  merge <- copy(test)[i,]
  x <- "mo_meaning"
  for(j in colnames(merge)){if(is.na(merge[[j]])) merge[[j]] <- NULL }
  
  file <- files[grepl(pattern = paste0("WHERECLAUSE_", test[i,][["table"]]), x = files)]
  
  
  
  file <- IMPORT_PATTERN(pat = file, dir = path)
  
  Result <- sum(as.numeric(merge(x = merge, y = file, by = colnames(merge)[!colnames(merge) %in%c("StudyVar", "id", "table")], all = F  )[["N"]]))
  
  concepts[id == merge$id, N := Result ]
  
  
}

setorder(concepts, "StudyVar")
report <- concepts[, .(StudyVar, N)][, sum := sum(N), by = "StudyVar"]

missing <- concepts[N == 0, ][, `:=` (keep = NULL, date_column = NULL, date_column2 = NULL, colkeep2 = NULL, N = NULL, id = NULL, Outcome = NULL)]

missing <- missing[,colnames(missing)[(colSums(is.na(as.matrix(missing)) , na.rm = T) != nrow(missing))], with =F]

