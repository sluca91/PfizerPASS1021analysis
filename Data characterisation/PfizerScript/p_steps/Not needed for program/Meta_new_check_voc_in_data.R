


unique(FILE_TEMP[["coding_system"]])[unique(FILE_TEMP[["coding_system"]]) %in% unique(TEMP[["Voc"]])]



scheme[j][["table"]] == "EVENTS"

codesheet = FILE
c.voc = "coding_system"
c.concept = "event_abbreviation"
c.codes = "code"
file = COVID19DX
f.code = "Code"
f.voc =  "Voc"
c.startwith = start_with_colls
f.concept = "event_abbreviation"


CheckFoundConcepts <- function(codesheet, c.voc, c.concept, c.codes, file, f.code,f.concept, f.voc, c.startwith){
  
  CODES_VARS <- c(c.codes,c.concept , c.voc)
  CODES_VARS_NEW <- c("Code", "Concept" , "Voc")
  CODES <- unique(codesheet[,..CODES_VARS])
  setnames(CODES, CODES_VARS, CODES_VARS_NEW)
  CODES <- CODES[,Code := gsub("\\.","",Code)]
  
  CONCEPT_VARS <- c(f.code, f.concept, f.voc)
  CONCEPT_VARS_NEW <- c("Code", "Concept" , "Voc")
  CONCEPT <- unique(file[,..CONCEPT_VARS])
  setnames(CONCEPT, CONCEPT_VARS, CONCEPT_VARS_NEW)
  CONCEPT <- CONCEPT[,Code := gsub("\\.","",Code)]
  
  CHECK <- sqldf(
    
    "
  SELECT DISTINCT 
  t1.Concept,
  t1.Code_2 as Code_codesheet,
  t1.Voc as Voc_codesheet,
  t2.Code_2 as Code_data,
  t2.Voc as Voc_data
  
  FROM CODES t1 
  
  LEFT JOIN CONCEPT t2 on(
                          t1.Voc = t2.Voc AND 
                          t1.Concept = t2.Concept AND
                          (
                          substr(t1.Code, 1, length(t1.Code)) = substr(t2.Code, 1, length(t1.Code))
                          )
                          )
  
  
  "
    
    
    
  )
  
  
  
  
  
}
                                


INPUT <- unique(FILE[,.(coding_system, code)])[,Code_2 := gsub("\\.","",code)][, code := NULL]
setnames(INPUT, "coding_system", "Voc" )






CHECK <- sqldf(
  
  "
  SELECT DISTINCT 
  t1.Code_2 as Code_codesheet,
  t1.Voc as Voc_codesheet,
  t2.Code_2 as Code_data,
  t2.Voc as Voc_data
  
  FROM INPUT t1 
  
  LEFT JOIN CONCEPT t2 on(
                          t1.Voc = t2.Voc AND 
                          
                          (
                          substr(t1.Code_2, 1, length(t1.Code_2)) = substr(t2.Code_2, 1, length(t1.Code_2))
                          )
                          )
  
  
  "
  
  
  
)



scheme[j][["table"]] != "EVENTS"


unique(TEMP[["event_code"]]) %in% 
