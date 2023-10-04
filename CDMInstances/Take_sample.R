

CreateSampleCDM <- function(Inputfolder, Outputfolder, N, Identifier_table, Identifier_name, Delimiter) {

  library("data.table")
  
  if (gsub("/", "", Inputfolder, ) == gsub("/", "", Outputfolder, )) stop("Change path!!! You may not want to overwrite the input files")

  PER <- unique(fread(paste0(Inputfolder, "/", Identifier_table, ".csv"), stringsAsFactors = F)[[Identifier_name]])
  PER <- sample(PER, size = N)



  files <- list.files(Inputfolder, pattern = paste0("*.", "csv"))
  dir.create(Outputfolder, showWarnings = FALSE)


  for (i in files) {
    File <- unique(fread(paste0(Inputfolder, "/", i), stringsAsFactors = F))
    if (i == "OBSERVATION_PERIODS.csv") File <- File[op_meaning == "ambulatory_hopsital_visit", op_meaning := "ambulatory_hospital_visit"]

    if (any(colnames(File) == Identifier_name)) {
      File <- File[get(Identifier_name) %in% PER, ]
      fwrite(File, file = paste0(Outputfolder, "/", i), sep = Delimiter, col.names = T, row.names = F, na = "", append = F)
    } else {
      fwrite(File, file = paste0(Outputfolder, "/", i), sep = Delimiter, col.names = T, row.names = F, na = "", append = F)
    }
  }
}

#file_folder <- dirname(rstudioapi::getSourceEditorContext()$path)

CreateSampleCDM(
  Inputfolder = "H:/RTI_SIM_CSV_1000k_20220610/",
  Outputfolder = "H:/RTI_SIM_CSV_10k_20220610/",
  Identifier_name = "person_id",
  Delimiter = ";",
  N = 10000,
  Identifier_table = "PERSONS"
)


