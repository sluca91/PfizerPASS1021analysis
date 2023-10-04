

setwd(projectFolder)
setwd('..')
setwd('..')

dir_base <- getwd()

if(is.null(StudyName)){path_dir <- path}else{
  path_dir <- paste0(dir_base,"/CDMInstances/",StudyName,"/")
}


#Set the path to where you want your report to be saved(make sure that the output folder already exists)
output_dir <- paste0(projectFolder,"/g_output/")

pre_dir <- paste0(projectFolder,"/p_steps/")

g_intermediate <- paste0(projectFolder,"/g_intermediate/")
tmp <- paste0(g_intermediate,"tmp/")
populations_dir <- paste0(g_intermediate,"populations/")
concepts_dir <- paste0(g_intermediate,"concepts/")
medication_dir <- paste0(g_intermediate,"medications/")
vaccins_dir <- paste0(g_intermediate,"vaccins/")
meta_dir <- paste0(projectFolder,"/p_meta_data/")
matching_dir <- paste0(populations_dir, "Matching/")
dbmatching <- paste0(matching_dir,"matching.db")
aesi_dir <- paste0(populations_dir, "AESI/")
propensity_dir <- paste0(populations_dir, "propensity/")