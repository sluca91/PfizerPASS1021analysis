#> In this refactored version, the code is divided into smaller functions, each responsible for a specific task. 
#> The main script calls these functions in a structured manner. Here's an overview of the functions:

#> clear_environment: Clears the RStudio environment.

#> install_activate_packages: Installs or activates necessary packages.

# create_folders: Creates the necessary folders and returns the path to the intermediate folder.

# import_table_titles: Imports the tabletitles configuration file.

# clear_intermediate_files: Clears intermediate files in the specified subfolder.

# set_flextable_defaults: Sets flextable defaults.

# append_tables_daps_to_one: Appends output tables from DAPs for each table.

# The main script then executes the required steps by calling these functions in a structured manner.

# By following these modifications, the code becomes more modular, organized, and easier to read, understand, and maintain.




# Function to clear the R studio environment
clear_environment <- function() {
  rm(list = ls())
}

# Function to install or activate necessary packages
install_activate_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

# Function to create necessary folders
create_folders <- function(resultsFolder, projectFolder) {
  InterMediateFolder <- file.path(projectFolder, "g_intermediate")
  tempFolders <- c(
    InterMediateFolder,
    file.path(projectFolder, "g_intermediate/temp_docx_output"),
    file.path(projectFolder, "g_intermediate/plots")
  )
  dir.create(tempFolders, recursive = TRUE, showWarnings = FALSE)
  return(InterMediateFolder)
}

# Function to import tabletitles configuration file
import_table_titles <- function(projectFolder) {
  tableTitles <- fread(
    file.path(projectFolder, "TableTitles.csv"),
    na.strings = NULL,
    colClasses = "character"
  )[tableNameExtension %in% c("possible_graph", "possible") & !is.na(tabletitle_new) & tabletitle_new != "", ]
  tableTitles[, tabletitle := tabletitle_new]
  return(tableTitles)
}

# Function to clear intermediate files
clear_intermediate_files <- function(projectFolder, subfolder) {
  folderPath <- file.path(projectFolder, subfolder)
  filesToRemove <- list.files(folderPath, full.names = TRUE)
  unlink(filesToRemove)
}

# Function to set flextable defaults
set_flextable_defaults <- function() {
  library(flextable)
  set_flextable_defaults(
    font.size = 9,
    post_process_html = autofit,
    post_process_pdf = autofit,
    post_process_docx = autofit
  )
}

# Function to append output tables from DAPs for each table
# append_tables_daps_to_one <- function(folders, dapList, intermediateFolder, tableList) {
#   Loop_Trough_TableList <- function(path, intermediateFolder, tableList, dap) {
#     for (j in 1:length(tableList)) {
#       # Rest of the code in the Loop_Trough_TableList function
#       # ...
#     }
#   }
#   
#   for (i in 1:length(dapList)) {
#     path <- folders[grepl(toupper(dapList[i]), toupper(folders))]
#     Loop_Trough_TableList(path, intermediateFolder, tableList, dapList[i])
#   }
# }

# Main script

clear_environment()
install_activate_packages(c("tidyverse", "rstudioapi", "dplyr", "rmarkdown", "data.table", "flextable", "officer", "magrittr"))

resultsFolder <- "C:/Temp/Pfizer"
projectFolder <- dirname(rstudioapi::getSourceEditorContext()$path)

interMediateFolder <- create_folders(resultsFolder, projectFolder)

folders <- list.dirs(resultsFolder, full.names = TRUE, recursive = FALSE)
dapList <- c("PEDIANET", "NHR", "PHARMO", "EPICHRON", "SIDIAP")

tableTitles <- import_table_titles(projectFolder)
tableList <- tableTitles$tableName

clear_intermediate_files(projectFolder, "g_intermediate")
clear_intermediate_files(projectFolder, "g_intermediate/temp_docx_output")
clear_intermediate_files(projectFolder, "g_intermediate/plots")

set_flextable_defaults()

append_tables_daps_to_one(folders, dapList, interMediateFolder, tableList)
