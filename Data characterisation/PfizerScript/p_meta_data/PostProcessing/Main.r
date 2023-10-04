#------------------------------------------------------------------------------------------------------------
# The purpose of this document is to create one word document with the output of all the tables in a specific 
# order and layout as configured by the configuration document called TableTitles.csv 
# It has information about:
#    -(tableName)           the name of the table;
#    -(tableNumber)         the number of the table;
#    -(tableNameExtension)  the optional extension a tableName can have;
#    -(ndaps)               the number of Database Access Providers(DAP) to be merged into one table;
#    -(nby_vars)            the number of byvariables used to merge different DAPs to each other;  
#    -(nheaderrows)         the number of headerrows the tableformat.csv specifies for the active table;
#    -(tableorder)          the order in which the tables appear in the final word document;   
#    -(tableextensionorder) the order in which tables with extensionnames have to be processed;  
#    -(tabletitle)          the title of the table;

#------------------------------------------------------------------------------------------------------------
# First the document will loop through all the tabletitles and take from every DAP the received outputtable 
# and append these into one table for each table in the tablelist.
# To these cumulative tables a variable named DAP will be added that is filled with the name of the 
# corresponding DAP so the information can be attributed to that DAP at a later moment
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
# Clear the R studio environment to start with a clean slate          
#------------------------------------------------------------------------------------------------------------
rm(list=ls()) #clear environment

#------------------------------------------------------------------------------------------------------------
# Next install or activate the necessary packages
#------------------------------------------------------------------------------------------------------------
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)


if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

if(!require(rstudioapi)){install.packages("dplyr")}               # Install dplyr
library(dplyr)       

if(!require(rmarkdown)){install.packages("rmarkdown")}
suppressPackageStartupMessages(library(rmarkdown))

if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))

if(!require(flextable)){install.packages("flextable")}
suppressPackageStartupMessages(library(flextable))
library(flextable)


if(!require(officer)){install.packages("officer")}
suppressPackageStartupMessages(library(officer))
library(officer)

if(!require(magrittr)){install.packages("magrittr")}
suppressPackageStartupMessages(library(magrittr))
library(magrittr)

#------------------------------------------------------------------------------------------------------------
#create folders and vectors and import configuration files
#------------------------------------------------------------------------------------------------------------

# resultsFolder is the folder where the results are placed that need to be postprocessed.
# So it is the input folder for the postProcessing
# It is the only path that needs to be adjusted manually
# Some of the output of the postprocessing is also placed there in the plotDirectories directory
# This can take up a lot of room since it is a collection of graphs. 
# So best to point it to a local drive and put your input data there


resultsFolder <- "C:/Temp/Pfizer"

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)

OutputFolder <- paste0(projectFolder,'/d_output')

# create a vector with a list of all the folders in the p_inputs folder
#folders <- list.dirs(paste0(projectFolder,"/p_inputs"), full.names =  T, recursive = F)
folders <- list.dirs(paste0(resultsFolder), full.names =  T, recursive = F)


# create a vector with the DAPs involved
    #DAPlist <- c("ARS","CPRD","EPICHRON","PEDIANET","PHARMO")
    # -------------------------------------------------------------------------------
    # nextline is the order in DAPS requested by Margaret Haugh
    # 
    # DAPlist <- c("ARS","PEDIANET","UOSL","PHARMO","EPICHRON", "SIDIAP")
    # ------------------------------------------------------------------------------- 
    
    # ! The data from Oslo is being delivered with the prefix UOSL_
    # ! This is NOT the name of the registry but of the DAP and should be changed into: NHR_
    # ! This can be done with the script: paste0(projectFolder,"/b_functions/Tools/DAP_to_datasource_rename.r")
    # ! Do not forget to change the name of the directory to NHR and in the DAPlist below it should also say NHR and not UOSL 


    # DAPlist <- c("TEST")
    DAPlist <- c("PEDIANET","NHR","PHARMO","EPICHRON","SIDIAP")
    # DAPlist <- c("PEDIANET","UOSL")
    # DAPlist <- c("PEDIANET")
    # DAPlist <- c("PHARMO")
    # DAPlist <- c("EPICHRON")

# Import tabletitles configuration file
TableTitles <-fread(paste0(projectFolder,'/a_configuration/01_TableTitles/TableTitles.csv'), na.strings = NULL ,colClasses = "character")[tableNameExtension %in% c("possible_graph", "possible") & !is.na(tabletitle_new) & tabletitle_new != "",]
TableTitles <- TableTitles[, tabletitle := tabletitle_new]


# create a vector with the tables involved
TableList <- pull(TableTitles, tableName)
# [c(1,2,9)] 
# rm(TableList2,TableList3)


#------------------------------------------------------------------------------------------------------------ 
#make defined functions available by sourcing them
#------------------------------------------------------------------------------------------------------------
source(paste0(projectFolder,"/b_functions/T5_01_main_functions.R" ))
source(paste0(projectFolder,"/b_functions/T5_02_print_functions.R" ))
source(paste0(projectFolder,"/b_functions/T5_03_test_functions.R" ))
source(paste0(projectFolder,"/b_functions/T5_04_merge_functions.R" ))
source(paste0(projectFolder,"/b_functions/T5_05_format_functions.R" ))
source(paste0(projectFolder,"/b_functions/T5_06_forrestplot_functions.R" ))

#------------------------------------------------------------------------------------------------------------
#Print in the console a list of the files in the intermediate folder and remove those files
#------------------------------------------------------------------------------------------------------------
# full.names = TRUE is filename and the path, full.names = False just the filenames
list.files(
  paste0(projectFolder,'/d_output')
  ,full.names = F)


# unlink removes the files that are the result of the list.files functio. full.names has to be TRUE

unlink(
  list.files(
    paste0(projectFolder,'/d_output')
    ,full.names = T)
) 

#------------------------------------------------------------------------------------------------------------
#Print in the console a list of the files in the intermediate folder and remove those files
#------------------------------------------------------------------------------------------------------------
# full.names = TRUE is filename and the path, full.names = False just the filenames
list.files(
  paste0(projectFolder,'/d_output/01_docx')
  ,full.names = F)


# unlink removes the files that are the result of the list.files functio. full.names has to be TRUE

unlink(
  list.files(
    paste0(projectFolder,'/d_output/01_docx')
    ,full.names = T)
) 

#------------------------------------------------------------------------------------------------------------
#Print in the console a list of the files in the intermediate folder and remove those files
#------------------------------------------------------------------------------------------------------------
# full.names = TRUE is filename and the path, full.names = False just the filenames
list.files(
  paste0(projectFolder,'/d_output/02_plots')
  ,full.names = F)


# unlink removes the files that are the result of the list.files functio. full.names has to be TRUE

unlink(
  list.files(
    paste0(projectFolder,'/d_output/02_plots')
    ,full.names = T)
) 




#------------------------------------------------------------------------------------------------------------
# Set flextable defaults
#------------------------------------------------------------------------------------------------------------

?set_flextable_defaults


set_flextable_defaults(
  font.size = 9,
  post_process_html = autofit,
  post_process_pdf = autofit,
  post_process_docx = autofit
)


#------------------------------------------------------------------------------------------------------------
# Call the function that appends the outputtables, that are available from the DAPS, into one for each  table
#------------------------------------------------------------------------------------------------------------

Append_Tables_Daps_To_One(folders = folders,
                          Daplist =  DAPlist ,
                          OutputFolder = OutputFolder,
                          tableList = TableList
                          )




# --------------------------------------------------------------------------------
# masking values when they are < 5 from the actual value to the value: <5
# When value percentage < 0.01 and value = 0 then value percentage = 0
# If value percentage equals pattern: 0.00 then value = < 0.01
# --------------------------------------------------------------------------------


source(paste0(projectFolder,"/Main_sub_01_format_and_mask.R" ))



#--------------------------------------------------------------------------------
# Make corrections due to wrong output of t4
#--------------------------------------------------------------------------------
source(paste0(projectFolder,"/Main_sub_02_correct_labels.R" ))


###


#--------------------------------------------------------------------------------
# Print the tables for each DAP into one folder. One .docx document for one table
#--------------------------------------------------------------------------------

Print_Tables(folders =folders, 
             OutputFolder =  OutputFolder, 
             Daplist =  DAPlist, 
             tablelist =  TableList, 
             tabletitles =  TableTitles
             )

# --------------------------------------------------------------------------------
# the AESI tables in this case table 16 and table 20 have to be split in different
# tables based on type of AESI and stacked with the information of the several daps.
# --------------------------------------------------------------------------------

source(paste0(projectFolder,"/Main_sub_03_word_output_AESI.R" ))





# -------------------------------------------------------------------------------
# Run Roels ForrestPlots
# -------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------
# Make forest plots
#------------------------------------------------------------------------------------------------------------

files <- c("Table16S31","Table16S32", "Table16S33", "Table16S34", "Table16S35", "Table16S36", "Table16S37", "Table16S38", "Table16S39", "Table16S310", "Table16S311", "Table16S312", "Table16S313")

agebands <-  c("0-1" , "2-4", "5-11", "12-15", "16-17" , "18-29", "30-39", "40-49", "50-59", "60-64", "65-69", "70-79", "80+ years")

order <- c(1:length(files))

for(i in 1:length(files)){
  TEMP <- readRDS(paste0(OutputFolder, "/",files[i],".rds"))[, band := agebands[i]][, order := order[i]]
  
  if(i ==1){fpFile <- TEMP}else{fpFile <-  rbind(fpFile, TEMP)}
  
}

VAC <- fpFile[, c("DAP", "Event_name", "band","VAC_NumPerYear", "VAC_IR", "VAC_IR_CI.lb", "VAC_IR_CI.ub", "order"), with = F]
setnames(VAC, c("VAC_NumPerYear", "VAC_IR", "VAC_IR_CI.lb", "VAC_IR_CI.ub"), c("NumPerYear", "IR", "lb", "ub"))[, group := "Vaccinated"]


CTR <- fpFile[, c("DAP", "Event_name", "band","CTR_NumPerYear", "CTR_IR", "CTR_IR_CI.lb", "CTR_IR_CI.ub", "order"), with = F]
setnames(CTR, c("CTR_NumPerYear", "CTR_IR", "CTR_IR_CI.lb", "CTR_IR_CI.ub"), c("NumPerYear", "IR", "lb", "ub"))[, group := "Unvaccinated"]

fpFile <- rbind(VAC, CTR)[Event_name == "Coagulation disorders: thromboembolism", Event_name := "Coagulation disorders thromboembolism"]
rm(VAC, CTR)

# fpFile <- fpFile[DAP == "UOSL", DAP := "NHR"]

lapply(c("NumPerYear", "IR", "lb", "ub"), function(x) fpFile <- fpFile[, eval(x) := as.numeric(get(x))])

scheme <- unique(fpFile[, .(DAP, Event_name)])
#expand.grid(unique(fpFile[["DAP"]]), unique(fpFile[["Event_name"]]))

scheme1 <- unique(fpFile[, .(Event_name)])[1,]
scheme2 <- unique(fpFile[, .(DAP)])

for(z in 1:nrow(scheme)){
  
  s.width <- 12
  s.height <- 7
  row <- 1 
  col <- 1
  
  pdf(paste0(projectFolder,"/d_output/02_plots/",scheme[z,][["DAP"]]," - ",scheme[z,][["Event_name"]],".pdf"),width = (col * s.width), height = row * s.height, onefile = T)
  par(mfrow=c(row,col),mar = c(10.1, 10.1, 1.1, 10.1),oma = c(0,9,0,6))
  
  #for(t in 1:nrow(scheme2)){
  
  event_tmp <- fpFile[Event_name == scheme[z,][["Event_name"]] & DAP == scheme[z,][["DAP"]],] 
  setorder(event_tmp, order,group)
  event_tmp <- event_tmp[group == "Vaccinated", band := ""]
  
  my_forestplot2(
    d = as.data.frame(event_tmp), 
    cn.var.ylab = c("group"), 
    cn.var.ylab2 = c("band"),
    cn.sufc = c('IR',"lb","ub"), 
    cn.sufc.l = c('IR','LL','UL'),
    cn.IR = 'IR', 
    cn.LL = 'lb',	
    cn.UL = 'ub', 
    title = paste0(scheme[z,][["DAP"]]),
    log.scale = F,
    log.xaxis = T,
    x.label = 'Log incidence rate/10.000 PY (+ 95% CI)',
    y.group = "group",
    sep = NULL
    
  )
  
  rm(event_tmp)
  gc()
  
  #}
  
  dev.off()
  
  
}





source(paste0(projectFolder,"/b_functions/Tools/ArrangeImages.R" ))

source(paste0(projectFolder,"/b_functions/Tools/copy_pdf_render_and_write_as_png.R" ))


# When everything has run the map: paste0(resultsFolder,"/plotDirectories") and the map: paste0(OutputFolder,"/01_docx") 
# have to be zipped and send to Daniel Weibel 


