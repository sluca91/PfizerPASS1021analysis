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
# First clear the R studio environment to start with a clean slate          
#------------------------------------------------------------------------------------------------------------
rm(list=ls()) #clear environment

#------------------------------------------------------------------------------------------------------------
# Next install or activate the necessary packages
#------------------------------------------------------------------------------------------------------------
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

resultsFolder <- "H:/02 projecten/RWE/Pfizer/temp"

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)

InterMediateFolder <- paste0(projectFolder,'/g_intermediate')

# create a vector with a list of all the folders in the p_inputs folder
#folders <- list.dirs(paste0(projectFolder,"/p_inputs"), full.names =  T, recursive = F)
folders <- list.dirs(paste0(resultsFolder), full.names =  T, recursive = F)


# create a vector with the DAPs involved
#DAPlist <- c("ARS","CPRD","EPICHRON","PEDIANET","PHARMO")
DAPlist <- c("ARS","UOSL","EPICHRON","PEDIANET","PHARMO", "SIDIAP")

# Import tabletitles configuration file
TableTitles <-fread(paste0(projectFolder,'/TableTitles.csv'), na.strings = NULL ,colClasses = "character")[tableNameExtension %in% c("possible_graph", "possible") & !is.na(tabletitle_new) & tabletitle_new != "",]
TableTitles <- TableTitles[, tabletitle := tabletitle_new]


# create a vector with the tables involved
TableList <- pull(TableTitles, tableName)
# [c(1,2,9)] 
# rm(TableList2,TableList3)


#------------------------------------------------------------------------------------------------------------ 
#make defined functions available by sourcing them
#------------------------------------------------------------------------------------------------------------
source(paste0(projectFolder,"/t5functions/t5functions.R" ))

#------------------------------------------------------------------------------------------------------------
#Print in the console a list of the files in the intermediate folder and remove those files
#------------------------------------------------------------------------------------------------------------
# full.names = TRUE is filename and the path, full.names = False just the filenames
list.files(
  paste0(projectFolder,'/g_intermediate')
  ,full.names = F)


# unlink removes the files that are the result of the list.files functio. full.names has to be TRUE

unlink(
  list.files(
    paste0(projectFolder,'/g_intermediate')
    ,full.names = T)
) 

#------------------------------------------------------------------------------------------------------------
#Print in the console a list of the files in the intermediate folder and remove those files
#------------------------------------------------------------------------------------------------------------
# full.names = TRUE is filename and the path, full.names = False just the filenames
list.files(
  paste0(projectFolder,'/g_intermediate/temp_docx_output')
  ,full.names = F)


# unlink removes the files that are the result of the list.files functio. full.names has to be TRUE

unlink(
  list.files(
    paste0(projectFolder,'/g_intermediate/temp_docx_output')
    ,full.names = T)
) 

#------------------------------------------------------------------------------------------------------------
#Print in the console a list of the files in the intermediate folder and remove those files
#------------------------------------------------------------------------------------------------------------
# full.names = TRUE is filename and the path, full.names = False just the filenames
list.files(
  paste0(projectFolder,'/g_intermediate/plots')
  ,full.names = F)


# unlink removes the files that are the result of the list.files functio. full.names has to be TRUE

unlink(
  list.files(
    paste0(projectFolder,'/g_intermediate/plots')
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
                          intermediateFolder = InterMediateFolder,
                          tableList = TableList
                          )




###
#Make corrections due to wrong output of t4
source(paste0(projectFolder,"/t5functions/correct_labels.R" ))


###


#------------------------------------------------------------------------------------------------------------
# Print the tables for each DAP into one folder. One .docx document for one table
#------------------------------------------------------------------------------------------------------------

Print_Tables(folders =folders, 
             IntermediateFolder =  InterMediateFolder, 
             Daplist =  DAPlist, 
             tablelist =  TableList, 
             tabletitles =  TableTitles
             )



#------------------------------------------------------------------------------------------------------------
# Make forest plots
#------------------------------------------------------------------------------------------------------------

files <- c("Table16S3_1", "Table16S3_2", "Table16S3_3", "Table16S3_4", "Table16S3_5", "Table16S3_6", "Table16S3_7", "Table16S3_8", "Table16S3_9", "Table16S3_10", "Table16S3_11", "Table16S3_12", "Table16S3_13")

agebands <-  c("0-1" , "2-4", "5-11", "12-15", "16-17" , "18-29", "30-39", "40-49", "50-59", "60-64", "65-69", "70-79", "80+ years")

order <- c(1:length(files))

for(i in 1:length(files)){
  TEMP <- readRDS(paste0(InterMediateFolder, "/",files[i],".rds"))[, band := agebands[i]][, order := order[i]]
  
  if(i ==1){fpFile <- TEMP}else{fpFile <-  rbind(fpFile, TEMP)}
  
}

VAC <- fpFile[, c("DAP","System", "Event_name", "band","VAC_NumPerYear", "VAC_IR", "VAC_IR_CI.lb", "VAC_IR_CI.ub", "order"), with = F]
setnames(VAC, c("VAC_NumPerYear", "VAC_IR", "VAC_IR_CI.lb", "VAC_IR_CI.ub"), c("NumPerYear", "IR", "lb", "ub"))[, group := "Vaccinated"]


CTR <- fpFile[, c("DAP","System", "Event_name", "band","CTR_NumPerYear", "CTR_IR", "CTR_IR_CI.lb", "CTR_IR_CI.ub", "order"), with = F]
setnames(CTR, c("CTR_NumPerYear", "CTR_IR", "CTR_IR_CI.lb", "CTR_IR_CI.ub"), c("NumPerYear", "IR", "lb", "ub"))[, group := "Unvaccinated"]

fpFile <- rbind(VAC, CTR)[Event_name == "Coagulation disorders: thromboembolism", Event_name := "Coagulation disorders thromboembolism"]
rm(VAC, CTR)

fpFile <- fpFile[DAP == "UOSL", DAP := "NHR"]

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
      
  pdf(paste0(projectFolder,"/g_intermediate/plots/",scheme[z,][["DAP"]]," - ",scheme[z,][["Event_name"]],".pdf"),width = (col * s.width), height = row * s.height, onefile = T)
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




###


#TEST_Table6 <-fread("H:/02 projecten/AstraZenecaT5/TEST_Table6.csv", na.strings = NULL ,colClasses = "character")
#formatstable6redux <- fread("H:/02 projecten/AstraZenecaT5/formatstable6redux.csv", na.strings = NULL ,colClasses = "character")









# set projectFolder as current working directory
# setwd(projectFolder)
# go two directories up in the ladder
# getwd()
# setwd('..')
# setwd('..')
# set projectFolder with the workingdirectory path
# projectFolder <- getwd()