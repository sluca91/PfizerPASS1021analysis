#Author: Roel Elbers Drs.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/04/2023

#Aim: create an flowchart that gives an overview of all the scripts and it's inputs and outputs.

#Input 1: PROGRAM
#Output 1: flowchart in meta_dir in folder Codebook


#This packages gives problems with installation in some computers so they are not loaded in the packages script.
###
if(!require("DiagrammeR")){install.packages("DiagrammeR")}
suppressPackageStartupMessages(library(DiagrammeR))

if(!require("htmlwidgets")){install.packages("htmlwidgets")}
suppressPackageStartupMessages(library(htmlwidgets))

if(!require("webshot")){install.packages("webshot")}
suppressPackageStartupMessages(library(webshot))

if (is.null(Sys.which("phantomjs"))) {
  # If PhantomJS is not installed, install it
  webshot::install_phantomjs()
}

###
#Make a copy of the program object
PROGRAM_new <- PROGRAM[,.(PROGRAM, FILE, TYPE, FORMAT, FUNCTIONS,FOLDER_VAR)]
#Make a better name for the in and outputs of the scripts also including the type of file (csv,rds,db)
PROGRAM_new <- PROGRAM_new[, FILE := paste0(FILE, " (",FORMAT,")")]

# The flowchart is created using DiagrammeR::grViz. This functions needs 3 inputs
#  1. blocks for the scripts
#  2. blocks for the inputs and outputs of those scripts
#  3. arrows to connect the blocks from point 1 and 2

# 1
# Set the blocks for the scripts/processes
###
#Give an id to every script. This id will be used in the name of the block
x1 <- unique(PROGRAM_new$PROGRAM)
x2 <- 1:length(x1)
#Write the input string for the grViz function
processes <- paste0("process",x2," [fillcolor = coral1, label =  '",x1,"']", collapse = " ")
###

# 2
# Create the blocks for the input and output data sets.
###

#Give an id to every data set. This id will be used in the name of the block.
y1 <- unique(PROGRAM_new$FILE)
y2 <- 1:length(y1)

#Give colors for the different types of in and output data sets
# green -> tables form the CDM
# light blue -> information form the meta layer
# grey -> intermediate data files
# dark grey -> end point data files

col <- rep(NA, length(y1))
col[substr(y1, nchar(y1) - 4,nchar(y1)) == "(CDM)"] <- "darkseagreen1"
col[y1 %in% unique(PROGRAM_new[FOLDER_VAR == "meta_dir",][["FILE"]])] <- "cyan"

#Determine if an output is used as an input elsewhere, If not it is an end point.
end <- unique(PROGRAM_new[TYPE == "OUTPUT",][["FILE"]])[!unique(PROGRAM_new[TYPE == "OUTPUT",][["FILE"]]) %in% unique(PROGRAM_new[TYPE == "INPUT",][["FILE"]])]
col[y1 %in% end] <- "azure4" 
col[is.na(col)] <- "azure2"

#Write the input string for the grViz function
datasets <- paste0("data",y2," [label = '",y1,"', shape = folder, fillcolor = ",col,"]", collapse = " ")
###

# 3
# Create the arrows that connect the blocks.
###
#Make tables with the id's to join to the program file. These id's are representing the block names.
id_processes <- as.data.table(cbind(x1,x2))
id_datasets <- as.data.table(cbind(y1,y2))

#Add the id's by a join
temp0 <- unique(PROGRAM_new[,.(PROGRAM,FILE,TYPE)])
temp2 <- merge(temp0, id_processes, by.x = "PROGRAM", by.y = "x1" )
temp2 <- merge(temp2, id_datasets, by.x = "FILE", by.y = "y1" )

#Make the strings for the arrows. Note that input and outut need a reversed direction.
temp2 <- temp2[TYPE == "INPUT" , arrow := paste0("data",y2," -> process",x2) ]
temp2 <- temp2[TYPE == "OUTPUT" , arrow := paste0("process",x2," -> data",y2) ]

#Write the input string for the grViz function
arrows <- paste0(temp2$arrow, collapse = " ")
###

#Write the object
###
tmp2 <- DiagrammeR::grViz(paste0("digraph {

      graph [layout = dot]
      
      # define the global styles of the nodes. We can override these in box if we wish
      node [shape = rectangle, style = filled, fillcolor = Linen]
      
      ",processes," ",datasets,"
      
      # edge definitions with the node IDs
      ",arrows,"


}



"))

###

#Print the flowchart to the rstudio viewer
tmp2

#Make a html widget form it so it can be saved instead of save it from the viewer manually
saveWidget(tmp2, paste0(meta_dir,"Codebook/","Flowchart",".html"))

#This html is not in the correct pixel size and therefore not sharp when zooming in. Therefor make an automated screenshot in which the
#pixel size can be increased using zoom.
webshot(url = paste0(meta_dir,"Codebook/","Flowchart",".html"), file = paste0(meta_dir,"Codebook/","Flowchart",".png"), zoom = 10)
#, vwidth = 441 * 1.6, vheight = 351 * 1.6



rm(PROGRAM_new, x1,x2, y1, y2, col, end, datasets, processes, id_datasets, id_processes, temp0, temp2, arrows)
