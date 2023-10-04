#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 08/02/2022

##Aim
#

##in/output
#Input 1: PERSONS1
#Input 2: M_Studycohort3.rds
#Output 1: FIGURE1.pdf, FIGURE1.png

###
#Code if added to matching flow
#PROGRAM2 <-fread(paste0(meta_dir,"Program_table_shells.csv"), stringsAsFactors = F, na.strings = "")
#SCRIPT <- RUN_SCRIPT(name = "Step_99_Figure8.R")
#PERSONS <- readRDS(SCRIPT[["INPUT1"]][["path"]])[,.(person_id, FIRST_PFIZER, birth_date )][!is.na(FIRST_PFIZER),]
#M_Studycohort <- readRDS(SCRIPT[["INPUT2"]][["path"]])[,.(person_id, FIRST_PFIZER, birth_date, S_Group )][S_Group %in% c("EXPOSED", "UNMATCHED"), ]
###


#Get all PERSONS that started with a first Pfizer
PERSONS <- readRDS(paste0(tmp,"PERSONS1.rds"))[,.(person_id, FIRST_PFIZER, birth_date )][!is.na(FIRST_PFIZER),]

#Get all PERSONS that went through the matching and received a first Pfizer
M_Studycohort <- as.data.table(readRDS(paste0(populations_dir,"M_Studycohort.rds")))[,.(person_id, FIRST_PFIZER, birth_date, group )][group %in% c("EXPOSED", "UNMATCHED"), ]

bands1 <- as.data.table(CreateBands(c(0,16,40,50,60,70,80), NEWBORNS = F))
bands2 <- as.data.table(CreateBands(c(80,130), NEWBORNS = F))[band0 == "080-129", band0 := "80+"]

bands_new <- rbind(bands1, bands2)[,.(INT, band0)]
setnames(bands_new, "band0", "band")
rm(bands1, bands2)


#Get the excluded persons that received a first Pfizer and add to included persons with first Pfizer
EXCLUDED <- PERSONS[["person_id"]][!PERSONS[["person_id"]] %in% M_Studycohort[["person_id"]]]  
PERSONS <- PERSONS[person_id %in% EXCLUDED,][, group := "EXCLUDED"]
TEMP <- rbindlist(list(PERSONS,M_Studycohort ), use.names = T)

#Add age bands using bands for moment of first vaccination created at Step_00_SetParameters
TEMP <- TEMP[, AGE_T0 := floor(time_length(interval(birth_date, FIRST_PFIZER),"year"))]
TEMP <- merge(x = TEMP, y = bands_new, by.x = "AGE_T0",by.y = "INT")

#Create aggregation (for time) variables
TEMP <- TEMP[, week := format(as.Date(FIRST_PFIZER), "%W")]
TEMP <- TEMP[, year := year(FIRST_PFIZER)]
TEMP <- TEMP[, label1 := paste0(year,"-",week)]
TEMP <- TEMP[, month := month(FIRST_PFIZER)]
TEMP <- TEMP[, label2 := paste0(year,"-",month)]
TEMP[group == 'EXPOSED', `:=` (group = "MATCHED")]
#Change label names
#M_Studycohort <- M_Studycohort[S_Group == "", S_Group := ""]

TEMP1 <- TEMP[, .(nb = .N), by = c("group", "label1", "year", "week")]
setorder(TEMP, year, week )

TEMP2 <- TEMP[, .(nb = .N), by = c("group", "band")]
setorder(TEMP2, band)

stack <- sort(unique(TEMP[["group"]]))

stack2 <- data.table(group = sort(unique(TEMP[["group"]])))

stack2[group == 'EXCLUDED', `:=` (order = 3, color = '#f8766d')] 
stack2[group == 'UNMATCHED', `:=` (order = 2, color = '#00ba38' )]
stack2[group == 'MATCHED', `:=` (order = 1, color = '#619cff')]
setorder(stack2, order)

colors <- stack2[["color"]]
stack2 <- stack2[["group"]]

TEMP1 <- INPUTMATRIX(
  d = TEMP1,
  value = "nb",
  type = "none",
  var = "label1",
  var.v= unique(TEMP[["label1"]]),
  cat = "group" ,
  cat.v = stack2 , 
  per = F,
  mask = F
  
)


TEMP2 <- INPUTMATRIX(
  d = TEMP2,
  value = "nb",
  type = "none",
  var = "band",
  var.v= unique(TEMP2[["band"]]),
  cat = "group" ,
  cat.v =  stack2, 
  per = T,
  perdir = "col",
  mask = F
  
)

# PlotFigure1 function - could be placed in functions folder, but since the whole
# script is for Figure 1 only, it is placed here - ZKS
PlotFigure1 <- function(data_matrix1, data_matrix2, group_names, color_selection){
  par(mfrow = c(2,1))
  max.y <- round(max(colSums(data_matrix1)) + (0.1 * max(colSums(data_matrix1))),0)
  barplot(data_matrix1, 
          main = "", 
          ylim = c(0, 1.3*max.y), 
          xlab = "Week", 
          ylab = "N"  , 
          col = color_selection, 
          cex.axis = 0.65, 
          cex.names = 0.65, 
          las = 2, 
          cex.lab = 0.8)
  
  legend("top",
         legend = rev(group_names), 
         col = rev(color_selection), 
         pch = 15, 
         ncol = 3, 
         cex = 0.5, 
         bty = 'n')
  
  barplot(data_matrix2, 
          main = "", 
          ylim = c(0, 100), 
          xlab = "Ageband", 
          ylab = "%" , 
          col = color_selection, 
          cex.axis = 0.65, 
          cex.names = 0.65, 
          las = 1, 
          cex.lab = 0.8)
  
}

# Save as pdf, width and height in inch
pdf(paste0(output_dir, DAP,"_Figure1.pdf"), width = 8, height = 12, onefile = T)
PlotFigure1(TEMP1, TEMP2, stack2, colors)
dev.off() 

# Save as png, width and height in pixels, need to define resolution for 
# better visualization
png(paste0(output_dir, DAP,"_Figure1.png"), width = 800, height = 1200, res = 150)
PlotFigure1(TEMP1, TEMP2, stack2, colors)
dev.off()

rm(TEMP, TEMP1, TEMP2, PERSONS, M_Studycohort, colors, stack, bands_new)
