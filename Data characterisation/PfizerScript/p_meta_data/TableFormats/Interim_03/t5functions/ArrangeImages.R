# organise images

test <- list.files(path = "C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/TableFormats/Interim_03/g_intermediate/plots")

library(stringr)
strings = c("TGAS_1121", "MGAS_1432", "ATGAS_1121") 
strings %>% str_replace(".*_", "")
# [1] "_1121" "_1432" "_1121"
# Or:
strings %>% str_replace("^[A-Z]*", "")
# [1] "_1121" "_1432" "_1121"

t1 <- test %>% str_replace(".* - ", "")
t1
t1_5 <- unique(t1)
t1_5



t2 <- t1 %>% str_replace(".pdf", "")
t2

t3 <- t2 %>% str_replace_all(" ", "_")
t3

t4 <-unique(t3)
t4

# pad <- "C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/TableFormats/Interim_03/g_intermediate/PlotDirectories/"
pad <- "C:/Temp/PlotDirectories/"



dir <- paste0(pad,t4)




dir

dir.create(pad)
# dir.create("C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/TableFormats/Interim_03/g_intermediate/PlotDirectories")

# dir.create(path=dir[1])

for(i in 1:length(t4)){
  if (dir.exists(dir[i]) == FALSE){
  dir.create(path=dir[i])
    }

}
t4


DAPlist <- c("PEDIANET","PHARMO","EPICHRON","UOSL","SIDIAP")

install.packages("readxl")

library("readxl")
xcel <- read_excel("C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/TableFormats/Interim_03/t5functions/mapping pictures nieuw.xlsx",sheet = "Blad1") 

TargetPad <- "C:/Temp"
TargetExtendPad <- "/PlotDirectories/"
SourcePad <- "C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/TableFormats/Interim_03/g_intermediate"
Source1Pad <- "C:/Temp/"
Source2Pad <- "/Plots/"

for(j in 1:nrow(xcel)){
for (i in 1:length(DAPlist)){
pdf <- paste0(DAPlist[i]," - ",xcel[j,3])

TargetDir <- paste0(TargetPad,TargetExtendPad,xcel[j,2])
SourceDir <- paste0(SourcePad,Source2Pad)

SourcePDF <- paste0(SourceDir,pdf)
TargetPDF <- paste0(TargetDir,"/",pdf)

if (file.exists(SourcePDF)){
file.copy(SourcePDF, TargetPDF)
}
  }
}


#-------------------------------------------------------------------------------------------------------------------------------------------- 
#kopieren van fig 2 
#-------------------------------------------------------------------------------------------------------------------------------------------- 

TargetPad <- "C:/Temp"
TargetExtendPad <- "/PlotDirectories/"
TargetExtendPad2 <-"/fig2/" 
SourcePad <- "C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/TableFormats/Interim_03/g_intermediate"
Source1Pad <- "C:/Temp/"


for(j in 1:nrow(xcel)){
  for (i in 1:length(DAPlist)){
    pdf <- paste0(DAPlist[i],"_Fig2_",xcel[j,1],".pdf")
    pdf2 <- paste0("Fig2_",DAPlist[i],"_",xcel[j,2],".pdf")
    
    TargetDir <- paste0(TargetPad,TargetExtendPad,xcel[j,2],TargetExtendPad2)
    if (not(dir.exists(TargetDir))){dir.create(TargetDir)}
    SourceDir <- paste0(Source1Pad,DAPlist[i],"/")
    
    SourcePDF <- paste0(SourceDir,pdf)
    TargetPDF <- paste0(TargetDir,"/",pdf2)
    
    if (file.exists(SourcePDF)){
      file.copy(SourcePDF, TargetPDF)
    }
  }
}
library(png)
library(pdftools)

pad <- "C:/Temp/PlotDirectories/"

dirlist <- list.files(path = pad,recursive=TRUE)

paddirlist <- paste0(pad,dirlist)

paddirlist

for(j in 1:length(paddirlist)){
      pdfbestand <- paddirlist[j]
      if(file.exists(pdfbestand)){
        p1 <- pdftools::pdf_render_page(pdfbestand)
        png1 <- str_replace(pdfbestand,".pdf", "")
        png2 <- paste0(png1,".png")
        png::writePNG(p1, png2)
      }
  }
  
  
  




dirlist# file.copy
# file.rename
# file.


# fl = list.files(dirIn, full.names = TRUE)
# dn = list.files(dirIn, full.names = FALSE)



# ?str_replace_all
# rename UOSL with NHR

pad <- "C:/Temp/UOSL"

dirlist <- list.files(path = pad,recursive=FALSE)
dirlist

dirlist2 <- dirlist %>% str_replace_all("UOSL_","NHR_")
dirlist2

for(j in 1:length(dirlist)){
  bestand <- dirlist[j]
  p_bestand <- paste0(pad,"/",bestand)
  if(file.exists(p_bestand)){
    b2 <- str_replace(bestand,"UOSL_","NHR_")
    p_b2 <- paste0(pad,"/",b2)
    file.rename(p_bestand,p_b2)
  }
}

