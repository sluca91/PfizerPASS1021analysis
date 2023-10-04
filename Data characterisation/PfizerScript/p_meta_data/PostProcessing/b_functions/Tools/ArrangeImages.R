
if(!require(readxl)){install.packages("readxl")}
library(readxl)


# DAPlist <- c("PEDIANET","PHARMO","EPICHRON","NHR","SIDIAP")


xcel <- read_excel(paste0(projectFolder,"/a_configuration/04_Mapping_dir_picture_pdf/Mapping_dir_picture_pdf.xlsx"),sheet = "Blad1") 

# Create a list with all the files that are in the d_output/02_plots directory
plotlist <- list.files(path = paste0(projectFolder,'/d_output/02_plots'))

# remove everything that is in front of - 
pl1 <- plotlist %>% str_replace(".* - ", "")
pl1

# remove the double pdfs from the list
pl1_5 <- unique(pl1)
pl1_5

# remove the .pdf extension
pl2 <- pl1 %>% str_replace(".pdf", "")
pl2

# replace spaces with underscores in the list
pl3 <- pl2 %>% str_replace_all(" ", "_")
pl3

# again remove any duplicates
pl4 <-unique(pl3)
pl4

# pl5 <- as.character(xcel[,4])

# define target path
TargetPath <- paste0(resultsFolder,"/PlotDirectories/")

# DirectoryList <- paste0(TargetPath,pl4)
# DirectoryList <- paste0(TargetPath,xcel[,4])


# DirectoryList

TargetPath

# remove the directory from the previous run
unlink(
    TargetPath
    , recursive = TRUE
)



dir.create(TargetPath)


# for(i in 1:length(pl4)){
#   if (dir.exists(DirectoryList[i]) == FALSE){
#   dir.create(path=DirectoryList[i])
#     }
# 
# }
# 
# pl4


TargetPad <- "C:/Temp/Pfizer"
TargetExtendPad <- "/PlotDirectories/"
SourcePad <- "C:/Users/jmaaskan/Documents/GitHub/Pfizer/Data characterisation/PfizerScript/p_meta_data/PostProcessing/d_output"
# Source1Pad <- "C:/Temp/Pfizer"
Source2Pad <- "/02_plots/"
j <- 1
i <- 1

for(j in 1:nrow(xcel)){
for (i in 1:length(DAPlist)){
  
if (dir.exists(paste0(TargetPad,TargetExtendPad,xcel[j,4])) == FALSE){
    dir.create(paste0(TargetPad,TargetExtendPad,xcel[j,4]))
}  
  
src_pdf <- paste0(DAPlist[i]," - ",xcel[j,3])
tgt_pdf <- paste0(DAPlist[i]," - ",xcel[j,5])

TargetDir <- paste0(TargetPad,TargetExtendPad,xcel[j,4])
SourceDir <- paste0(SourcePad,Source2Pad)

SourcePDF <- paste0(SourceDir,src_pdf)
TargetPDF <- paste0(TargetDir,"/",tgt_pdf)

if (file.exists(SourcePDF)){
file.copy(SourcePDF, TargetPDF)

  }
}
}  


#-------------------------------------------------------------------------------------------------------------------------------------------- 
# Figure 2 is being copied from every dap and renamed based on the title of the AESI which is corresponding in this case to a certain number
# and looked up in an xcelsheet. Also the name of the DAP that is used as the source of the specific file is added to the name of the PDF.
# and a file folder will be created with the name of the adverse event that contains the pdf's of every DAP with that speceific adverse event. 
# 
# Source: C:/temp/pfizer/NHR_Fig2_1.pdf    Target: C:/Temp/PlotDirectories/Guillain-Barre_syndrome/EPICHRON_Guillain-Barre syndrome.pdf
#                                                  C:/Temp/PlotDirectories/Guillain-Barre_syndrome/PEDIANET_Guillain-Barre syndrome.pdf  
#-------------------------------------------------------------------------------------------------------------------------------------------- 

TargetFolder <- resultsFolder
TargetExtendFolder <- "/PlotDirectories/"
TargetExtendFolder2 <-"/fig2/" 

Source1Folder<- resultsFolder
 j <- 1
 i <- 1

for(j in 1:nrow(xcel)){
  for (i in 1:length(DAPlist)){
    # SourceName: "PEDIANET_Fig2_1.pdf"
    pdf <- paste0(DAPlist[i],"_Fig2_",xcel[j,1],".pdf")
    # TargetName: "Fig2_PEDIANET_Guillain-Barre_syndrome.pdf"
    pdf2 <- paste0("Fig2_",DAPlist[i],"_",xcel[j,4],".pdf")
    
    # TargetParentDir: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig2/"
    TargetParentDir <- paste0(TargetFolder,TargetExtendFolder,xcel[j,4])

    # TargetDir: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig2/"
    TargetDir <- paste0(TargetFolder,TargetExtendFolder,xcel[j,4],TargetExtendFolder2)
    
    if (not(dir.exists(TargetParentDir))){dir.create(TargetParentDir)}
    if (not(dir.exists(TargetDir))){dir.create(TargetDir)}
    
    # SourceDir:"C:/Temp/Pfizer/PEDIANET/"
    SourceDir <- paste0(Source1Folder,"/",DAPlist[i],"/")
    
    # SourcePDF: "C:/Temp/Pfizer/PEDIANET/PEDIANET_Fig2_1.pdf"
    SourcePDF <- paste0(SourceDir,pdf)
    # TargetPDF: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig2/Fig2_PEDIANET_Guillain-Barre_syndrome.pdf"
    TargetPDF <- paste0(TargetDir,pdf2)
    
    if (file.exists(SourcePDF)){
      file.copy(SourcePDF, TargetPDF)
    }
  }
}


 #-------------------------------------------------------------------------------------------------------------------------------------------- 
 # figure1.pdf
 # Source: C:/temp/pfizer/NHR_Fig2_1.pdf    Target: C:/Temp/PlotDirectories/Guillain-Barre_syndrome/EPICHRON_Guillain-Barre syndrome.pdf
 #                                                  C:/Temp/PlotDirectories/Guillain-Barre_syndrome/PEDIANET_Guillain-Barre syndrome.pdf  
 #-------------------------------------------------------------------------------------------------------------------------------------------- 
 
 TargetFolder <- resultsFolder
 TargetExtendFolder <- "/PlotDirectories/"
 TargetExtendFolder3 <-"/Figure1/" 
 
 Source1Folder<- resultsFolder
 
 for (i in 1:length(DAPlist)){
   # SourceName: "PEDIANET_Figure1.pdf" = # TargetName: "PEDIANET_Figure1.pdf"
   
   pdf <- paste0(DAPlist[i],"_Figure1.pdf")
   
   # TargetDir: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig3/"
   TargetDir <- paste0(TargetFolder,TargetExtendFolder,TargetExtendFolder3)
   
   if (not(dir.exists(TargetDir))){dir.create(TargetDir)}
   
   # SourceDir:"C:/Temp/Pfizer/PEDIANET/"
   SourceDir <- paste0(Source1Folder,"/",DAPlist[i],"/")
   
   # SourcePDF: "C:/Temp/Pfizer/PEDIANET/PEDIANET_Fig2_1.pdf"
   SourcePDF <- paste0(SourceDir,pdf)
   # TargetPDF: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig2/Fig2_PEDIANET_Guillain-Barre_syndrome.pdf"
   TargetPDF <- paste0(TargetDir,pdf)
   
   if (file.exists(SourcePDF)){
     file.copy(SourcePDF, TargetPDF)
   }
 }
 
 
 
 
 
 
 
 
 #-------------------------------------------------------------------------------------------------------------------------------------------- 
 # fig3_1.pdf and figure1.pdf
 # Source: C:/temp/pfizer/NHR_Fig2_1.pdf    Target: C:/Temp/PlotDirectories/Guillain-Barre_syndrome/EPICHRON_Guillain-Barre syndrome.pdf
 #                                                  C:/Temp/PlotDirectories/Guillain-Barre_syndrome/PEDIANET_Guillain-Barre syndrome.pdf  
 #-------------------------------------------------------------------------------------------------------------------------------------------- 
 
 TargetFolder <- resultsFolder
 TargetExtendFolder <- "/PlotDirectories/"
 TargetExtendFolder3 <-"/fig3/" 
 
 Source1Folder<- resultsFolder

   for (i in 1:length(DAPlist)){
     # SourceName: "PEDIANET_Fig3_1.pdf" = # TargetName: "PEDIANET_Fig3_1.pdf"

     pdf <- paste0(DAPlist[i],"_Fig3_1.pdf")
     
     # TargetDir: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig3/"
     TargetDir <- paste0(TargetFolder,TargetExtendFolder,TargetExtendFolder3)
     
     if (not(dir.exists(TargetDir))){dir.create(TargetDir)}
     
     # SourceDir:"C:/Temp/Pfizer/PEDIANET/"
     SourceDir <- paste0(Source1Folder,"/",DAPlist[i],"/")
     
     # SourcePDF: "C:/Temp/Pfizer/PEDIANET/PEDIANET_Fig2_1.pdf"
     SourcePDF <- paste0(SourceDir,pdf)
     # TargetPDF: "C:/Temp/Pfizer/PlotDirectories/Guillain-Barre_syndrome/fig2/Fig2_PEDIANET_Guillain-Barre_syndrome.pdf"
     TargetPDF <- paste0(TargetDir,pdf)
     
     if (file.exists(SourcePDF)){
       file.copy(SourcePDF, TargetPDF)
     }
   }
 
 
 
 
 


# file.copy
# file.rename
# file.


# fl = list.files(dirIn, full.names = TRUE)
# dn = list.files(dirIn, full.names = FALSE)



# ?str_replace_all


# Start testcode how str_replace works

# library(stringr)
# strings = c("TGAS_1121", "MGAS_1432", "ATGAS_1121") 
# strings %>% str_replace(".*_", "")
# # [1] "_1121" "_1432" "_1121"
# # Or:
# strings %>% str_replace("^[A-Z]*", "")
# # [1] "_1121" "_1432" "_1121"

# end testcode how str_replace works
