# Code below is used to render the pdf image, copy it and write it back as a png file.


library(png)
library(pdftools)
library(stringr) # for the str_replace function
# C:\Temp\Pfizer\PlotDirectories\severeCovid19
pad <- paste0(resultsFolder,"/PlotDirectories/")

dirlist <- list.files(path = pad,recursive=TRUE)

paddirlist <- paste0(pad,dirlist)

paddirlist

for(j in 1:length(paddirlist)){
  pdfbestand <- paddirlist[j]
  if(file.exists(pdfbestand)){
    p1 <- pdftools::pdf_render_page(pdfbestand, dpi = 300)
    png1 <- str_replace(pdfbestand,".pdf", "")
    png2 <- paste0(png1,".png")
    png::writePNG(p1, png2)
  }
}
