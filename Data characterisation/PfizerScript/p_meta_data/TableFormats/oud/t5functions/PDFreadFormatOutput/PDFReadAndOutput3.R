library(gridExtra)
library(grid)

dir <- 'H:/02 projecten/RWE/Pfizer/temp/'
file1 <- 'PEDIANET/PEDIANET_fig3.pdf' 
dirfile1 <- paste0(dir,file1)
file2 <- 'EPICHRON/EPICHRON_fig3.pdf' 
dirfile2 <- paste0(dir,file2)
file3 <- 'PHARMO/PHARMO_fig3.pdf' 
dirfile3 <- paste0(dir,file3)
file4 <- 'SIDIAP/SIDIAP_fig3.pdf' 
dirfile4 <- paste0(dir,file4)

p1 <- readPDF(dirfile1,pages = 1)
p2 <- readPDF(dirfile2,pages = 1)
p3 <- readPDF(dirfile3,pages = 1)
p4 <- readPDF(dirfile4,pages = 1)

# add titles to plots
title1 <- "PEDIANET"
title2 <- "EPICHRON"
title3 <- "PHARMO"
title4 <- "SIDIAP"

grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
             bottom = "Title1", left = "Title2", right = "Title3", top = "Title4")

# export to single page PDF
pdf("all_plots.pdf")
grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
             bottom = "Title1", left = "Title2", right = "Title3", top = "Title4")
dev.off()
