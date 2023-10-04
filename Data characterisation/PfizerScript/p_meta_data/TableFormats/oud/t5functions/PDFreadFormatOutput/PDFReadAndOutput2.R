library(gridExtra)
library(pdftools)
dir <- 'H:/02 projecten/RWE/Pfizer/temp/'
file1 <- 'PEDIANET/PEDIANET_fig3.pdf' 
dirfile1 <- paste0(dir,file1)


file2 <- 'EPICHRON/EPICHRON_fig3.pdf' 
dirfile2 <- paste0(dir,file2)

file3 <- 'PHARMO/PHARMO_fig3.pdf' 
dirfile3 <- paste0(dir,file3)

file4 <- 'SIDIAP/SIDIAP_fig3.pdf' 
dirfile4 <- paste0(dir,file4)


# p1 <- magick::image_read_pdf(dirfile1)
p1 <- pdf_image(dirfile1)
p1

# p2 <- magick::image_read_pdf(dirfile2)
p2 <- pdf_image(dirfile2)
p2

# p3 <- magick::image_read_pdf(dirfile3)
p3 <- pdf_image(dirfile3)
p3

# p4 <- magick::image_read_pdf(dirfile4)
p4 <- pdf_image(dirfile4)
p4

# p11 <- pdftools::pdf_render_page(dirfile1)
# p22 <- pdftools::pdf_render_page(dirfile2)
# p33 <- pdftools::pdf_render_page(dirfile3)
# p44 <- pdftools::pdf_render_page(dirfile4)


# p12 <- grob(p11)
# p23 <- grob(p22)
# p34 <- grob(p33)
# p45 <- grob(p44)


# pdf1 <- "plot1.pdf"
# pdf2 <- "plot2.pdf"
# pdf3 <- "plot3.pdf"
# pdf4 <- "plot4.pdf"
# 
# # import plots from PDFs
# plot1 <- pdf_render_page(pdf1, page = 1)
# plot2 <- pdf_render_page(pdf2, page = 1)
# plot3 <- pdf_render_page(pdf3, page = 1)
# plot4 <- pdf_render_page(pdf4, page = 1)

# add titles to plots
title1 <- "PEDIANET"
title2 <- "EPICHRON"
title3 <- "PHARMO"
title4 <- "SIDIAP"

arrangedGrob <- grid.arrange(
  grobs = list(
    arrangeGrob(p1, top = title1),
    arrangeGrob(p2, top = title2),
    arrangeGrob(p3, top = title3),
    arrangeGrob(p4, top = title4)
  ),
  ncol = 2,
  nrow = 2
)

# export to single page PDF
pdf("all_plots.pdf")
grid.arrange(
  grobs = list(
    arrangeGrob(p1, top = title1),
    arrangeGrob(p2, top = title2),
    arrangeGrob(p3, top = title3),
    arrangeGrob(p4, top = title4)
  ),
  ncol = 2,
  nrow = 2
)
dev.off()
