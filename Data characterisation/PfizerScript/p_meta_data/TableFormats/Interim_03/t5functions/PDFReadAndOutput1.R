library(ggplot2)

install.packages("gridExtra")
# Let's say you create multiple plots using lapply()
library(gridExtra)

p <- lapply(names(mtcars), function(x) {
  ggplot(mtcars, aes(x)) + 
    geom_histogram()
})
# Save list of p plots:

getwd()

ggsave(
   filename = "plots.pdf", 
   plot = marrangeGrob(p, nrow=1, ncol=1), 
   width = 15, height = 9
)





library(ggplot2)
library(gridExtra)

pdf("plots.pdf", onefile = TRUE)
cuts <- unique(diamonds$cut)
for(i in 1:length(cuts)){
  dat <- subset(diamonds, cut==cuts[i])
  top.plot <- ggplot(dat, aes(price,table)) + geom_point() + 
    opts(title=cuts[i])
  bottom.plot <- ggplot(dat, aes(price,depth)) + geom_point() + 
    opts(title=cuts[i])
  grid.arrange(top.plot, bottom.plot)
}
dev.off()


install.packages("ggpubr")
library(ggpubr)


data("ToothGrowth")
head(ToothGrowth)

# mtcars 
data("mtcars")
mtcars$name <- rownames(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
head(mtcars[, c("name", "wt", "mpg", "cyl")])


bxp <- ggboxplot(ToothGrowth, x = "dose", y = "len",
                 color = "dose", palette = "jco")
bxp

dp <- ggdotplot(ToothGrowth, x = "dose", y = "len",
                color = "dose", palette = "jco", binwidth = 1)
dp


# Bar plot (bp)
bp <- ggbarplot(mtcars, x = "name", y = "mpg",
                fill = "cyl",               # change fill color by cyl
                color = "white",            # Set bar border colors to white
                palette = "jco",            # jco journal color palett. see ?ggpar
                sort.val = "asc",           # Sort the value in ascending order
                sort.by.groups = TRUE,      # Sort inside each group
                x.text.angle = 90           # Rotate vertically x axis texts
)
bp + font("x.text", size = 8)


# Scatter plots (sp)
sp <- ggscatter(mtcars, x = "wt", y = "mpg",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "cyl", palette = "jco", # Color by groups "cyl"
                shape = "cyl"                   # Change point shape by groups "cyl"
)+
  stat_cor(aes(color = cyl), label.x = 3)       # Add correlation coefficient
sp


ggarrange(bxp, dp, bp, sp + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

getwd()

pdf(file = "./temp.pdf", height = 5, width = 5)
plot(mtcars$mpg, mtcars$cyl, type = "p")
dev.off()
file.show("temp.pdf")

# same issue with ggplot
# pdf(file = "./temp.pdf", height = 5, width = 5)
# ggplot(data = mtcars, mapping = aes(x = mpg, y = cyl)) +
#   geom_point()
# dev.off()

p0 <- magick::image_read_pdf('./temp.pdf')
p0

p1 <- pdftools::pdf_render_page('./temp.pdf')
png::writePNG(p1, "temp.png")
file.show("temp.png")



dir <- 'H:/02 projecten/RWE/Pfizer/temp/'
file1 <- 'PEDIANET/PEDIANET_fig3.pdf' 
dirfile1 <- paste0(dir,file1)
p1 <- pdftools::pdf_render_page(dirfile1)
png::writePNG(p1, "PEDIANET_fig3.png")
file.show("PEDIANET_fig3.png")
file.show(dirfile1)





file2 <- 'EPICHRON/EPICHRON_fig3.pdf' 
dirfile2 <- paste0(dir,file2)

file3 <- 'PHARMO/PHARMO_fig3.pdf' 
dirfile3 <- paste0(dir,file3)

file4 <- 'SIDIAP/SIDIAP_fig3.pdf' 
dirfile4 <- paste0(dir,file4)


p1 <- magick::image_read_pdf(dirfile1)
p1

p2 <- magick::image_read_pdf(dirfile2)
p2

p3 <- magick::image_read_pdf(dirfile3)
p3

p4 <- magick::image_read_pdf(dirfile4)
p4

p11 <- pdftools::pdf_render_page(dirfile1)
p22 <- pdftools::pdf_render_page(dirfile2)
p33 <- pdftools::pdf_render_page(dirfile3)
p44 <- pdftools::pdf_render_page(dirfile4)

png::writePNG(p11, "temp.png")
file.show("temp.png")

ggarrange(p1, p2, p3, p4, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


?gridExtra
