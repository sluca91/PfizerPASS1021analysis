getParameters <- PEDIANET_list_Fig2[["PEDIANET_I_COVID19VAED_AESI"]][["plot"]][["data"]]

xExposed <-as.data.table(getParameters)[group == "EXPOSED", ][["time"]]
yExposed <-as.data.table(getParameters)[group == "EXPOSED", ][["surv"]]

plot(NULL, xlim = c(0,500), ylim = c(0,7000), xlab = "time", ylab = "N")
lines(x,y)

?plot

# 
getCOVID19VAED_AESI <- PEDIANET_list_Fig2[["PEDIANET_I_COVID19VAED_AESI"]]

plot_COVID19VAED_AESI <- getCOVID19VAED_AESI[["plot"]]
table_COVID19VAED_AESI <- getCOVID19VAED_AESI[["table"]]
cumevents_COVID19VAED_AESI <- getCOVID19VAED_AESI[["cumevents"]]
datasurvplot_COVID19VAED_AESI <- getCOVID19VAED_AESI[["data.survplot"]]
datasurvtable <- getCOVID19VAED_AESI[["data.survtable"]]

library(ggplot2)
ggplot(getCOVID19VAED_AESI)



library(gridExtra)
library(grid)

getCOVID19VAED_AESI

ggarrange(getCOVID19VAED_AESI, getCOVID19VAED_AESI, getCOVID19VAED_AESI, getCOVID19VAED_AESI, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
