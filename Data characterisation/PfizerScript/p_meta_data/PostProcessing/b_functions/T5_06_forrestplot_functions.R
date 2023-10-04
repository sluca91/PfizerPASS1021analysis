
# ----------------------------------------------------------------------------------------------------------------------------------
# forrestplot function made by roel
# 
# ----------------------------------------------------------------------------------------------------------------------------------

# d = as.data.frame(event_tmp)
# 
# cn.var.ylab = c("group") 
# cn.var.ylab2 = c("band") 
# 
# 
# cn.sufc = c('IR',"lb","ub")
# cn.sufc.l = c('IR','LL','UL')
# cn.IR = 'IR' 
# cn.LL = 'lb'	
# cn.UL = 'ub' 
# title = paste0(scheme[z,][["Event_name"]]," - ", scheme[z,][["DAP"]])
# log.scale = F
# log.xaxis = T
# x.label = 'Incidence rate/10.000 PY (+ 95% CI)'
# y.group = "group"
# sep = NULL


my_forestplot2 <- function(
    
  ## Parameters
  d,                                                         ## Data object
  cn.var.ylab,                      ## Column names (string) of columns which make up the label (can be a vector, see example below)
  cn.var.ylab2 = NULL, 
  #suffix columns
  cn.sufc,    ## Vector of columns names of the columns you want R to show behind the graph
  cn.IR,                                  ## Column name (string) of IR
  cn.LL,                                  ## Column name (string) of CI LL of IR
  cn.UL,      ## Column name (string) of CI UL of IR
  title = 'Forest plot',
  log.scale = F,
  log.xaxis = F,
  cn.sufc.l, ## column text
  x.label = NULL,
  y.group = NULL,
  sep = NULL
){
  
  #x="IR"
  #d = as.data.frame(event_tmp)
  #ifelse(is.na(d[[x]]), d[paste0("l.",x)] <- "NA", d[paste0("l.",x)] <- as.character(format(d[[x]],digits = 2)))
  #ifelse(is.na(d[[x]]), d[paste0("v.",x)] <- as.numeric(0.0), d[paste0("v.",x)] <- as.numeric(d[[x]]))
  
  
  d <- as.data.table(d)
  for(x in cn.sufc){
    d <- d[,eval(paste0("l.",x)) := fifelse(is.na(get(x)), "NA" , as.character(format(get(x),digits = 2)))]
    
    if(log.xaxis | log.scale){
      d <- d[,eval(x) := fifelse(is.na(get(x)), 0.0000001 , get(x))]
      d <- d[,eval(x) := fifelse(get(x) == 0, 0.0000001 , get(x))]
    }else{
      d <- d[,eval(x) := fifelse(is.na(get(x)), 0 , get(x))]  
    }
  }
  d <- as.data.frame(d)
  
  
  
  
  ## Set to character
  for(i in 1:length(cn.var.ylab)) d[,cn.var.ylab[i]] <- as.character(d[,cn.var.ylab[i]])
  if(!is.null(cn.var.ylab2)) for(i in 1:length(cn.var.ylab2)) d[,cn.var.ylab2[i]] <- as.character(d[,cn.var.ylab2[i]])
  
  #### Here the log scale is applied if input says so. There is a separation of data in plot and numbers in suffix columns
  if (log.scale) {
    
    d[[paste0(cn.IR,"2")]] <- log(d[[cn.IR]])
    d[[paste0(cn.LL,"2")]] <- log(d[[cn.LL]])
    d[[paste0(cn.UL,"2")]] <- log(d[[cn.UL]])
    
    
    
  } else{
    
    d[[paste0(cn.IR,"2")]] <- d[[cn.IR]]
    d[[paste0(cn.LL,"2")]] <- d[[cn.LL]]
    d[[paste0(cn.UL,"2")]] <- d[[cn.UL]]
    
  }
  
  
  #for(i in 1:length(cn.sufc)){
  #  if (max(nchar(as.character(round(d[[cn.sufc[[i]]]],digits = 2)))) > 8) d[[cn.sufc[[i]]]] <-  format(d[[cn.sufc[[i]]]], scientific = T, digits = 2) else{
  #    d[[cn.sufc[[i]]]] <- as.character(format(round(d[[cn.sufc[i]]],2)),width=3)
  #  }
  #}
  
  ## Number of rows
  n <- nrow(d)
  
  ## X and  Y limits of plot
  xl <- range(c((d[[paste0(cn.IR,"2")]]+0.0000001), (d[[paste0(cn.LL,"2")]]+0.0000001), (d[[paste0(cn.UL,"2")]]+0.0000001),na.rm = TRUE))
  yl <- c(0, 1+n)
  
  d <- d[n:1,]
  
  ## Create plot
  
  if (log.xaxis) xlabel <- 'Log incidence rate (+ 95% CI)' else xlabel <- 'Incidence rate (+ 95% CI)'
  if (!is.null(x.label)) xlabel <- x.label 
  
  
  if(log.xaxis == F) plot(NULL, xlim = xl, ylim = yl, main = title, xlab = xlabel, ylab = '', axes = F, yaxs = 'i', cex=1)
  if(log.xaxis == T) plot(NULL, xlim = xl, ylim = yl, main = title, xlab = xlabel, ylab = '', axes = F, yaxs = 'i', cex=1, log = "x")
  grid(ny = NA, nx = NULL)                           
  
  abline(h = 1:n, col = 'lightgrey', lty = 'dotted', lwd=2)
  
  ## Add IR and CI bounds
  
  if(is.null(y.group)){
    points(x = d[[paste0(cn.IR,"2")]], y = c(1:n), pch = 16, cex = 1)
    segments(x0 = d[[paste0(cn.LL,"2")]], y0 = c(1:n), x1 = d[[paste0(cn.UL,"2")]])
    
  }else{
    
    d$row <- c(1:n)
    sym <- c(16, 17,21, 3)
    seg <- c("black","black","darkgray", "azure4" )
    #j=1
    
    vec <- unique(d[[y.group]])
    
    for(j in 1:length(vec)){
      
      d_tmp <- d[d[[y.group]] == vec[j],]  
      points(x = d_tmp[[paste0(cn.IR,"2")]], y = d_tmp[["row"]], pch = sym[j], cex = 1)
      segments(x0 = d_tmp[[paste0(cn.LL,"2")]], y0 = d_tmp[["row"]], x1 = d_tmp[[paste0(cn.UL,"2")]], col = seg[j])
      rm(d_tmp)
      
      
      
    }
    rm(sym, vec)
    
    
    
    #Add separator line
    ###
    
    if(!is.null(sep)){
      tmp <- as.data.table(d)[, tmp := seq_len(.N), by = sep][, tmp2 := .N , by = sep][tmp == tmp2,][["row"]]
      tmp <- tmp[1:length(tmp) - 1] + 0.5
      abline(h = tmp, col = "darkgrey", lwd = 1.3)
      rm(tmp)
    }
    
    ###
    
    
    #abline(h = d[d[["row"]] != nrow(d) & d[["Ageband"]] == "80+" & d[["Group"]] == "Vaccinated",][["row"]] - 0.5, col = "darkgrey", lwd = 1.3)
  }
  
  points(x = d[d[[paste0("l.",cn.IR)]] == "NA",][[paste0(cn.IR,"2")]], y = d[d[[paste0("l.",cn.IR)]] == "NA",][["row"]], pch = 4, cex = 1, col = "red")
  
  ## Add axes
  axis(1, las = 1, cex.axis = 1)
  
  box()
  
  ## Add label
  cn.var.ylab <- cn.var.ylab[!is.null(cn.var.ylab) & !is.na(cn.var.ylab)]
  d$lab <- do.call(paste, c(d[cn.var.ylab], sep = ' - '))
  
  if(!is.null(cn.var.ylab2)) cn.var.ylab2 <- cn.var.ylab2[!is.null(cn.var.ylab2) & !is.na(cn.var.ylab2)]
  if(!is.null(cn.var.ylab2)) d$lab2 <- do.call(paste, c(d[cn.var.ylab2], sep = ' - '))
  
  #############
  
  mtext(d$lab, side = 2, line = 1, outer = F, at = c(1:n), las = 2, cex = 0.6, adj = 1, font = 2)
  if(!is.null(cn.var.ylab2)) mtext(d$lab2, side = 2, line = 7, outer = F, at = c(1:n) - 0.5, las = 2, cex = 1, adj = 1, font = 1)
  
  mtext("x-axis is generated automatically", side = 1, line = 5, outer = F, cex = 0.5)
  
  for (i in 1:length(cn.sufc)){
    mtext(cn.sufc.l[i], side = 4, line = 2+((i-1)*3), outer = F, at = n + 1, cex = 0.6, las=2, font = 2)
    
    #mtext(format(d[[cn.sufc[i]]],digits=2), side = 4, line = 2+((i-1)*3), outer = F, at = c(1:n), cex = 0.6, las=2)
    mtext(d[[paste0("l.",cn.sufc[i])]], side = 4, line = 2+((i-1)*3), outer = F, at = c(1:n), cex = 0.6, las=2)
    
  }
  
  
  
  ## End function
}

