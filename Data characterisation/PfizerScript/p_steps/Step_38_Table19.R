
Main <- function(){
w.dat <- readRDS(paste0(populations_dir, "weights.rds"))
calc.stat <- function(vec){
  Qval <- quantile(vec, probs = seq(0, 1, by = .01))
  mean.v <- mean(vec)
  sd.v <- sd(vec)

  pct <- c("25%","75%", "1%","99%", "0%", "100%")  
  idx.sel <- NULL
  for (i in 1:length(pct)){
    idx.sel[i] <- which(names(Qval) == pct[i])
  }
  
  
  quant.vec <- as.numeric(Qval[idx.sel])
  ## quant.vec <- c(Q1, Q3, P1, P99, min, max)
  
  val <- c(mean.v, sd.v, quant.vec)
  return(val)
}

test <- which(w.dat$IPTW_full_cor != w.dat$IPTW_full_cor_trim)
if (length(test) == 0) {cut.v <- NA} else {
  cut.v <- unique(w.dat$IPTW_full_cor_trim[test])
}

w.dist <- data.frame(calc.stat(w.dat$IPTW_full_cor_trim[which(w.dat$group == 0)]), 
                     calc.stat(w.dat$IPTW_full_cor_trim[which(w.dat$group == 1)]))
w.dist <- rbind(w.dist, rep(cut.v,2))
colnames(w.dist) <- c("vaccinated","unvaccinated")
rownames(w.dist) <- c("mean","sd","Q1","Q3","P1","P99","min","max","cutoff")

rm(w.dat)
write.csv(w.dist, file = paste0(output_dir, DAP, '_Table19.csv'))
saveRDS(w.dist, file = paste0(output_dir, DAP, '_Table19.rds'))
}

Main()