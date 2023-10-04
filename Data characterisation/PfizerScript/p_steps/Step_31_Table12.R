## Author(s) : Ivonne Martin (I.Martin@umcutrecht.nl)
## Date      : 8 Aug 2022; edited 10 Jan 2023

#########################################################################################################
## Function to create table 12
#########################################################################################################

fun_tab12 <- function(M_Studycohort, M_Studycohort_Covariates_T0, AESI_Cov_info){
  
  MS_Cov <- M_Studycohort_Covariates_T0
  d <- as.data.table(subset(M_Studycohort, group != "UNMATCHED"))
  
  rm(M_Studycohort, M_Studycohort_Covariates_T0)
  
  # Checking the name in the Table shells with the list of Covariates names created in sharepoint
  Tab12info <- subset(AESI_Cov_info, Table == 12)
  Tab12files <- sapply(subset(AESI_Cov_info, Table == 12)[,1],as.character)
  
  # There are multiple variable names, such as CDC and Charlson
  Tab12files[grep("L_CHARLSON_COV", Tab12files)] <- c("L_CHARLSON_COV", rep("",length(grep("L_CHARLSON_COV", Tab12files)) - 1))
  Tab12files[grep("V_CDC_COV", Tab12files)] <- c("V_CDC_COV", rep("",length(grep("V_CDC_COV", Tab12files)) - 1))
  Tab12files <- Tab12files[-which(Tab12files == "" | Tab12files == "NA")]
  Tab12files <- str_trim(Tab12files, side = "right")
  
  IPW_covariates <- readRDS(paste0(populations_dir, "IPW_covariates.rds"))
  
  idx.pull <- NULL
  for (i in 1:length(Tab12files)){
    temp <- grep(toupper(Tab12files[i]), IPW_covariates$var)
    idx.pull[i] <- ifelse(length(temp) != 0, temp, NA)
  }
  
  
  SMD_vec <- data.frame(toupper(Tab12files), round(as.numeric(IPW_covariates$SMD[idx.pull]),3))
  
  
  CNCov <- sapply(gsub("_T0","",colnames(MS_Cov)), as.character)
  
  vars <- tolower(Tab12files)
  if(any(!vars %in% tolower(CNCov))) warning ('Some variables are not in the M_Studycohort_Covariates_T0, the corresponding column will be filled with NAs')
  
  idx.excl <- which(!vars %in% tolower(CNCov))
  
  if(length(idx.excl) != 0){
    Mat.excl <- matrix(3,nrow = nrow(MS_Cov), ncol = length(idx.excl))
    colnames(Mat.excl) <- paste0(toupper(vars[idx.excl]),'_T0')
    MS_Cov <- cbind(MS_Cov,Mat.excl)
  }
  
  # new colnames after combining
  CNCov <- sapply(gsub("_T0","",colnames(MS_Cov)), as.character)
  colnames(MS_Cov) <- CNCov 
  
  idx.incl <- which(vars %in% tolower(CNCov))
  
  #idx.covvars <- unique(match(vars[idx.incl], tolower(CNCov)))
  idx.covvars <- match(vars[idx.incl], tolower(CNCov))
  
  rm(CNCov)
  ## creating binary variables for the presence/absence of 'N_CEREBROVASCULAR_AESI' and 'R_RESPCHRONICALGORITHM_COV' . 
  
  ## Cerebrovascular
  #if (all(grep('N_CEREBROVASCULAR_AESI', Tab12files) %in% idx.excl)){
  #  MS_Cov$N_CEREBROVASCULAR_bin_AESI <- rep(3,nrow(MS_Cov))
  #} else {
  #  MS_Cov$N_CEREBROVASCULAR_bin_AESI <- ifelse(MS_Cov$N_CEREBROVASCULAR_AESI != 0,1,0)
  #}
  # Respiratory
  #if (all(grep('R_RESPCHRONICALGORITHM_COV', Tab12files) %in% idx.excl)){
  #  MS_Cov$R_RESPCHRONICALGORITHM_bin_COV <- rep(3,nrow(MS_Cov))
  #} else {
  #  MS_Cov$R_RESPCHRONICALGORITHM_bin_COV <- ifelse(MS_Cov$R_RESPCHRONICALGORITHM_COV != 0,1,0)
  #}
  
  CNCov <- colnames(MS_Cov)
  
  MCov <- MS_Cov[,c(1,2,idx.covvars), with = FALSE]
  MCov[,3:ncol(MCov)] <- lapply(MCov[,3:ncol(MCov)],factor)
  
  #colnames(MCov)[grep("N_CEREBROVASC_AESI", colnames(MCov))] <- paste0(colnames(MCov)[grep("N_CEREBROVASC_AESI", colnames(MCov))],c("_1","_2"))
  #colnames(MCov)[grep("R_RESPCHRONICALGORITHM_COV", colnames(MCov))] <- paste0(colnames(MCov)[grep("R_RESPCHRONICALGORITHM_COV", colnames(MCov))],c("_1","_2"))
  
  combined_data <- merge(x = d, y = MCov, by = c("person_id","id"), all.x = T)
  combined_data$group <- relevel(as.factor(combined_data$group), ref = "EXPOSED")
  
  var.sel <- colnames(MCov)[3:ncol(MCov)]
  TabUnmatched <- CreateTableOne(vars = var.sel, strata = "group", factorVars = var.sel, smd = TRUE, data = combined_data, test = FALSE)
  
  t12 <- print(TabUnmatched, catDigits = 2, contDigits = 2, noSpaces = TRUE,smd = TRUE,formatOptions = list(scientific = FALSE))
  create.output <- function(obj, col.ASD, opt = "out"){
    Mat.Val <- rw.names <- NULL
    col.ASD.out.v <- NULL
    
    for (i in 1:length(obj)){
      if(names(obj)[[i]] == "L_CHARLSON_COV" | names(obj)[[i]] == "V_CDC_COV"){
        # Here we need to be careful with its possible values, 
        out.val <- matrix(0,nrow=4,ncol = 2)
        n.lvl <- length(obj[[i]]$level)
        if (n.lvl == 1){ 
          # For example when there is only 3, because the considered DAP has no data 
          if(any(!c("0","1","2") %in% obj[[i]]$level)) { out.val <- matrix(NA, nrow = 4, ncol = 2)
          col.ASD.out <- c(NA, rep("",3)) 
          } else {
            out.val[c(as.numeric(obj[[i]]$level) + 2)] <- obj[[i]][5:6]
            col.ASD.out <- c(col.ASD[grep(names(obj)[[i]],col.ASD[,1]),2], rep("",3))
          }
        }
        if(n.lvl == 2){
          out.val[c(as.numeric(obj[[i]]$level) + 2),] <- round(as.matrix(obj[[i]][,5:6], ncol=2, nrow = 2),1)
          col.ASD.out <- c(col.ASD[grep(names(obj)[[i]],col.ASD[,1]),2], rep("",3))
        }      
        if (n.lvl == 3){
          out.val[c(as.numeric(obj[[i]]$level) + 2),] <- as.matrix(obj[[i]][,5:6], ncol = 2,nrow = 3)
          out.val <- round(matrix(unlist(out.val), ncol = 2),2)
          col.ASD.out <- c(col.ASD[grep(names(obj)[[i]],col.ASD[,1]),2], rep("",3))
        }
        out.val[1,] <- c("","")
      } else {
        
        if(all((nrow(obj[[i]]) != 2) & (nrow(obj[[i]]) < 3) & any(!obj[[i]]['level'] == 1))) {
          val.n <- diff(as.numeric(obj[[i]][c(5,1)]))
          val.perc <- as.numeric(val.n/obj[[i]][1])
          out.val <- round(as.numeric(c(val.n, val.perc)),2)
        } 
        
        if (nrow(obj[[i]]) == 2){
          out.val <- as.numeric(obj[[i]][which(obj[[i]]$level == 1),5:6])
          out.val <- c(round(out.val[1],3), round(out.val[2],2))
        }
        
        col.ASD.out <- col.ASD[grep(names(obj)[[i]],col.ASD[,1]),2]
      }
      
      if(names(obj)[[i]] == "L_CHARLSON_COV" | names(obj)[[i]] == "V_CDC_COV"){
        naam <- c(names(obj)[[i]],paste0(names(obj)[[i]],"_", c("0","1","2")))
      } else {
        naam <- names(obj)[[i]]
      }
      
      rw.names <- c(rw.names,naam)
      Mat.Val <- rbind(Mat.Val,out.val)
      col.ASD.out.v <- c(col.ASD.out.v, col.ASD.out)
      print(paste0("Calculating the ",i,"th covariate"))
    }
    rownames(Mat.Val) <- rw.names
    
    if(opt == "out"){ Mat.Val <- cbind(Mat.Val,col.ASD.out.v)}
    return(Mat.Val)
  }
  
  #col.ASD <- t12[which(t12[,3] != ""),3]
  col.ASD <- SMD_vec
  
  
  Tb12 <- cbind(create.output(TabUnmatched$CatTable$EXPOSED,col.ASD, opt = "in"), create.output(TabUnmatched$CatTable$CONTROL, col.ASD, opt = "out"))
  
  if (length(idx.excl) != 0){
    Tb12[unlist(sapply(vars[idx.excl], function(x) grep(x, tolower(rownames(Tb12))))),] <- "NA"
  }
  ## Prepare the output
  
  ## To insert the blank lines for the subheader
  idx.ins <- grep("IM_IMC_COV", rownames(Tb12))
  Tb12 <- cbind(Tab12info[,1:2],rbind(Tb12[1:idx.ins,],rep("",5), Tb12[(idx.ins +1):nrow(Tb12),]))
  
  ## save it for the intermediate output to be used in the propensity model 
  
  colnames(Tb12)[3:ncol(Tb12)] <- c("V.n","V.p","C.n","C.p","ASD")
  
  PS.indicator <- function(x){
    x1 <- as.numeric(as.character(x))
    val <- ifelse(x1 > 0.1, 1,0)
    return(val)
  }
  
  Tb12$ind.in.PS <- PS.indicator(Tb12$ASD)
  saveRDS(Tb12, file = paste0(propensity_dir,"prop_tab12.rds"))
  
  d12 <- data.frame(Tb12[,c(2,3:7)])
  d12.p <- d12
  d12.p[,c(2,4)] <- apply(d12.p[,c(2,4)], c(1,2), function(x) 
    x <- ifelse(as.numeric(x) < 5 & as.numeric(x)!= 0 & !is.na(as.numeric(x)), '<5', as.numeric(x)))
  colnames(d12.p) <- c("LABEL","vac.n", "vac.perc", "ctr.n", "ctr.perc","ASD")
  d12.p <- rbind(c("N",t12[1,1], 100, t12[1,2],100, " "),d12.p)
  
  # hard-coded removal for aesthetic table 
  d12.p[which(d12.p$LABEL %in% c("Charlson Comorbidity Index", "CDC at-risk groups","Surrogates of frailty")),c(2,4)] <- c("","")
  
  # colnames(d12) <- c("LABEL","vac.n", "vac.perc", "ctr.n", "ctr.perc","ASD")
  # d12 <- rbind(c("N",t12[1,1], 100, t12[1,2],100, " "),d12)
  rm(IPW_covariates)
  return(d12.p)
}
