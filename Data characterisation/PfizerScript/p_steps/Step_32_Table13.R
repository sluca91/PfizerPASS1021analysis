## Author(s) : Ivonne Martin (I.Martin@umcutrecht.nl)
## Date      : 5 July 2022; edited 10 Jan 2023

fun_tab13 <- function(M_Studycohort, M_Studycohort_Covariates_T0, AESI_Cov_info){
  
  MS_Cov <- M_Studycohort_Covariates_T0
  d <- as.data.table(subset(M_Studycohort, group != "UNMATCHED"))
  
  rm(M_Studycohort, M_Studycohort_Covariates_T0)
  
  # Checking the name in the Table shells with the list of Covariates names created in sharepoint
  Tab13info <- subset(AESI_Cov_info, Table == 13)
  Tab13files <- sapply(Tab13info[,1],as.character)
  Tab13files <- str_trim(Tab13files, side = "right")
  
  IPW_covariates <- readRDS(paste0(populations_dir, "IPW_covariates.rds"))
  
  idx.pull.T13 <- NULL
  for (i in 1:length(Tab13files)){
    temp <- grep(toupper(Tab13files[i]), IPW_covariates$var)
    idx.pull.T13[i] <- ifelse(length(temp) != 0, temp, NA)
  }
  
  SMD_vec_T13 <- data.frame(cbind(toupper(Tab13files), round(as.numeric(IPW_covariates$SMD[idx.pull.T13]),3)))
  
  
  
  CNCov <- sapply(gsub("_T0","",colnames(MS_Cov)), as.character)
  vars <- tolower(Tab13files)
  
  # check whether there is covariate that does not exists in specific DAP
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
  
  idx.covvars <- unique(match(vars[idx.incl], tolower(CNCov)))
  
  MCov <- MS_Cov[,c(1,2,idx.covvars), with = FALSE]
  MCov[,3:ncol(MCov)] <- lapply(MCov[,3:ncol(MCov)],factor)
  
  combined_data <- merge(x = d, y = MCov, by = c("person_id","id"), all.x = T)
  combined_data$group <- relevel(as.factor(combined_data$group), ref = "EXPOSED")
  
  var.sel <- colnames(MCov)[3:ncol(MCov)]
  TabUnmatched <- CreateTableOne(vars = var.sel, strata = "group", factorVars = var.sel, smd = TRUE, data = combined_data, test = FALSE)
  
  t13 <- print(TabUnmatched, noSpaces = TRUE,smd = TRUE)
  
  create.output <- function(obj, col.ASD, opt = "out"){
    Mat.Val <- rw.names <- NULL
    col.ASD.out.v <- NULL
    
    for (i in 1:length(obj)){
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
      rw.names <- c(rw.names,names(obj)[[i]])
      Mat.Val <- rbind(Mat.Val,out.val)
      col.ASD.out.v <- c(col.ASD.out.v, col.ASD.out)
      #print(i)
    }
    #
    rownames(Mat.Val) <- rw.names
    
    if(opt == "out"){ Mat.Val <- cbind(Mat.Val,col.ASD.out.v)}
    return(Mat.Val)
  }
  
  #col.ASD <- t13[2:nrow(t13),3]
  col.ASD <- SMD_vec_T13
  Tb13 <- cbind(create.output(TabUnmatched$CatTable$EXPOSED, col.ASD, opt = "in"), create.output(TabUnmatched$CatTable$CONTROL, col.ASD, opt = "out"))
  
  if (length(idx.excl) != 0){
    Tb13[unlist(sapply(vars[idx.excl], function(x) grep(x, tolower(rownames(Tb13))))),] <- "NA"
  }
  
  ## save it for the intermediate output to be used in the propensity model 
  Tb13 <- cbind(Tab13info[,c(1:9)],Tb13) 
  colnames(Tb13)[10:14] <- c("V.n","V.p","C.n","C.p","ASD")
  Tb13$ind.in.PS <- ifelse(Tb13$ASD > 0.1, 1, 0)
  saveRDS(Tb13, file = paste0(propensity_dir,"prop_tab13.rds"))
  
  d13 <- as.data.frame(Tb13[,c(2,10:14)])
  d13.p <- d13
  d13.p[,c(2,4)] <- apply(d13.p[,c(2,4)], c(1,2), function(x) 
      x <- ifelse(as.numeric(x) < 5 & as.numeric(x)!= 0 & !is.na(as.numeric(x)), '<5', as.numeric(x)))
  d13.p <- rbind(c("N",t13[1,1], 100, t13[1,2],100, " "),d13.p)
  
  colnames(d13.p) <- c("LABEL","vac.n", "vac.perc", "ctr.n", "ctr.perc","ASD")
  
  ## To follow the format of table shells :
  d13.p <- rbind(d13.p[1,],rep(" ",6),d13.p[2:11,],rep("",6), d13.p[12:nrow(d13),])
  d13.p[c(1,2,13),1] <- c("N","Comedications","Other vaccinations")  
  
  rm(IPW_covariates)
  
  return(d13.p)}
