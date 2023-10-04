


SeparateCodes <- function(codesheet,code_col,separator,vars){
  new2 <-list()
    
    for(j in code_col){
    
    codesheet_k <- codesheet[!is.na(codesheet[[j]]),c(vars,j)]
    codesheet1 <- codesheet_k[grepl(pattern = separator,codesheet_k[[j]]), c(vars,j)]
    codesheet2 <- codesheet_k[!grepl(pattern = separator,codesheet_k[[j]]), c(vars,j)]
    codesheet2$Code_new <- codesheet2[[j]]
    
    new <-list()
    
        for(i in 1:nrow(codesheet1)){
        new[[i]] <- cbind(codesheet1[i,][rep(seq_len(nrow(codesheet1[i,])), each = length(strsplit(as.character(codesheet1[i,j]),separator)[[1]])),], "Code_new" =  strsplit(as.character(codesheet1[i,j]), ',')[[1]])
        
        }
    
    file <- do.call(rbind,new)
    rm(new)
    file2 <- rbind(file,codesheet2)
    file2$system <- j
    colnames(file2)[colnames(file2) == j] <- 'Original code'
    new2[[j]] <- file2
    rm(file,codesheet_k, codesheet1,codesheet2,file2)
    
    }
    
  file3 <- do.call(rbind,new2)
  rm(new2)

return(file3)
}




