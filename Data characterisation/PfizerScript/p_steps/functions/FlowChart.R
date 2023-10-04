
FlowChart2 <- function(file, expr_list, group_by_any, exclude = F){

TEMP <- copy(file)
#j=1

for (j in 1:length(expr_list)){

  var <- names(expr_list)[[j]]
  
  TEMP <- copy(file)
  TEMP <- TEMP[, eval(var) :=  fifelse(eval(expr_list[[j]]), T, F)][, c(group_by_any, var), with = F]
  
  TEMP <- TEMP[, .(Met =  any(get(var))) , by = group_by_any][, Critiria := var]
  
  if(j == 1) TEMP2 <- TEMP
  if(j > 1) TEMP2 <- rbindlist(list(TEMP, TEMP2), fill = T, use.names = T)
  
  if(exclude) file <- file[eval(expr_list[[j]]),]
  
  rm(TEMP, var)
  gc()
  
}  

form <- paste0(paste0(group_by_any, collapse = " + ")," ~ ", "Critiria")

TEMP2 <- dcast(TEMP2 , formula = form, value.var = "Met" )

TEMP2[[deparse(substitute(expr_list))]] <- rowSums(TEMP2[, names(expr_list) , with = F]) == length(expr_list)

setcolorder(TEMP2, names(expr_list))

if(!exclude) return(TEMP2)
if(exclude){
  
  list <- list()
  
  list[["file"]] <- file
  list[["flowchart"]] <- TEMP2
  
  return(list)
  
  
  }

}

test <- FlowChart2(
        file = SPELLS,
        group_by_any = "person_id",
        expr_list = Second_CheckList,
        exclude = F
)

Flowchart <- test$flowchart




Flowchart <- function(file, expr_list, id = NULL, type = 'exclude', strata = NULL){    
  
  FlowChart <- list()
  
  file <- copy(file)
  
  for (j in 1:length(expr_list)){
    
    before <- nrow(file)
    if(!is.null(id)) before2 <- length(unique(file[[id]]))
    
    file <- file[eval(expr_list[[j]]),]
    
    after <- nrow(file)
    if(!is.null(id)) after2 <- length(unique(file[[id]]))
    
    FlowChart[[paste("Step_",j)]]$step <- names(expr_list)[j]
    FlowChart[[paste("Step_",j)]]$before_rows <- before
    FlowChart[[paste("Step_",j)]]$after_rows <- after
    
    if(!is.null(id)) FlowChart[[paste("Step_",j)]]$before_subjects <- before2
    if(!is.null(id)) FlowChart[[paste("Step_",j)]]$after_subjects <- after2
    
    rm(before,after,before2,after2)
    gc()
  } 
  
  
  FlowChart <- as.data.table(do.call(rbind,FlowChart))
  
  return(list(file = file, FlowChart = FlowChart))
  
  
}
