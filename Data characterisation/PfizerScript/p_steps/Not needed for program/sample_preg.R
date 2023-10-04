



dummy_preg <- readRDS(paste0(tmp, "PERSONS3.rds"))[sex_at_instance_creation == "F",]
#[YEAR_BIRTH < 2004 & YEAR_BIRTH > 1977,]

dummy_preg <- dummy_preg[, `:=` (startDT = birth_date + (18*365), endDT = birth_date + (44*365))]




create <- function(birth, start, end, id, data, v.age, v.ir){
  
  data <- data[is.na(get(end)), eval(end) := Sys.Date()]

  file <- data.table(id = as.character(), date = as.character())
  
  for(i in 1:nrow(data)){
    
    temp <- data[i,]
    start2 <- temp[[start]]
    end2 <- temp[[end]]
    birth2 <- temp[[birth]]
    id2 <- temp[[id]]
    rm(temp)
    
    range <- seq.Date(start2, end2, 1)
    age <- as.integer(rep(birth2, length(range)))
    
    age <- year(range)-age
    
    x <- rep(v.ir, c(diff(v.age),40))
    
    temp <- as.data.table(cbind(x,c(1:length(x))))
    colnames(temp) <- c("kans","Age")
    
    age2 <- as.data.table(age)
    colnames(age2) <- c("Age")
    
    probs <- unlist(c(as.data.frame(merge(x = age2, y = temp, by = "Age" )[,2])))
    rm(temp)
    events <- rbinom(length(probs), 1, probs)
    
    events <- which(events == 1)
    if(length(events) > 0){
      events <- start2 + events
      
      file <- rbind(file,as.data.table(cbind(id = rep(id2,length(events)), date = events)))
    }
   print(i) 
  }  
  return(file) 
}


test <- create(
  birth = "YEAR_BIRTH", 
  start = "startDT",
  end = "endDT",
  id = "person_id" ,
  data = dummy_preg,
  v.age = 18 ,
  v.ir = 120/1000/365
  
)



test <- test[, start := as.Date(as.integer(date), origin = "1970-01-01")][, end := start + (30 * 9)]
test <- merge(test, dummy_preg[,.(person_id, YEAR_BIRTH)], by.x = "id", by.y = "person_id", all.x = T)
test <- test[, age := (year(start) - YEAR_BIRTH)]
test <- test[, .(id,start,end)]

setnames(test, "id", "person_id")

saveRDS(test ,paste0(path_dir,"/dummyPREG.rds"))

