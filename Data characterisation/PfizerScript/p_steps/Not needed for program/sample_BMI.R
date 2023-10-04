



dummy <- readRDS(paste0(tmp, "PERSONS.rds"))[sex_at_instance_creation == "F",]
#[YEAR_BIRTH < 2004 & YEAR_BIRTH > 1977,]

dummy <- dummy[, `:=` (startDT = birth_date + (18*365), endDT = birth_date + (90*365))]




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
  data = dummy,
  v.age = 18 ,
  v.ir = 120/1000/365
  
)

test <- test[, start := as.Date(as.integer(date), origin = "1970-01-01")]

id <- unique(test$id)
height <- sample( 120:195 ,length(id), replace = T)
weight <- sample( 50:130 ,length(id), replace = T)

test <- merge(x = test, y = as.data.table(cbind(id, height, weight)), by = "id", all.x = T)

test <- test[, BMI := as.numeric(weight)/(as.numeric(height)/100)^2, by = row.names(test)]
test1 <- copy(test)[, mo_source_value := as.numeric(height) + sample(-10:10, 1), by = row.names(test)][, mo_unit := "cm"][, `:=` (person_id = id, mo_meaning = "height")][, mo_date1 := start + sample(-50:50, 1)][, .(person_id, mo_date1, mo_meaning, mo_unit, mo_source_value)]
test2 <- copy(test)[, mo_source_value := as.numeric(weight) + sample(-5:5, 1), by = row.names(test)][, mo_unit := "kg"][, `:=` (person_id = id, mo_meaning = "weight")][, mo_date1 := start + sample(-50:50, 1)][, .(person_id, mo_date1, mo_meaning, mo_unit, mo_source_value)]
test3 <- copy(test)[, mo_source_value := as.numeric(BMI) + sample(-2:2, 1), by = row.names(test)][, mo_unit := "kg/m2"][, `:=` (person_id = id, mo_meaning = "bmi")][, mo_date1 := start + sample(-50:50, 1)][, .(person_id, mo_date1, mo_meaning, mo_unit, mo_source_value)]

lapply(colnames(test1), function(x) test1 <- test1[, eval(x) :=  as.character(get(x))])
lapply(colnames(test2), function(x) test2 <- test2[, eval(x) :=  as.character(get(x))])
lapply(colnames(test3), function(x) test3 <- test3[, eval(x) :=  as.character(get(x))])

file <- rbindlist(
      
      list(
      fread(paste0(path_dir,"MEDICAL_OBSERVATIONS_new.csv"))[0],
      test1[sample(1:nrow(test) , nrow(test) * 0.7, replace = F),],
      test2[sample(1:nrow(test) , nrow(test) * 0.8, replace = F),],
      test3[sample(1:nrow(test) , nrow(test) * 0.6, replace = F),]
      ),
      fill = T, use.names = T
      )[, mo_date := NULL]

file <- file[, mo_date := paste0(year(mo_date1),sprintf("%02d",month(mo_date1)),sprintf("%02d",day(mo_date1))), by = row.names(file)][, mo_date1 := NULL]

fwrite(file, paste0(path_dir,"MEDICAL_OBSERVATIONS_intirim2BMI.csv"))



