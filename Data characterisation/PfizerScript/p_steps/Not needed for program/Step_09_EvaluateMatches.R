


check0 <- readRDS(paste0(populations_dir,"M_Studycohort.rds"))
TEST <- readRDS(paste0(populations_dir,"MATCH_PAIRS.rds"))
#TEST2 <- readRDS(paste0(populations_dir,"MATCH_PAIRS_SQL.rds"))

SPELLS <- readRDS(paste0(tmp,"OBS_SPELLS.rds"))
SPELLS1 <- readRDS(paste0(tmp,"OBS_SPELLS1.rds"))

CheckOverlap <- function(file, id, start, end){
  file <- copy(file)
  file <- file[,id := as.integer(rownames(file))]
  key <- c(id, start, end)
  
  setkeyv(file, key)
  overlap <- as.data.table(foverlaps(file[get(end) >= get(start),],file[get(end) >= get(start),], type="any", which=TRUE))
  overlap <- overlap[xid!=yid,]
  overlap <- unique(merge(x= overlap, y = file, by.x = "xid", by.y = "id", all = F)[[id]])
  
  return(overlap)
}

check8a <- length(CheckOverlap(file = SPELLS, id = "person_id", start = "op_start_date", end = "op_end_date")) 
check8b <- length(CheckOverlap(file = SPELLS1, id = "person_id", start = "op_start_date", end = "op_end_date")) 
rm(SPELLS1)
gc()

temp <- sqldf("
              SELECT
              t1.*,
              t2. op_start_date as old_op_start_date,
              t2. op_end_date as old_op_end_date
              
              FROM check0 t1
              
              left join SPELLS t2 on (t1.person_id = t2.person_id AND (T0 BETWEEN t2.op_start_date AND t2.op_end_date))
              
              
              ")

check0 <- as.data.table(temp)[, old_op_start_date := as.Date(old_op_start_date, origin = "1970-01-01")][, old_op_end_date := as.Date(old_op_end_date, origin = "1970-01-01")]

check <- merge(x = TEST[,.(Exposed,Control)], y = check0, by.x = "Exposed",by.y = "person_id")

check <- merge(x = check, y = check0, by.x = "Control",by.y = "person_id")

check6 <- check[ FIRST_PFIZER.x %between% list(old_op_start_date.x + 365, old_op_end_date.x) & FIRST_PFIZER.x %between% list(old_op_start_date.y + 365 , old_op_end_date.y) , x := "correct"]
check6 <- check[,.(Control,Exposed,FIRST_PFIZER.x, old_op_start_date.x, old_op_end_date.x, old_op_start_date.y, old_op_end_date.y,x)]

check1 <- check[,.(sex_at_instance_creation.x,sex_at_instance_creation.y)][sex_at_instance_creation.x == sex_at_instance_creation.y, x := "correct"]
check2 <- check[,.(birth_date.x,birth_date.y)][,x0 := birth_date.x - birth_date.y][x0 %between% list(-731, 731), x := "correct"]
check3 <- check[,.(FIRST_PFIZER.x,FIRST_PFIZER.y,FIRST_OTHER.y)][FIRST_PFIZER.x < FIRST_PFIZER.y, x := "correct"  ][FIRST_PFIZER.x < FIRST_OTHER.y, x := "correct"  ][is.na(FIRST_PFIZER.y) & is.na(FIRST_OTHER.y), x := "correct"  ]


COV <- readRDS(paste0(concepts_dir,"COVID19DX.rds"))
check4 <- merge(x = TEST[,.(T0,Exposed,Control)], y = COV, by.x = "Exposed",by.y = "person_id", all.x = T)
check4 <- merge(x = check4, y = COV, by.x = "Control",by.y = "person_id", all.x = T)[,.(Exposed,Control,T0,Date.x,Date.y)]
check4 <- check4[, c.EXP := (T0 - Date.x)/365.25 ][,c.EXP2 := fifelse(c.EXP < 100 & c.EXP > 0,T,F, na = F)]
check4 <- check4[, c.CON := (T0 - Date.y)/365.25 ][,c.CON2 :=fifelse(c.CON < 100 & c.CON > 0, T,F, na = F)]
check4 <- check4[!is.na(Control),]

check4a <- check4[c.EXP2 == T,][,.(Exposed,Control, Date.x)]
check4a <- check4a[, x := T][,.(Exposed,Control, x)]
check4b <- check4[c.CON2 == T,][,.(Exposed,Control, Date.y)]
check4b <- check4b[, x := T][,.(Exposed,Control, x)]
check4 <- merge(x = TEST[,.(T0,Exposed,Control)], y = unique(check4a), by = c("Exposed","Control"), all.x = T)
check4 <- merge(x = check4, y = unique(check4b), by = c("Exposed","Control"), all.x = T)
check4 <- check4[(x.x == T & x.y == T) | (x.x == F & x.y == F) | (is.na(x.x & is.na(x.y))), x := "correct"]
check4 <- check4[!is.na(Control),]

INF <- readRDS(paste0(vaccins_dir,"INF.rds"))
check5 <- merge(x = TEST[,.(T0,Exposed,Control)], y = INF, by.x = "Exposed",by.y = "person_id", all.x = T)
check5 <- merge(x = check5, y = INF, by.x = "Control",by.y = "person_id", all.x = T)[,.(Exposed,Control,T0,Date.x,Date.y)]

check5 <- check5[,T02 := as.Date(paste0(year(T0),sprintf("%02d",month(T0)),"01"), "%Y%m%d")]
check5 <- check5[,Date.x2 := as.Date(paste0(year(Date.x),sprintf("%02d",month(Date.x)),"01"), "%Y%m%d")]
check5 <- check5[,Date.y2 := as.Date(paste0(year(Date.y),sprintf("%02d",month(Date.y)),"01"), "%Y%m%d")]


check5 <- check5[, c.EXP := (T02 - Date.x2)/365.25 ][,c.EXP2 := fifelse(c.EXP < 5 & c.EXP >= 0 & Date.x < T0,T,F, na = F)]
check5 <- check5[, c.CON := (T02 - Date.y2)/365.25 ][,c.CON2 :=fifelse(c.CON < 5 & c.CON >= 0 & Date.y < T0, T,F, na = F)]

#ID-00000118#
#ID-00511687#

check5 <- check5[!is.na(Control),]
#check5 <- check5[c.EXP2 == c.CON2, x := "correct"]


check5a <- check5[c.EXP2 == T,][,.(Exposed,Control, Date.x)]
check5a <- check5a[, x := T][,.(Exposed,Control, x)]
check5b <- check5[c.CON2 == T,][,.(Exposed,Control, Date.y)]
check5b <- check5b[, x := T][,.(Exposed,Control, x)]
check5 <- merge(x = TEST[,.(T0,Exposed,Control)], y = unique(check5a), by = c("Exposed","Control"), all.x = T)
check5 <- merge(x = check5, y = unique(check5b), by = c("Exposed","Control"), all.x = T)
check5 <- check5[(x.x == T & x.y == T) | (x.x == F & x.y == F) | (is.na(x.x & is.na(x.y))), x := "correct"]
check5 <- check5[!is.na(Control),]


#rm(check,check1,check2,check3,check4,check5a,check5b,check5,check0,check6,INF,TEST)

check0 <- as.data.table(readRDS(paste0(populations_dir,"M_Studycohort.rds")))

check0 <- check0[, x := (op_start_date - T0)]
check0 <- check0[, x2 := (op_end_date - T0)]

TEST1 <- readRDS(paste0(populations_dir,"MATCH_PAIRS.rds"))[!is.na(Control),]

if(exists("TEST2")){
  TEST2 <- readRDS(paste0(populations_dir,"MATCH_PAIRS_SQL.rds"))[!is.na(Control),]
  x <- merge(TEST1, TEST2, by = "Exposed", all.x = T)
  x2 <- x[nb_match.x == nb_match.y, x := "correct"]
  x <- x[is.na(Control.y) ,]
  
}


REG <- readRDS(paste0(concepts_dir,"REGION.rds"))
check7a <- merge(x = TEST1[,.(T0,Exposed,Control)], y = REG, by.x = "Exposed",by.y = "person_id", all.x = T, allow.cartesian = T)
#[, dis := abs(mo_date - T0)][, min := min(dis), by = Exposed][min == dis,]
check7b <- merge(x = TEST1[,.(T0,Exposed,Control)], y = REG, by.x = "Control",by.y = "person_id", all.x = T, allow.cartesian = T)
#[, dis := abs(mo_date - T0)][, min := min(dis), by = Exposed][min == dis,]

check7 <- merge(check7a[, .(Exposed, Control, REGION)],check7b[, .(Exposed, Control, REGION)], by = c("Exposed","Control"), allow.cartesian = T)[REGION.x == REGION.y, x := "correct"]


check7 <- unique(check7[ x == "correct",])

check7 <- merge(TEST[,.(Exposed, REGION, nb_match)],check7, by = "Exposed", all.x = T)
#check7_not_min <- check7
check7 <- check7[REGION == "NO_REGION" & is.na(REGION.x) & is.na(REGION.y) ,x := "correct",]

check7 <- check7[REGION != "NO_REGION" & nb_match == 0,x := "correct"]

COV2 <- readRDS(paste0(vaccins_dir,"CoV2.rds"))
CONTROLS <- nrow(readRDS(paste0(tmp,"CONTROL.rds")))

if(exists("TEST2")){vec <- c("check1", "check2", "check3", "check4", "check5","check6","check7", "x2")}else{
  vec <- c("check1", "check2", "check3", "check4", "check5","check6","check7")
}

for(i in vec){
  temp <- get(i)
  print(i)
  print(sum(temp[["x"]] == "correct"))
  print(sum(is.na(temp[["x"]])))
  #saveRDS(temp, paste0(tmp,i,".rds"))
  rm(temp)
}

print("check8")
print(check8a)
print(check8b)

print(paste0(nrow(TEST), " pairs and ", CONTROLS, " controls"))
print(paste0(sum((as.logical(TEST[["INFP5"]]))), " pairs with INFP5 TRUE and ", nrow(INF)," captured rows for INF"))
print(paste0(sum((as.logical(TEST[["FIRST_COV_INF"]]))), " pairs with FIRST_COV_INF TRUE"))
print(paste0(sum(is.na(TEST[["Control"]])), " UNMATCHED"))
print(paste0((nrow(TEST[REGION == "NO_REGION",])), " pairs with NO_REGION"))
print(paste0(sum((as.logical(COV2[["inputed"]]))), " covid vaccines manufactuers inputed from ", nrow(COV2)))



