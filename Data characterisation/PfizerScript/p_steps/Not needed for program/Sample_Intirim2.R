


TEST <- fread(paste0(path_dir,"PERSONS.csv"))[, race := NULL]

TEST$race <- "white"

fwrite(TEST, paste0(path_dir,"PERSONS.csv"))

OP <- fread(paste0(path_dir,"OBSERVATION_PERIODS.csv"))[,order := seq_len(.N), by = "person_id"][order == 1,]
PER <- fread(paste0(path_dir,"PERSONS.csv"))

date_start <- merge(OP, PER, by = "person_id")[, .(person_id ,op_start_date)]
setnames(date_start, "op_start_date", "date")
date_start <- date_start[, date := as.Date(as.character(date), format = "%Y%m%d")]



TEST <- fread(paste0(path_dir,"MEDICAL_OBSERVATIONS_new.csv"))[0]


person_id <- date_start[["person_id"]]

mo_source_value <- sample(c("never","former","current"), length(person_id), replace = T)

mo_meaning <- rep("status", length(person_id))

mo_date <- date_start[["date"]] + rpois(1:length(person_id), 35)

file1 <- as.data.table(cbind(person_id, mo_source_value, mo_meaning, mo_date))
file1 <- file1[, mo_date2 := as.Date(as.integer(mo_date), origin = "1970-01-01")]
file1 <- file1[, mo_date := paste0(year(mo_date2),sprintf("%02d",month(mo_date2)),sprintf("%02d",day(mo_date2))), by = row.names(file1)][, mo_date2 := NULL]




mo_source_value <- sample(c("low","middle","high"), length(person_id), replace = T)

mo_meaning <- rep("SOCIO", length(person_id))

mo_date <- date_start[["date"]] + rpois(1:length(person_id), 35)

file2 <- as.data.table(cbind(person_id, mo_source_value, mo_meaning, mo_date))
file2 <- file2[, mo_date2 := as.Date(as.integer(mo_date), origin = "1970-01-01")]
file2 <- file2[, mo_date := paste0(year(mo_date2),sprintf("%02d",month(mo_date2)),sprintf("%02d",day(mo_date2))), by = row.names(file2)][, mo_date2 := NULL]

TEST <- rbindlist(list(TEST,file1, file2), use.names = T, fill = T)

fwrite(TEST, paste0(path_dir,"MEDICAL_OBSERVATIONS_intirim2.csv"))

OP <- fread(paste0(path_dir,"OBSERVATION_PERIODS.csv"))[,order := seq_len(.N), by = "person_id"][order == 1,]
PER <- fread(paste0(path_dir,"PERSONS.csv"))

date_start <- merge(OP, PER, by = "person_id")[, .(person_id ,op_start_date)]
setnames(date_start, "op_start_date", "date")
date_start <- date_start[, date := as.Date(as.character(date), format = "%Y%m%d")]



#TEST <- fread(paste0(path_dir,"PERSONS.csv"))
VAC <- fread(paste0(path_dir,"VACCINES.csv"))[0]

ATC_VAC <- c("J07AL01","J07BK02","J07AF01","J07AM01","J07AJ01","J07BF01","J07BD01","J07BE02","J07BJ01","J07AG02","J07BC01","J07BK03","J07BM01","J07AH02","J07BH01","J07BB03","J07BX03")


person_id <- date_start[sample(1:nrow(date_start), 0.1 * nrow(date_start)),]
vx_atc <- sample(ATC_VAC, nrow(person_id), replace = T)
date <- person_id[["date"]] + rpois(1:nrow(person_id), 35)
person_id <- person_id$person_id

file2 <- as.data.table(cbind(person_id,vx_atc, date))
file2 <- file2[, date2 := as.Date(as.integer(date), origin = "1970-01-01")]
file2 <- file2[, vx_admin_date := paste0(year(date2),sprintf("%02d",month(date2)),sprintf("%02d",day(date2))), by = row.names(file2)][, date := NULL][, date2 := NULL]

temp1 <- file2


ATC_VAC <- c("MMR", "HPV")

person_id <- date_start[sample(1:nrow(date_start), 0.1 * nrow(date_start)),]
vx_type <- sample(ATC_VAC, nrow(person_id), replace = T)
date <- person_id[["date"]] + rpois(1:nrow(person_id), 35)
person_id <- person_id$person_id

file2 <- as.data.table(cbind(person_id,vx_type, date))
file2 <- file2[, date2 := as.Date(as.integer(date), origin = "1970-01-01")]
file2 <- file2[, vx_admin_date := paste0(year(date2),sprintf("%02d",month(date2)),sprintf("%02d",day(date2))), by = row.names(file2)][, date := NULL][, date2 := NULL]

temp2 <- file2

temp3 <- rbindlist(list(VAC, temp1, temp2), use.names = T, fill = T )

#temp4 <- temp3[1:400,][, vx_admin_date := vx_admin_date + 368]


fwrite(temp3, paste0(path_dir,"VACCINES_intirim2.csv"))



