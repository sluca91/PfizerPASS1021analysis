


TEST <- fread(paste0(path_dir,"PERSONS.csv"))
OP <- fread(paste0(path_dir,"OBSERVATION_PERIODS.csv"))
setorder(OP, person_id, op_start_date)
OP <- OP[ ,.SD[1], by = c("person_id")]

MO <- fread(paste0(path_dir,"MEDICAL_OBSERVATIONS_AP.csv"))[0]

person_id <- unique(TEST$person_id)

reg <- as.character(1:5000)

mo_source_value <- sample(reg ,length(person_id), replace = T)

MO_new <- as.data.table(cbind(person_id, mo_source_value))[, mo_meaning := "start_gp" ][, mo_origin := "GP" ]

MO_new <- merge(MO_new, OP[,.(person_id, op_start_date)], by = "person_id")[, op_start_date := as.character(op_start_date)]
setnames(MO_new,"op_start_date", "mo_date")

MO_new2 <- MO_new[as.numeric(sample(rownames(MO_new),(ceiling(0.5 * nrow(MO_new))) )),][,mo_date := as.Date(as.character(mo_date),"%Y%m%d")]
MO_new2 <- MO_new2[, mo_date2 := mo_date + rpois(1, 35), by = row.names(MO_new2)]

MO_new2 <- MO_new2[, mo_date3 := paste0(year(mo_date2),sprintf("%02d",month(mo_date2)),sprintf("%02d",day(mo_date2))), by = row.names(MO_new2)][, mo_date := NULL][, mo_date2 := NULL]
setnames(MO_new2, "mo_date3", "mo_date")
MO_new2$mo_source_value2 <- sample(reg ,nrow(MO_new2), replace = T)
MO_new2 <- MO_new2[ mo_source_value != mo_source_value2, ][, mo_source_value := NULL]
setnames(MO_new2, "mo_source_value2", "mo_source_value")

MO_new4 <- rbindlist(list(MO,MO_new,MO_new2), use.names = T, fill = T)

MO_new4 <- MO_new4[sample(c(1:nrow(MO_new4)), 0.1 * nrow(MO_new4)), mo_date := "20211201" ]

#MO_new4 <- MO_new4[sample(c(1:nrow(MO_new4)), 0.8 * nrow(MO_new4)), ]

fwrite(MO_new4,file = paste0(path_dir,"MEDICAL_OBSERVATIONS_new.csv"), sep = ";")





