dataset <- data.frame(
  RDS_filename = character(),
  CheckVariable = character(),
  UpdateVariable = character(),
  stringsAsFactors = FALSE
)

# Function to add a new row of data to the dataset
addData <- function( RDS_filename, CheckVariable, UpdateVariable) {
  newRow <- data.frame( RDS_filename = RDS_filename, CheckVariable = CheckVariable, UpdateVariable = UpdateVariable)
  dataset <<- rbind(dataset, newRow)
}

# Add some Tabledata and VariableData to the dataset
addData( "table1.rds", "N", "PER")
addData( "table2.rds", "N", "PER")
addData( "Table2_00-01.rds", "N", "PER")
addData( "table2_02-04.rds", "N", "PER")
addData( "Table2_05-11.rds", "N", "PER")
addData( "Table2_12-15.rds", "N", "PER")
addData( "Table2_16-17.rds", "N", "PER")
addData( "Table2_18-29.rds", "N", "PER")
addData( "Table2_30-39.rds", "N", "PER")
addData( "Table2_40-49.rds", "N", "PER")
addData( "Table2_50-59.rds", "N", "PER")
addData( "Table2_60-69.rds", "N", "PER")
addData( "Table2_70-79.rds", "N", "PER")
addData( "table2_F.rds", "N", "PER")
addData( "table2_M.rds", "N", "PER")
addData( "table3_FirstDose.rds", "N", "PER")
addData( "table3_ThirdDose.rds", "N", "PER")
addData( "table4.rds", "N1", "PER1")
addData( "table4.rds", "N3", "PER3")
addData( "table5.rds", "N1", "PER1")
addData( "table5.rds", "N3", "PER3")

addData( "table6.rds", "n.excl", "perc.excl")
addData( "table10.rds", "N", "PER")
addData( "table11.rds", "N", "PER")
addData( "table11.rds", "N.1", "PER.1")
addData( "table12.rds", "vac.n", "vac.perc")
addData( "table12.rds", "ctr.n", "ctr.perc")

addData( "table13.rds", "vac.n", "vac.perc")
addData( "table13.rds", "ctr.n", "ctr.perc")
addData( "table14.rds", "vac.n", "vac.perc")
addData( "table14.rds", "ctr.n", "ctr.perc")


# addData( "table20.rds", "N", "PER")
# addData( "table2_02-04.rds", "N", "PER")
# addData( "table2_02-04.rds", "N", "PER")
# addData( "table2_02-04.rds", "N", "PER")
# addData( "table2_02-04.rds", "N", "PER")
# addData( "table2_02-04.rds", "N", "PER")


# addData(3, "Sam", 32, 55000)


for (i in 1:nrow(dataset)) {
  rowData <- dataset[i, ]
  
  rewind_to_0_with_an_n_of_zero(OutputFolder=OutputFolder,
                                RDS_filename=rowData$RDS_filename,
                                CheckVariable=rowData$CheckVariable,
                                UpdateVariable=rowData$UpdateVariable)
  
  # mask_value_when_n_less_than_5(OutputFolder=OutputFolder,
  #                               RDS_filename=rowData$RDS_filename,
  #                               CheckandUpdateVariable =rowData$CheckVariable)  
  

}







# rewind_to_0_with_an_n_of_zero(OutputFolder=OutputFolder,
#                               RDS_filename="table5.rds",
#                               CheckVariable="N1",
#                               UpdateVariable="PER1")
# 
# rewind_to_0_with_an_n_of_zero(OutputFolder=OutputFolder,
#                               RDS_filename="table5.rds",
#                               CheckVariable="N3",
#                               UpdateVariable="PER3")



