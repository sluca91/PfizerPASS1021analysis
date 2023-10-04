
#Create flowchart
###

#This packages gives problems with installation in some computers
if(!require("DiagrammeR")){install.packages("DiagrammeR")}
suppressPackageStartupMessages(library(DiagrammeR))



PROGRAM_new <- PROGRAM[,.(PROGRAM, FILE, TYPE, FORMAT, FUNCTIONS,FOLDER_VAR)]
PROGRAM_new <- PROGRAM_new[, FILE := paste0(FILE, " (",FORMAT,")")]

x1 <- unique(PROGRAM_new$PROGRAM)
x2 <- 1:length(x1)

processes <- paste0("process",x2," [fillcolor = coral1, label =  '",x1,"']", collapse = " ")


y1 <- unique(PROGRAM_new$FILE)
y2 <- 1:length(y1)

col <- rep(NA, length(y1))

col[substr(y1, nchar(y1) - 4,nchar(y1)) == "(CDM)"] <- "darkseagreen1"
col[y1 %in% unique(PROGRAM_new[FOLDER_VAR == "meta_dir",][["FILE"]])] <- "cyan"

end <- unique(PROGRAM_new[TYPE == "OUTPUT",][["FILE"]])[!unique(PROGRAM_new[TYPE == "OUTPUT",][["FILE"]]) %in% unique(PROGRAM_new[TYPE == "INPUT",][["FILE"]])]
col[y1 %in% end] <- "azure4" 

col[is.na(col)] <- "azure2"



datasets <- paste0("data",y2," [label = '",y1,"', shape = folder, fillcolor = ",col,"]", collapse = " ")


id_processes <- as.data.table(cbind(x1,x2))
id_datasets <- as.data.table(cbind(y1,y2))

temp0 <- unique(PROGRAM_new[,.(PROGRAM,FILE,TYPE)])

temp2 <- merge(temp0, id_processes, by.x = "PROGRAM", by.y = "x1" )
temp2 <- merge(temp2, id_datasets, by.x = "FILE", by.y = "y1" )
#temp2 <- merge(temp2, id_datasets, by.x = "FILE.y", by.y = "y1" )

temp2 <- temp2[TYPE == "INPUT" , arrow := paste0("data",y2," -> process",x2) ]
temp2 <- temp2[TYPE == "OUTPUT" , arrow := paste0("process",x2," -> data",y2) ]


arrows <- paste0(temp2$arrow, collapse = " ")


tmp2 <- DiagrammeR::grViz(paste0("digraph {

graph [layout = dot]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

",processes," ",datasets,"

# edge definitions with the node IDs
",arrows,"


}



"))

tmp2

rm(PROGRAM_new, x1,x2, y1, y2, col, end, datasets, processes, id_datasets, id_processes, temp0, temp2, arrows)

# install.packages("DiagrammeRsvg")
# library("DiagrammeRsvg")
# 
# install.packages("rsvg")
# library("rsvg")
# 
# # 1. Make a play graph
# tmp = DiagrammeR::grViz('digraph{a->b; c->a; c->b; c->d;}')
# 
# # 2. Convert to SVG, then save as png
# tmp = DiagrammeRsvg::export_svg(tmp)
# tmp = charToRaw(tmp) # flatten
# rsvg::rsvg_png(tmp, "g.png", width = 2000, height = 2000) # saved graph as png in current working directory
# 


#Estimate origin column
###



tables <- unique(PROGRAM[TYPE == "OUTPUT" & FORMAT %in% c("rds") ,][["FILE"]])

ORIGIN <- data.table(table = as.character(), colls = as.character(), format_file = as.character(), col = as.character())

for(i in 1:length(tables)){
  
  if(file.exists(unique(unlist(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT" & FORMAT == "rds",][["result"]])))){
  colls_output <- colnames(readRDS(unique(unlist(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT" & FORMAT == "rds",][["result"]]))))
  }
  
  x0 <- unique(PROGRAM[FILE == tables[i] & TYPE == "OUTPUT", ][["PROGRAM"]])
  x <-PROGRAM[PROGRAM == x0 & TYPE == "INPUT" & FORMAT %in% c("rds", "CDM"), ][["FILE"]]
  
  if(length(x) > 0){
    for(j in 1: length(x)){
      
      
      type <- unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT"][["FORMAT"]]))
      
      if(type == "rds"){
        if(file.exists(unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "rds",][["result"]])))){
        colls_input <- colnames(readRDS(unique(unlist(PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "rds",][["result"]]))))
        }
      }
      
      if(type == "CDM"){ 
        if(file.exists(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1])))){
        colls_input <-colnames(fread(paste0(path_dir,(list.files(path_dir,PROGRAM[FILE == x[j] & PROGRAM == x0 & TYPE == "INPUT" & FORMAT == "CDM",][["result"]])[1]))))
              
        }
      }
      
      if(exists("colls_input") & exists("colls_output")){  
      colls <- colls_output[colls_output %in% colls_input]
      
      if(length(colls) > 1){
        col <- colls
        colls <- paste0(x[j],".", colls)
        if(type == "CDM") colls <- paste0("CDM.", colls)
        table <- rep(tables[i], length(colls))
        format_file <- rep(type, length(colls))
        ORIGIN <- rbind(ORIGIN, as.data.table(cbind(table, colls, format_file, col)))
        rm(table, col, format_file)
      }
      rm(colls, colls_input)
      }
      }
    }
  
  if(exists("colls_output")) rm(colls_output)
  rm(x, x0)
  
}













###







#Create Codebook
###



temp <- PROGRAM[TYPE == "OUTPUT" & FORMAT == "rds",]
tables <- matrix(ncol = 3,nrow = nrow(temp))
colnames(tables) <- c("Name","N_rows","Key")
wb <- createWorkbook()
addWorksheet(wb, "Program steps")
writeDataTable(wb, "Program steps", PROGRAM[,.(PROGRAM,TYPE, FILE,FORMAT,FOLDER,FUNCTIONS)], tableStyle = "TablestyleLight8")
setColWidths(wb, sheet = "Program steps", cols = c(1:6), widths = c(50,20,50,20,160,50))

addWorksheet(wb, "Flowchart program steps")
insertImage(wb, "Flowchart program steps", file = paste0(dir_base,"/Flowchart.png"), width = 12, height = 8)


update <- fread(paste0(meta_dir,"Codebook/Update_codebook.csv"))



for (i in 1:nrow(temp)) {
  if(file.exists(temp[i,][["result"]])){
  
  temp2 <- readRDS(temp[i,][["result"]])
  if(class(temp2)[1] != "list"){ 
  
  cols <- colnames(temp2)
  format <- unlist(lapply(cols, function(x) class(temp2[[x]])))
  order <- c(1:length(cols))
  
  temp3 <- as.data.table(cbind(cols,format,order))
  
  update_temp <- update[table == temp[i,]$FILE,]
  temp3 <- merge(temp3, update_temp[,.(cols, origin,Description)], by = "cols", all.x = T)
  tmp.c <- c(colnames(temp3),"origin2")
  
  #EST <- merge(temp[i,][,.(FILE, FORMAT)], ORIGIN, by.x = c("FILE", "FORMAT"), by.y = c("table", "format_file"))
  #EST <- merge(temp[i,][,.(FILE, FORMAT)], ORIGIN, by.x = c("FILE"), by.y = c("table"))
  #temp3 <- merge(temp3, EST, by.x= "cols", by.y = "col", all.x = T)
  #setnames(temp3, "colls", "origin2")
  #temp3 <-temp3[,..tmp.c]
  #setcolorder(temp3, c("cols",	"format",	"origin", "origin2","Description"))
  #rm(EST)
  
  setorder(temp3, order)[,order := NULL]
  
  addWorksheet(wb, temp[i,]$FILE)
  writeDataTable(wb, temp[i,]$FILE, temp3, tableStyle = "TablestyleLight8")
  setColWidths(wb, sheet = temp[i,]$FILE,cols = 1:ncol(temp3) , widths = 30)
  #fwrite(temp3,paste0(meta_dir,"Codebook/",temp[i,]$FILE,".csv") , sep = ";")
  
  tables[i,"N_rows"] <- nrow(temp2)
  tables[i,"Name"] <- temp[i,]$FILE
  tables[i,"Key"] <- temp[i,]$KEY
  
  rm(temp2, cols,format,temp3)
  gc()
  }
  }else{
    tables[i,"N_rows"] <- 0
    tables[i,"Name"] <- temp[i,]$FILE
    tables[i,"Key"] <- temp[i,]$KEY
    
  }
    
}

#fwrite(as.data.table(tables),paste0(meta_dir,"Codebook/Tables",".csv") , sep = ";")

addWorksheet(wb, "TABLES")
writeDataTable(wb, "TABLES", as.data.frame(tables), tableStyle = "TablestyleLight8")
setColWidths(wb, sheet = "TABLES",cols = 1:ncol(tables) , widths = 30)
worksheetOrder(wb) <- c(1,2,length(names(wb)), c(3:(length(names(wb)) - 1)))
  
saveWorkbook(wb, file = paste0(meta_dir,"Codebook/","Codebook",".xlsx"), overwrite = TRUE)



rm(tables,temp, update, update_temp)
gc()


###

#mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)
#dbListTables(mydb)

#colnames(dbGetQuery(mydb,"SELECT * FROM C_AMI_AESI"))




