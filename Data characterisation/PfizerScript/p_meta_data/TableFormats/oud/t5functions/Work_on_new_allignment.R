


x <-  c("Table16","Table20")


i=1


for(i in 1:length(x)){
  
  table <- readRDS(paste0(InterMediateFolder,"/", x[i], ".rds"))
  
  cols <- colnames(table)[!colnames(table) %in% c("Event_name", "AESI", "System", "DAP")]
  format <- fread(paste0(projectFolder,"/Format", x[i], ".csv"))
  
  format <- format[seq_len(.N) == nrow(format), DAP := "DAP"][, c("DAP",cols) , with = F]
  
  if("AESI" %in% colnames(table)) col <- "AESI"
  if("Event_name" %in% colnames(table)) col <- "Event_name"
  table[get(col) == "Coagulation disorders: thromboembolism", eval(col) := "Coagulation disorders thromboembolism"]
  
  aesi <- unique(table[[col]])
  
  j=2
  for(j in 1:length(aesi)){
    tableTmp <- table[get(col) == aesi[j] ,]
    tableTmp <- tableTmp[, c("DAP",cols) , with = F]
    
  #TableTitles[tableName == x[i],][["tabletitle"]]
  
  t.formats = format
  t.content = tableTmp
  t.title = aesi[j]
  filename = paste0(InterMediateFolder,"/temp_docx_output/",x[i],"_",aesi[j],".docx")
  t.orient = "landscape"  
  
  # start createflextableobject1
  ###  
  TableFormats <- copy(t.formats)
  d <- copy(t.content)
  
  
  header_rows <- nrow(TableFormats)
  
  d <- rbind(TableFormats,d)
  d[is.na(d)] <- ""
  
  ft <- flextable(d[c((header_rows + 1):nrow(d)),]) ## Flextable of the data (excl. header(s))
  ft <- delete_part(ft, part = "header")  ## Remove the header
  
  
  for(k in header_rows:1){
    ft <- add_header_row(x = ft, values = fun1(as.character(d[k,]))[[1]], colwidths = fun1(as.character(d[k,]))[[2]]) ## Adds second row
  }
  #ft <- merge_v(ft, j = ~ col1)          ## Merge similar values of AECAT 
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- border_inner(ft)
  ft <- border_outer(ft)
  ft <- set_caption(ft, caption = t.title, style = "Table Caption")
  ft <- font(ft, fontname = "Times New Roman", part = "all")    
  
  # end function createflextableobject
  
  #pull(tabletitle, tabletitle)
  
  # start function PrintToWord
  
  TemplatePath <- paste0(projectFolder,"/t5functions/wordTemplates/template_word.docx")
  
  doc_section_1 <- read_docx(path = TemplatePath)
  
  # Add flextable to doc body
  doc_section_1 <- body_add_flextable( doc_section_1, value = ft, align = "left", keepnext = F,split=T)
  
  # create a prop_section to orient the page in a landscape format and make it an oddPage(?)
  ps <- prop_section( page_size = page_size(orient = t.orient), type = "oddPage")
  #pull(tabletitle, orient))
                      
  # Add the doc_section_1 to itself using a body_end_block_section with the property value of the block_section equal to the previous prop_section ps
  doc_section_1 <- body_end_block_section( x = doc_section_1,  value = block_section(property = ps))
  
  # print doc_section_ to its target
  print(doc_section_1, target = filename)
  
  # end function printtoword
  
  rm(d); rm(ft)
  rm(ps, doc_section_1, TemplatePath , tableTmp )
  }
 
  
  ###
  rm(table, cols, aesi, format, col)
  
    
}
