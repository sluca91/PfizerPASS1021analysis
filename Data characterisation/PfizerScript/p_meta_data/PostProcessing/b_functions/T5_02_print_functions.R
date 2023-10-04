
CreateFlextable <- function(TableFormats_new, DAP_tables2, tabletitle) {
  
  header_rows <- nrow(TableFormats_new)
  
  ft <- flextable(DAP_tables2[c((header_rows + 1):nrow(DAP_tables2)),]) ## Flextable of the data (excl. header(s))
  ft <- delete_part(ft, part = "header")  ## Remove the header
  
  
  for(RowNr in header_rows:1){
    ft <- add_header_row(x = ft, values = fun1(as.character(DAP_tables2[RowNr,]))[[1]], colwidths = fun1(as.character(DAP_tables2[RowNr,]))[[2]]) ## Adds second row
  }
  
  # ft <- colformat_num( ft, big.mark = ",", decimal.mark = ".") 
  # ft <- colformat_int(ft, big.mark = ",")
  # ft <- colformat_char(ft, prefix = "test: ")
  #ft <- merge_v(ft, j = ~ col1)          ## Merge similar values of AECAT 
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft <- bold(ft, bold = TRUE, part = "header")
  # ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- border_inner(ft)
  ft <- border_outer(ft)
  # ft <- set_caption(ft, caption = tabletitle, style = "Table Caption")
  ft <- add_header_lines(ft,values = tabletitle, top=TRUE)
  ft <- font(ft, fontname = "Times New Roman", part = "all") 
}         




CreateAndPrintToWordDocument <- function(projectFolder, ft, tabletitle, orient, OutputFolder, tablelist, TableNr, PageNr) {
  
  TemplatePath <- paste0(projectFolder,"/b_functions/wordTemplates/template_word.docx")
  
  
  doc_section_1 <- read_docx(path = TemplatePath)
  
  # Add flextable to doc body
  doc_section_1 <- body_add_flextable( doc_section_1, value = ft, align = "left", keepnext = F,split=T)
  
  # create a prop_section to orient the page in a landscape format and make it an oddPage(?)
  ps <- prop_section( page_size = page_size(orient = pull(tabletitle, orient)), type = "oddPage")
  
  # Add the doc_section_1 to itself using a body_end_block_section with the property value of the block_section equal to the previous prop_section ps
  doc_section_1 <- body_end_block_section( x = doc_section_1,  value = block_section(property = ps))
  
  # print doc_section_ to its target
  print(doc_section_1, target = paste0(OutputFolder,"/01_docx/",tablelist[TableNr],"_instance_",PageNr,".docx"))
  
}


CreateAndPrint_AESI_ToWordDocument <- function(projectFolder, ft,filename, tabletitle, orient, OutputFolder, tablelist, TableNr, PageNr) {
  
  TemplatePath <- paste0(projectFolder,"/b_functions/wordTemplates/template_word.docx")
  
  
  doc_section_1 <- read_docx(path = TemplatePath)
  
  # Add flextable to doc body
  doc_section_1 <- body_add_flextable( doc_section_1, value = ft, align = "left", keepnext = F,split=T)
  
  # create a prop_section to orient the page in a landscape format and make it an oddPage(?)
  ps <- prop_section( page_size = page_size(orient = pull(tabletitle, orient)), type = "oddPage")
  
  # Add the doc_section_1 to itself using a body_end_block_section with the property value of the block_section equal to the previous prop_section ps
  doc_section_1 <- body_end_block_section( x = doc_section_1,  value = block_section(property = ps))
  
  # print doc_section_ to its target
  print(doc_section_1, target = filename)
  
}

