Append_Tables_Daps_To_One <- function(folders, Daplist, intermediateFolder, tableList) {
  for (i in seq_along(Daplist)) {
    dap <- Daplist[i]
    path <- find_folder_path(folders, dap)
    Loop_Trough_TableList(path, intermediateFolder, tableList, dap)
  }
}

find_folder_path <- function(folders, dap) {
  dap_match <- grep(toupper(dap), toupper(folders), value = TRUE)
  return(folders[dap_match])
}

Loop_Trough_TableList <- function(path, intermediateFolder, tableList, dap) {
  for (j in seq_along(tableList)) {
    table <- tableList[j]
    fileLocation <- get_file_location(intermediateFolder, table)
    file_temp <- find_matching_file(path, table)
    if (length(file_temp) == 1) {
      temp <- read_data_from_file(path, file_temp)
      temp <- format_data(temp)
      temp[["DAP"]] <- dap
      save_data(temp, fileLocation)
    }
  }
}

get_file_location <- function(intermediateFolder, table) {
  return(file.path(intermediateFolder, paste0(table, ".rds")))
}

find_matching_file <- function(path, table) {
  file_temp <- list.files(pattern = paste0(table, ".csv"), path)
  return(file_temp)
}

read_data_from_file <- function(path, file_temp) {
  data <- read.csv(file.path(path, file_temp), sep = ",")
  return(as.data.table(data))
}

format_data <- function(temp) {
  for (i in seq_along(temp)) {
    temp[[i]] <- lapply(temp[[i]], format_strnr)
  }
  return(temp)
}

format_strnr <- function(x) {
  # Check if the value is in string format
  if (is.character(x)) {
    # Replace specific pattern with "< 0.01"
    pattern <- "\\b0\\.00\\b"
    if (grepl(pattern, x)) {
      x <- gsub(pattern, "< 0.01", x)
    } else {
      # Try converting the string to numeric
      y <- suppressWarnings(as.numeric(x))
      if (!is.na(y) && is.numeric(y)) {
        if (y %% 1 == 0) {
          # Format integer values
          x <- formatC(y, digits = 0, big.mark = ",", format = "f")
        } else if (y != round(y)) {
          # Format decimal values
          x <- formatC(y, digits = 2, decimal.mark = ".", big.mark = ",", format = "f")
        }
      }
    }
  }
  
  return(x)
}


save_data <- function(data, fileLocation) {
  if (file.exists(fileLocation)) {
    combined_data <- rbindlist(list(readRDS(fileLocation), data), use.names = TRUE, fill = FALSE)
    saveRDS(combined_data, fileLocation)
  } else {
    saveRDS(data, fileLocation)
  }
}




