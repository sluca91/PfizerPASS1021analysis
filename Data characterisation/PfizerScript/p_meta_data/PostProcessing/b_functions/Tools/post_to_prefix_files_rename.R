
# Function to rename files in a directory
post_to_prefix_files_rename <- function(directory,post_to_prefix) {
  # Get the list of files in the directory
  files <- list.files(directory, full.names = TRUE)
  pattern_01 <- paste0("_",post_to_prefix,"$")
  
  # Loop through each file
  for (file_path in files) {
    # Extract the filename and extension
    filename <- basename(file_path)
    extension <- tools::file_ext(filename)
    file_without_ext <- sub(paste0("\\.", extension, "$"), "", filename)
    
    # Check if the file matches the pattern _NHR
    if (grepl(pattern_01, file_without_ext)) {
      # Remove the _NHR postfix and create the new filename
      new_filename <- paste0(post_to_prefix,"_",sub(pattern_01, "", file_without_ext), ".", extension)
      
      # Construct the new file path
      new_file_path <- file.path(dirname(file_path), new_filename)
      
      # Rename the file
      file.rename(file_path, new_file_path)
      
      cat("Renamed", filename, "to", new_filename, "\n")
    }
  }
}

# # Specify the directory containing the files
# directory <- "/path/to/directory"
# 
# # Call the function to rename the files
# rename_files(directory)
