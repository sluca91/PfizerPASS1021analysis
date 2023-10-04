library(tools)

generate_latex_images <- function(images_per_row, rows_per_page, image_folder_path) {
  
  # Get all image file names in the folder
  image_files <- dir(image_folder_path, full.names = TRUE)
  
  # Calculate total number of images
  num_images <- length(image_files)
  
  # Calculate total number of pages needed
  num_pages <- ceiling(num_images / (images_per_row * rows_per_page))
  
  # Create LaTeX code for each page
  pages <- lapply(1:num_pages, function(page_num) {
    
    # Calculate start and end index of images for current page
    start_idx <- (page_num - 1) * images_per_row * rows_per_page + 1
    end_idx <- min(start_idx + images_per_row * rows_per_page - 1, num_images)
    
    # Get image file names for current page
    page_images <- image_files[start_idx:end_idx]
    
    # Create LaTeX code for current page
    page_code <- paste0("\\begin{figure}[htbp]\n",
                        "\\centering\n")
    for (i in 1:length(page_images)) {
      page_code <- paste0(page_code, "\\subfloat[Image ", i, "]{\\includegraphics[width=0.3\\textwidth]{", page_images[i], "}}\n")
      if (i %% images_per_row == 0 && i < length(page_images)) {
        page_code <- paste0(page_code, "\\\\\n")  # Add line break at the end of each row
      }
    }
    page_code <- paste0(page_code, "\\caption{Images for Page ", page_num, "}\n",
                        "\\label{fig:page_", page_num, "}\n",
                        "\\end{figure}\n")
    
    return(page_code)
  })
  
  # Combine LaTeX code for all pages
  all_pages_code <- paste0(pages, collapse = "\n")
  
  # Create final LaTeX code
  final_code <- paste0("\\documentclass{article}\n",
                       "\\usepackage{graphicx}\n",
                       "\\usepackage{subfig}\n",
                       "\\begin{document}\n",
                       all_pages_code,
                       "\\end{document}\n")
  
  return(final_code)
}



# Set the inputs
images_per_row <- 3
rows_per_page <- 2
outPutFolder <- "~/Documents/GitHub/Janssen-PASS-study/Data characterisation/Janssen_Script/g_output/"
image_folder_path <- "~/Documents/GitHub/Janssen-PASS-study/Data characterisation/Janssen_Script/g_output/JnJ_Figure1"

# Generate the LaTeX code
latex_code <- generate_latex_images(images_per_row, rows_per_page, image_folder_path)

# Write the LaTeX code to a file
outPutLatex <- paste0(outPutFolder,"/images.tex")
writeLines(latex_code,outPutLatex)

# Load the tinytex package
library(tinytex)

# Compile the LaTeX file into a PDF document
pdf_file <- paste0(outPutFolder,"/Figure1_All.pdf")
pdflatex(outPutLatex, clean = TRUE)

