


GetFreeDiskSpace <- function(path = tempdir()){
  disks <- system("wmic logicaldisk get size,freespace,caption", inter=TRUE)
  
  disks <- read.fwf(textConnection(disks[1:(length(disks)-1)]), 
                    widths=c(9, 13, 13), strip.white=TRUE, stringsAsFactors=FALSE)
  
  colnames(disks) <- disks[1,]
  disks <- disks[-1,]
  rownames(disks) <- NULL
  
  mem <- as.numeric(disks[disks[["Caption"]] == substr(path, 1, 2), 2])*(1 * 10^-9)
  
  return(mem)
  
}

