
#' Aim
#'
#'If you want to add age bands to a data frame with an age variable or other bands you can run this function and merge the output to the date frame
#'where you want to add the age band to
#'
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CreateBands
#' @keywords ??
#' @import data.table

NULL


#' @param bands A vector of integers representing the start/end of every band. The order of the integers needs to be ascending
#' @param NEWBORNS If TRUE age 0 is automatically addes as the first band. TRUE by default

#' @return A data.table data frame with an age band (bands/string), ages that are within the band (INT/integer), order for sorting (integer/Order) and an ageband with leading 0 (string/band0)

#' @export




CreateBands <- function(bands, NEWBORNS = T){
  
  #Determine the length of the highest number. This is needed to create the column band0. 
  length0 <- nchar(as.character(max(bands)))
  
  #If NEWBORNS == T, add a new ageband for age is 0 at the beginning of the vector bands
  if(NEWBORNS & bands[1] == 0 & bands[2] != 1){bands[1] <- 1}
  if(NEWBORNS & bands[1] == 0 & bands[2] == 1){bands <- bands[2:length(bands)]}
  
  #Create an empty list that serves as the starting file to append to
  bands_list <- list()
  
  #Create a data frame with all the wanted agebands and the age at which the band starts and ends
  for (k in 1:length(bands)){
    if(!k== length(bands)){
      bands_list[["band"]][k] <- paste0(bands[k],"-",bands[k+1]-1)
      bands_list[["ST"]][k] <- bands[k]
      bands_list[["END"]][k] <- bands[k+1]-1
      bands_list[["band0"]][k] <- paste0(formatC(bands[k], width = length0, format = "d", flag = "0"),"-",formatC(bands[k+1]-1, width = length0, format = "d", flag = "0"))}
    
  }
  
  bands <- as.data.frame(cbind("band" = bands_list[["band"]],"ST" = bands_list[["ST"]],"END" = bands_list[["END"]], "band0" = bands_list[["band0"]]))
  
  #Set all information in the data frame to character
  bands[,1:ncol(bands)] <- lapply(bands[,1:ncol(bands)], as.character)
  
  #Create an empty list that serves as the starting file to append to
  new <- list()
  
  #Stretch out the data frame created in the previous loop. The start and end age are converted to a vector which contains all the ages within and ageband
  for(i in 1:nrow(bands)){
    if(length(strsplit(bands[i,"band"],"-")[[1]])==2) {
      ST <- as.integer(strsplit(bands[i,"band"],"-")[[1]][1])
      EN <- as.integer(strsplit(bands[i,"band"],"-")[[1]][2])
      vec <- c(ST:EN)
      new[[i]] <-cbind(bands[i,][rep(seq_len(nrow(bands[i,])), each = length(vec)),],"new" = vec)
      
    } else new[[i]] <- bands[i,]
    
  }
  
  bands <- do.call(rbind,new)
  bands <- as.data.table(bands)[,INT := as.numeric(new)][,.(band,INT,band0)]
  
  #Add an order variable for sorting
  setorder(bands,INT)
  ORDER <- as.data.table(cbind("band" = unique(bands[,band]),"Order" = seq(from = 1, to = length(unique(bands[,band])),by = 1)))[,Order := as.integer(Order)]
  bands <- merge(x= bands, y= ORDER, by = "band", all.x = T) 
  
  #Create a label text for the NEWBORNS
  if(NEWBORNS){bands <- rbindlist(list(list("0",0,formatC(0, width = length0, format = "d", flag = "0"),0),bands))}
  
  setorder(bands,Order)
  
  
}
