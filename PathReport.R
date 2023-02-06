#Path report shows chosen path as data frame of files' list and their size.
#path -> directory path
#patt -> pattern for filtering files
#dironly -> to show only catalogs
#level -> depth of directory search

sizeReport <- function(path, patt = ".*", dironly = FALSE, level = Inf) {
  
  # If level equals 0 will show only given path and size
  if (level == 0){
    files <- path
  }
  # If level equals 1 will show files but will keep catalogs closed
  else if (level == 1){
    
    files <- list.files(path, recursive = FALSE, full.names = TRUE, include.dirs = TRUE)
  }
  # Else will open all catalogs in directory
  else{
    
    files <- list.files(path, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
    
  } 
  # Will show only catalogs
  if (dironly == TRUE){
    files <- files[file.info(files)$isdir]
  }
  
  # search for matches, filtering exact files
  files <- files[grep(patt, files)] 
  
  size_list <- file.info(files)$size
  report <- data.frame(path = files, size = size_list)
  
  return(report)
}


sizeReport("Path_to_report")
