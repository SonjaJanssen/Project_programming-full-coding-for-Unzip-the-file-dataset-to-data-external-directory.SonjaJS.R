## Project Programming: Unzip the File Dataset to an External Data Directory
# Codes created by Sonja Janssen-Sahebzad

# Lectures: By Professor Jeffrey Leek
# University: Johns Hopkins University 
# Notes: Created with R 4.2.2 for Windows in RStudio
# Date: 30 March 2023

#*******************************************************************************
## Unzip the file
## unzip(zipfile="./data/Dataset.zip", exdir="./data") ## codes tested = ok!
## Unzipped file dataSet to data exdirectory
#*******************************************************************************

# Function to unzip files
# Parameters:
# - zipfile: Path to the zip file
# - files: A character vector of files to extract (default is NULL, which extracts all)
# - list: Logical, if TRUE, list the files that would be extracted without actually extracting them (default is FALSE)
# - overwrite: Logical, if TRUE, overwrite existing files (default is TRUE)
# - junkpaths: Logical, if TRUE, junk paths and extract all files in the current working directory (default is FALSE)
# - exdir: The directory to which files should be extracted (default is the current working directory)
# - unzip: The command to be used for unzipping (default is "internal" for the internal R unzip function)
# - setTimes: Logical, if TRUE, set the times of the extracted files to the modification times of the original files (default is FALSE)
# Returns: A data frame with extracted file information (if list is TRUE), otherwise, invisible NULL


unzip_files <- function(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
                         junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE) {
  
  # Step 1: Check if using internal or external unzip
  if (identical(unzip, "internal")) {
    # If using the internal unzip function in R
    if (!list && !missing(exdir)) 
      dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
    
    # Step 2: Call the internal unzip function
    res <- .External(C_unzip, zipfile, files, exdir, list, 
                     overwrite, junkpaths, setTimes)
    
    if (list) {
      # Step 3: Extract file information and return as a data frame
      dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M", tz = "UTC")
      data.frame(Name = res[[1]], Length = res[[2]], Date = dates, 
                 stringsAsFactors = FALSE)
    } else {
      # Step 4: Return invisible extracted files
      invisible(attr(res, "extracted"))
    }
  } else {
    # If using an external unzip command
    
    WINDOWS <- .Platform$OS.type == "windows"
    
    if (!is.character(unzip) || length(unzip) != 1L || !nzchar(unzip)) 
      stop("'unzip' must be a single character string")
    
    zipfile <- path.expand(zipfile)
    
    if (list) {
      # Step 5: List files without actually extracting
      res <- if (WINDOWS) 
        system2(unzip, c("-ql", shQuote(zipfile)), stdout = TRUE)
      else system2(unzip, c("-ql", shQuote(zipfile)), stdout = TRUE, 
                   env = c("TZ=UTC"))
      
      l <- length(res)
      res2 <- res[-c(2, l - 1, l)]
      res3 <- gsub(" *([^ ]+) +([^ ]+) +([^ ]+) +(.*)", 
                   "\\1 \\2 \\3 \"\\4\"", res2)
      con <- textConnection(res3)
      on.exit(close(con))
      z <- read.table(con, header = TRUE, as.is = TRUE)
      dt <- paste(z$Date, z$Time)
      
      formats <- if (max(nchar(z$Date) > 8)) 
        c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y")
      else c("%m-%d-%y", "%d-%m-%y", "%y-%m-%d")
      
      slash <- any(grepl("/", z$Date))
      if (slash) 
        formats <- gsub("-", "/", formats, fixed = TRUE)
      
      formats <- paste(formats, "%H:%M")
      
      for (f in formats) {
        zz <- as.POSIXct(dt, tz = "UTC", format = f)
        if (all(!is.na(zz))) 
          break
      }
      
      z[, "Date"] <- zz
      z[c("Name", "Length", "Date")]
    } else {
      # Step 6: Extract files
      args <- character()
      if (junkpaths) 
        args <- c(args, "-j")
      if (overwrite) 
        args <- c(args, "-oq", shQuote(zipfile))
      else args <- c(args, "-nq", shQuote(zipfile))
      if (length(files)) 
        args <- c(args, shQuote(files))
      if (exdir != ".") 
        args <- c(args, "-d", shQuote(exdir))
      if (WINDOWS) 
        system2(unzip, args, stdout = NULL, stderr = NULL, 
                invisible = TRUE)
      else system2(unzip, args, stdout = NULL, stderr = NULL)
      invisible(NULL)
    }
  }
}

# Example Usage
# Unzipping a file and listing the extracted files
result <- unzip_files(zipfile = "./data/Dataset.zip", list = TRUE)
cat("Outcome: The following files have been extracted:\n")
print(result)

# Conclusions
# - The unzip_files function provides a convenient way to extract files from a zip archive in R.
# - It supports both internal R unzip functionality and external unzip commands.
# - The function allows users to list the files that would be extracted without actually extracting them.
# - Users can specify various parameters such as overwrite, junkpaths, and the extraction directory.
# - The function returns extracted file information, including file names, lengths, and modification dates.
# - It can be easily incorporated into data preprocessing or analysis workflows that involve handling zip files.
# - Overall, the function is a valuable tool for working with compressed datasets in R.

## Thank you. Sonja Janssen
