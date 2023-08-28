# Project full coding for Unzip the file dataset to data external directory
 R.programming assignment Unzip file
##   Lectures: By Professor Jeffrey Leek
##   University: John Hopkins University Bloomberg School of Public Health
##   Codes created by Sonja Janssen-Sahebzad
##   Notes: Created with R 4.2.2 for Windows in RStudio
##   Date:  30 March 2023

#*******************************************************************************
### Unzip the file
### unzip(zipfile="./data/Dataset.zip",exdir="./data")                           ### codes tested = ok!
### Unzipped file dataSet to data exdirectory
#*******************************************************************************
### Codes explained by Sonja Janssen-Sahebzad

### function (zipfile, files = NULL, list = FALSE, overwrite = TRUE, 
          junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE) 
{
###  if (identical(unzip, "internal")) {
    if (!list && !missing(exdir)) 
      dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
    res <- .External(C_unzip, zipfile, files, exdir, list, 
                     overwrite, junkpaths, setTimes)
    if (list) {
      dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M", tz = "UTC")
      data.frame(Name = res[[1]], Length = res[[2]], Date = dates, 
                 stringsAsFactors = FALSE)
    }
    else invisible(attr(res, "extracted"))
  }
###  else {
    WINDOWS <- .Platform$OS.type == "windows"
    if (!is.character(unzip) || length(unzip) != 1L || !nzchar(unzip)) 
      stop("'unzip' must be a single character string")
    zipfile <- path.expand(zipfile)
    if (list) {
      res <- if (WINDOWS) 
        system2(unzip, c("-ql", shQuote(zipfile)), stdout = TRUE)
      else system2(unzip, c("-ql", shQuote(zipfile)), stdout = TRUE, 
                   env = c("TZ=UTC"))
      l <- length(res)
      res2 <- res[-c(2, l - 1, l)]
      res3 <- gsub(" *([^ ]+) +([^ ]+) +([^ ]+) +(.*)", 
                   "\\1 \\2 \\3 \"\\4\"", res2)
 ###  con <- textConnection(res3)
      on.exit(close(con))
      z <- read.table(con, header = TRUE, as.is = TRUE)
      dt <- paste(z$Date, z$Time)
###      formats <- if (max(nchar(z$Date) > 8)) 
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
    }
    else {
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
