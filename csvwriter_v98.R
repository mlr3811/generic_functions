#' CSV Writer : Core Fx
#' @description Take a variable of any kind and saves it to csv file. Update 9.2.19
#' @name clears
#' @param variable, character element, drive
#' @param hello
#' @param working
#' @return working
#' @examples working

csv <- function(variable, varname=0, read=F) {
  #recent update adds function to clear a dataset by saving as csv and then reading this csv file

  newdrive <- paste0(savedesktop_global, "/R Scripts/RDataResults")
  timename <- as.character(Sys.time())
  timename <- gsub (" ", "_", timename)
  timename <- gsub (":", "_", timename)
  timename <- gsub ("-", "_", timename)
  if (varname==0) {varname <- deparse(substitute(variable))}

  csvname <- paste(varname, "_",timename,".csv", sep="")

  setwd(newdrive) #dataoutput folder
  if (class(try(as.data.frame(variable), silent=TRUE)) !="try-error") {
  write.csv(variable, file = csvname)
  }

  if (class(try(as.data.frame(variable), silent=TRUE)) =="try-error") {
    capture.output(variable, file=csvname)
  }

  output <- 0
  if (read==T){
    print(newdrive)
    df <- file.info(list.files(newdrive, pattern = "*.csv", full.names = T))
    csvfile_temp <- rownames(df)[which.max(df$mtime)]
    output <- read.csv(csvfile_temp, stringsAsFactors = F)[,-1]
    df <- file.info(list.files(newdrive, pattern = "*.csv", full.names = T))
    csvfile_temp <- rownames(df)[which.max(df$mtime)]
    file.remove(csvfile_temp)
  }
  invisible(output)
}
