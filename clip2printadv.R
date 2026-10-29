clip2print_adv <- function(x) {
  "Excel/clipboard to print"
  tableread <- read.table("clipboard",sep="\t", header = F, stringsAsFactors = F)
  #temp <- readClipboard()
  if (dim(tableread)[2]>1){
    allcols <- data.frame()
    for (a in 1:(dim(tableread)[2])){
      col_cur <- tableread[a]
      col_temp <- paste(col_cur, sep=",")
      if (a ==1){allcols <- paste(allcols,paste0("col_",a),"=", col_temp)}
      if (a >1){allcols <- paste(allcols, ",",paste0("col_",a),"=", col_temp)}
    }
    allcols <- paste(allcols, ", stringsAsFactors=F)")
    cat("df <- data.frame(", allcols)
  }
  if (dim(tableread)[2]==1){
    temp <- readClipboard()
    temp <- list(c(temp))
    yup <- paste(temp, sep=",")
    cat(yup)
  }
}
