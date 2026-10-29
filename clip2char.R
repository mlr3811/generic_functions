clip2char <- function(inputlist=0, quotes=F) {
  separator <- ","
  if (inputlist==0) {
    inputlist <- as.list(readClipboard())
  }
  clip_paste <- function(...) {
    paste(..., sep=separator)
  }
  output <- do.call("clip_paste", inputlist)
  if (quotes==T){output <- sapply(strsplit(output, ','), function(x) toString(dQuote(x)))}
  output
}
