clip2print <- function(x) {
  "Excel/clipboard to print"
  temp <- readClipboard()
  temp <- list(c(temp))
  yup <- paste(temp, sep=",")
  cat(yup)
}
