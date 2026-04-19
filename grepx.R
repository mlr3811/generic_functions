#greps multiple variables at a given time, text is a list of character objects
#update 7.9.20

grepx <- function(text="NULL", pattern="NULL", value=T) {
  # if (text=="NULL" | pattern=="NULL"){
  #   #print("pattern = c('#bill'"',"hi|tum","lo") will exclude bill, include if having lo, or if having hi or tum "))
  #   stop("Missing text or pattern")
  # }


  # text <- c("high", "higher", "highest")
  # pattern <- c("high", "#er", "hi", "#dumb")

  negs <- pattern[grepl(pattern = "#", x = pattern, ignore.case = T)==T]
  patternlen <- length(pattern)-1-length(negs)
  pattern <- pattern[!grepl(pattern = "#", x = pattern, ignore.case = T)]


  #getting the positive
  temp <- grep(text, pattern = pattern[1], value = T, ignore.case = T)
  if (patternlen>0){
    for (a in 1:patternlen){
      temp <- grep(temp, pattern = pattern[a+1], value=T, ignore.case = T)
    }
  }

  #eliminating the negative ones

  if (length(negs)>0){
    negs <- gsub(x = negs, pattern = "#", replacement = "")
    for (b in 1:length(negs)){
      temp <- temp[!grepl(temp, pattern = negs[b], ignore.case = T)]
    }


  }
  temp
}
