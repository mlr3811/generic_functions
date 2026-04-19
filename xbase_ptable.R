#' XBase : Table percentages
#' @description Provides list table percentages
#' @param (number of file in list desired, directory (defaulated to loading dir))
#' @return either a list of files in loading directtory or will open the file in R to edit
#' @examples sttest("gender", "mtcars, "mtcars.csv", "C:/Users")


ptable <- function(x=0,y=0, type_1row_2col=1, rounder=4 ) {
  xname <- as.character(tcharconv(deparse(substitute(x))))
  yname <- as.character(tcharconv(deparse(substitute(y))))
  x <- as.numeric(x)#[which(x[is.na(x)==FALSE])])
  y <- as.numeric(y)#[which(y[is.na(y)==FALSE])])
  type <- type_1row_2col
  if (type == 1) {
    if (mean(y, na.rm=TRUE)==0) {
      temp <- table(x)
      output <- round(prop.table(temp), rounder)*100
    }
    if (mean(y, na.rm=TRUE) != 0) {
      temp <- table(x, y)
      output <- round(prop.table(temp, 1), rounder)*100
    }
    print("Raw Table")
    printname <- paste("X%=",xname)
    printname2 <- paste("Y%=",yname)
    print(temp)
    print(printname)
    print(printname2)
    #print(output)
    return(output)
  }

  if (type == 2) {
    if (mean(y)==0) {
      temp <- table(x)
      output <- round(prop.table(temp), rounder)*100
    }
    if (mean(y) != 0) {
      temp <- table(x, y)
      output <- round(prop.table(temp, 2), rounder)*100
    }
    printname <- paste("X%=",xname)
    printname2 <- paste("Y%=",yname)
    print("Raw Table")
    print(temp)
    print(printname)
    print(printname2)
    print(output)
    return(output)
  }

}
