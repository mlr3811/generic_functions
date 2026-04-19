#' Power logistics Regression - Dependent V 10.22


#' @description Does a mass log regression on dataset
#' @param pdataset is the dataset with independent variables in columns, ids in rows
#' @param pvar2 is the dependent variable you want. e.g. one dependent
#' @param pvar3 and onwards are the independent variables to covary
#' @return saves excel file with results
#' @examples apowerlinreg_dep(clams, clams$lethality, clams$age). Which uses lethality as the dependent variable and the clams dataset columns as the independent variable


apowerlinreg_dep <-function(pdataset, pvar2=0, pvar3=0, pvar4=0, pvar5=0, pvar6=0, pvar7=0, wherepval = 1, rando= 0) {

  #making everything character vectors
  if (rando==0) {
    pvar2 <- tcharconv(deparse(substitute(pvar2)))
    pvar3 <- tcharconv(deparse(substitute(pvar3)))
    pvar4 <- tcharconv(deparse(substitute(pvar4)))
    pvar5 <- tcharconv(deparse(substitute(pvar5)))
    pvar6 <- tcharconv(deparse(substitute(pvar6)))
    pvar7 <- tcharconv(deparse(substitute(pvar7)))
  }

  #calls lelement fx
  #calls the simplelinreg fx
  #varlist <- as.list(c("Age", "Gender"))
  varlist <- as.list(colnames(pdataset))
  newresults <- invisible(lapply(varlist, zzblinreg, dataset=pdataset, depvar1=pvar2,var3=pvar3, var4=pvar4, var5=pvar5, var6=pvar6,var7=pvar7, power=1))

  temp3 <- invisible(cbind(lelement(newresults,2), lelement(newresults,3), lelement(newresults,4),
                 lelement(newresults,5), lelement(newresults,6), lelement(newresults,7), lelement(newresults,8)
                 , lelement(newresults,1)))
  #temp3 <- temp3[order(as.numeric(unlist(lelement(final,6))))]
  #temp3 <- rbind(temp3, )
  labelinfo <- paste(pvar3, pvar4, pvar5, pvar6, pvar7, sep=",")
  temp3[,9] <- labelinfo
  temp3[,10] <- pvar2
  names(temp3) <- c("P value", "B", "SE", "t value", "Lower CI", "Upper CI", "DF","Ind Var", "Covariates", "Dep Var")
  temp3 <- temp3[order(temp3[,1]),]

  timename <- as.character(Sys.time())
  timename <- gsub (" ", "_", timename)
  timename <- gsub (":", "_", timename)
  timename <- gsub ("-", "_", timename)
  csvname <- paste("Linreg_", pvar2, "_",timename,".csv", sep="")
  setwd("C:/Users/Manivel/Desktop/R Scripts/RDataResults") #dataoutput folder
  write.csv(temp3, file = csvname)
  print("Power linear regression completed - One Dep and Multiple Inds")
  invisible(temp3)
}


