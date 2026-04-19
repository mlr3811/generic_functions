#Power Linear Regression
# V 8.24
#all characters for variables. depvar is the first variable. var2 is the variable of interest
#var3-7 are covariates.
#gives output of 4 things (p, var, B, n)
#THIS APPEARS TO BE OFFICIAL - 9.22.18
#' @description Does a mass linear regression on dataset
#' @param pdataset is the dataset with DEPENDENT variables in columns, ids in rows
#' @param pvar2 and onwards are the independent variables you want
#' @return saves results in excel file
#' @examples apowerlinreg_dep(clams, clams$Dx_MDD, clams$age). Which predicts each column based on dx and age
#' @export

apowerlinreg_ind <-function(pdataset, pvar2=0, pvar3=0, pvar4=0, pvar5=0, pvar6=0, pvar7=0, wherepval = 1, rando= 0) {

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
  newresults <- lapply(varlist, blinreg, dataset=pdataset, var2=pvar2,var3=pvar3, var4=pvar4, var5=pvar5, var6=pvar6,var7=pvar7, power=1)

  temp3 <- cbind(lelement(newresults,1), lelement(newresults,2), lelement(newresults,3), lelement(newresults,4),
                 lelement(newresults,5), lelement(newresults,6), lelement(newresults,7), lelement(newresults,8))
  #temp3 <- temp3[order(as.numeric(unlist(lelement(final,6))))]
  #temp3 <- rbind(temp3, )
  labelinfo <- paste(pvar2, pvar3, pvar4, pvar5, pvar6, pvar7, sep=",")
  temp3[,9] <- labelinfo
  names(temp3) <- c("Dep Var", "P value", "B", "SE", "t value", "Lower CI", "Upper CI", "DF" ,"Ind Variables")

  timename <- as.character(Sys.time())
  timename <- gsub (" ", "_", timename)
  timename <- gsub (":", "_", timename)
  timename <- gsub ("-", "_", timename)
  csvname <- paste("Linreg_", pvar2, "_",timename,".csv", sep="")
  setwd("C:/Users/Manivel/Desktop/R Scripts/RDataResults") #dataoutput folder
  write.csv(temp3, file = csvname)
  print("Power linear regression completed - One Ind and Multiple Deps")
}



