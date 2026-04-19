#Power Correlation v 10.18
#will correlate a continuous variable against every column in a dataset

apowercorrelation <- function(powercontvar, powerdataset) {
  print(class(powercontvar))
  if (class(powercontvar) != "numeric") {
    powercontvarname <- powercontvar
    powercontvar <- powerdataset[,powercontvar]
  }


  #dataset with columnnames as the variable of interest. newttest has to have the correct variable that you want
  csvcorrsaver <- function(csvdataset, csvcontvarname) {
    varlist <- as.list(colnames(csvdataset))  #need to input datset name
    varnamelist <- varlist
    if (class(varnamelist)!="list") {varnamelist <- as.list(varnamelist)} #varname list is the list but in a matrix or character format, if you need it.
    final <- lapply(varnamelist, bcorrelationsimple, dataset=csvdataset, var2=csvcontvarname)

  }
  print(1)
  corrfile <- csvcorrsaver(powerdataset, powercontvarname)
  temp3 <- cbind(lelement(corrfile,2), lelement(corrfile,1), lelement(corrfile, 3), lelement(corrfile,4))
  names(temp3) <- c("P value", "Variable", "Coefficient", "n")
  varname1 <- paste("Power Corr_", powercontvarname)
  csvwriter(temp3, varname=varname1)
}


