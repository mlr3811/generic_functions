#' powerchitest v12.2.18
#' will take a dichtomous variable and run chi tests against a dataset of variables. Does fisher's exact if the chi doesn't work out too well.
#' dichtvar - character variable that is dicht
#' dataset - dataset name (e.g. kongdata)
#' addname - some randomname you'd like to add
#' newdrive - the drive you want to send it to


apowerchitest <- function (testvar, dataset, addname="pchitest", newdrive="C:/Users/Manivel/Desktop/R Scripts/RDataResults") {
  #activating these libraries, as csv writer is required

  dichtvar <- tcharconv(deparse(substitute(testvar)))


  indchitest <- function(dichtvar, dataset) {
    datacolnames <- colnames(dataset)
    results <- lapply(datacolnames, function(x) {

      var1val <- dataset[,dichtvar]
      groups <- as.data.frame(var1val)
      var2val <- dataset[,x]
      group2 <- as.data.frame(var2val)
      grouplevels <- levels(factor(var2val))



      #groupnums is the number in each group. Generating the groupnumbers in each group in the chi 2 test.
      groupnums <- character()
      grouplength<- length(grouplevels)

      for (a in 1:grouplength) {
        groupn <- length(which(var2val==grouplevels[a]))
        tempadd <- paste(grouplevels[a],"-",groupn,"; ")
        groupnums <- paste(groupnums, tempadd)
      }
      groupnums <- as.data.frame(groupnums, stringsAsFactors = FALSE)

      groups<-as.matrix(groups)
      groupsx <- as.matrix(group2)


      temp3 <- cbind("-", "-", "-", "-", "-", "-")


      #making sure chi sq works
      if (class(try(chisq.test(groups, groupsx, correct=FALSE), silent=TRUE)) !="try-error")
      {
        final <- chisq.test(groups, groupsx, correct=FALSE)
        chistat <- round(final[[1]],2)
        pstat <- round(final[[3]],5)

        #groupnum is the number of groups in the nontest variable

        groups0num <- final[[6]][1,2]
        group1num <- final[[6]][2,2]

        #obtaining the minimum expected value
        finalexpectmatrix <- as.matrix(as.list(final[[7]]))
        minexpindex <- which.min(finalexpectmatrix)
        minexp <- unlist(finalexpectmatrix[minexpindex])

        #this is allowing for fisher's exact test
        if (minexp < 5) {
          chistat <- c("Fisher")
          pstat <- c(" x ")
          var1val<- as.data.frame(var1val)
          var2val <- as.data.frame(var2val)
          tempdf <- cbind(var1val, var2val)
          fishtable <- table(tempdf)
          fishresults <- fisher.test(fishtable, alternative="two.sided", simulate.p.value = TRUE)
          pstat <- round(fishresults[[1]],5)
        }
        #print(length(groupnums))
        temp3 <- c(x, chistat, pstat, minexp, groupnums)
      }
      #print(temp3)
      temp3
    })
    #print(results)
    (sapply(results, rbind))
    #ldply(results, rbind)
  }
  chiresults <- indchitest(dichtvar, dataset)

  chiresults <- xlist_df(chiresults, dimfix=1)
  print(dim(chiresults))
  colnames(chiresults) <- c("Test Var", "Chi", "P", "Min Exp", "Groups")
  #chiresults <- sapply(chiresults, rbind)
  print("Power Chi Test Complete")
  printname <- paste("Chi", dichtvar)
  #print(chiresults)
  csv(chiresults, varname=printname, newdrive)
}
