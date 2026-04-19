#' Extension: Variable Viewer v 10.14
#' @description variable saver and histogram visualizer
#' @param variable is the variable of interest(data$var or "var")
#' @param dataset is the dataset if you desire
#' @param sortvar is the dichtomized variable that you want to sort by
#' @param bar tells it whether to do bar graph (with actual values on x axis)
#' @param save tells it whether to save
#' @return basic mean, median etc. in console and bargraph in the viewer
#' @examples sttest("gender", "mtcars, "mtcars.csv", "C:/Users")
#' @export
xv <- function(variable, anyvar2=0, dataset=0, sortvar="sortvar", save=0, bar=0, chilevel=7, debug=F) {
  #write.table(variable, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
  #variable <- ukb$sex
  anyvar2=0
  #Basic Calculating and Sorting------
  if (debug==T){
    print("variable1")
    print(variable)
    print("var2")
    print(anyvar2)
    print("dataset")
    if (dataset !=0) {describe(dataset)}
  }
  #if (is.null(variable)==T | is.null(anyvar2)==T){return("One variable is null")}
  #contvar is a 2nd variables
  #wrappers to get rid of existing plots
  try(invisible(dev.off()), silent=TRUE)
  try(invisible(dev.off()), silent=TRUE)
  try(invisible(dev.off()), silent=TRUE)
  try(invisible(dev.off()), silent=TRUE)
  #var2 = with average of this variable

  #print(variable)
  if (dataset ==0) {
    varbname <- deparse(substitute(variable))
    varb2name <- deparse(substitute(anyvar2))
  }
  print(paste("variable:",varbname))
  #print(variable)
  if (dataset != 0) {
    variable <- tcharconv(deparse(substitute(variable)))
    sortvar <- tcharconv(deparse(substitute(sortvar)))
    varbname <- dataset[,variable]
  }

  if (class(variable) == "character") {
    variable <- as.numeric(variable)
  }
  if (class(anyvar2) == "character") {
    anyvar2 <- as.numeric(anyvar2)
  }
  if (debug==T) {print("Made past Basic Calculating")}
  #print (varbname) SORT var--------------
  if (sortvar != "sortvar") {
    if (debug==T) {print("Entering sortvar")}
    sortvar <- dataset[,sortvar]
    if (sortvar == 0 | sortvar ==1 | is.na(sortvar)==TRUE){

      varb1 <- varbname[sortvar==1]
      varb2 <- varbname[sortvar==0]

      table <- list()
      numvar1 <- length(varb1)
      numvar2 <- length(varb2)
      modenum1 <- sum(varb1==getmode(varb1), na.rm=TRUE)
      modenum2 <- sum(varb2==getmode(varb2), na.rm=TRUE)
      maxlen <- 1.2*max(c(modenum1,modenum2 ), na.rm=TRUE)
      maxlen <- round(maxlen,0)
      meanvar1 <- mean(varb1, na.rm=TRUE)
      meanvar2 <- mean(varb1, na.rm=TRUE)
      modevar1 <- getmode(varb1)
      modevar2 <- getmode(varb2)
      sdvar1 <- mean(varb1, na.rm=TRUE)
      sdvar2<- mean(varb1, na.rm=TRUE)
      #table[1] <- c(meanvar1,meanvar2)
      #table[2] <- c(sdvar1, sdvar2)
      #table[3] <- c(modevar1, modevar2)

      histvarb1 <- hist(varb1,plot = FALSE)
      histvarb2 <- hist(varb2,plot = FALSE)
      ## calculate the range of the graph
      lowmin1 <- min(varb1, na.rm=TRUE)
      lowmin2 <- min(varb2, na.rm=TRUE)
      highmax1 <- max(varb1, na.rm=TRUE)
      highmax2 <- max(varb2, na.rm=TRUE)
      lowmin <- c(lowmin1, lowmin2)
      highmax <- c(highmax1,highmax2)
      lowmin <- min(lowmin)
      highmax <- max(highmax)

      xlim <- range(lowmin, highmax)
      ylim <- range(0,50)
      ## plot the first graph
      plot(histvarb1,xlim = xlim, ylim = ylim,
           col = rgb(1,0,0,0.4),xlab = 'Lengths',
           freq = TRUE, ## relative, not absolute frequency
           main = 'Distribution of varb1s and varb2s')
      ## plot the second graph on top of this
      opar <- par(new = FALSE)
      plot(histvarb2,xlim = xlim, ylim = ylim,
           xaxt = 'n', yaxt = 'n', ## don't add axes
           col = rgb(0,0,1,0.4), add = TRUE,
           freq = TRUE) ## relative, not absolute frequency
      ## add a legend in the corner
      legend('topleft',c('varb1s','varb2s'),
             fill = rgb(1:0,0,0:1,0.4), bty = 'n',
             border = NA)
      par(opar)


      return(table)

    }
    print("Sort Var not dichotomized")
  }
  #Sortvar region--------
  if (sortvar == "sortvar") {
    if (debug==T) {print("Entering Sortvar region")}
    timename <- as.character(Sys.time())
    timename <- gsub (" ", "_", timename)
    timename <- gsub (":", "_", timename)
    timename <- gsub ("-", "_", timename)
    info <- hist(variable, xlab=varbname)
    with(info, text(mids, counts, labels=counts, pos=1))

    if (bar != 0) {
      counts <- numeric()
      counts$fac <- as.factor(as.numeric(variable))
      bardata <- (table(as.numeric(counts$fac)))
      print(length(bardata))
      print(colnames(as.data.frame(bardata)))
      bargraph <- barplot(bardata)
      text(x = bargraph, counts$fac+0.4, paste("1,2"),  cex = 0.4)
      #countcolname <- levels(counts$fac)
      #colnames(counts) <- as.data.frame(countcolname)

      #counts$freq <- as.numeric(as.character(variable))

      #ylim <- c(0, 1.1*max(counts$freq))

      #print(length(counts))

      #axis(1, at=bargraph, labels=levels(counts$fac), tick=FALSE, las=2, line=-0.5, cex.axis=0.5)
    }

    numlevels1 <- dim(table(variable))
    numlevels2 <- dim(table(anyvar2))
    if (numlevels2 != 1) {
      if (debug==T){print("Entering dual variable")}
      # anyvar2 <- hpainflam$gender_n
      # variable <- hpainflam$group_n

      #doing chi squared and everything with that
      if (numlevels2<chilevel & numlevels1<chilevel){
        chitable <- table(anyvar2, variable)
        names(dimnames(chitable))  <- c(varb2name, varbname)
        print(chitable)
        chitest <- chisq.test(table(anyvar2, variable),correct = F)
        return(chitest)
      }

      #continuous and continous
      if (numlevels1 > chilevel & numlevels2 > chilevel){
        return("Use bplot - Two continous variables")
      }

      #doing any aggregate function
      if (numlevels1<chilevel | numlevels2<chilevel){
        #identifying hte appropriate variable
        smallvar <- anyvar2
        bigvar <- variable
        if (numlevels1<chilevel){
          smallvar <- variable
          bigvar <- anyvar2
        }
        df <- data.frame(smallvar, bigvar)
        aggtable_output <- aggregate(. ~ smallvar, df, function(x) c(mean = mean(x, na.rm=T), sd = sd(x, na.rm = T), min= min(x, na.rm = T), max=max(x, na.rm = T), total_n= length(x)), na.action=na.pass)
        return(aggtable_output)

      }
    }
    #calculates na
    numvar <- as.numeric(variable)
    navar <- length(which(is.na(numvar)==TRUE))

    variable <- as.numeric(variable)
    temp1 <- summary(variable)

    temp1colname <- labels(temp1)
    std1 <- sd(variable, na.rm =TRUE)
    totaln <- round(length(variable),1)
    validn <- round(totaln-navar,1)

    varsummary <- as.data.frame(c(temp1, std1, validn, totaln))
    #varsummary <- lapply(varsummary, round, x=4)
    varsumrownames <- (c(temp1colname, "SD", "Valid N", "Total N"))

    row.names(varsummary) <- varsumrownames
    #colnames(varsummary[1]) <- "SD"
    #colnames(varsummary[2]) <- "Total N"
    return(varsummary)
  }

  if (save==1) {
    setwd("C:/Users/Manivel/Desktop/R Scripts/RDataResults")
    csvname <- paste("NL_", variable, timename,".csv", sep="")
    print(csvname)
    write.csv(varbname, file = csvname)
  }

}

