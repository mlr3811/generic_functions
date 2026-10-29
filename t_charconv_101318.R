#Tools : Character converter
#takes character variable that is unparsed or with $ and gets the right thing
#"hpainflam$age" -> age, "\"age\"" -> "age"
#outputs character string of that variable
#list lets you
#look a blinreg as this incorporates tcharconv to allow reading of any string

tcharconv <- function (x ,vectorlist=0, datasetfinder=0) {
  if (vectorlist ==0) {


    #if (typeof(x) == "character") {return(x)}
    #varname <- deparse(substitute(x))
    varname <- x

    if (length(grep("\"",varname)) >0) {
      varname <- substr(varname, 2,nchar(varname)-1)
    }
    dollarloc <- 1 + grep("\\$",strsplit(varname, split="")[[1]])
    dollarloc <- type.convert(dollarloc,"numeric")
    if (typeof(dollarloc) != "integer") {
      dollarloc <- 1

    }


    if (datasetfinder==0) {
      varname <- substr(varname, dollarloc, nchar(varname))
    }

    if (datasetfinder != 0) {
      #print(varname)
      varname <- substr(varname, 1, dollarloc-2)
    }


    return(varname)
  }

  #defunct code..not needed
  output <- list()
  if (vectorlist != 0) {

    varlist <- as.matrix(x)

    for (num in varlist) {

      if (typeof(num) == "character") {varname<- num}
      if (typeof(num) != "character") {

        varname <- deparse(substitute(num))
        print(varname)
        dollarloc <- 1 + grep("\\$",strsplit(varname, split="")[[1]])
        dollarloc <- type.convert(dollarloc,"numeric")

        if (typeof(dollarloc) != "integer") {
          dollarloc <- 1

        }


        varname <- substr(varname, dollarloc, nchar(varname))
      }

      output <- c(output, varname)

    }
    output <- mapply(as.character, lelement(output,1))

    print(output)
    return(output)
  }


}


