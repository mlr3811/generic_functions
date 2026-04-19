var2clip <- function(x, rownames=F, colnames=F){
  write.table(x, "clipboard-16384", sep="\t", row.names=rownames, col.names = colnames)
}