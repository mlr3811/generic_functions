winsorize <- function(x, na_rm=T){
  y <- quantile(x, probs=c(.05, .95), na.rm = na_rm)
  x[which(x<y[1])] <- y[1]
  x[which(x>y[2])] <- y[2]
  x
}