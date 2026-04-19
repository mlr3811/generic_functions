varmatch_generic <- function(var_vector, df_key, na_add=F){
  
  neworder <- df_key[match(var_vector, df_key[,1]),2]
  neworder[which(is.na(neworder)==T)] <- var_vector[which(is.na(neworder)==T)]
  #print("vartable is vector; first df_key col is uncorrected var")
  if(na_add==T){neworder <- df_key[match(var_vector, df_key[,1]),2]}
  neworder
}