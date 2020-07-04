
## START OF function
f_ma7_norm <- function(vec){
  id <- vec[,1] #get id name
  v <- vec[,-1] #remove id name; just todal cases
  dates <- colnames(v)
  n_days <- length(dates)
  ma <- zoo::rollmean(t(v), k = 7, align = "right")
  norm <- ma / max(v)
  norm <- t(norm)
  colnames(norm) <- dates[7:(n_days)]
  norm <- cbind(id, norm)
  norm
}
###################### END OF FUNCTION 

