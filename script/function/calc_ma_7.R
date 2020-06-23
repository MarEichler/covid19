
## START OF WEEKLY AVERAGE 
f_ma7 <- function(vec){
  id <- vec[,1] #get id name
  v <- vec[,-1] #remove id name; just todal cases
  dates <- colnames(v)
  n_days <- length(dates)
  ma <- zoo::rollmean(t(v), k = 7, align = "right")
  ma <- t(ma)
  colnames(ma) <- dates[7:(n_days)]
  ma <- cbind(id, ma)
  ma
}
###################### END OF WEEKLY AVG FUNCTION 



