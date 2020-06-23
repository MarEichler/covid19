
## START OF WEEKLY PERC DIFF
f_perc7 <- function(vec){
  id <- vec[,1] #get id name
  v <- vec[,-1] #remove id name; just todal cases
  dates <- colnames(v)
  n_days <- length(dates)
  perc <- Delt(t(v), k = 7)
  perc <- t(perc)
  colnames(perc) <- dates
  rownames(perc) <- NULL
  perc <- cbind(id, perc)
  perc
}
###################### END  FUNCTION 

