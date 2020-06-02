
f_ma7 <- function(vec){
  id <- vec[,1] #get id name
  v <- vec[,-1] #remove id name; just todal cases
  dates <- colnames(v)
  n_days <- length(dates)
  gf_ma <- zoo::rollmean(t(v), k = 7, align = "left")
  gf_ma <- t(gf_ma)
  colnames(gf_ma) <- dates[1:(n_days-6)]
  gf_ma <- cbind(id, gf_ma)
  gf_ma
}

