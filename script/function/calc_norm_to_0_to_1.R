
## NORMALIZE TO O TO 1
f_norm01 <- function(vec){
  id <- vec[,1] #get id name
  v <- vec[,-1] #remove id name
  norm <- v/max(v)
  norm <- cbind(id, norm)
  norm
}
###################### END OF FUNCTION

