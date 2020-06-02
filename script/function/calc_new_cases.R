
#GROWTH FACTOR FUNCTION 
f_NewCases <- function(vec){
  id <- vec[,1] #get id name
  tc <- vec[,-1] #remove id name; just todal cases
  n_days <- length(tc) #number of days available
  dates <- colnames(tc) #dates 
  
  #make into a vector 
  colnames(tc) <- NULL 
  tc <- as.vector(t(tc))

  #create vectors of total cases  for 'today' and 'yesterday'
  tc_today <- tc
  tc_yesterday <- c(0, tc[1:(n_days-1)])
  
  #calculate new cases for each day 
  nc <- tc_today - tc_yesterday 
  
  
  nc <- t(nc)
  colnames(nc) <- dates
  new_cases <- cbind(id, nc)
  new_cases 
}
########### END OF GROWTH FACTOR FUNCTION ####
