
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

#APPLY TO DATA FRAME 
f_DataFrame <- function(df, vec_func){
  last_row <- nrow(df)
  datalist = list()
  for (i in 1:last_row){
    datalist[[i]] <- vec_func(df[i,])
   #    print(i)
  }
  new_df <- bind_rows(datalist)
  new_df
}
###### END OF APPLY TO DATA FRAME FUNCTION #####