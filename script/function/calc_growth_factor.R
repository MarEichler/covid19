
#GROWTH FACTOR FUNCTION 
f_GrowthFactor <- function(vec){
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
  
  #create vectors of new cases  for 'today' and 'yesterday'
  nc_today <- nc
  nc_yesterday <- c(0, nc[1:(n_days-1)])
  
  #calculate growth factor
  #if new_cases_yesterday = 0; set growth rate equal to new cases today
  # technically gf = Inf; difficult to work with so just do literally how many new cases there was
  gf <- ifelse(nc_yesterday == 0, nc_today, nc_today/nc_yesterday)
  
  gf <- t(gf)
  colnames(gf) <- dates
  growth_factor <- cbind(id, gf)
  growth_factor
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