library(tidyverse)

#get total confirmed cases
total_cases <- read.csv("map/covid_confirmed_usafacts.csv") 

#total number of columsn 
last_col <- ncol(total_cases)

#information columns in data set
info_cols <- c(1:4)

#date starts in column 5
s_date <- 5

#rename date columns into date format 
names(total_cases)[s_date:last_col] <- format(as.Date(names(total_cases)[s_date:last_col], format = "X%m.%d.%y"), format = "%Y-%m-%d")

#rename columns 
names(total_cases)[info_cols] <- c("countyFIPS", "county_name", "state", "stateFIPS")



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

f_DataFrame <- function(df, vec_func){
  last_row <- nrow(df)
  datalist = list()
  for (i in 1:last_row){
    datalist[[i]] <- vec_func(df[i,])
       print(i)
  }
  new_df <- bind_rows(datalist)
  new_df
}


total_cases_state <- total_cases  %>% 
  select(-c(1:2, 4)) %>% 
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)
total_cases_state


total_cases_county <- total_cases %>%
  select(-c(2:4)) %>%
  filter(countyFIPS != 0) %>%
  as.tibble()

total_cases_county

test <- total_cases_county[1,]

f_GrowthFactor(test)

gf_county <- f_DataFrame(total_cases_county, f_GrowthFactor)
save(gf_county, file = "data/gf_county.rda")

gf_state <- f_DataFrame(total_cases_state, f_GrowthFactor)
save(gf_state,file="data/gf_state.rda")



