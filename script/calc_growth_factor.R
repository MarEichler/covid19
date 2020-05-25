library(tidyverse)

#get total confirmed cases
#download data here: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"

#import link
total_cases <- read.csv(link)

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

#save unedited version of csv 
write.csv(total_cases, "data/covid_confirmed_usafacts.csv")
####################################################

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
       print(i)
  }
  new_df <- bind_rows(datalist)
  new_df
}
###### END OF APPLY TO DATA FRAME FUNCTION #####


### SUM UP COUNTIES FOR EACH STATE
total_cases_state <- total_cases  %>% 
  select(-c(1:2, 4)) %>% 
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)


#REMOVE EXCESS INFO COLUMNS; REMOVE DATA NOT ASSIGNED TO A COUNTY 
total_cases_county <- total_cases %>%
  select(-c(2:4)) %>%
  filter(countyFIPS != 0) %>%
  as_tibble()


#CALCULATE GROWTH FACTOR BY COUNTY 
gf_county <- f_DataFrame(total_cases_county, f_GrowthFactor)
save(gf_county, file = "data/gf_county.rda")

#CALCULATE GROWTH FACTOR BY COUNTY 
gf_state <- f_DataFrame(total_cases_state, f_GrowthFactor)
save(gf_state,file="data/gf_state.rda")

#MAKE DATA TIDY AND CHOP INTO GROUPS 
gf_county_tidy <- gf_county %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))
save(gf_county_tidy, file = "data/gf_county_tidy.rda") 

gf_state_tidy <- gf_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 
save(gf_state_tidy, file = "data/gf_state_tidy.rda")

