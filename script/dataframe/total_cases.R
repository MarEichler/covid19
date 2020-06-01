#total cases broken down by state and county 
#get total confirmed cases
#download data here: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"

#import link
total_cases <- read.csv(link)

#total number of columsn 
last_col <- ncol(total_cases)


#rename date columns into date format 
names(total_cases)[5:last_col] <- format(as.Date(names(total_cases)[5:last_col], format = "X%m.%d.%y"), format = "%Y-%m-%d")

#rename columns 
names(total_cases)[1:4] <- c("countyFIPS", "county_name", "state", "stateFIPS")

#save unedited version of csv 
write.csv(total_cases, "data/covid_confirmed_usafacts.csv") #save a verson of original data in csv form 
save(total_cases, file = "data/total_cases.rda")


