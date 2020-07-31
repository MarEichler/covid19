#total cases broken down by state and county 
#get total confirmed cases
#download data here: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"

#import link
total_deaths <- read.csv(link)

#total number of columsn 
last_col <- ncol(total_deaths)

#rename date columns into date format 
names(total_deaths)[5:last_col] <- format(as.Date(names(total_deaths)[5:last_col], format = "X%m.%d.%y"), format = "%Y-%m-%d")

#rename columns 
names(total_deaths)[1:4] <- c("countyFIPS", "county_name", "state", "stateFIPS")


#save unedited version of csv 
write.csv(total_deaths, "data/covid_deaths_usafacts.csv") #save a verson of original data in csv form 
save(total_deaths, file = "data/total_deaths.rda")




