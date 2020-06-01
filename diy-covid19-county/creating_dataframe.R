load("data/covid19_county.rda")

gf_labels <- c("0", "0-1", "1-2", "2+")
gf_breaks <-  c(-Inf, 0,1, 2, Inf)
gf_right <- TRUE


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



#APPLY TO DATA FRAME 
f_DataFrame <- function(df, vec_func){
  last_row <- nrow(df)
  datalist = list()
  for (i in 1:last_row){
    datalist[[i]] <- vec_func(df[i,])
  }
  new_df <- bind_rows(datalist)
  new_df
}


county_wider <-  covid19_county %>%
  select(-nc) %>%
  pivot_wider(names_from = date, values_from = gf)


county_rollmean <- f_DataFrame(county_wider, f_ma7)


min_date <- min(colnames(county_rollmean)[-1])
max_date <- max(covid19_county$date)
select_days <- seq(as.Date(min_date), as.Date(max_date), "week") 

weeks <- data.frame(
  "week_start_day" = select_days
  , "week" = seq(1, length(select_days), 1)
)

county_week <- county_rollmean %>%
  pivot_longer(c(-1), names_to = "date", values_to = "gf_ma") %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% select_days) %>%
  left_join(., weeks, by = c("date" = "week_start_day"))%>%
  mutate( growth_factor = cut(gf_ma, breaks = gf_breaks , labels = gf_labels , right = gf_right)) 

save(county_week, file = "diy-covid19-county/county_week.rda")


  