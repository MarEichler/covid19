f_WeeklyAvg <- function(vec){
  id <- vec[,1]
  v <- vec[, -1]
  dates <- colnames(v)
  min_date <- as.Date(min(dates))
  max_date <- as.Date(max(dates))
  
  select_days <- seq(min_date, max_date, "week")
  week_numb <-  seq(1, length(select_days), 1)
  
  
  week <- c() 
  for (i in week_numb){
    week <- c(week, rep(i, 7))
  }
  
  
  remove_days <- 7 - as.integer(max_date - max(select_days)) - 1
  actual_days <- length(week) - remove_days 
  
  week <- week[1:actual_days]
  
  colnames(v) <- NULL
  rownames(v) <- "gf"
  
  gf_mean <- cbind(t(v), week) %>%
    as_tibble() %>%
    group_by(week) %>%
    summarize(gf_mean = mean(gf)) %>%
    select(gf_mean)
  
  gf_mean <- t(gf_mean)
  colnames(gf_mean) <- paste("Week", week_numb)
  gf_mean <- cbind(id, gf_mean)
  gf_mean
}


#weekly gf avg 
state_weekly_wide <- f_DataFrame(as_tibble(gf_state_wide), f_WeeklyAvg)


min_date <- as.Date(min(covid19_state$date))
max_date <- as.Date(max(covid19_state$date))

start_days <- seq(min_date, max_date, "week")

week_df <- data.frame(
  "start_day" = start_days, 
  "end_day" = start_days + 6,
  "week" = paste("Week", seq(1, length(start_days), 1))
)

covid19_state_weekly <- state_weekly_wide %>%
  pivot_longer(c(-1), names_to = "week", values_to = "gf_ma") %>%
  mutate( growth_factor = cut(gf_ma, breaks = gf_breaks , labels = gf_labels , right = gf_right)) %>%
  left_join(., week_df, by = c("week" = "week"))



save(covid19_state_weekly, file = "data/covid19_state_weekly.rda") # send to data folder 