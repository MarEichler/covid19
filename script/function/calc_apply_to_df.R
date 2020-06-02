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