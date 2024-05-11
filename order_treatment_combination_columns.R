order_trt_comb_col <- function(dataframe, column_number, proper_order) {
  
  temp_df <- dataframe
  
  for (i in 1:dim(dataframe[,column_number])[1]) {
    dataframe_index <- which(dataframe[,column_number] == proper_order[i])
    temp_df[i,] <- dataframe[dataframe_index,]
  }
  
  dataframe <- temp_df
  
  return(dataframe)
  
}
