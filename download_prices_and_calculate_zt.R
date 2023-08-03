library(tidyquant)
library(zoo)
library(dplyr)
library(lubridate)

download_prices_and_calculate_zt <- function(pair, init_date_treino, end_date_treino) {
  
  init_date_download <- as.Date(init_date_treino) - days(2000)
  
  # 1. Downloading the data
  df1 <- tq_get(pair[1], from = init_date_download, to = end_date_treino)
  df2 <- tq_get(pair[2], from = init_date_download, to = end_date_treino)

  
  # 2. Calculating 1000 days moving averages and dividing the closing price by the moving average
  df1 <- df1 %>%
    arrange(date) %>%
    mutate(mavg_1000 = rollmeanr(close, 1000, fill = NA),
           normalized_close = close / mavg_1000)
  
  df2 <- df2 %>%
    arrange(date) %>%
    mutate(mavg_1000 = rollmeanr(close, 1000, fill = NA),
           normalized_close = close / mavg_1000)
  
  # 3. Dropping dates earlier than the initial date
  df1 <- df1 %>%
    filter(date >= as.Date(init_date_treino))
  
  df2 <- df2 %>%
    filter(date >= as.Date(init_date_treino))
  
  # Combining the data
  df <- full_join(df1, df2, by = "date", suffix = c(paste0("_", pair[1]), paste0("_", pair[2])))
  
  # Selecting the columns of interest
  df <- df %>% 
    select(date, paste0("close_", pair[1]), paste0("close_", pair[2]), 
           paste0("normalized_close_", pair[1]), paste0("normalized_close_", pair[2]))
  
  # Calculating Zt
  df <- df %>% mutate(Zt = df[[paste0("normalized_close_", pair[1])]] - df[[paste0("normalized_close_", pair[2])]])
  
  
  return(df)
}



