calculate_metrics_old <- function(df, pair) {
  metrics <- list()
  
  # Determine where we hold some position
  hold_position <- df[[paste0(pair[1], "_qtd")]] > 0 | df[[paste0(pair[2], "_qtd")]] > 0
  
  # Calculate metrics using all data, but using 0 when all money is in cash
  metrics$sharpe <- ifelse(hold_position,
                           sqrt(252)*mean(df$operational_return, na.rm = TRUE) / sd(df$operational_return, na.rm = TRUE),
                           0)
  metrics$std_dev <- ifelse(hold_position, sd(df$operational_return, na.rm = TRUE), 0)
  metrics$return <- ifelse(hold_position,
                           (last(df$total_portfolio) - first(df$total_portfolio)) / first(df$total_portfolio),
                           0)
  metrics$log_return <- ifelse(hold_position, log(last(df$total_portfolio) / first(df$total_portfolio)), 0)
  metrics$geometric_return <- ifelse(hold_position,
                                     (last(df$total_portfolio) / first(df$total_portfolio)) ^ (1 / nrow(df)) - 1,
                                     0)
  
  return(metrics)
}

calculate_metrics <- function(df, pair) {
  metrics <- list()
  
  # Determine where we hold some position or state is "sell"
  hold_or_sell <- (df[[paste0(pair[1], "_qtd")]] > 0 | df[[paste0(pair[2], "_qtd")]] > 0) | df$state == "sell"
  
  # Filter the data frame to only the rows where we hold some position or state is "sell"
  df_filtered <- df[hold_or_sell, ]
  
  # Calculate the daily returns
  df_filtered$daily_return <- c(0, diff(df_filtered$total_portfolio) / df_filtered$total_portfolio[-nrow(df_filtered)])
  
  # If there are no rows where we hold a position or state is "sell", return all zeros
  if(nrow(df_filtered) == 0) {
    metrics$sharpe <- 0
    metrics$std_dev <- 0
    metrics$return <- 0
    metrics$log_return <- 0
    metrics$geometric_return <- 0
  } else {
    # Otherwise, calculate metrics using only the rows where we hold some position or state is "sell"
    metrics$sharpe <- mean(df_filtered$daily_return, na.rm = TRUE) / sd(df_filtered$daily_return, na.rm = TRUE)
    metrics$std_dev <- sd(df_filtered$daily_return, na.rm = TRUE)
    metrics$return <- (last(df_filtered$total_portfolio) - first(df_filtered$total_portfolio)) / first(df_filtered$total_portfolio)
    metrics$log_return <- log(last(df_filtered$total_portfolio) / first(df_filtered$total_portfolio))
    metrics$geometric_return <- (last(df_filtered$total_portfolio) / first(df_filtered$total_portfolio)) ^ (1 / nrow(df_filtered)) - 1
  }
  
  return(metrics)
}

