

Simulation_pair_trading <- function(df, params, pair){
  
  # Buy and sell transaction costs
  trans_cost <- 5
  
  # Generate dynamic column names
  qtd_1 <- paste0(pair[1], "_qtd")
  qtd_2 <- paste0(pair[2], "_qtd")
  close_1 <- paste0(pair[1], "_compra_close")
  close_2 <- paste0(pair[2], "_compra_close")
  df[['state']]<-"hold"
  
  for (i in 2:nrow(df)){
    
    # Calculate available cash for transactions
    cash_avail <- df$cash[i-1] - trans_cost
    
    # Check buying condition
    if(df$Zt[i] >= params$x0 & df$Zt[i] <= params$x1 & cash_avail > 0 & df[[qtd_1]][i-1] == 0 & df[[qtd_2]][i-1] == 0) {
      df$dia_compra[i] <- df$date[i]
      
      df[[qtd_1]][i] <- cash_avail*0.5 / df[[paste0("close_",pair[1])]][i]
      df[[close_1]][i] <- df[[paste0("close_",pair[1])]][i]
      
      df[[qtd_2]][i] <- -1 * (cash_avail*0.5 / df[[paste0("close_",pair[2])]][i])
      df[[close_2]][i] <- df[[paste0("close_",pair[2])]][i]
      
      df$cash[i] <- df$cash[i-1] - (df[[qtd_1]][i] * df[[close_1]][i]) + (df[[qtd_2]][i] * df[[close_2]][i]) - 2*trans_cost
      
      df$state[i]<-"buy"
      
    } else if((df$Zt[i] < params$M | df$Zt[i] > params$x2) & df[[qtd_1]][i-1] != 0 & df[[qtd_2]][i-1] != 0) {
      # Sell condition
      df$cash[i] <- df$cash[i-1] + (abs(df[[qtd_1]][i-1]) * df[[paste0("close_",pair[1])]][i]) + (abs(df[[qtd_2]][i-1]) * df[[paste0("close_",pair[2])]][i]) - 2*trans_cost
      
      df[[qtd_1]][i] <- 0
      df[[qtd_2]][i] <- 0
      df[[close_1]][i] <- 0
      df[[close_2]][i] <- 0
      
      df$state[i]<-"sell"
      
    } else if(!is.na(df$dia_compra[i-1])) {
      # Hold condition
      df$dia_compra[i] <- df$dia_compra[i-1]
      df[[qtd_1]][i] <- df[[qtd_1]][i-1]
      df[[qtd_2]][i] <- df[[qtd_2]][i-1]
      df[[close_1]][i] <- df[[close_1]][i-1]
      df[[close_2]][i] <- df[[close_2]][i-1]
      df$cash[i] <- df$cash[i-1]
      
    } else {
      df$cash[i] <- df$cash[i-1]
    }
    
    # Calculate portfolio value
    df$total_portfolio[i] <- df$cash[i] + (df[[qtd_1]][i] * df[[paste0("close_",pair[1])]][i]) + (abs(df[[qtd_2]][i]) * df[[paste0("close_",pair[2])]][i])
    
  }
  
  return(df)
}

operational_return <- function(df){
  
  # Create a new column for operational return and initialize it with 0s
  df$operational_return <- 0
  
  # Initialize variables to hold the previous portfolio total for buy and sell states
  buy_total <- 0
  sell_total <- 0
  
  # Loop over all rows of the dataframe
  for(i in 1:nrow(df)){
    
    # Check if the state is 'buy' or 'sell'
    if(df$state[i] == "buy"){
      # Update buy_total with the current total portfolio
      buy_total <- df$total_portfolio[i]
    }else if(df$state[i] == "sell"){
      # Update sell_total with the current total portfolio
      sell_total <- df$total_portfolio[i]
      
      # Calculate the operational return for this operation (sell - buy)
      operation_return <- (sell_total - buy_total)/buy_total
      
      # Assign the calculated return to the current row's 'operational_return' column
      df$operational_return[i] <- operation_return
      
      # Reset buy_total and sell_total for the next operation
      buy_total <- 0
      sell_total <- 0
    }
  }
  
  # Return the dataframe with the new 'operational_return' column
  return(df)
}



holding_days <- function(df){
  # Initialize a new column for holding days
  df$holding_days <- 0
  
  # Iterate over the rows of the dataframe
  for(i in 2:nrow(df)){
    # If the current state is "hold", increment the holding days by 1 from the previous day
    if(df$state[i] == "hold"){
      df$holding_days[i] <- df$holding_days[i - 1] + 1
    }
  }
  
  # Return the modified dataframe
  return(df)
}



#df_simulation <- Simulation_pair_trading(df, params, pair)

#df_simulation <- operational_return(df_simulation)

#df_simulation <- holding_days(df_simulation)


#View(df_simulation)
