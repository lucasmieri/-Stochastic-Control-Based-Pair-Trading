compra <- function(pair, df, data, params){
  # Find the row with the date in df
  i <- which(df$date == as.Date(data))
  
  if(length(i) > 0){
    
    # Find the row with the date-1 in df
    i_prev <- i - 1
    
    if((i_prev > 0) & (df$cash[i_prev]>0)){
      # Update dia_compra with current date
      df$dia_compra[i] <- as.Date(data)
      # Use all cash from date-1 to take a position long on pair[1] and short on pair[2]
      df[[paste0(pair[1], "_qtd")]][i] <- df$cash[i_prev] / df[[paste0("close_", pair[1])]][i_prev]
      df[[paste0(pair[1], "_compra_close")]][i] <- df[[paste0("close_", pair[1])]][i]
      df[[paste0(pair[2], "_qtd")]][i] <- df$cash[i_prev] / df[[paste0("close_", pair[2])]][i_prev]
      df[[paste0(pair[2], "_compra_close")]][i] <- df[[paste0("close_", pair[2])]][i]
      df$total_portfolio[i]<-df$cash[i_prev]
      
      params$dia_compra<-data
    }
  }
  
  return(df)
}

nada <- function(pair, df, data){
  # Find the row with the date in df
  i <- which(df$date == as.Date(data))
  
  if(length(i) > 0){
    # Copy quantities and cash from the previous day
    df[[paste0(pair[1], "_qtd")]][i] <- df[[paste0(pair[1], "_qtd")]][i - 1]
    df[[paste0(pair[2], "_qtd")]][i] <- df[[paste0(pair[2], "_qtd")]][i - 1]
    df$cash[i] <- df$cash[i - 1]
    df[[paste0(pair[1], "_compra_close")]][i] <- df[[paste0(pair[1], "_compra_close")]][i - 1]
    df[[paste0(pair[2], "_compra_close")]][i] <- df[[paste0(pair[2], "_compra_close")]][i - 1]
  }
  
  
  return(df)
}

venda<-function(params,df,pair,date){
  
}

get_trade_params <- function(params, df, pair, date){
  # Find the row with the date in df
  i <- which(df$date == as.Date(date))
  
  if(length(i) > 0){
    # Assign quantities and purchase prices to params
    params$dia_compra<-data
    params[[paste0(pair[1], "_qtd")]] <- df[[paste0(pair[1], "_qtd")]][i]
    params[[paste0(pair[1], "_compra_close")]] <- df[[paste0(pair[1], "_compra_close")]][i]
    params[[paste0(pair[2], "_qtd")]] <- df[[paste0(pair[2], "_qtd")]][i]
    params[[paste0(pair[2], "_compra_close")]] <- df[[paste0(pair[2], "_compra_close")]][i]
  }
  
  return(params)
}

venda <- function(params, df, pair, date){
  # Find the row with the date in df
  i <- which(df$date == as.Date(date))
  
  if(length(i) > 0){
    # Calculate returns based on params
    # pair1: long
    retorno_pair1 <- params[[paste0(pair[1], "_qtd")]] * params[[paste0(pair[1], "_compra_close")]] *
      (1 + df[[paste0("close_", pair[1])]][i] - params[[paste0(pair[1], "_compra_close")]])
    
    
    # pair2: short
    retorno_pair2 <- params[[paste0(pair[2], "_qtd")]] * params[[paste0(pair[2], "_compra_close")]] *
      (1 - df[[paste0("close_", pair[2])]][i] + params[[paste0(pair[2], "_compra_close")]])
    
    
    # Update cash in df with the sum of the returns
    df$cash[i] <- df$cash[i] + retorno_pair1 + retorno_pair2
  }
  
  return(df)
}

# Example usage
data <- "2017-01-03"

df <- compra(pair, df, "2017-01-03", params)
params$dia_compra<-"2017-01-03"

params <- get_trade_params(params, df, pair, data)

df<-venda(params, df,pair,  "2017-01-04")


