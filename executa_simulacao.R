execute_simulation <- function(pair, init_date_treino, end_date_treino, valor_inicial) {
  
  # Step 1: Download prices and calculate zt
  df <- download_prices_and_calculate_zt(pair, init_date_treino, end_date_treino)
  
  # Step 2: Parametrizacao
  params <- parametrizacao(df, pair)
  
  # Step 3: Modelagem
  params <- modelagem(params)
  
  # Step 4: Initialize portfolio
  df <- inicia_portfolio(df, pair, valor_inicial)
  
  # Step 5: Simulation of pair trading
  df_simulation <- Simulation_pair_trading(df, params, pair)
  
  # Step 6: Calculate operational return
  df_simulation <- operational_return(df_simulation)
  
  # Step 7: Calculate holding days
  df_simulation <- holding_days(df_simulation)
  
  # Calculate metrics
  metrics <- calculate_metrics(df_simulation, pair)
  params$sharpe <- metrics$sharpe
  params$std_dev <- metrics$std_dev
  params$return <- metrics$return
  params$log_return <- metrics$log_return
  params$geometric_return <- metrics$geometric_return
  
  # Return the final data frame
  return(list("df_final" = df_simulation, "params" = params))

}

execute_simulations <- function(pairs, init_date_treino, end_date_treino, valor_inicial) {
  
  results <- list()
  
  for (i in seq_along(pairs)) {
    pair <- pairs[[i]]
    
    # Ensure both stocks exist in the time period
    tryCatch({
      result <- execute_simulation(pair, init_date_treino, end_date_treino, valor_inicial)
      results[[paste0(pair[1], "_", pair[2])]] <- list(df_final = result$df_final, params = result$params)
    }, error = function(e) {
      message("An error occurred with pair: ", paste0(pair, collapse = " "), ". Skipping this pair.")
      results[[paste0(pair[1], "_", pair[2])]] <- NULL
    })
  }
  
  return(results)
}

