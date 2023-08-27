inicia_portfolio <- function(df, pair, valor_inicial) {
  df$cash <- 0
  df$total_portfolio <- 0
  df$dia_compra<-NA
  
  df[[paste0(pair[1], "_qtd")]] <- 0
  df[[paste0(pair[1], "_compra_close")]] <- 0
  df[[paste0(pair[2], "_qtd")]] <- 0
  df[[paste0(pair[2], "_compra_close")]]<- 0
  
  df$cash[1] <- valor_inicial
  df$total_portfolio[1] <- valor_inicial
  
  return(df)
}