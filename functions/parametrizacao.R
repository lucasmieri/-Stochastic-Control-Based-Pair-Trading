library(tidyquant)
library(zoo)
library(dplyr)
library(lubridate)


parametrizacao <- function(df, pair){
  
  # Create the model using normalized prices
  model <- lm(df[[paste0("normalized_close_", pair[2])]] ~ df[[paste0("normalized_close_", pair[1])]])
  
  # Tidy the model and extract coefficients
  tm <- tidy(model)
  
  alpha <- tm$estimate[1]
  beta <- tm$estimate[2]
  eps <- model$residuals
  
  # Calcular zt e dzt
  
  dzt <- diff(df$Zt)
  szt <- df$Zt |> head(-1)
  model <- lm(dzt ~ szt)
  tm <- tidy(model)
  a <- tm$estimate[1]
  b <- tm$estimate[2]
  theta <- -tm$estimate[2]
  mu <- tm$estimate[1] / theta
  sigma <- sd(eps)
  
  params <- list( pair=pair,
                  alpha = alpha,
                  beta = beta,
                  a = a,
                  b = b,
                  theta = theta,
                  mu = mu,
                  sigma = sigma,
                  rho=0.1, #fator de desconto, desconto a valor presente
                  K0 = 0.001, #taxa fixa 
                  M = -0.2)
  
  return(params)
}


