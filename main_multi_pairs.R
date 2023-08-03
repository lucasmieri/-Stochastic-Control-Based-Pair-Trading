library(tidyquant)
library(dplyr)
library(tidyr)
library(broom)
library(Rcpp)
library(rootSolve)
library(ggplot2)
library(zoo)

source("F:\\l_mie\\Documents\\new_version\\download_prices_and_calculate_zt.R")
source("F:\\l_mie\\Documents\\new_version\\parametrizacao.R")
source("F:\\l_mie\\Documents\\new_version\\modelagem.R")
source("F:\\l_mie\\Documents\\new_version\\inicia_portfolio.R")
source("F:\\l_mie\\Documents\\new_version\\simulacao.R")
source("F:\\l_mie\\Documents\\new_version\\metrics.R")
source("F:\\l_mie\\Documents\\new_version\\executa_simulacao.R")

pairs <- list(
  #c("BBDC3.SA", "BBDC4.SA"),  # Banco Bradesco S.A.
  c("ITUB3.SA", "ITUB4.SA"),  # Itaú Unibanco Holding S.A.
  c("PETR3.SA", "PETR4.SA"),  # Petroleo Brasileiro S.A. - Petrobras
  c("ELET3.SA", "ELET6.SA"),  # Centrais Elétricas Brasileiras S.A. - Eletrobras
  c("GGBR3.SA", "GGBR4.SA"),  # Gerdau S.A.
  c("CMIG3.SA", "CMIG4.SA"),  # Companhia Energética de Minas Gerais - Cemig
  c("BRKM3.SA", "BRKM5.SA"),   # Braskem S.A.
  #swap orders
  #c("BBDC4.SA", "BBDC3.SA"),  # Banco Bradesco S.A.
  c("ITUB4.SA", "ITUB3.SA"),  # Itaú Unibanco Holding S.A.
  c("PETR4.SA", "PETR3.SA"),  # Petroleo Brasileiro S.A. - Petrobras
  c("ELET6.SA", "ELET3.SA"),  # Centrais Elétricas Brasileiras S.A. - Eletrobras
  c("GGBR4.SA", "GGBR3.SA"),  # Gerdau S.A.
  c("CMIG4.SA", "CMIG3.SA"),  # Companhia Energética de Minas Gerais - Cemig
  c("BRKM5.SA", "BRKM3.SA")   # Braskem S.A.
)


init_date_treino <- "2017-01-01"
end_date_treino <- "2022-12-31"
valor_inicial <- 100000


results <- execute_simulations(pairs, init_date_treino, end_date_treino, valor_inicial)

saveRDS(results, file = "simulation_results.rds")

