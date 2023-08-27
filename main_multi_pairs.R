# This script performs the following tasks:
# 1. Identifies and loads necessary R packages, installing them if absent.
# 2. Determines the directory of the currently open script in RStudio.
# 3. Constructs the base directory path by appending the 'functions' sub-directory to the script's directory.
# 4. Lists and subsequently sources relevant R files from the 'functions' sub-directory.

required_packages <- c("tidyquant", "dplyr", "tidyr", "broom", "Rcpp", "rootSolve", 
                       "ggplot2", "zoo", "rstudioapi")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
base_dir <- file.path(script_dir, "functions")

files_to_source <- c("download_prices_and_calculate_zt.R", "parametrizacao.R", 
                     "modelagem.R", "inicia_portfolio.R", "simulacao.R", "metrics.R", 
                     "executa_simulacao.R")

lapply(paste0(base_dir, "/", files_to_source), source)



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

saveRDS(results, file = file.path(script_dir, "simulation_results.rds"))

