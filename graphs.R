library(tidyquant)
library(dplyr)
library(tidyr)
library(broom)
library(Rcpp)
library(rootSolve)
library(ggplot2)
library(zoo)
script_dir_graph <- dirname(rstudioapi::getSourceEditorContext()$path)
base_dir_graph <- file.path(script_dir_graph, "graphs")

results <- readRDS(file = file.path(script_dir_graph,"simulation_results.rds"))

# Define a function to create and save the plot
create_save_plot <- function(result, params, pair) {
  
  # Create the plot
  p <- ggplot(result, aes(x = date, y = Zt)) +
    geom_line(color = "blue") +
    theme_minimal() +
    labs(title = paste("Long:",pair[1], "- Short:",pair[2], sep = " "),
         x = "Data",
         y = "Zt") +
    geom_hline(aes(yintercept = params$x0), color = "red", linetype = "dashed") +
    geom_hline(aes(yintercept = params$x1), color = "green", linetype = "dashed") +
    geom_hline(aes(yintercept = params$x2), color = "purple", linetype = "dashed") +
    geom_hline(aes(yintercept = params$M), color = "orange", linetype = "dashed")+
    annotate("text", x = min(result$date), y = params$x0, label = paste("x0 = ", round(params$x0, 2)), hjust = -0.1, color = "red") +
    annotate("text", x = min(result$date), y = params$x1, label = paste("x1 = ", round(params$x1, 2)), hjust = -0.4, color = "green") +
    annotate("text", x = min(result$date), y = params$x2, label = paste("x2 = ", round(params$x2, 2)), hjust = -0.7, color = "purple") +
    annotate("text", x = min(result$date), y = params$M, label = paste("M = ", round(params$M, 2)), hjust = -1, color = "orange")
  
  
  # Save the plot
  script_dir_graph <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  graphs_dir <- file.path(script_dir, "graphs")
  
  
  file_name <- paste0(pair[1], "_", pair[2], ".png")
  
  full_path <- file.path(graphs_dir, file_name)
  
  ggsave(filename = full_path, plot = p)
  
}
######################################################################################################


# Apply the function for each pair
for(pair_name in names(results)){
  create_save_plot(results[[pair_name]]$df_final, results[[pair_name]]$params, strsplit(pair_name, "_")[[1]])
}

######################################################################################################
create_distributions <- function(results) {
  
  # Step 1: Combine all df_final
  all_df_final <- bind_rows(lapply(results, function(x) x$df_final))
  
  
  # Step 2: Filter data
  operational_return_data <- all_df_final %>% 
    filter(state != "hold") %>%
    pull(operational_return)
  
  holding_days_data <- all_df_final %>% 
    filter(state == "hold") %>%
    pull(holding_days)
  
  # Step 3: Create and save distributions
  p1 <- ggplot(data = data.frame(operational_return = operational_return_data), aes(x = operational_return)) +
    geom_histogram(color = "black", fill = "lightblue", bins = 100) +
    labs(title = "Distribution of Operational Return",
         x = "Operational Return",
         y = "Frequency")
  
  script_dir_graph <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  graphs_dir <- file.path(script_dir, "graphs")
  
  
  file_name <- paste0("operational_return_distribution.png")
  
  full_path <- file.path(graphs_dir, file_name)
  
  ggsave(filename = full_path, plot = p1)
  
  p2 <- ggplot(data = data.frame(holding_days = holding_days_data), aes(x = holding_days)) +
    geom_histogram(color = "black", fill = "lightblue", bins = 100) +
    labs(title = "Distribution of Holding Days",
         x = "Holding Days",
         y = "Frequency")
  
  file_name <- paste0("holding_days_distribution.png")
  
  full_path <- file.path(graphs_dir, file_name)
  
  
  ggsave(filename = full_path, plot = p2)
}

# Execute the function
create_distributions(results)



all_df_final <- bind_rows(lapply(results, function(x) x$df_final))

# Filter data where state is not "hold"
filtered_data <- all_df_final %>% 
  filter(state != "hold") %>%
  pull(operational_return)

# Perform descriptive analysis
summary_stat <- summary(filtered_data)
mean_val <- mean(filtered_data, na.rm = TRUE)
sd_val <- sd(filtered_data, na.rm = TRUE)
skewness_val <- skewness(filtered_data, na.rm = TRUE)
kurtosis_val <- kurtosis(filtered_data, na.rm = TRUE)

# Print results
print(paste("Summary: ", summary_stat))
print(paste("Mean: ", mean_val))
print(paste("Standard Deviation: ", sd_val))
print(paste("Skewness: ", skewness_val))
print(paste("Kurtosis: ", kurtosis_val))

######################################################################################################
# Function to create a dataframe of metrics from results
extract_metrics <- function(results) {
  metrics <- data.frame()
  
  # Define the names of the parameters we expect in every 'params' list
  param_names <- c("sharpe_ratio", "std_dev", "return", "log_return", "geo_return")
  
  # Iterate through each pair result in the list
  for (pair_result in results) {
    params <- pair_result$params
    
    # Check if all expected parameters are present
    if (all(param_names %in% names(params))) {
      # Add the params data to the metrics dataframe
      metrics <- rbind(metrics, params)
    }
  }
  
  return(metrics)
}

# Use the function to get the metrics
metrics <- extract_metrics(results)


create_metrics_distributions <- function(metrics, dir) {
  # Create a distribution for each metric
  for (metric_name in names(metrics)) {
    p <- ggplot(metrics, aes_string(x=metric_name)) +
      geom_histogram(color="black", fill="lightblue", bins=50) +
      labs(title = paste("Distribution of", metric_name),
           x = metric_name,
           y = "Frequency")
    
    # Save the plot
    ggsave(filename = paste0(dir, "/", metric_name, "_distribution.png"), plot = p)
  }
}

# Use the function to create the distributions
create_metrics_distributions(metrics, base_dir_graph)

create_metrics_plot <- function(df, pair) {
  # Calculate metrics
  metrics <- calculate_metrics(df, pair)
  
  # Create a data frame for plotting
  plot_df <- data.frame(Date = df$date, Total_Portfolio = df$total_portfolio)
  
  # Create the plot
  p <- ggplot(plot_df, aes(x = Date, y = Total_Portfolio)) +
    geom_line(color = "blue") +
    theme_minimal() +
    labs(title = paste("Metrics for pair:", pair[1], "-", pair[2], sep = " "),
         x = "Data",
         y = "Total Portfolio") +
    annotate("text", x = min(plot_df$Date), y = min(plot_df$Total_Portfolio),
             label = paste("Sharpe: ", round(metrics$sharpe, 2),
                           "\nStd Dev: ", round(metrics$std_dev, 2),
                           "\nReturn: ", round(metrics$return, 2),
                           "\nLog Return: ", round(metrics$log_return, 2),
                           "\nGeometric Return: ", round(metrics$geometric_return, 2)),
             hjust = 0, vjust = 0, color = "black")
  
  # Save the plot7
  
  
  script_dir_graph <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  graphs_dir <- file.path(script_dir, "graphs")
  
  
  file_name <- paste0("Metrics_",pair[1], "_", pair[2], ".png")
  
  full_path <- file.path(graphs_dir, file_name)
  
  ggsave(filename = full_path, plot = p)
}

# Apply the function for each pair
for(pair_name in names(results)) {
  create_metrics_plot(results[[pair_name]]$df_final, strsplit(pair_name, "_")[[1]])
}


######################################################################################################

# Function to create a dataframe of summary statistics for each metric
summary_stats <- function(metrics) {
  stats <- data.frame()
  
  # Calculate summary statistics for each metric
  for(metric_name in names(metrics)) {
    metric_data <- pull(metrics, metric_name)
    stats_row <- data.frame(
      metric = metric_name,
      mean = mean(metric_data, na.rm = TRUE),
      sd = sd(metric_data, na.rm = TRUE),
      skewness = skewness(metric_data, na.rm = TRUE),
      kurtosis = kurtosis(metric_data, na.rm = TRUE)
    )
    stats <- rbind(stats, stats_row)
  }
  
  return(stats)
}

# Use the function to get the summary statistics
summary_stats(metrics)
