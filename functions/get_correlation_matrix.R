# Function to take a dataframe (containing pairwise correlation inputs)
# and return a 2D correlation matrix

get_correlation_matrix <- function(df) {
  
  # Subset DF to include only the RSFC values of all participants
  start_index <- which(colnames(df) == 'SM_x_SM')
  
  df.rsfc <- df[, start_index:(start_index+90)] # modify depending on # of networks
  
  # Compute average RSFC values
  
  df.avg <- sapply(df.rsfc, mean)
  
  # Save RSFC values as correlation matrices (extracting network list from column names)
  
  df.avg <- as.data.frame(df.avg)
  df.avg$corr <- rownames(df.avg)
  df.avg <- separate(df.avg, corr, c("x", "y"), "_x_")
  
  network_list <- unique(df.avg$x)
  result <- matrix(NA, length(network_list), length(network_list), dimnames = list(network_list, network_list))
  result[cbind(df.avg$x, df.avg$y)] <- df.avg$df.avg
  result[is.na(result)] <- t(result)[is.na(result)]
  
  return (result)
}

