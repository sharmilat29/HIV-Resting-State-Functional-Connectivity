# Function to perform ANOVAs on all correlations based on two groups

anova_all <- function(df, cat_var, start_index) {
  
  result <- data.frame(matrix(ncol = 2, nrow = 0))
  
  for(i in start_index:ncol(df)) {
    
    dep_var <- colnames(df)[i]
    
    formula <- as.formula(paste(dep_var, cat_var, sep = " ~ "))
    
    res <- summary(lm(formula, df))
    
    new_row <- c(dep_var, res[["coefficients"]][2,4])
    
    result <- rbind(result, new_row)
  }
  
  colnames(result) <- c("correlation", "p_val")
  result$p_val <- as.double(result$p_val)
  
  return (result)
}

