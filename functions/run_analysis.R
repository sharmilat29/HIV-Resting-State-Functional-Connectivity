### Function to run the main portion of analysis for all group-pairs
### taking the group_status_term and one dataframe as input

# - Covariate correction
# - Lasso regression
# - ANOVA on lasso results
# - Correct for multiple comparisons

run_analysis <- function(df, status.term) {
  
  corrected_df <- covariate_correction(df, "Age", "Sex", "Race", "Education")
  
  lasso_result <- lasso_regression(corrected_df, status.term)
  
  if(length(lasso_result[[1]]) == 0) {
    print ("No correlations found by lasso regression!")
    return (0)
  }
  else
  {
    
    rs_vals <- corrected_df[, c(lasso_result[[1]]), drop = FALSE]
    
    ready_df <- data.frame(status.term = corrected_df[[status.term]], rs_vals)
    
    anova_result <- anova_all(ready_df, "status.term", 2)
    
    anova_result <- anova_result[order(anova_result$p_val), ]

    p <- data.matrix(anova_result$p_val)
    
    rownames(p) <- anova_result$correlation
    
    p_new <- data.matrix(p.adjust(p, method = "fdr"))
    
    rownames(p_new) <- rownames(p)
    
    p_new <- data.matrix(p_new[order(p_new),])
    
    rbind()
    
    return (p_new)
  }
}

