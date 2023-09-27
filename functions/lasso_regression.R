# Function to perform lasso regression on RSFC-correlation predictors on the group status (hiv + or - etc)
# Returns dataframe with predictors and coefficients

lasso_regression <- function(df, response) {
  
  library(glmnet)
  
  y <- df[[response]]
  
  start_index <- which(colnames(df) == 'SM_x_SM')
  
  x <- data.matrix(df[, c(start_index:(start_index+90))])
  
  cv_model <- cv.glmnet(x, y, alpha = 1)
  
  best_lambda <- cv_model$lambda.min
  
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  
  result <- as.matrix(coef(best_model))
  
  corr <- rownames(result)
  
  vals <- result[,1]
  
  lasso_result <- data.frame(corr, vals)
  
  lasso_result <- lasso_result[lasso_result$vals != 0 & lasso_result$corr != "(Intercept)", ]
  
  return(lasso_result)
  
}



