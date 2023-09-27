# Function to remove the effect of specified covariates on the network correlation values

covariate_correction <- function(df, covar1, covar2, covar3, covar4) {
  
  regressor <- paste(covar1, covar2, covar3, covar4, sep = "+")
  
  start_index <- which(colnames(df) == 'SM_x_SM')
  
  for(i in start_index:(start_index + 90)){
    dep_var <- colnames(df)[i]
    formula <- as.formula(paste(dep_var, regressor, sep = "~"))
    res <- summary(lm(formula, df))
    betas <- coef(res)[,1]
    for(j in 1:nrow(df)) {
      
      orig <- df[j,i]
      c1 <- df[j, covar1]
      c2 <- df[j, covar2]
      c3 <- df[j, covar3]
      c4 <- df[j, covar4]
      
      corrected <- as.numeric(orig) - as.numeric(betas[2]*c1) - as.numeric(betas[3]*c2) - as.numeric(betas[4]*c3) - as.numeric(betas[5]*c4)
      df[j,i] <- corrected
    }
  }
  
  return(df)
  
}