### Function to generate bar plots for specific network correlations
### Inputs : Dataframes with 2 groups + list of significant p-vals
### Output: Barplot with group differences with 95% CI

generate_barplots <- function(df, status.term, p_vals, ALPHA, legend.title) 
{
  
  sig_df <- data.frame(matrix(nrow = 0, ncol = 8))
  
  notNull <- FALSE
  
  for(i in 1:nrow(p_vals)) {
    
    adjusted_pval <- p_vals[i]
    corrName <- rownames(p_vals)[i]
    
    if(adjusted_pval < ALPHA)
    {
      notNull <- TRUE
      vals <- data.frame(hiv_status = df[[status.term]], df[corrName])
      h.plus <- vals[vals$hiv_status == 1,]
      h.minus <- vals[vals$hiv_status == 0, ]
      
      n1 <- nrow(h.plus)
      n2 <- nrow(h.minus)
      
      mean1 <- mean(h.plus[,2])
      mean2 <- mean(h.minus[,2])
      sd1 <- sd(h.plus[, 2])
      sd2 <- sd(h.minus[,2])
      
      se1 <- sd1/sqrt(n1)
      se2 <- sd2/sqrt(n2)
      
      degrees.freedom1 = n1 - 1
      t.score1 = qt(0.975, df=degrees.freedom1,lower.tail=FALSE)
      
      degrees.freedom2 = n2 - 1
      t.score2 = qt(0.975, df=degrees.freedom2,lower.tail=FALSE)
      
      margin.error1 <- t.score1 * se1
      margin.error2 <- t.score2 * se2
      
      CI.lb1 <- mean1 - margin.error1
      CI.ub1 <- mean1 + margin.error1
      
      CI.lb2 <- mean2 - margin.error2
      CI.ub2 <- mean2 + margin.error2
      
      new_row <- c(1, corrName, mean1, sd1, se1, CI.lb1, CI.ub1, n1)
      sig_df <- rbind(sig_df, new_row)
      
      new_row <- c(0, corrName, mean2, sd2, se2, CI.lb2, CI.ub2, n2)
      sig_df <- rbind(sig_df, new_row)
    }
  }
  
  colnames(sig_df) <- c("group_status", "network_composite", "mean", "sd", "se", "CI_lower", "CI_upper", "n")
  
  sig_df$group_status <- as.factor(sig_df$group_status)
  sig_df$mean <- as.numeric(sig_df$mean)
  sig_df$sd <- as.numeric(sig_df$sd)
  sig_df$n <- as.numeric(sig_df$n)
  sig_df$se <- as.numeric(sig_df$se)
  sig_df$CI_lower <- as.numeric(sig_df$CI_lower)
  sig_df$CI_upper <- as.numeric(sig_df$CI_upper)
  
  
  if(notNull) 
  {
      result <- ggplot(sig_df, aes(x=network_composite, y=mean, fill = group_status)) +
        geom_col(position = "dodge", color = "black", width = 0.4)+ aes(x = fct_inorder(network_composite)) +
        xlab("") + ylab("") + theme_minimal() + ylim(-0.8, 0.8) +
        scale_fill_brewer(palette = "Set1", direction = -1) +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper), width=0.5, position=position_dodge(0.4))
    
      result <- result + guides(fill = guide_legend(title = legend.title))
    
      print(result)
    
      return(result)
  }
  
}