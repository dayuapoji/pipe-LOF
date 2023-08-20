get_pred <- function(model_rf, pipe, df) {
  
  # create df predictors, with pipe age from 0 to 100
  df_pred <- NULL
  for (i in 0:100) {
    df_i <- df %>% 
      # select pipe charateristics
      .[.$PipeEMD == pipe, ] %>% 
      distinct(PipeEMD, .keep_all = T)
    # change pipe age
    df_i$PipeAge <- i
    df_pred <- rbind(df_pred, df_i)
  }
  
  # compute predictions
  result <- predict(model_rf, data = df_pred %>% select(-PipeEMD))$predictions 
  
  # compute quantile
  df_quant <- predict(model_rf, data = df_pred %>% select(-PipeEMD),
                      type = "quantiles",
                      quantiles = c(0.1, 0.5, 0.9))$predictions %>% 
    set_colnames(c('Q1', 'Q2', 'Q3'))
  
  # store in a data frame
  df_results <- data.frame(PipeEMD = df_pred$PipeEMD,
                           PipeMat = df_pred$PipeMat,
                           PipeDia = df_pred$PipeDia,
                           PipeAge = df_pred$PipeAge,
                           FailRatePred = result) %>%
    cbind(df_quant)
  
  print(paste('compute', pipe, 'done...'))
  
  return(df_results)
}
