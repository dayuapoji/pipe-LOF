tune_rf <- function(df, ctrl) {
  
  # get number of predictors (excl. response feature)
  p <- ncol(df)-1 
  
  # set tuning grid
  tunegrid <- expand.grid(mtry = c(1, 3, round(sqrt(p)), round(p/3), round(p/2), p),
                          splitrule = c('variance', 'extratrees'),
                          min.node.size = c(1, 3, 5, 10, 20))
  
  # initialize list
  cv_result <- NULL
  
  for (numtrees in c(1, 10, 50, 100, 200, 300)) {
    
    # training
    rf_model <- train(FailRate ~ ., data = df,
                      
                      # cv control
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = tunegrid,
                      num.trees = numtrees,
                      verbose = FALSE)
    
    # save results in a data frame
    result<- data.frame(rf_model$results, 
                        ntrees = rep(numtrees, 
                                     each = nrow(rf_model$results)))
    
    # append
    cv_result <- rbind(cv_result, result)
    
    # computation progress
    print(paste("numtrees = ", numtrees, "done"))
  }
  
  return(cv_result)
}
