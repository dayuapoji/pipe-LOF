
# Specify the control parameters for the tuning
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     verboseIter = FALSE,
                     search = 'grid')

# cross validation
cv_result <- tune_rf(df_train, ctrl)


write.csv(cv_result, "../output/cv_result.csv", row.names = FALSE)