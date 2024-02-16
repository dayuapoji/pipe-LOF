# Install and load necessary packages
install.packages("xgboost")
install.packages("caret")
library(xgboost)
library(caret)

# ...

# ==============================================================================
# TRAIN MODEL WITH XGBOOST AND HYPERPARAMETER TUNING
# ==============================================================================

# Create a train control object for hyperparameter tuning
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Define a grid of hyperparameters to search over
grid <- expand.grid(
  nrounds = c(100),  # Number of boosting rounds
  max_depth = c(3, 6, 9),  # Maximum depth of trees
  eta = c(0.01, 0.1, 0.3),  # Learning rate
  gamma = 0,  # Regularization parameter
  colsample_bytree = 1,  # Fraction of features to be randomly sampled for each tree
  min_child_weight = 1,  # Minimum sum of instance weight (hessian) needed in a child
  subsample = 1  # Fraction of samples used for fitting the trees
)

# Train the XGBoost model with hyperparameter tuning
model_xgb <- train(
  Lambda ~ .,
  data = df_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid
)

# Print the best hyperparameters
print(model_xgb)

# Make predictions using the best model
predictions <- predict(model_xgb, newdata = df_test)


# ...

# Create a data frame with predictions
df_pred <- df_ml %>%
  filter(!PipeEMD %in% train_pipeEMD) %>%
  mutate(Prediction = predictions)

# Scatterplot of Actual vs. Predicted
ggplot(data = df_test, aes(x = Lambda, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted") +
  theme_minimal()

# Line plot of predictions vs. actual for a specific PipeEMD
ggplot(data = df_pred %>% filter(PipeEMD == unique(df_pred$PipeEMD)[1]), aes(x = PipeAge)) +
  geom_line(aes(y = Lambda, color = "Actual")) +
  geom_line(aes(y = Prediction, color = "Predicted")) +
  geom_point(aes(y = Prediction, color = "Predicted")) +
  geom_smooth(aes(y = Prediction, color = "Predicted"), method = 'loess') +
  labs(x = "PipeAge", y = "Value") +
  scale_color_manual(
    values = c(Actual = "blue", Predicted = "red"),
    labels = c(Actual = "Actual", Predicted = "Predicted")
  ) +
  theme_minimal()
