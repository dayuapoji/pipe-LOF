# ==============================================================================
# PREPARING DATA FRAME FOR MACHINE LEARNING MODELING
# ==============================================================================


df_ml <- left_join(df_model, df_pipe, by = 'PipeEMD') %>%
  # left_join(cova, by = 'PipeEMD') #%>%
  
  # remove year installation as the predictor
  subset(select = -c(YearInstall)) %>%
  
  # previous year info as new predictors
  group_by(PipeEMD) %>%
  mutate(PrevPipeAge = lag(PipeAge),
         PrevInverseTBF = lag(InverseTBF)) %>% 
  ungroup() %>%
  mutate(DeltaPipeAge = PipeAge - PrevPipeAge) %>%
  
  # clean
  drop_na()


# ==============================================================================
# DATA SPLITTING
# ==============================================================================

# Get unique PipeEMD values
unique_pipeEMD <- unique(df_ml$PipeEMD)

# Set a random seed for reproducibility
set.seed(1)

# Determine the number of unique values to include in the training set
train_percent <- 0.8
num_train <- round(length(unique_pipeEMD) * train_percent)

# Randomly sample the unique PipeEMD values for the training set
train_pipeEMD <- sample(unique_pipeEMD, num_train)

# Create df_train and df_test
df_train <- df_ml %>% filter(PipeEMD %in% train_pipeEMD) %>% select(-PipeEMD)
df_test <- df_ml %>% filter(!PipeEMD %in% train_pipeEMD) %>% select(-PipeEMD)


# ==============================================================================
# TRAIN MODEL
# ==============================================================================

control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        verboseIter = FALSE,
                        search = 'grid')

hyperparameter_grid <- expand.grid(mtry = c(2, 4, 6),
                                   splitrule = c("extratrees"),
                                   min.node.size = c(1, 3, 5, 10))


set.seed(1)
# Perform hyperparameter tuning
tuned_model <- train(InverseTBF ~ .,  data = df_train,
                     method = "ranger",
                     trControl = control,
                     tuneGrid = hyperparameter_grid,
                     num.trees = 50)

# View the best hyperparameters and model performance
print(tuned_model)
# extratrees, min node size = 1, mtry = 6
# saveRDS(tuned_model, file = 'tuned_model50.rds')
tuned_model <- readRDS('tuned_model50.rds')

# predict
predictions <- predict(tuned_model, newdata = df_test)

# create df validation
df_val <- df_ml %>% filter(!PipeEMD %in% train_pipeEMD) %>%
  mutate(Prediction = predictions)

# plot
ggplot(df_val) +
  geom_point(aes(x = InverseTBF, y = Prediction), alpha = 0.5) +
  coord_equal() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw()

(cor(df_val$InverseTBF, df_val$Prediction))^2

# plot individual pipe
set.seed(1)
pipes <- sample(unique(df_val$PipeEMD), size = 9)

for (i in pipes) {
  
  print(
    
    ggplot(df_val %>% filter(PipeEMD == i)) +
      
      # stat model
      geom_line(aes(x = PipeAge, y = InverseTBF, color = PipeEMD),
                color = 'blue', size = 1) +
      
      # machine learning
      geom_point(aes(x = PipeAge, y = Prediction, color = PipeEMD),
                 color = 'red', alpha = 0.5) +
      # geom_smooth(aes(x = PipeAge, y = Prediction), method = 'loess') +
      
      # actual data
      geom_point(data = df_tbf_expand %>% filter(PipeEMD == i),
                 aes(x = PipeAgeFailure, y = InverseTBF),
                 color = 'black') +
      
      ggtitle(paste("Pipe =", i)) +
      scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
      ylim(0, 1) +
      labs(x = "Pipe Age (Year)", y = "1/TBF") +
      guides(color = "none") +
      theme_bw(base_size = 10)
  )
}

############################################################################

fig_test <- ggplot(df_val) +
  
  # stat model
  geom_line(aes(x = PipeAge, y = InverseTBF, group = PipeEMD),
            color = 'blue', size = 0.5) +
  
  # machine learning
  geom_point(aes(x = PipeAge, y = Prediction, group = PipeEMD),
             color = 'red', size = 1, alpha = 0.5) +
  # geom_smooth(aes(x = PipeAge, y = Prediction), method = 'loess') +
  
  # actual data
  # geom_point(data = df_tbf_expand %>% filter(PipeEMD == i),
  #            aes(x = PipeAgeFailure, y = InverseTBF),
  #            color = 'black') +
  
  # ggtitle(paste("Pipe =", i)) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  ylim(0, 1) +
  labs(x = "Pipe Age (Year)", y = "1/TBF") +
  guides(color = "none") +
  theme_bw(base_size = 10)

ggsave(paste0('../output-UESI/fig_ml.pdf'), plot = fig, 
       width = 7.5, height = 5)
