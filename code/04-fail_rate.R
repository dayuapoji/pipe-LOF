# ==============================================================================
# PREPARE TBF AND FAILURE RATE DATA 
# ==============================================================================

# create df TBF and failure rates
df_fail_rate <- df_fail %>% 
  
  # select relevant cols and join pipe df to fail df
  select(DateFail, PipeEMD) %>%
  left_join(df_pipe %>% select(PipeEMD, PipeMat, PipeDia, PipeLengthEMD, YearInstall), 
            by = c("PipeEMD")) %>%
  
  # create pipe age col in year unit
  mutate(PipeAge = (DateFail - YearInstall) / dyears(1)) %>%
  
  # remove rows with NA, this is related to the missing pipes: TO CHECK!
  drop_na() %>%
  
  # Add the FailUnit column
  group_by(across(everything())) %>%
  mutate(FailUnit = ifelse(n() > 1, n(), 1)) %>%
  # normalize by length?
  # mutate(FailUnit = FailUnit/PipeLengthEMD) %>%
  ungroup() %>%
  
  # Remove duplicates, keeping only one unique row
  distinct(across(everything()), .keep_all = TRUE) %>%

  # compute Time Between Failure (TBF) for each individual pipe
  arrange(PipeEMD, PipeAge) %>% group_by(PipeEMD) %>%
  mutate(TBF = c(NA, diff(PipeAge))) %>%
  # remove rows with NA, i.e. the first recorded failures with unknown TBF
  drop_na() %>%

  # compute fail rate as failure unit / TBF
  mutate(FailRate = FailUnit / TBF) %>%
  ungroup()


# fig_data <- 
ggplot(df_fail_rate) +
  geom_point(aes(x = PipeAge, y = FailRate, color = PipeMat)) +
  # geom_smooth(aes(x = PipeAge, y = FailRate, color = PipeMat), method = 'gam') +#, color = PipeMat))
  labs(x = 'Pipe Age',
       y = 'Empirical Failure Rate') +
  ylim(0, 200) +
  theme_bw()

# ggsave("../output/fig_data.pdf", fig_data,
#        width = 9, height = 6)

# ==============================================================================
# PREPARE DATA FOR MODELING
# ==============================================================================

df <- df_fail_rate %>%
  # select pipe char
  select(FailRate, PipeEMD, PipeMat, PipeDia, PipeLengthEMD, PipeAge) #%>% #, YearInstall) %>%
  # left_join(cova, by = 'PipeEMD') %>%
  # drop_na() %>%
  # ungroup()

# patch imbalance 0 to 10 years by creating synthetic data with fail rate 0
# NEED TO FIND BETTER SOLUTIONS!
df_synth <- NULL
for (pipe in unique(df$PipeEMD)) {
  # create df predictors, with pipe age from 0 to 100
  df_pipeEMD <- NULL
  for (i in 0:10) {
    df_i <- df %>% filter(PipeEMD == pipe) # select pipe charateristics
    # change pipe age
    df_i$PipeAge <- i
    df_i$FailRate <- 0
    df_pipeEMD <- rbind(df_pipeEMD, df_i)
  }
  df_synth <- rbind(df_synth, df_pipeEMD)
}

# write.csv(df_synth, "../output/df_synth.csv", row.names = FALSE)
# df_synth <- read_csv("../output/df_synth.csv")

# bind df failure rate
df <- rbind(df_synth, df)

set.seed(123)

# Create a vector of random indices for the training set
train_index <- sample(1:nrow(df), round(0.9 * nrow(df)))

# Split the data into training and testing sets
df_train <- df[train_index, ] %>% select(-PipeEMD)
df_test <- df[-train_index, ] %>% select(-PipeEMD)


# ==============================================================================
# TRAIN MODEL
# ==============================================================================

# Fit the random forest model using ranger
model_rf <- ranger(FailRate ~ ., data = df_train, 
                   splitrule = 'extratrees',
                   mtry = ncol(df_train)-1,
                   min.node.size = 3,
                   num.trees = 200,
                   quantreg = TRUE,
                   importance = 'permutation')

# ==============================================================================
# FEATURE IMPORTANCE
# ==============================================================================

# Extract feature importance scores from the model
feature_importance <- as.data.frame(model_rf$variable.importance) %>% 
  set_colnames('Importance') %>%
  mutate(Feature = rownames(.)) 

# Create the ggplot bar plot
# fig_varimp <- 
ggplot(feature_importance, 
               aes(x = reorder(Feature, -Importance), 
                   y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Feature",
       y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels

# ggsave("../output/fig_varimp.pdf", fig_varimp,
#        width = 9, height = 6)

# ==============================================================================
# MODEL EVALUATION
# ==============================================================================

# Make predictions on the test set using the trained model
predictions <- predict(model_rf, data = df_test)$predictions

ggplot() +
  # geom_point(aes(x = df_test$FailRate, y = predictions[, 2])) +
  geom_point(aes(x = df_test$FailRate, y = predictions)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  coord_equal() +
  xlim(0, 200) +
  ylim(0, 200) +
  theme_bw()

# ==============================================================================
# PLOT INDIVIDUAL DEGRADATAION MODELS
# ==============================================================================

pipe_select <- c('33935-D$S$6', '45193-A$A$6', 
                 'E-26102-A$C$8', 'E-30336-C$C$12')

ggplot(df %>% 
         # filter(PipeMat == 'S') %>% 
         filter(PipeEMD %in% pipe_select) %>% 
         drop_na(),
       aes(x = PipeAge, y = FailRate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlim(0, 100) +
  theme_bw() +
  facet_wrap(~ PipeEMD, ncol = 2)

# note: fail rate produce better pattern, i.e., almost all increasing pattern, which is 
# not the case in TBF plot

df_results <- NULL
for (pipe in pipe_select) {
  # RF predict
  pred_result <- get_pred(model_rf, pipe, df)
  # store results in df
  df_results <- rbind(df_results, pred_result)
}

# fig_indiv_plots <- 
ggplot(df_results) +
  # plot quantile
  geom_ribbon(aes(x = PipeAge, ymin = Q1, ymax = Q3), 
              fill = "orange", alpha = 0.25) +
  # plot prediction (mean)
  geom_point(aes(x = PipeAge, y = FailRatePred), color = 'red', size = 0.5) +
  geom_smooth(aes(x = PipeAge, y = FailRatePred), method = 'loess') +
  # plot empirical data
  geom_point(data = df %>% 
               filter(PipeEMD %in% pipe_select) %>% 
               filter(PipeAge > 10),
             aes(x = PipeAge, y = FailRate), 
             color = 'black', size = 2, shape = 15) +
  labs(x = 'Pipe Age',
       y = 'Estimated Pipe-Specific Failure Rate') +
  # ylim(0, 500) +
  theme_bw() +
    facet_wrap(~ PipeEMD, ncol = 2)

# ggsave("../output/fig_indiv_plots.pdf", fig_indiv_plots,
#        width = 9, height = 6)

# ==============================================================================
# PLOT BATCH DEGRADATION MODELS
# ==============================================================================
# 
# df_results <- NULL
# 
# for (pipe in sample(df$PipeEMD, 200)) {
# 
#   # RF predict
#   pred_result <- get_pred(model_rf, pipe, df)
# 
#   df_results <- rbind(df_results, pred_result)
# }
# 
# # fig_degr_plots <-
# ggplot(df_results) +
#   geom_smooth(aes(x = PipeAge, y = FailRatePred, color = PipeEMD),
#               linewidth = 0.25, alpha = 0.5,
#               method = 'loess', se = F) +
#   # ylim(0, 1) +
#   xlab('Pipe Age') +
#   ylab('Estimated Pipe-Specific Failure Rate') +
#   theme_bw() +
#   theme(legend.position = "none")

# ggsave("../output/fig_degr_plots.pdf", fig_degr_plots,
#        width = 9, height = 6)

