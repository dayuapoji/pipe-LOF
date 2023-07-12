# ANALYZE FAILURE RATE ############################################

# Pipe system =====================================================
df_failure_rate <- df_fail %>% select(YearFailure, PipeEMD, PipeMat) %>%
  
  left_join(df_pipe %>% select(PipeEMD, YearInstall), 
            by = "PipeEMD") %>%
  
  mutate(PipeAge = YearFailure - YearInstall) %>%
  # some age are negative, due to replacement that werent updated?
  filter(PipeAge > 0) %>%
  
  left_join(df_length, 
            by = c("PipeAge", "PipeMat", "YearFailure" = "Year")) %>%
  
  mutate(FailureRate = 1 / PipeLength) %>%
# Calculate failure rate (failures per mile per year) 
# by grouping PipeAge and PipeMat
  group_by(PipeAge) %>% #, PipeMat) %>%
  # total of failure rate per total years considered in failure coutns
  summarise(FailureRate = sum(FailureRate)/length(failure_years), 
            .groups = "drop")

# Plot age vs failure rate
ggplot(df_failure_rate, aes(x = PipeAge, y = FailureRate)) +#, color = PipeMat)) +
  geom_point() +
  
  geom_hline(yintercept =  0.2, linetype = 'dashed') + # AWWA benchmark
  xlab("Pipe Age") +
  ylab("Failure Rate (Failures per Mile per Year)")  +
  # scale_x_continuous(n.breaks = 10, limits = c(0, 100)) +
  # ylim(0, 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# NOTE: failure rate of older pipes drop, maybe problem of unrecorded pipe
# replacement?

# Single pipe type===================================================
df_failure_rate <- df_fail %>% 
  select(YearFailure, PipeEMD, PipeMat, PipeDia) %>%
  
  left_join(df_pipe %>% select(PipeEMD, YearInstall), 
            by = "PipeEMD") %>%
  
  mutate(PipeAge = YearFailure - YearInstall) %>%
  # some age are negative, due to replacement that werent updated?
  filter(PipeAge > 0) %>%
  # select only a type of pipe mat and dia
  filter(PipeMat == 'C') %>%
  filter(PipeDia == 12) %>%
  
  left_join(df_length, 
            by = c("PipeAge", "PipeMat", "YearFailure" = "Year")) %>%
  
  mutate(FailureRate = 1 / PipeLength) %>%
  # Calculate failure rate (failures per mile per year) 
  # by grouping PipeAge and PipeMat
  group_by(PipeAge) %>% #, PipeMat) %>%
  # total of failure rate per total years considered in failure coutns
  summarise(FailureRate = sum(FailureRate)/length(failure_years), 
            .groups = "drop")

# Plot age vs failure rate
ggplot(df_failure_rate, aes(x = PipeAge, y = FailureRate)) +#, color = PipeMat)) +
  geom_point() +
  
  geom_hline(yintercept =  0.2, linetype = 'dashed') + # AWWA benchmark
  xlab("Pipe Age") +
  ylab("Failure Rate (Failures per Mile per Year)")  +
  # scale_x_continuous(n.breaks = 10, limits = c(0, 100)) +
  # ylim(0, 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# Individual pipe ===================================================

df_failure_rate <- df_fail %>% 
  select(YearFailure, PipeEMD, PipeMat, PipeDia) %>%
  
  left_join(df_pipe %>% select(PipeEMD, YearInstall), 
            by = "PipeEMD") %>%
  
  mutate(PipeAge = YearFailure - YearInstall) %>%
  # some age are negative, due to replacement that werent updated?
  filter(PipeAge > 0) %>%
  # select only a pipe ID
  filter(PipeEMD == 'E-17386$C$6') %>%
  
  left_join(df_length, 
            by = c("PipeAge", "PipeMat", "YearFailure" = "Year")) %>%
  
  mutate(FailureRate = 1 / PipeLength) %>%
  # Calculate failure rate (failures per mile per year) 
  # by grouping PipeAge and PipeMat
  group_by(PipeAge) %>% #, PipeMat) %>%
  # total of failure rate per total years considered in failure coutns
  summarise(FailureRate = sum(FailureRate)/length(failure_years), 
            .groups = "drop")

# Plot age vs failure rate
ggplot(df_failure_rate, aes(x = PipeAge, y = FailureRate)) +#, color = PipeMat)) +
  geom_point() +
  
  geom_hline(yintercept =  0.2, linetype = 'dashed') + # AWWA benchmark
  xlab("Pipe Age") +
  ylab("Failure Rate (Failures per Mile per Year)")  +
  # scale_x_continuous(n.breaks = 10, limits = c(0, 100)) +
  # ylim(0, 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
