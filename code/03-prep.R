# ==============================================================================
# PREPARE PIPE DATA
# ==============================================================================

df_pipe <- orig_pipe %>% 
  .[c(colnames(.)[c(5, 10, 8, 2, 4, 17)])] %>%
  set_names(c('PipeEMD', 'PipeRMID', 'YearInstall',
              'PipeMat', 'PipeDia', 
              'PipeLengthRMID')) %>%
  
  # SIMPLIFIED VERSION
  filter(PipeMat %in% c('A', 'C', 'N', 'S' )) %>%
  filter(PipeDia <= 16) %>%
  filter(YearInstall >= 1953) %>%

  # change date format

  mutate(YearInstall = ymd(paste0(YearInstall, "-01-01"))) %>%
  # drop rows with NA
  drop_na() %>%
  # compute PipeLengthEMD
  group_by(PipeEMD) %>%
  summarise(YearInstall = first(YearInstall),
            PipeMat = first(PipeMat),
            PipeDia = first(PipeDia),
            PipeLength = sum(PipeLengthRMID, na.rm = TRUE) / 5280 # in mile
            ) %>% 
  ungroup()

 
# Plot
fig_pipe1 <- ggplot(df_pipe) +
  geom_col(aes(x = as.numeric(format(YearInstall, "%Y")), 
               y = PipeLength, 
               fill = PipeMat)) +
  scale_x_continuous(breaks = seq(1953, 2023, 10)) +
  xlab("Installation Year") +
  ylab("Pipe Length (Miles)") +
  labs(fill = "Pipe Material") +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.8, 0.7),
        legend.box.background = element_rect(fill = "white", 
                                             color = "grey", 
                                             size = 1))



fig_pipe2 <- ggplot(df_pipe) +
  geom_col(aes(x = as.factor(PipeDia), y = PipeLength, 
               fill = PipeMat)) +
  xlab("Pipe Diameter") +
  ylab("Pipe Length (Miles)") +
  labs(fill = "Pipe Material") +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.2, 0.7),
        legend.box.background = element_rect(fill = "white", 
                                             color = "grey", 
                                             size = 1))

# save
ggsave(paste0('../output-UESI/fig_data_pipe.pdf'), 
       plot = ggarrange(fig_pipe1, fig_pipe2, nrow = 1), 
       width = 7.5, height = 3.5)

# ==============================================================================
# PREPARE FAILURE DATA
# ==============================================================================

df_fail <- orig_fail %>% 
  .[c(colnames(.)[c(2, 3, 12, 14, 13, 41)])] %>% #4, 16, 18, 
  set_names(c('ReportNum', # to avoid duplication
              'DateFailure', #'YearFailure',
              # 'FailurePart', 'FailureType',
              'PipeExt', 'PipeMat', 'PipeDia', 'PipeRMID')) %>%
  
  # SIMPLIFIED VERSION
  filter(PipeMat %in% c('A', 'C', 'N', 'S' )) %>%
  filter(PipeDia <= 16) %>%
  # filter(YearInstall >= 1990) %>%
  
  # keep only one instance of duplicated rows
  distinct(ReportNum, .keep_all = T) %>%
  # change date format
  mutate(DateFailure = ymd(DateFailure)) %>%
  # PipeMat consists rows of NA, remove them
  filter(!is.na(PipeMat)) %>%
  # create PipeEMD column
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia))

# Check for duplicated ReportNum values in df_fail
duplicated_reportnum <- duplicated(df_fail$ReportNum)
if (any(duplicated_reportnum)) {
  cat("There are duplicated ReportNum values in the dataframe.\n")
} else {
  cat("There are no duplicated ReportNum values in the dataframe.\n")
}

# Drop the ReportNum column from df_fail
df_fail <- subset(df_fail, select = -c(ReportNum, PipeRMID))

# Filter rows in df_fail where PipeEMD is in df_pipe
df_fail <- df_fail[df_fail$PipeEMD %in% unique(df_pipe$PipeEMD), ]
  
# Plot
ggplot(df_fail %>%
         group_by(YearFailure = as.numeric(format(DateFailure, "%Y")),
                  PipeMat) %>%
         summarise(FailureCount = n())) +
  geom_bar(aes(x = YearFailure, 
               y = FailureCount,
               fill = PipeMat),
           stat = "identity") +
  scale_x_continuous(breaks = seq(1990, 2023, 5)) +
  xlab("Year") +
  ylab("Failure Count") +
  labs(fill = "Pipe Material") +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.1, 0.8),
        legend.box.background = element_rect(fill = "white", 
                                             color = "grey", 
                                             size = 1))
length(unique(df_pipe$PipeEMD)) - length(unique(df_fail$PipeEMD))
# ==============================================================================
# CHECK DATA
# ==============================================================================
# 
# # number of unique pipe ID in pipe df
# length(unique(df_pipe$PipeEMD))
# # write_csv(df_pipe, "../output/df_pipe.csv")
# 
# # number of unique pipe ID in fail df
# length(unique(df_fail$PipeEMD))
# 
# missing_EMD <- df_fail[!(df_fail$PipeEMD %in% df_pipe$PipeEMD), ]
# length(unique(missing_EMD$PipeEMD))
# # write_csv(missing_EMD, "../output/missing_EMD.csv")
# 
# 
# df_fail <- df_fail[df_fail$PipeEMD %in% df_pipe$PipeEMD, ]
# 
# length(unique(df_fail$PipeEMD))
# # write_csv(df_fail, "../output/df_fail.csv")
# 
# sum(is.na(df_fail$PipeEMD))
# 
# missing_RMID <- df_fail[is.na(df_fail$PipeRMID), ]
# length(unique(missing_RMID$PipeEMD))
# 
# # write_csv(missing_RMID, "../output/missing_RMID.csv")
# 
# 
# # PipeEMD in df_pipe that are not in df_fail (no failure record)
# length(setdiff(df_pipe$PipeEMD, df_fail$PipeEMD))
# 
# # PipeEMD in df_fail that are not in df_pipe: problem?
# missing_pipes <- data.frame(PipeEMD = setdiff(df_fail$PipeEMD, df_pipe$PipeEMD))
# missing_pipes_failures <- df_fail %>% filter(PipeEMD %in% missing_pipes$PipeEMD)
# # write_csv(missing_pipes, "../output/missing_pipes.csv")
# # write_csv(missing_pipes_failures, "../output/missing_pipes_failures.csv")


# check inconsistent year installation
# check_YearInstall <- merge(df_fail, df_pipe, 
#                            by = "PipeEMD", 
#                            suffixes = c("_fail", "_pipe")) %>%
#   filter(YearInstall_fail != YearInstall_pipe) %>%
#   select(PipeEMD, YearInstall_fail, YearInstall_pipe) 
# 
# nrow(check_YearInstall)
# 
# check_YearInstall <- check_YearInstall %>%
#   filter(YearInstall_fail > 1000)
# 
# nrow(check_YearInstall)