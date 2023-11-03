# ==============================================================================
# PREPARE PIPE DATA
# ==============================================================================

df_pipe <- orig_pipe %>% 
  .[c(colnames(.)[c(5, 10, 8, 2, 4, 17)])] %>%
  set_names(c('PipeEMD', 'PipeRMID', 'YearInstall',
              'PipeMat', 'PipeDia', 
              'PipeLengthRMID')) %>%
  
  # clean year install
  filter(YearInstall > 0) %>%
  # change date format
  # mutate(YearInstall = as.numeric(as.character(YearInstall))) %>%
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
  # exclude pipes with some charasteristics
  # filter(., PipeMat %in% c('A', 'C', 'S')) %>%
  # filter(PipeDia <= 16)
 
# Plot
ggplot(df_pipe, 
       aes(x = as.factor(YearInstall), y = PipeLength, 
           fill = PipeMat)) +
  geom_col() +
  xlab("Year Install") +
  scale_x_discrete(breaks = pretty(df_pipe$YearInstall, n = 10)) +
  ylab("Total Pipe Length (Miles)") +
  theme_bw() 

ggplot(df_pipe,
       aes(x = as.factor(PipeDia), y = PipeLength, 
           fill = PipeMat)) +
  geom_col() +
  xlab("Pipe Diameter") +
  ylab("Total Pipe Length (Miles)") +
  theme_bw()


# ==============================================================================
# PREPARE FAILURE DATA
# ==============================================================================

df_fail <- orig_fail %>% 
  .[c(colnames(.)[c(2, 3, 4, 16, 18, 12, 14, 13, 41)])] %>%
  set_names(c('ReportNum', # to avoid duplication
              'DateFailure', 'YearFailure',
              'FailurePart', 'FailureType',
              'PipeExt', 'PipeMat', 'PipeDia', 'PipeRMID')) %>%
  
  # keep only one instance of duplicated rows
  distinct(ReportNum, .keep_all = T) %>%
  # change date format
  mutate(DateFailure = ymd(DateFailure)) %>%
  # PipeMat consists rows of NA, remove them
  filter(!is.na(PipeMat)) %>%
  # create PipeEMD column
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia))

  
# Plot
ggplot(df_fail %>%
         group_by(YearFailure) %>%
         summarise(FailureCount = n()),
       aes(x = YearFailure, y = FailureCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Year") +
  ylab("Failure Count") +
  ylim(c(0, 1200)) +
  theme_bw()

# ==============================================================================
# CHECK DATA
# ==============================================================================

# number of unique pipe ID in pipe df
length(unique(df_pipe$PipeEMD))
write_csv(df_pipe, "../output/df_pipe.csv")

# number of unique pipe ID in fail df
length(unique(df_fail$PipeEMD))

missing_EMD <- df_fail[!(df_fail$PipeEMD %in% df_pipe$PipeEMD), ]
length(unique(missing_EMD$PipeEMD))
write_csv(missing_EMD, "../output/missing_EMD.csv")


df_fail <- df_fail[df_fail$PipeEMD %in% df_pipe$PipeEMD, ]
length(unique(df_fail$PipeEMD))
write_csv(df_fail, "../output/df_fail.csv")

sum(is.na(df_fail$PipeRMID))

missing_RMID <- df_fail[is.na(df_fail$PipeRMID), ]
length(unique(missing_RMID$PipeEMD))

write_csv(missing_RMID, "../output/missing_RMID.csv")


# PipeEMD in df_pipe that are not in df_fail (no failure record)
length(setdiff(df_pipe$PipeEMD, df_fail$PipeEMD))

# PipeEMD in df_fail that are not in df_pipe: problem?
missing_pipes <- data.frame(PipeEMD = setdiff(df_fail$PipeEMD, df_pipe$PipeEMD))
missing_pipes_failures <- df_fail %>% filter(PipeEMD %in% missing_pipes$PipeEMD)
write_csv(missing_pipes, "../output/missing_pipes.csv")
write_csv(missing_pipes_failures, "../output/missing_pipes_failures.csv")


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