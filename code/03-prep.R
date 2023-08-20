# ==============================================================================
# PREPARE FAILURE DATA
# ==============================================================================

df_fail_2023 <- orig_fail_2023 %>% 
  .[c(colnames(.)[c(2, 4, 5, 7, 8, 11, 13, 14, 15, 17, 21)])] %>%
  set_names(c('ReportNum', # to avoid duplication
              'DateFail', 'YearFail',
              'CoordE', 'CoordN', 'City',
              'PipeExt', 'PipeDia', 'PipeMat', #'YearInstall',
              'FailPart', 'FailType')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0) %>%
  # TO CHECK!
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia )) %>%
  # remove 2023 records
  filter(YearFail < 2023) # 2023 data haven't been received

df_fail_2022 <- orig_fail_2022 %>% 
  .[c(colnames(.)[c(2, 4, 5, 7, 8, 11, 13, 14, 15, 17, 21)])] %>%
  set_names(c('ReportNum',
              'DateFail', 'YearFail',
              'CoordE', 'CoordN', 'City',
              'PipeExt', 'PipeDia', 'PipeMat', #'YearInstall',
              'FailPart', 'FailType')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0) %>%
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia ))

df_fail_2021 <- orig_fail_2021 %>% 
  .[c(colnames(.)[c(2, 3, 4, 6, 7, 10, 12, 13, 14,  16, 20)])] %>%
  set_names(c('ReportNum', 
              'DateFail', 'YearFail',
              'CoordE', 'CoordN', 'City',
              'PipeExt', 'PipeDia', 'PipeMat', #'YearInstall',
              'FailPart', 'FailType')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0) %>%
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia ))

# bind df
df_fail <- rbind(df_fail_2023,
                 df_fail_2022, 
                 df_fail_2021) %>%
  # keep only one instance of duplicated rows
  distinct(ReportNum, .keep_all = T) %>%
  # change date format
  mutate(DateFail = ymd(DateFail)) %>%
  # exclude pipes with some charasteristics
  filter(., PipeMat %in% c('A', 'C', 'S')) %>%
  filter(PipeDia <= 16)
  
# Plot
ggplot(df_fail %>%
         group_by(YearFail) %>%
         summarise(FailCount = n()),
       aes(x = YearFail, y = FailCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Year") +
  ylab("Fail Count") +
  theme_bw()

# ==============================================================================
# PREPARE PIPE DATA
# ==============================================================================

df_pipe_2022 <- orig_pipe_2022 %>% 
  .[c(colnames(.)[c(2, 3, 4, 5, 9, 10, 13, 17)])] %>%
  set_names(c('PipeMat', 'PipeExt', 'PipeDia', 'PipeEMD', 'YearInstall',
              'PipeRMID', 'PipeLengthExt', 'PipeLengthRMID')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0)

df_pipe <- df_pipe_2022 %>%
  # get pipe length for every PipeEMD
  group_by(PipeEMD) %>%
  # TO CHECK!
  summarise(PipeLengthEMD = sum(PipeLengthRMID, na.rm = TRUE) / 5280, # in mile
            PipeMat = first(PipeMat),
            PipeDia = first(PipeDia),
            YearInstall = first(YearInstall)) %>%
  ungroup() %>% 
  # clean year install
  filter(YearInstall > 0) %>%
  # change date format
  # mutate(YearInstall = as.numeric(as.character(YearInstall))) %>%
  mutate(YearInstall = ymd(paste0(YearInstall, "-01-01"))) %>%
  # exclude pipes with some charasteristics
  filter(., PipeMat %in% c('A', 'C', 'S')) %>%
  filter(PipeDia <= 16)
  
  
# Plot
# fig_pipe <- 
ggplot(df_pipe, 
       aes(x = as.factor(YearInstall), y = PipeLengthEMD, 
           fill = PipeMat)) +
  geom_col() +
  xlab("Year Install") +
  scale_x_discrete(breaks = pretty(df_pipe$YearInstall, n = 10)) +
  ylab("Total Pipe Length (Miles)") +
  theme_bw() #+
  # theme(legend.position = 'none')


# fig_dia <- 
ggplot(df_pipe, 
       aes(x = as.factor(PipeDia), y = PipeLengthEMD, 
           fill = PipeMat)) +
  geom_col() +
  xlab("Pipe Diameter") +
  ylab("Total Pipe Length (Miles)") +
  theme_bw()

# ggsave("../output/fig_pipe.pdf", plot_grid(fig_pipe, fig_dia, ncol = 2),
#        width = 9, height = 6)

# ==============================================================================
# CHECK DATA
# ==============================================================================

# number of unique pipe ID in pipe df
length(unique(df_pipe$PipeEMD))

# number of unique pipe ID in fail df
length(unique(df_fail$PipeEMD))

# PipeEMD in df_pipe that are not in df_fail (no failure record)
length(setdiff(df_pipe$PipeEMD, df_fail$PipeEMD))

# PipeEMD in df_fail that are not in df_pipe: problem?
missing_pipes <- data.frame(PipeEMD = setdiff(df_fail$PipeEMD, df_pipe$PipeEMD))
missing_pipes_failures <- df_fail %>% filter(PipeEMD %in% c(setdiff(df_fail$PipeEMD, df_pipe$PipeEMD)))

# write_csv(missing_pipes, "../output/missing_pipes.csv")
# write_csv(missing_pipes_failures, "../output/missing_pipes_failures.csv")


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