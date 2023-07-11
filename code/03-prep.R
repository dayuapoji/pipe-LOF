# DATA PREPARATION #################################################

# Failure DF ======================================================
df_fail_2023 <- orig_fail_2023 %>% 
  .[c(colnames(.)[c(2, 4, 5, 7, 8, 11, 13, 14, 15, 16, 17, 21)])] %>%
  set_names(c('ReportNum', # to avoid duplication
              'DateFailure', 'YearFailure',
              'CoordE', 'CoordN', 'City',
              'PipeExt', 'PipeDia', 'PipeMat', 'YearInstall',
              'FailurePart', 'FailureType')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0) %>%
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia )) %>%
  # remove 2023 records
  filter(YearFailure < 2023) # 2023 data haven't been received

df_fail_2022 <- orig_fail_2022 %>% 
  .[c(colnames(.)[c(2, 4, 5, 7, 8, 11, 13, 14, 15, 16, 17, 21)])] %>%
  set_names(c('ReportNum',
              'DateFailure', 'YearFailure',
              'CoordE', 'CoordN', 'City',
              'PipeExt', 'PipeDia', 'PipeMat', 'YearInstall',
              'FailurePart', 'FailureType')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0) %>%
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia ))

df_fail_2021 <- orig_fail_2021 %>% 
  .[c(colnames(.)[c(2, 3, 4, 6, 7, 10, 12, 13, 14, 15, 16, 20)])] %>%
  set_names(c('ReportNum', 
              'DateFailure', 'YearFailure',
              'CoordE', 'CoordN', 'City',
              'PipeExt', 'PipeDia', 'PipeMat', 'YearInstall',
              'FailurePart', 'FailureType')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0) %>%
  mutate(PipeEMD = paste0(PipeExt, "$", PipeMat, "$", PipeDia ))


df_fail <- rbind(df_fail_2023,
                 df_fail_2022, 
                 df_fail_2021) %>%
  # keep only one instance of duplicated rows
  distinct() #%>%
# filter(., PipeMat %in% c('S')) %>%
# filter(., PipeDia == 8) %>%
# filter(., YearFailure == 2022)

# Plot
ggplot(df_fail %>%
         group_by(YearFailure) %>%
         summarise(FailureCount = n()),
       aes(x = YearFailure, y = FailureCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Year") +
  ylab("Failure Count") +
  theme_bw()

# Pipe DF ========================================================
df_pipe_2022 <- orig_pipe_2022 %>% 
  .[c(colnames(.)[c(2, 3, 4, 5, 9, 10, 13, 17)])] %>%
  set_names(c('PipeMat', 'PipeExt', 'PipeDia', 'PipeEMD', 'YearInstall',
              'PipeRMID', 'PipeLengthExt', 'PipeLengthRMID')) %>%
  # remove rows with all "NA" values
  filter(rowSums(!is.na(.)) > 0)

df_pipe <- df_pipe_2022 %>%
  group_by(PipeEMD) %>%
  summarise(PipeLengthEMD = sum(PipeLengthRMID, na.rm = TRUE) / 5280, # in mile
            PipeMat = first(PipeMat),
            PipeDia = first(PipeDia),
            YearInstall = first(YearInstall)) %>%
  ungroup() %>% 
  mutate(YearInstall = as.numeric(as.character(YearInstall))) %>%
  mutate(YearInstall = ifelse(YearInstall < 1000, # should we remove?
                              1900 + YearInstall, 
                              YearInstall)) 
# NOTE: need to exclude pipes with some charasteristics

# Plot
ggplot(df_pipe %>% filter(., PipeMat %in% c('A', 'C', 'S')), 
       aes(x = as.factor(YearInstall), y = PipeLengthEMD, 
          fill = PipeMat)) +
  geom_col() +
  xlab("Year Install") +
  scale_x_discrete(breaks = pretty(df_pipe$YearInstall, n = 20)) +
  ylab("Total Pipe Length (Miles)") +
  theme_bw()

ggplot(df_pipe, 
       aes(x = as.factor(PipeDia), y = PipeLengthEMD, 
           fill = PipeMat)) +
  geom_col() +
  xlab("Pipe Diameter") +
  ylab("Total Pipe Length (Miles)") +
  theme_bw()

ggplot(df_pipe, 
       aes(x = as.factor(PipeMat), y = PipeLengthEMD, 
           fill = as.factor(PipeDia))) +
  geom_col() +
  xlab("Pipe Material") +
  ylab("Total Pipe Length (Miles)") +
  theme_bw()

# Length DF ======================================================

# sort failure years
failure_years <- sort(unique(df_fail$YearFailure))

# Create a list of data frames with Year column filled for each year
df_list <- lapply(failure_years, function(year) {
  df <- df_pipe
  df$Year <- year
  return(df)
})

# Combine the data frames in the list using rbind
df_list <- do.call(rbind, df_list)

# Create df length, consists of pipe length of every pipe mat and
# pipe age in every year
df_length <- df_list %>%
  # filter (Year >= 2015) %>%
  mutate(PipeAge = Year - YearInstall) %>%
  filter(PipeAge > 0) %>%
  # at that particular year, sum the length of all pipes with 
  # same age and same material type
  group_by(Year, PipeAge, PipeMat) %>%
  summarize(PipeLength = round(sum(PipeLengthEMD), 2), 
            .groups = 'drop') #%>%
# group_by(Year, PipeMat) %>%
# arrange(PipeAge) %>%
# mutate(CumPipeLength = round(cumsum(PipeLength), 2))

ggplot(df_length %>% 
         filter(., PipeMat %in% c('A', 'C', 'S')) %>%
         filter(., Year %in% c(1990, 2000, 2010, 2020)),
       aes(x = PipeAge, y = PipeLength,
           fill = PipeMat)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~ Year, nrow = 2, ncol = 2)
  

