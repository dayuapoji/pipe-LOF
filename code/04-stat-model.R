# ==============================================================================
# PREPARING TBF DATA FRAME
# ==============================================================================

df_tbf <- left_join(df_fail, 
                    df_pipe %>% subset(select = -c(PipeMat, PipeDia)),
                    by = 'PipeEMD') %>%
  
  # get pipe age at failure
  mutate(PipeAgeFailure = (DateFailure - YearInstall) / dyears(1)) %>%
  # clean pipe age at failure < 0
  filter(PipeAgeFailure > 0) %>%
  
  # compute Time Between Failure (TBF) for each individual pipe
  arrange(PipeEMD, PipeAgeFailure) %>% group_by(PipeEMD) %>%
  mutate(TBF = c(NA, diff(PipeAgeFailure))) %>%
  
  # get InverseTBF param
  mutate(InverseTBF = 1/TBF) %>%
  
  # remove PipeEMD with only 1 failure
  group_by(PipeEMD) %>%
  filter(n() > 1) %>%
  ungroup()

# df_tbf <- df_tbf %>% filter(InverseTBF <= 1)

# plot
ggplot(df_tbf) +
  geom_point(aes(x = PipeAgeFailure, y = InverseTBF, color = PipeMat)) +
  # guides(color = "none") +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  ylim(0, 1) +
  labs(x = "Pipe Age (Year)", y = "1/TBF", color = 'Pipe Material') +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.1, 0.8),
        legend.box.background = element_rect(fill = "white", 
                                             color = "grey", 
                                             size = 1))

# ==============================================================================
# EXPANDING TBF DATA FRAME
# ==============================================================================

df_tbf_expand <- NULL
for (pipe in unique(df_tbf$PipeEMD)) {
  df_expand <- df_tbf %>%
    subset(select = c(PipeEMD, PipeMat, PipeDia, PipeLength, PipeAgeFailure, InverseTBF)) %>%
    filter(PipeEMD == pipe) %>%
    complete(PipeAgeFailure = seq(ceiling(min(PipeAgeFailure)),
                                  ceiling(max(PipeAgeFailure)),
                                  by = 0.2)) %>%
    arrange(PipeAgeFailure) %>%  # Add this line to order PipeAgeFailure
    fill(PipeEMD, PipeMat, PipeDia, PipeLength, InverseTBF, .direction = "up") %>%
    
    # clean
    mutate_all(~ ifelse(is.infinite(.), NA, .)) %>%
    drop_na() #%>%
    # filter(InverseTBF <= 1)
  
  df_tbf_expand <- rbind(df_tbf_expand, df_expand)
}

df_tbf_expand <- df_tbf_expand %>% filter(InverseTBF <= 1)

# plot
fig <- ggplot(df_tbf_expand) +
  geom_point(aes(x = PipeAgeFailure, y = InverseTBF, color = PipeMat),
             alpha = 0.5) +
  # guides(color = "none") +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  labs(x = "Pipe Age (Year)", y = "1/TBF", color = 'Pipe Material') +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.1, 0.8),
        legend.box.background = element_rect(fill = "white", 
                                             color = "grey", 
                                             size = 1))


# save
ggsave(paste0('../output-UESI/fig_tbf.pdf'), plot = fig, 
       width = 7.5, height = 5)

# ==============================================================================
# STATISTICAL MODEL
# ==============================================================================

# df_tbf_expand <- df_tbf

df_model <- NULL
for (pipe in unique(df_tbf_expand$PipeEMD)) {
  
  df <- df_tbf_expand %>% filter(PipeEMD == pipe)
  y = df$InverseTBF    # 1/TBF for one pipe
  x = df$PipeAgeFailure    # failure ages for one pipe
  params = monotone_fit(y, x)$par    # these are the two estimated parameters you care about!
  # Define PipeAge as a vector
  PipeAge <- seq(0, 70, by = 1) #max(df$PipeAgeFailure
  
  # Calculate InverseTBF based on the parameters
  InverseTBF <- params[1] * params[2] * PipeAge^(params[2] - 1)
  print(params)
  # Create df_model data frame
  df_model_pipe <- data.frame(PipeEMD = pipe, PipeAge = PipeAge, InverseTBF = InverseTBF)
  
  # store results
  df_model <- rbind(df_model, df_model_pipe)
  
  # # plot
  # print(ggplot(df_model_pipe) +
  #         # geom_point(aes(x = PipeAge, y = InverseTBF, color = PipeEMD)) +
  #         geom_line(aes(x = PipeAge, y = InverseTBF, color = PipeEMD)) +
  # 
  #         geom_point(data = df_tbf_expand %>% filter(PipeEMD == pipe),
  #                    aes(x = PipeAgeFailure, y = InverseTBF),
  #                    color = 'black') +
  #         ggtitle(pipe) +
  #         guides(color = "none") +
  #         xlim(0, 50) +
  #         theme_bw()
  # )
}

length(unique(df_tbf_expand$PipeEMD))
length(unique(df_model$PipeEMD))

# plot
fig <- ggplot(df_model) +
  geom_line(aes(x = PipeAge, y = InverseTBF, color = PipeEMD),
            alpha = 0.5) +
  guides(color = "none") +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  ylim(0, 1) +
  labs(x = "Pipe Age (Year)", y = "1/TBF") +
  theme_bw(base_size = 10)

# save
ggsave(paste0('../output-UESI/fig_statmod.pdf'), plot = fig, 
       width = 7.5, height = 5)

set.seed(1)
pipes <- sample(df_tbf %>% 
                 group_by(PipeEMD) %>%
                 filter(n() > 2) %>% # more than 3 failures
                 ungroup() %>%
                 .$PipeEMD, size = 9)

for (i in pipes) {
  print(
    ggplot() +
      
      geom_line(data = df_model %>% filter(PipeEMD == i),
                aes(x = PipeAge, y = InverseTBF),
                color = 'red',
                size = 1, alpha = 0.5) +
      
      geom_point(data = df_tbf_expand %>% filter(PipeEMD == i),
                 aes(x = PipeAgeFailure, y = InverseTBF)) +
      
      ggtitle(paste("Pipe =", i)) +
      scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
      ylim(0, 1) +
      labs(x = "Pipe Age (Year)", y = "1/TBF") +
      guides(color = "none") +
      theme_bw(base_size = 10)
  )
}

#############################################################################

pipes <- c('43597$A$6', 'E-31964-A$A$4', 'E-18112$C$4', '38231-A$A$6')

fig <- NULL
for (i in 1: length(pipes)) {
  
  fig[[i]] <- ggplot() +
    
    geom_line(data = df_model %>% filter(PipeEMD == pipes[i]),
              aes(x = PipeAge, y = InverseTBF),
              color = 'red',
              size = 1, alpha = 0.5) +
    
    geom_point(data = df_tbf_expand %>% filter(PipeEMD == pipes[i]),
               aes(x = PipeAgeFailure, y = InverseTBF)) +
    
    ggtitle(paste("Pipe =", pipes[i])) +
    scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
    ylim(0, 1) +
    labs(x = "Pipe Age (Year)", y = "1/TBF") +
    guides(color = "none") +
    theme_bw(base_size = 10)
  
  # save
  # ggsave(paste0('../output-UESI/fig_statmod_',i,'.pdf'), plot = fig, 
  #        width = 7.5, height = 5)
  
}

ggsave(paste0('../output-UESI/fig_tbf_statmod.pdf'), 
       plot = ggarrange(fig[[1]], fig[[3]], fig[[4]], nrow = 1),
       width = 12, height = 4)

df_pipe %>% filter(PipeEMD == pipes[[4]])



