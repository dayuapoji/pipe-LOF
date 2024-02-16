
get_prediction <- function(df_init, end_year, model) {

  df_pred <- NULL

  for (i in seq(1, end_year, by = 1)) {

    # compute predictions & create new df
    prediction <- predict(tuned_model, newdata = df_init)
    
    df_new <- df_init %>%
      mutate(InverseTBF  = prediction,
             DeltaPipeAge = df_init$PipeAge - df_init$PrevPipeAge)

    # store data
    df_pred <- rbind(df_pred, df_new)

    # iteration, df_new to be df_init
    df_init <- df_new %>%
      mutate(PipeAge = 1 + i,
             PrevPipeAge = df_new$PipeAge,
             PrevInverseTBF = df_new$InverseTBF,
             DeltaPipeAge = df_new$PipeAge - df_new$PrevPipeAge)
  }

  return(df_pred)
}


min(df_tbf_expand %>% filter(PipeMat == 'C', PipeDia == 6) %>% .$PipeLength)
max(df_tbf_expand %>% filter(PipeMat == 'C', PipeDia == 6) %>% .$PipeLength)

df_init <- data.frame(PipeAge = 1,
                      InverseTBF  = 0,
                      PipeMat = 'A',
                      PipeDia = 6,
                      PipeLength = 0.08,
                      PrevPipeAge = 0,
                      PrevInverseTBF = 0,
                      DeltaPipeAge = 1)

df_pred1 <- get_prediction(df_init, 70, tuned_model)


df_init <- data.frame(PipeAge = 1,
                      InverseTBF  = 0,
                      PipeMat = 'C',
                      PipeDia = 6,
                      PipeLength = 0.08,
                      PrevPipeAge = 0,
                      PrevInverseTBF = 0,
                      DeltaPipeAge = 1)
df_pred2 <- get_prediction(df_init, 70, tuned_model)

df_init <- data.frame(PipeAge = 1,
                      InverseTBF  = 0,
                      PipeMat = 'N',
                      PipeDia = 6,
                      PipeLength = 0.08,
                      PrevPipeAge = 0,
                      PrevInverseTBF = 0,
                      DeltaPipeAge = 1)

df_pred3 <- get_prediction(df_init, 70, tuned_model)

df_init <- data.frame(PipeAge = 1,
                      InverseTBF  = 0,
                      PipeMat = 'S',
                      PipeDia = 6,
                      PipeLength = 0.08,
                      PrevPipeAge = 0,
                      PrevInverseTBF = 0,
                      DeltaPipeAge = 1)

df_pred4 <- get_prediction(df_init, 70, tuned_model)

fig_pred <- ggplot(data = rbind(df_pred1, df_pred2, df_pred3, df_pred4)) +
  geom_point(aes(x = PipeAge, y = InverseTBF, color = PipeMat)) +
  geom_smooth(aes(x = PipeAge, y = InverseTBF, color = PipeMat), 
              size = 0.5, method = 'loess', linetype = 'dashed') +
  
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  # ylim(0, 1) +
  labs(x = "Pipe Age (Year)", y = "1/TBF") +
  # guides(color = "none") +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.2, 0.7),
        legend.box.background = element_rect(fill = "white", 
                                             color = "grey", 
                                             size = 1))

ggsave(paste0('../output-UESI/fig_ml_pred.pdf'), plot = fig, 
       width = 7.5, height = 5)

ggsave(paste0('../output-UESI/fig_ml_test_pred.pdf'), 
       plot = ggarrange(fig_test, fig_pred, nrow = 1), 
       width = 10, height = 4)

 




