

df_results <- rbind(read_csv('../output-UESI/df_results1.csv'),
                    read_csv('../output-UESI/df_results2.csv'),
                    read_csv('../output-UESI/df_results3.csv'))


# Sample 10% of the data points
# set.seed(1)  # for reproducibility
# sample_indices <- sample(nrow(df_results), size = 0.01 * nrow(df_results))


# Create a custom discrete color scale with specific color assignments
# Create the ggplot with color scale and labeled data points
fig <- ggplot(df_results %>% filter(InverseTBF <= 1)) +
  geom_point(aes(x = PipeAge, y = InverseTBF, color = InverseTBF),
             alpha = 0.5) +
  

  # geom_text_repel(
  #   data = df_results %>% filter(InverseTBF >= 0.6),  # Subset of data to label
  #   aes(x = PipeAge, y = InverseTBF, label = PipeEMD),  # Label with PipeEMD
  #   size = 1, 
  #   alpha = 0.5,
  #   box.padding = 0.5, 
  #   point.padding = 0.5, 
  #   segment.size = 0.25,
  #   max.overlaps = Inf) +
  
  scale_colour_gradientn(colours = c("green","orange","red"),
                         values = c(0, 0.5, 1)) + 
  
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  ylim(0, 1) +
  labs(x = "Pipe Age (Year)", y = "1/TBF") +
  guides(color = "none") +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank())

ggsave(paste0('../output-UESI/fig_lof_noID.pdf'), plot = fig, 
       width = 7.5, height = 5)



