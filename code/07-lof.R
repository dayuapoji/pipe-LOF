
df_lof <- df_pipe %>%
  mutate(PipeAge = 2023 - year(YearInstall))

df_results <- NULL

system.time(
  
  for (i in 1:100) {
    
    # computation control
    print(paste('compute', i))
    
    # get ml prediction
    df_pred <- get_prediction(data.frame(PipeAge = 1,
                                         InverseTBF  = 0,
                                         PipeMat = df_lof$PipeMat[i],
                                         PipeDia = df_lof$PipeDia[i],
                                         PipeLength = df_lof$PipeLength[i],
                                         PrevPipeAge = 0,
                                         PrevInverseTBF = 0,
                                         DeltaPipeAge = 1),
                              end_year = df_lof$PipeAge[i],
                              tuned_model)
    
    # extract 1/TBF at the present year (2023)
    df_result <- data.frame(PipeEMD = df_lof$PipeEMD[i],
                            PipeAge = tail(df_pred$PipeAge, 1),
                            InverseTBF = tail(df_pred$InverseTBF, 1))
    
    # store results
    df_results <- rbind(df_results, df_result)
    
  }
)



# ==============================================================================
# PARALELLIZATION
# ==============================================================================

# Load the necessary packages
library(doParallel)
library(foreach)

# Set the number of cores to use (adjust this based on your system)
num_cores <- 12  # You can change this number based on your CPU capacity

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Initialize the result data frame
df_results <- NULL

# Define a function to perform the computation for each chunk of iterations
process_chunk <- function(start, end, df_lof, tuned_model) {
  
  df_results_chunk <- NULL
  
  for (i in start:end) {
    # computation control
    print(paste('compute', i))
    
    # get ml prediction
    df_pred <- get_prediction(data.frame(PipeAge = 1,
                                         InverseTBF  = 0,
                                         PipeMat = df_lof$PipeMat[i],
                                         PipeDia = df_lof$PipeDia[i],
                                         PipeLength = df_lof$PipeLength[i],
                                         PrevPipeAge = 0,
                                         PrevInverseTBF = 0,
                                         DeltaPipeAge = 1),
                              end_year = df_lof$PipeAge[i],
                              tuned_model)
    
    # extract 1/TBF at the present year (2023)
    df_result <- data.frame(PipeEMD = df_lof$PipeEMD[i],
                            PipeAge = tail(df_pred$PipeAge, 1),
                            InverseTBF = tail(df_pred$InverseTBF, 1))
    
    # store results
    df_results_chunk <- rbind(df_results_chunk, df_result)
  }
  
  return(df_results_chunk)
}

# Split the iterations into chunks
chunk_size <- 10
chunks <- split(1:30, ceiling(1:30 / chunk_size))

system.time(
  # Use parallel foreach to process the chunks in parallel
  df_results <- foreach(chunk = chunks) %dopar% {
    start <- min(chunk)
    end <- max(chunk)
    process_chunk(start, end, df_lof, tuned_model)
  }  
)

# Combine the results from all chunks
df_results <- do.call(rbind, df_results)

# Stop the parallel backend
stopCluster(cl)

