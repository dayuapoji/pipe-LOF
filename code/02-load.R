# ==============================================================================
# LOAD DATA
# ==============================================================================

# Failure data
orig_fail_2023 <- read_csv('../data/PRP_Leak_2023.csv')
orig_fail_2022 <- read_csv('../data/PRP_Leak_2022.csv')
orig_fail_2021 <- read_csv('../data/PRP_Leak.csv')

# Pipe data
orig_pipe_2022 <- read_csv('../data/PRP_Pipe_2022.csv')

# Predictors
# orig_cova <- read_csv('../data/cova.csv')
# orig_soil <- read_csv('../data/soil.csv')
# orig_soil_index <- read_csv('../data/soil_index.csv')
