# ==============================================================================
# LOAD DATA
# ==============================================================================

# Pipe data
orig_pipe <- read_csv('../data/PRP_Pipe_2020RMIDs.csv')

# Failure data
orig_fail <- read_csv('../data/PRP_Leak_2020RMIDs.csv')

# Predictors
# orig_cova <- read_csv('../data/cova.csv')[, 1:12]
# orig_soil <- read_csv('../data/soil.csv')
# orig_soil_index <- read_csv('../data/soil_index.csv')
