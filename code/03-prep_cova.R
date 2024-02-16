# Soil properties data
soil <- orig_soil[,1] %>% 
  # round all values to 2 decimals
  cbind(round(orig_soil[, 2:ncol(orig_soil)], digits = 2)) 
  # join with soil index by SoilType

soil_index <- orig_soil_index %>% 
  .[, 2:3] %>% distinct(SoilType, .keep_all = TRUE)

cova_soil <- soil %>% 
  left_join(., soil_index, by ='SoilType') %>%
  # this follows the cova main df, need to be confirmed
  mutate(SoilIndex = ifelse(SoilType == "SiltyClay", 7, SoilIndex)) %>%
  # mutate(SoilIndex = ifelse(SoilType == "SandyClayLoam", 4, SoilIndex))
  mutate(SoilIndex = as.character(SoilIndex)) %>% 
  drop_na()
  

# Environmental data
cova_ops <- orig_cova %>% #.[,1:9]
  # only retain pipe ops columns
  .[, c('PipeEMD', 'Pres_mean', 'Pres_max')] %>%
  # to avoid assumption, remove all rows with any NA values
  drop_na() %>%
 # Group by PipeEMD and retain row with the highest Pres_mean
  group_by(PipeEMD) %>%
  filter(Pres_mean == max(Pres_mean)) %>%
  ungroup() %>%
  # convert pressure to numeric, then round to 2 decimals  
  mutate(across(Pres_mean:Pres_max, as.numeric)) %>%
  mutate_at(vars(Pres_mean, Pres_max), ~round(., 2)) %>%
  drop_na()

cova_env <- orig_cova %>% 
  # only retain pipe env columns
  .[, c(1, 5:12)] %>%
  # to avoid assumption, remove all rows with any NA values
  drop_na() %>%
  # convert pressure to numeric, then round to 2 decimals  
  mutate(across(GWT:CapilaryRise, as.numeric)) %>%
  mutate_at(vars(GWT, GWT_var, CapilaryRise), ~round(., 2)) %>%
  drop_na() %>%
  distinct(PipeEMD, .keep_all = TRUE)


cova <- cova_ops %>%
  left_join(., cova_env, by = 'PipeEMD') %>%
  left_join(., cova_soil, by = 'SoilIndex') %>%
  drop_na() #%>%
  # select(
  #   -Pres_max,
  #   -GWT_var,
  #   -SoilType,
  #   -EC_min,
  #   -EC_max,
  #   -k_min,
  #   -k_max,
  #   -WC_min,
  #   -WC_max,
  #   -WP_min,
  #   -WP_max,
  # )
  
 