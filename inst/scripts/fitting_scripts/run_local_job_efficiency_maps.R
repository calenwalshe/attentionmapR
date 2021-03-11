library(tidyverse)
load(file = '/tmp/map_parameters_df') # load parameters

# main code to estimate the optimal efficiency gain maps
efficiency_map_1D <- searchR::estimate_efficiency_maps(map_parameters_df, 
                                              optim_params = list(n_cores = 14, maxit = 1000, tolerance = 1e-20, NP = 1000))


# for each efficiency great a map with uniform efficiency
efficiency_map_1D_uniform <- efficiency_map_1D %>% 
  group_by(efficiency, map_prior, prior_type, subject, contrast, prior.scale, scale.dist, sp_const) %>% 
  nest() %>%
  group_by(efficiency, subject, prior_type) %>%
  filter(row_number() == 1) %>%
  unnest() %>%
  mutate(y.scale = efficiency) %>%
  mutate(map_prior = "flat")

efficiency_map_1D <- rbind(efficiency_map_1D, efficiency_map_1D_uniform)

save(efficiency_map_1D, file = '/tmp/efficiency_map_1D')
