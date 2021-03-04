library(tidyverse)

load('/tmp/efficiency_map_1D') # load efficiency estimated maps

maps_nested <- efficiency_map_1D %>% 
  group_by(n_rings,sp_const, prior.scale, scale.dist, prior_type, 
           map_prior, subject, params, contrast, efficiency, seed_val, label, n_trials) %>% 
  nest()

maps_nested$file_id <- paste0('/main/calen/fitmaps/', stringi::stri_rand_strings(nrow(maps_nested), 16), '.mat')

# interpolate to estimated gain maps to the full retina. to be used in actual search.
#source('~/Dropbox/Calen/Work/search/modeling/optimal_dp_scale_map/_code/_estimate_maps/fit_maps.R', echo=FALSE)
system("rm -rf /main/calen/fitmaps/*")
split_maps <- split(maps_nested, rep(1:(length(maps_nested$data)/16), length.out = length(maps_nested$data)))
file_id <- map(split_maps, function(x) {
  maps <- searchR::fit_maps(maps_1d = x$data)
  searchR::store_maps_disk(x$file_id, maps)
})

maps_nested_job <- maps_nested
save(file = '/tmp/maps_nested_job', maps_nested_job)

print(paste("Saved... ", '/tmp/maps_nested_job'))
