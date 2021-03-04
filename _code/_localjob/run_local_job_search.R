load(file = '/tmp/maps_nested_job')
n.row <- nrow(maps_nested_job)
# Run the search code with the estimated optimal gain maps in parallel
ptm <- proc.time()
model_search_lst <- parallel::mclapply(1:n.row, FUN = function(x) {try({
  if(maps_nested_job$prior_type[x] == "polar") {
    prior_type <- 2
  } else if(maps_nested_job$prior_type[x] == "uniform") {
    prior_type <- 1
  }
  paste("Currently processing prior type:", prior_type)
  
  bgScale  <- maps_nested_job$scale.dist[x]
  n_trials <- maps_nested_job$n_trials[x]
  
  # call matlab code from the wrapper function with the supplied parameters
  single_search <- searchR::run_single_search(map_path = maps_nested_job$file_id[x], 
                                              seed_val = maps_nested_job$seed_val[x],
                                     contrast = maps_nested_job$contrast[x],
                                     efficiency = maps_nested_job$efficiency[x],
                                     n_trials = n_trials, 
                                     radius = 8 / bgScale,
                                     priorType = prior_type,
                                     search_params = maps_nested_job$params[x])
})}, mc.cores = 16)
proc.time() - ptm
maps_nested_job$raw_search <- model_search_lst
maps_nested_job$radius <- 8 / maps_nested_job$scale.dist


save(file = '/tmp/maps_nested_job', maps_nested_job)

print(paste("Saved... ", '/tmp/maps_nested_job'))