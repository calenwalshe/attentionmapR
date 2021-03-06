drasdo <- "[.92, .28, 1]"
#watson <- "[.89, .182, 1]"
single_search <- searchR::run_single_search(map_path = NULL, 
                                              contrast = .175,
                                              efficiency = NULL,
                                              n_trials = 12, 
                                              radius = 8,
                                              priorType = 1,
                                              search_params = drasdo,
                                              single_thread = TRUE)

opt_crt <- searchR::find_optimal_criterion(single_search)
opt_val <- opt_crt$optim$bestmem

imported_model <- searchR::import_model(single_search, criterion = opt_val) %>%
  #mutate(radius = 8) %>%
  humansearchdata::add_threshold(.) %>%
  humansearchdata::add_accuracy()

summarized_model <- searchR::summary_search(imported_model)
