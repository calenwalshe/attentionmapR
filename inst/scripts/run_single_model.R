drasdo <- "[.87, .25, 1]"



single_search <- searchR::run_single_search(map_path = NULL,
                                            seed_val = 0,
                                            contrast = .175,
                                            efficiency = NULL,
                                            n_trials = 1200,
                                            radius = 8 / 1,
                                            priorType = 1,
                                            search_params = drasdo,
                                            single_thread = TRUE)

single_search$offset <- .5 # only valid for anqi
opt_crt <- searchR::find_optimal_criterion(single_search)
opt_val <- opt_crt$optim$bestmem

imported_model <- searchR::import_model(single_search, criterion = opt_val) %>%
  #mutate(radius = 8) %>%
  humansearchdata::add_threshold(.) %>%
  humansearchdata::add_accuracy()

summarized_model <- searchR::summary_search(imported_model)


tt <- summarized_model %>%
  filter(type %in% c("cr", "hit"))

tt$prop %>% sum
