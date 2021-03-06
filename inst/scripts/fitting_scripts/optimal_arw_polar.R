# run local job.
library(tidyverse)
combined_search <- humansearchdata::combined_search
neural_resource <- attentionmapsR::neural_resource
efficiency <- 1
prior_type <- "polar"
params_detection <- "[.756, .2, 1]"
start_params <- NULL

human_search_nested <- combined_search %>%
  mutate(contrast = .175) %>%
  group_by(subject, contrast, experiment, sample_type) %>%
  filter(experiment %in% c("uniform", "polar")) %>%
  nest(.key = "imported_human") %>%
  filter(subject == "arw", sample_type == "polar")

human_data <- human_search_nested$imported_human[[1]] %>%
  searchR::summary_search(.)

seed_val  <- sample(1:10000, 1)
set.seed(seed_val)
timenow   <- timestamp()
storedir  <- paste0('/tmp/', timenow, '/')
dir.create(storedir)
file_code <- stringi::stri_rand_strings(1, 16)
file_id   <- paste0(storedir, file_code)

n_parallel  <- 1
cl          <- parallel::makeCluster(n_parallel)

optim_results <- attentionmapsR::optimize_map(efficiency = 1,
                                              prior_type = prior_type,
                                              params_detection = params_detection,
                                              seed_val = seed_val,
                                              NP = 4,
                                              n_trials = 2400*4,
                                              n_parallel = n_parallel,
                                              itermax = 1,
                                              lower_bound = list(c(1, .001, 0, 1,    1, 1,  1)),
                                              upper_bound = list(c(1,    5, 0, 1,      1,  1,  1)),
                                              single_thread = TRUE,
                                              neural_resource = neural_resource,
                                              start_params = start_params,
                                              human_data = human_data,
                                              subject_fit = T,
                                              store_pop = file_id,
                                              cl = cl)

try({
  full_step_results <- purrr::map(list.files(path = storedir, pattern = file_code, full.name = T), function(x) {
    try({
      load(x);return(results_list)
    })
  })

  optim_results$full_step_results <- full_step_results

  try({save(file = paste0(storedir, 'optim_results_', file_code, '.rda'), optim_results)})
})

parallel::stopCluster(cl)
tmpenv <- environment()
save(file = paste0('/tmp/arwuniformtmpenvironemnt', file_code), tmpenv)
