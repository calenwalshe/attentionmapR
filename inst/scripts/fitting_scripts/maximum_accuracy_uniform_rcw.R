# run local job.
unlink(".Rdata")

library(tidyverse)
source('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/attentionmapR/R/optimize_fit_map.R', echo=FALSE)
load(file = '~/Dropbox/Calen/Work/search/modeling/_analysis/_code/count_resource/neural_resource.rda')
load(file = "~/Dropbox/Calen/Work/search/search_experiments/pre_pandemic/combined_search.rdata")
efficiency <- .79
prior_type <- "uniform"
params_detection <- "[.92, .28, 1]"
start_params <- global_start$start_params

human_search_nested <- combined_search %>%
  mutate(contrast = .175) %>%
  group_by(subject, contrast, experiment, sample_type) %>%
  filter(experiment %in% c("uniform", "polar")) %>%
  nest(.key = "imported_human") %>%
  filter(subject == "rcw", sample_type == "uniform")

human_data <- human_search_nested$imported_human[[1]] %>% 
  searchR::summary_search(.)

seed_val <- sample(1:10000, 1)
set.seed(seed_val)
timenow <- timestamp()
storedir <- paste0('/tmp/', timenow, '/')
dir.create(storedir)
file_code <- stringi::stri_rand_strings(1, 16)
file_id   <- paste0(storedir, file_code)

n_parallel <- 8
cl <- parallel::makeCluster(n_parallel, outfile = '/tmp/max_acc_uniform_rcw.txt')

optim_results <- optimize_map(efficiency = efficiency, 
                              prior_type = prior_type, 
                              params_detection = params_detection, 
                              seed_val = seed_val,
                              NP = 16,
                              n_trials = 2400*8,
                              n_parallel = n_parallel,
                              itermax = 2,
                              lower_bound = list(c(1, 4.028408, 0,    1,  0,   1,  efficiency)),
                              upper_bound = list(c(1, 4.028408,    0,    1,  .3,   1, efficiency)),
                              single_thread = FALSE,
                              neural_resource = neural_resource, 
                              start_params = start_params, 
                              human_data = human_data, 
                              subject_fit = F, 
                              store_pop = file_id,
                              cl = cl)

parallel::stopCluster(cl)
full_step_results <- purrr::map(list.files(path = storedir, pattern = file_code, full.name = T), function(x) {
  try({
    load(x);return(results_list)
  })
})

optim_results$full_step_results <- full_step_results

try({save(file = paste0(storedir, 'optim_results_', file_code, '.rda'), optim_results)})