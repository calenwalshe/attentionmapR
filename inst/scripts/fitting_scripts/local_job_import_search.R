# This import script imports the results from the search modeling into R. Formats for accuracy and prepares it for analysis.
rm(list = ls())
library(tidyverse)
library(parallel)

load('/tmp/maps_nested_job')

trials <- unlist(map(maps_nested_job$raw_search, class))
maps_nested_job <- maps_nested_job[!(trials == "try-error"), ]

# estimate two types of criterion values. leads to different levels of search accuracy
optimal_criterion   <- mclapply(maps_nested_job$raw_search, FUN = function(x) searchR::find_optimal_criterion(x), mc.cores = 16) # optimal criterion for search
maps_nested_job$optimal_criterion <- unlist(map(optimal_criterion, c(1,1)))

# Import the model, use the importer function defined in pacakge searchR
n.row <- 1:nrow(maps_nested_job)
maps_nested_job$imported_model_optimal <- mclapply(n.row, function(idx) {
  x <- maps_nested_job[idx, ]$raw_search[[1]]
  y <- maps_nested_job[idx, ]$optimal_criterion[[1]]
  imported_model <- searchR::import_model(x, criterion = y) %>%
  humansearchdata::add_threshold(.) %>%
  humansearchdata::add_accuracy()
  }, mc.cores = 28)


# Compute summary statistics for the search
maps_nested_job$summarized_model_optimal <- mclapply(maps_nested_job$imported_model_optimal, function(x) {
  summarized_model <- searchR::summary_search(x)
}, mc.cores = 16)

# import human data
combined_search <- humansearchdata::combined_search
# Import, and join human search with model search into a single dataframe
human_search_nested <- combined_search %>%
  mutate(contrast = .175) %>%
  group_by(subject, contrast, experiment, sample_type) %>%
  filter(experiment %in% c("uniform", "polar")) %>%
  nest(.key = "imported_human")

human_search <- map(human_search_nested$imported_human, searchR::summary_search)
human_search_nested$summarized_human <- human_search

maps_nested_job <- maps_nested_job %>%
  dplyr::rename(sample_type = prior_type)

human_model_joined <- left_join(maps_nested_job, human_search_nested, by = c("contrast", "subject", "sample_type"))

human_model_joined$map_prior <- as.character(human_model_joined$map_prior) # tidy

# compute error (hit + cr)
error <- map2(human_model_joined$summarized_human, human_model_joined$summarized_model_optimal,
              function(x, y) {
                x$prop <- ifelse(x$prop == 0, 1/2400, x$prop);
                x$prop <- x$prop / sum(x$prop);
                y$prop <- ifelse(y$prop == 0, 1/2400, y$prop);
                y$prop <- y$prop / sum(y$prop);

                #-sum(x$prop * log(y$prop))

                sum(c(y[y$type == "cr", "prop"][[1]], y[y$type == "hit", "prop"][[1]]))
              })
human_model_joined$error <- unlist(error)

# rename columns
human_model_joined <- human_model_joined %>%
  mutate(model = summarized_model_optimal, human = summarized_human)

human_model_joined$efficiency <- round(human_model_joined$efficiency,3)

save(file = "/tmp/human_model_joined", human_model_joined) #save output
print(paste("Saved... ", '/tmp/human_model_joined'))
