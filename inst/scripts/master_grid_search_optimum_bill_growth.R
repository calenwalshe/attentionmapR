source('~/Dropbox/Calen/Work/searchR/R/summary_figures.R', echo=TRUE)
library(tidyverse)
efficiency_map_1D$n_trials <- 2400 * 4
#efficiency_map_1D$label <- "fits"

efficiency_map_1D <- efficiency_map_1D %>% group_by(subject, sample_type, label) %>%
  mutate(seed_val = sample(1:1000, 1))

save(file = '/tmp/efficiency_map_1D', efficiency_map_1D)
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/run_local_job_interpolate_maps.R',
                         'local_job_interpolate_maps', exportEnv = "")

# Perform the actual search with the estimated gain fields #
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/run_local_job_search.R',
                         'local_job_search', exportEnv = "search_results")

gc(reset = TRUE)

# Import/Transform/Store the search results
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/local_job_import_search.R',
                         'local_job_import_search', exportEnv = "")

load("/tmp/human_model_joined")

human_model_joined$a <- unlist(purrr::map(human_model_joined$data, function(x) {x$a %>% unique()}))
human_model_joined$b <- unlist(purrr::map(human_model_joined$data, function(x) {x$b %>% unique()}))
human_model_joined$c <- unlist(purrr::map(human_model_joined$data, function(x) {x$c %>% unique()}))
human_model_joined$d <- unlist(purrr::map(human_model_joined$data, function(x) {x$d %>% unique()}))
human_model_joined$g_min <- unlist(purrr::map(human_model_joined$data, function(x) {x$g_min %>% unique()}))
human_model_joined$g_max <- unlist(purrr::map(human_model_joined$data, function(x) {x$g_max %>% unique()}))

human_model_joined <- human_model_joined %>%
  ungroup() %>%
  mutate(efficiency_percent = round(efficiency / max(efficiency), 3)) %>%
  mutate(efficiency_rank = dense_rank(efficiency),
         intercept_rank = dense_rank(a),
         a_rank = dense_rank(a),
         b_rank = dense_rank(b),
         c_rank = dense_rank(c),
         d_rank = dense_rank(d),
         g_min_rank = dense_rank(g_min),
         g_max_rank = dense_rank(g_max),
         sp_const = b)

human_model_joined <- human_model_joined %>% arrange(efficiency)

human_model_joined$label[1:4] <- "best_fit"
human_model_joined$label[c(5,6,7,8)] <- "optimal"
human_model_joined$label[c(7,8,9,12)] <- "flat"
human_model_joined$label[c(13, 14)] <- c("max_attn", "max_attn_c")
human_model_joined$label[c(15)] <- "max_attn_b"
# Display/Print/Analyze the results
rank <- 1:(length(human_model_joined$cp %>% unique()))
figures <- purrr::map(rank, function(x) {
  optimal <- human_model_joined %>%
    #filter(efficiency == .779) %>%
    filter(scale.dist == 1) %>%
    #filter(c >= .9, a <= 1.2) %>%
    #filter(efficiency_rank %in% 3) %>%
    #filter(cp_rank %in% x) %>%
    #filter(b_2_rank == 1) %>%
    #filter(a == .45) %>%
    mutate(sp_const = label) %>% filter(subject == "arw")
  fig <- plot_full_summary(optimal %>% ungroup())
  fig.out <- gridExtra::grid.arrange(grobs = fig, ncol = 3)
  ggsave(file = paste0('~/Dropbox/Calen/Dropbox/', "test", '.pdf'), fig.out, width = 45, height = 25, units = "in")
  return(fig)
})


# Display/Print/Analyze the results
best_fit_uniform <- human_model_joined %>%
  mutate(sp_const = round(scale.dist,3)) %>%
  filter(map_prior == "uniform") %>%
  filter(((subject == "anqi") & (b == .5 & a == .5 & efficiency_rank == 2)) |
           ((subject == "rcw") & (b == .5 & a == .4 & efficiency_rank == 3)))
cowplot::plot_grid(plotlist =plot_full_summary(best_fit_uniform))

best_fit_polar <- human_model_joined %>%
  mutate(sp_const = round(scale.dist,3)) %>%
  filter(map_prior == "polar") %>%
  filter(((subject == "anqi") & (b_rank == 4 & a == .5 & efficiency_rank == 1 & cc == .95)) |
           ((subject == "rcw") & (b_rank == 4 & a == .4 & efficiency_rank == 1 & cc == .8)))
cowplot::plot_grid(plotlist =plot_full_summary(best_fit_polar))
