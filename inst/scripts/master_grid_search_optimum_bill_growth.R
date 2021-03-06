library(tidyverse)
library(searchR) 

# get the parameters for estimating the models
map_parameters_df <- searchR::create_linear_map_parameters() %>% as_tibble()

retina_params <- searchR::create_retinal_constants(version = "standard")

load(file = '~/Dropbox/Calen/Work/search/gain_map/neural_resource.rda')
map_parameters_df$neural_resource <- list(neural_resources = neural_resource$neural_resource)
map_parameters_df$c_deg <- list(neural_resource$eccentricity)
map_parameters_df$num_ring <- list(NA)
map_parameters_df$spacing <- map(map_parameters_df$c_deg, function(x) {searchR::sp_xy(x,0)})
map_parameters_df$pix_count <- map(map_parameters_df$spacing, function(x) {x^2 * 120^2})

map_parameters_df <- unnest(map_parameters_df)

map_parameters_df <- map_parameters_df %>% 
  group_by(sp_const, efficiency, prior.scale, n_rings, prior_type, map_prior, subject, scale.dist, params) %>% 
  nest()

map_parameters_df$efficiency <- round(map_parameters_df$efficiency, 3)


map_parameters_df <- map_parameters_df %>% filter(map_prior == "polar")
n_row <- nrow(map_parameters_df)

efficiency_list <- lapply(1:n_row, FUN = function(x) {
  total_gain <- map_parameters_df[x, ]$efficiency
  prior_type <- map_parameters_df[x, ]$prior_type
  parameter_values <- map_parameters_df[[x, "data"]][[1]]
  params_detection <- as.numeric(stringr::str_extract_all(map_parameters_df[[x, "params"]], "\\..[:number:]+|[:number:]")[[1]])
  
  if(total_gain == 1) {
    models <- expand.grid(a = c(1), b = c(0))
  } else {
    #models <- expand.grid(a = c((seq(0, .7, by = .1)), .375, .4, .425, .45, .475, .5), b = c((64 * (1/2)^(1:12)), .383, .422, .35, .45, .4))
    #models <- expand.grid(a = c(.4, .8, 1.2), b = c(.5))
    #models[nrow(models) + 1,] = c(.5, .383) #anqi slope should be  .091
    #models[nrow(models) + 1,] = c(.4, .422) #rcw slope should be .123
    #models <- expand.grid(a = c((seq(0, .75, by = .25)), .4, .45, .5, 1.5, total_gain), b = c((16 * (1/2)^(1:6)), .45, .4)) %>%
    #  unique()
    #models <- expand.grid(a = c(.05, .1, .15, .2, .25, .3, .325, .35, .375, .4, .425, .5, 1.5), b = c(.4, .45, .5, .55, .6, .65, 1, 1.5, 2, 2.5, 3)) %>%
    #  unique()
    #models.1 <- expand.grid(a = c(.45), b_2 = c(1), c = c(1.25, 1.5, 1.75, 1.8, 1.85)) %>%
    #  unique()    
    #models.2 <- expand.grid(a = c(.43), b_2 = c(1), c = c(1.25, 1.5, 1.75, 1.8, 1.85)) %>%
    #  unique()        
    #models.1 <- expand.grid(a = seq(0, .5, .1), b_2 = 1, c = (16 * 1/(2^(1:8))))
    #models.2 <- expand.grid(a = seq(1), b_2 = c(2), c = (16 * 1/(2^(1:8))))
    models.2 <- expand.grid(a = seq(.25), b_2 = c(1), c = c(1))
    
    models <- rbind(models.2) %>% unique()
    
    #models[nrow(models)+1,] <- c(.459, 1, 1.853)
    #models[nrow(models)+1,] <- c(.4596,1, 1.853)
    #models[nrow(models)+1,] <- c(1 * total_gain,1)
    #models <- models %>% filter(!((a > 1) & (b != 1)))
    # test points for anqi and calen.
  }
  
  G <- sum(parameter_values$neural_resource) * total_gain
  neural_resource <- parameter_values$neural_resource
  
  f.growth <- function(a, b_1, b_2, c_1, c_2, decay = F) {
    xmax <- 8
    x <- parameter_values$c_deg
    if(!decay) { # experiment is uniform
      fh <- 1
      fl <- a + (1-a)*(1-exp(-(x/b_1)^c_1));
      fh <- a + (1-a)*(1-exp(-((xmax-x)/b_2)^c_2));      
      val <- apply(cbind(fl, fh), 1, min)      
    } else { # experiment is polar
      if(a == 1) { # gain starts at 1 and decreases monotonically
        a <- ifelse(a==1, 0, a) 
        b_2 <- b_1
        fl <- a + (1-a)*(1-exp(-(x/b_1)^c_1));
        fh <- a + (1-a)*(1-exp(-((xmax-x)/b_2)^c_2));        
        val <- fh
      } else {
        fl <- a + (1-a)*(1-exp(-(x/b_1)^c_1));
        fh <- a + (1-a)*(1-exp(-((xmax-x)/b_2)^c_2));        
        val <- apply(cbind(fl, fh), 1, min)
      }
    }
    return(val)
  }

  
  f.optim <- function(a, b_1, b_2, c, decay = F) {
    val <- f.growth(a, b_1, b_2, c_1 = c, c_2 = c, decay)
    gain <- val
    neural_response <- neural_resource * gain
    
    nrs <- sum(neural_response)
    error <- sqrt(sum((G - nrs)^2))
    if(nrs > G) {
      error <- 2*error
    }
    return(error)
  }
  
  decay <- (prior_type == "polar")
  
  if(decay != TRUE) {
    models <- models %>% filter(b_2 == first(b_2))
  }
  
  model_rows <- 1:nrow(models)
  b_1 <- parallel::mclapply(model_rows, function(x) {
    a <- models[x,"a"][[1]]
    b_2 <- models[x, "b_2"][[1]]
    c <- models[x,"c"][[1]]
    f <- purrr::partial(f.optim, a = a, b_2 = b_2, c = c, decay = decay); 
    par <- DEoptim::DEoptim(f, lower = 0, upper = 10, 
                            DEoptim::DEoptim.control(reltol = 1e-8, 
                                                     trace = 50, 
                                                     steptol = 100, 
                                                     VTR = .001))$optim$bestmem    
  }, mc.cores = 16)

  models$b_1 <- unlist(b_1)
  models$decay <- decay
  
  dp_vals <- humandetectiondata::f.dprime.no.uni(8, 
                                                 parameter_values$c_deg * 120, 
                                                 0, 1, 
                                                 p_kSp = params_detection[2], 
                                                 p_k0 = params_detection[1]) # compute d' on the rings for the no uncertainty case.
  models <- models %>%
    rowwise() %>%
    mutate(y.scale = list(f.growth(a, b_1, b_2, c_1 = c, c_2 = c, decay)),
           y.dprime = list(dp_vals))
  
  parameter_values_tmp <- parameter_values %>% 
    group_by(contrast) %>% 
    nest()
  
  combined_models <- merge(parameter_values_tmp, models) %>% 
    tibble() %>% 
    unnest()
  
  efficiency_map <- combined_models
  
  return(efficiency_map)
})

map_parameters_df$data <- efficiency_list

map_parameters_df.1 <- map_parameters_df %>%
  unnest()

efficiency_map_1D <- map_parameters_df.1

efficiency_map_1D <- efficiency_map_1D %>%
  group_by(efficiency, sp_const, prior_type, map_prior, subject, contrast, scale.dist, a, b_1, b_2, c) %>%
  nest()

efficiency_map_1D$overcount <- map(1:nrow(efficiency_map_1D), function(x) {
  abs(efficiency_map_1D$data[[x]]$y.scale %*% efficiency_map_1D$data[[x]]$neural_resource - sum(efficiency_map_1D$data[[x]]$neural_resource * efficiency_map_1D[x, "efficiency"][[1]])) > .01
}) %>% 
  unlist()
  
efficiency_map_1D <- efficiency_map_1D %>% 
  mutate(sp_const = stringi::stri_rand_strings(n(), 16)) %>%
  unnest()

######
######
######
efficiency_map_1D$n_trials <- 2400 * 4
#efficiency_map_1D$label <- "fits"

efficiency_map_1D <- efficiency_map_1D %>% group_by(subject, sample_type, label) %>% 
  mutate(seed_val = sample(1:1000, 1))

save(file = '/tmp/efficiency_map_1D', efficiency_map_1D)
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/_localjob/run_local_job_interpolate_maps.R', 'local_job_interpolate_maps', exportEnv = "")


# To load the previous run: load('/tmp/maps_nested_job')

# Perform the actual search with the estimated gain fields #
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/_localjob/run_local_job_search.R', 
                         'local_job_search', 
                         exportEnv = "search_results")

gc(reset = TRUE)

# Import/Transform/Store the search results
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/_localjob/local_job_import_search.R',
                         'local_job_import_search',
                         exportEnv = "")

load("/tmp/human_model_joined")

human_model_joined$a <- unlist(purrr::map(human_model_joined$data, function(x) {x$a %>% unique()}))
human_model_joined$b <- unlist(purrr::map(human_model_joined$data, function(x) {x$b %>% unique()}))
human_model_joined$c <- unlist(purrr::map(human_model_joined$data, function(x) {x$c %>% unique()}))
human_model_joined$d <- unlist(purrr::map(human_model_joined$data, function(x) {x$d %>% unique()}))
human_model_joined$g_min <- unlist(purrr::map(human_model_joined$data, function(x) {x$g_min %>% unique()}))
human_model_joined$g_max <- unlist(purrr::map(human_model_joined$data, function(x) {x$g_max %>% unique()}))

#human_model_joined <- human_model_joined %>% filter(!(a == 1 & b_2 != 1 & b_1 != 1))
#human_model_joined[human_model_joined$a == 1, "b_1"] <- 0


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

llr <- purrr::map2(human_model_joined$summarized_human, human_model_joined$summarized_model_optimal, function(x, y) {
    merged_df <- left_join(x, y, by = c("radius", "dist.group", "dist.group.click", "type"))
    merged_df$prop.x <- ifelse(merged_df$prop.x == 0, 1/2400, merged_df$prop.x)
    merged_df$prop.y <- ifelse(merged_df$prop.y == 0, 1/2400, merged_df$prop.y)
    merged_df$prop.y <- merged_df$prop.y / sum(merged_df$prop.y)
    merged_df$prop.x <- merged_df$prop.x / sum(merged_df$prop.x)

    llr <- -sum(merged_df$prop.x * log(merged_df$prop.y))
    })

human_model_joined$llr <- unlist(llr)

human_model_joined$label[1:4] <- "best_fit"
human_model_joined$label[c(5,6,10,11)] <- "optimal"
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
    mutate(sp_const = label)
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
