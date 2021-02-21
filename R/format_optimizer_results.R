#' Supply an environment to the function. Format it back to something acceptable.
#' Note. This is a very custom function. It accepts an environment that is output from a estimation job.
#' This is a brittle function and should be used with caution if any changes to the optimization
#' procedure are made.
#' @param optimized_results
#'
#' @return
#' @export
#'
#' @examples
format_optimizer_results <- function(optimized_results) {
  sample_type <- optimized_results$human_search_nested$sample_type
  subject     <- optimized_results$human_search_nested$subject

  scale.dist <- optimized_results$human_data$radius %>% unique() %>% .[1] / 8 # a hack, but good. replace with something better?
  map_prior  <- sample_type
  prior_type <- sample_type
  contrast   <- .175 # only thing we are doing for now. change later.

  prior.scale   <- 1 # legacy
  n_rings       <- NA # legacy, not used.
  detect_params <- optimized_results$params_detection
  params        <- c(optimized_results$optim_results$optim$bestmem, optimized_results$optim_results$optim$bestval)
  
  best_node     <- purrr::map(optimized_results$full_step_results, function(x) {
    (x[[1]]$error == optimized_results$optim_results$optim$bestval)
  }) %>% 
  unlist() %>% 
  which()

  best_node <- best_node[1]
  a <- optimized_results$full_step_results[[best_node]][[1]]$gain_map$a
  b <- optimized_results$full_step_results[[best_node]][[1]]$gain_map$b
  c         <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$c
  d          <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$d
  g_min          <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$g_min
  g_max          <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$g_max
  efficiency  <-
    optimized_results$optim_results$optim$bestmem[7] #optimized_results$full_step_results[[best_node]][[1]]$gain_map$efficiency
  seed_val    <-
    optimized_results$full_step_results[[best_node]][[1]]$seed_val
  error       <-
    optimized_results$full_step_results[[best_node]][[1]]$error

  accuracy <-
    optimized_results$full_step_results[[best_node]][[1]]$A %>% filter(type %in% c("cr", "hit")) %>% .$prop.model %>% sum()

  y.scale <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$y.scale
  y.dprime <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$y.dprime
  c_deg    <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$eccentricity
  sp_const <- stringi::stri_rand_strings(1, 16)

  converged    <-
    optimized_results$full_step_results[[best_node]][[1]]$gain_map$didconverge

  format_optim <-
    data.frame(
      sample_type = sample_type,
      subject = subject,
      a = a,
      b = b,
      c = c,
      d,
      g_min = g_min,
      g_max = g_max,
      efficiency = efficiency,
      scale.dist = scale.dist,
      map_prior = map_prior,
      prior_type = prior_type,
      contrast = contrast,
      prior.scale = prior.scale,
      n_rings = n_rings,
      params = detect_params,
      y.scale = y.scale,
      y.dprime = y.dprime,
      c_deg = c_deg,
      sp_const = sp_const,
      seed_val = seed_val,
      error = error,
      accuracy = accuracy
    )

  return(format_optim)
}
