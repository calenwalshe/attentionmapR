#' The main control function for finding an optimal map. Either matched to humans or maximize accuracy.
#'
#' @param efficiency
#' @param prior_type
#' @param params_detection
#' @param neural_resource
#' @param n_parallel
#' @param human_data
#' @param itermax
#' @param n_trials
#' @param NP
#' @param start_params
#' @param subject_fit
#' @param store_pop
#' @param lower_bound
#' @param upper_bound
#' @param seed_val
#' @param single_thread
#' @param cl
#'
#' @return
#' @export
#'
#' @examples
optimize_map <- function(efficiency,
                         prior_type,
                         params_detection,
                         lower_bound = list(c(.001, .001, .001, .001, .6, 1, .65)),
                         upper_bound = list(c(4.5, 5, 4.5, 5, 1, 1, 1)),
                         seed_val = 1,
                         neural_resource,
                         n_parallel = 1,
                         human_data,
                         itermax = 2,
                         n_trials = 2,
                         NP = 2,
                         start_params = NULL,
                         subject_fit = F,
                         store_pop = NULL,
                         single_thread = FALSE,
                         cl = NULL) {
  # A wrapper function for the objective function.
  # Only required because of lack in consistency between
  # how different optimization package handle input variables.
  # This wrapper is designed for use with DEoptim.
  wrapper_full_objective <- function(x) {
    (return({
      full_objective(
        a = x[1],
        b = x[2],
        c = x[3],
        d = x[4],
        g_min = x[5],
        g_max = x[6],
        efficiency = x[7]
      )
    }))
    #return(Inf) # If there is a failure, return Inf. DEOptim handles this.
  }

  if (is.null(cl)) {
    cl <- parallel::makeCluster(1)
  } # not sure if necessary but ensures the code will run asis even when no cluster is supplied.

  parallel::clusterExport(
    cl = cl,
    varlist = c(
      "wrapper_full_objective",
      "n_trials",
      "single_thread",
      "efficiency",
      "prior_type",
      "params_detection",
      "neural_resource",
      "full_objective",
      "f.optim",
      "subject_fit",
      "f.growth",
      "human_data",
      "estimate_candidate_gain_map",
      "seed_val",
      "store_pop"
    ),
    envir = environment()
  )

  parallel::clusterEvalQ(cl = cl, library(searchR))
  parallel::clusterEvalQ(cl = cl, library(tidyverse))
  parallel::clusterEvalQ(cl = cl, library(humansearchdata))
  parallel::clusterEvalQ(cl = cl, library(humandetectiondata))

  lower_vals <- lower_bound[[1]]
  upper_vals <- upper_bound[[1]]
  if (!is.null(efficiency)) {
    lower_vals[7] <- efficiency
    upper_vals[7] <- efficiency
  }

  optim_results <- DEoptim::DEoptim(
    wrapper_full_objective,
    lower = lower_vals,
    upper = upper_vals,
    DEoptim::DEoptim.control(
      cluster = cl,
      itermax = itermax,
      NP = NP,
      storepopfrom = 0,
      initialpop = start_params,
      CR = 1
    )
  )
  return(optim_results)
}
#' Create a single map and save it.
#'
#' @param a
#' @param b_2
#' @param c
#' @param efficiency
#' @param prior_type
#' @param params_detection
#' @param neural_resource
#'
#' @return
#' @export
#'
#' @examples
get_single_gain_map <-
  function(a,
           b_2,
           c,
           efficiency,
           prior_type,
           params_detection,
           neural_resource) {
    data = data.frame(c_deg = neural_resource$eccentricity, y.scale = y.scale)

    my_map <- searchR::fit_maps(list(data))

    file_id <-
      paste0('/main/calen/', stringi::stri_rand_strings(1, 16), '.mat')

    searchR::store_maps_disk(file_id, my_map)

    return(file_id)

  }

#' Find a map that satisfies the total efficiency gain relationship.
#'
#' @param efficiency
#' @param prior_type
#' @param params_detection
#' @param neural_resource
#' @param a
#' @param b_2
#' @param cp
#'
#' @return
#' @export
#'
#' @examples
estimate_candidate_gain_map <-
  function(efficiency,
           prior_type,
           params_detection,
           neural_resource,
           g_min,
           g_max,
           a,
           b,
           c,
           d) {
    total_gain <- efficiency
    prior_type <- prior_type
    params_detection_parsed <-
      as.numeric(stringr::str_extract_all(params_detection, "\\..[:number:]+|[:number:]")[[1]])
    neural_resource <- neural_resource

    G <- efficiency

    f.optim <- f.optim
    f.growth <- f.growth

    if (a == 0) {
      f <-
        purrr::partial(
          f.optim,
          g_min = g_min,
          g_max = g_max,
          a = a,
          b = b,
          d = d,
          efficiency = efficiency,
          neural_resource = neural_resource,
          G = G
        )
    } else {
      f <-
        purrr::partial(
          f.optim,
          g_min = g_min,
          g_max = g_max,
          b = b,
          d = d,
          c = c,
          efficiency = efficiency,
          neural_resource = neural_resource,
          G = G
        )
    }

    free_var_hat_optim <- DEoptim::DEoptim(
      f,
      lower = 0,
      upper = 30,
      DEoptim::DEoptim.control(
        reltol = 1e-8,
        trace = 50,
        steptol = 100,
        VTR = .001
      )
    )
    free_var_hat <- free_var_hat_optim$optim$bestmem

    if (a == 0) {
      c <- free_var_hat
    } else {
      a <- free_var_hat
    }

    didconverge <- (free_var_hat_optim$optim$bestval < .001)

    dp_vals <- humandetectiondata::f.dprime.no.uni(8,
                                                   neural_resource$eccentricity * 120,
                                                   0,
                                                   1,
                                                   p_kSp = params_detection_parsed[2],
                                                   p_k0 = params_detection_parsed[1]) # compute d' on the rings for the no uncertainty case.

    models <-
      list(
        y.scale = f.growth(
          g_min = g_min,
          g_max = g_max,
          a = a,
          b = b,
          c = c,
          d = d,
          eccentricity = neural_resource$eccentricity
        ),
        eccentricity = neural_resource$eccentricity,
        y.dprime = dp_vals,
        g_min = g_min,
        g_max = g_max,
        a = a,
        b = b,
        c = c,
        d = d,
        didconverge = didconverge
      )
    return(models)
  }

#' Weibull growth. Customized to have a front and back end.
#' See paper Walshe & Geisler (2021) for details
#' @param a
#' @param b_1
#' @param b_2
#' @param c_1
#' @param c_2
#'
#' @return
#' @export
#'
#' @examples
f.growth <- function(g_min, g_max, a, b, c, d, eccentricity) {
  x    <- eccentricity
  xmax <- 8
  if (a == 0) {
    val <- g_min + (g_max - g_min) * (1 - exp(-((xmax - x) / c) ^ d))
  } else if (c == 0) {
    val <- g_min + (g_max - g_min) * (1 - exp(-(x / a) ^ b))
  } else {
    fl <- g_min + (g_max - g_min) * (1 - exp(-(x / a) ^ b))
    fh <- g_min + (g_max - g_min) * (1 - exp(-((xmax - x) / c) ^ d))
    val <- apply(cbind(fl, fh), 1, min)
  }

  return(val)
}

#' The objective to ensure the gain map satisfies the resource constraint.
#' @param a
#' @param b_1
#' @param b_2
#' @param c
#'
#' @return
#' @export
#'
#' @examples
f.optim <-
  function(g_min,
           g_max,
           a,
           b,
           c,
           d,
           efficiency,
           neural_resource,
           G) {
    gain <-
      f.growth(
        g_min = g_min,
        g_max = g_max,
        a = a,
        b = b,
        c = c,
        d = d,
        eccentricity = neural_resource$eccentricity
      )
    neural_response <- neural_resource$neural_resource * gain # neural_resource is precomputed.

    nrs   <- sum(neural_response)
    G_tot <- G * sum(neural_resource$neural_resource)

    error <- sqrt(sum((G_tot - nrs)^2)) # RSSE (most standard error metrics should work)
    if (nrs > G) {
      error <- 2 * error # penalty for gain that are above G. We want to get as close as possible *below* G.
    }
    return(error)
  }

#' Create the objective function used to solve the map.
#'
#' @param a
#' @param b
#' @param c
#' @param d
#' @param g_min
#' @param g_max
#' @param efficiency
#'
#' @return
#'
#'
#' @export
#' @examples
full_objective <- function(a, b, c, d, g_min, g_max, efficiency) {
  if (prior_type == "polar") {
    prior_code <- 2
  } else {
    prior_code <- 1
  }

  # Step 1. Use the supplied parameters to estimate a candidate attention gain map.
  gain_map <-
    estimate_candidate_gain_map(
      efficiency,
      prior_type,
      params_detection,
      neural_resource,
      a = a,
      b = b,
      c = c,
      d = d,
      g_min = g_min,
      g_max = g_max
    )
  # If those parameters return no map satisfying the constraints produce an Inf.
  if (!gain_map$didconverge) {
    # If a map gain constraint not satisfied then fail
    return(Inf)
  }

  # package up the attention map
  attention_map_1D <-
    data.frame(c_deg = gain_map$eccentricity,
               y.scale = gain_map$y.scale)

  # turn the 1D attention map into a 2D attention field.
  my_map   <- searchR::fit_maps(list(attention_map_1D))
  file_id <-
    paste0('/tmp/', stringi::stri_rand_strings(1, 16), '.mat')
  searchR::store_maps_disk(file_id, my_map)

  ntrials <- n_trials
  radius <- 8
  contrast <- .175
  seed_val <- sample(1:100000, 1) # new random seed each time the code runs.


  # Step 2. Run the search with the new attention map.
  model_search <-
    searchR::run_single_search(
      file_id,
      efficiency,
      contrast,
      ntrials,
      radius,
      priorType = prior_code,
      search_params = params_detection,
      seed_val = seed_val,
      single_thread = single_thread
    ) # generic code to run the model. key concept is that the attention map is loaded into the model from disk at runtime via the "file_id" parameter.

  # Step 3. Transform the raw output from the search into a format used for a. maximum accuracy calculation b. maximum likelihood calculation.
  opt_crit <-
    model_search %>%
    mutate(offset = .5) %>%
    searchR::find_optimal_criterion() %>%
    .$optim %>% .$bestmem # optimal criterion search.

  model_search_summary <-
    model_search %>% searchR::import_model(., criterion = opt_crit) %>%
    humansearchdata::add_threshold(.) %>%
    humansearchdata::add_accuracy() %>%
    searchR::summary_search() # compute the performance of the model resulting from using the estimated attention map

  # Rare, but to avoid 0s (bad for maximum likelihood) we substitute the proportion with 1 (2 * num_trials_human)
  model_search_summary$prop <-
    ifelse(model_search_summary$prop == 0,
           1 / 9600,
           model_search_summary$prop)

  human_data$prop <-
    ifelse(human_data$prop == 0, 1 / 2400, human_data$prop)

  # Normalize so sum(prop) = 1
  model_search_summary$prop <- model_search_summary$prop / sum(model_search_summary$prop)
  human_data$prop           <- human_data$prop / sum(human_data$prop)

  # Formatting for maximum likelihood calculation. And for data storage.
  A <-
    merge(
      model_search_summary,
      human_data,
      by = c("radius", "dist.group", "dist.group.click", "type"),
      suffixes = c(".model", ".human")
    )
  A <-
    A %>% group_by(dist.group, type) %>% summarize(
      prop.model = sum(prop.model, na.rm = TRUE),
      prop.human = sum(prop.human, na.rm = TRUE)
    )

  gc(reset = TRUE) # some silliness to try and avoid crashes on the machine "lennon". don't think it is working.

  if (subject_fit) {
    error <- -sum(A$prop.human * log(A$prop.model)) # neg. log likelihood
  } else {
    error <-
      sum(A$prop.model[A$type == "fh" |
                         A$type == "miss" | A$type == "fa"]) # error
  }

  # formatting for storing intermediate data.
  if (!is.null(store_pop)) {
    results_list <- list(
      list(
        gain_map = gain_map,
        a = gain_map$a,
        b = gain_map$b,
        c = gain_map$c,
        d = gain_map$d,
        g_min = gain_map$g_min,
        g_max = gain_map$g_max,
        A = A,
        efficiency = efficiency,
        file_id = store_pop,
        seed_val = seed_val,
        error = error
      )
    )

    save(file = paste0(store_pop, stringi::stri_rand_strings(1, 16)),
         results_list)
  }
  return(error)

}
