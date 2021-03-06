% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimize_fit_map.R
\name{optimize_map}
\alias{optimize_map}
\title{The main control function for finding an optimal map. Either matched to humans or maximize accuracy.}
\usage{
optimize_map(
  efficiency,
  prior_type,
  params_detection,
  lower_bound = list(c(0.001, 0.001, 0.001, 0.001, 0.6, 1, 0.65)),
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
  cl = NULL
)
}
\arguments{
\item{cl}{}
}
\value{

}
\description{
The main control function for finding an optimal map. Either matched to humans or maximize accuracy.
}
