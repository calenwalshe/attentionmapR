library(tidyverse)

#load(file = '/tmp/best_fits')
map(list.files('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/attentionmapR/R/', full.names = T), source)

# Anqi
global_start <- new.env()
global_start$start_params <- best_fit_anqi_uniform$optim_results$member$pop
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_anqi_uniform.R', 'best_fit_anqi_uniform',
                         exportEnv = "best_fit_anqi_uniform",
                         importEnv = global_start)
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), best_fit_anqi_uniform)

global_start$start_params <- best_fit_anqi_polar$optim_results$member$pop
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_anqi_polar.R', 'best_fit_anqi_polar',
                         exportEnv = "best_fit_anqi_polar",
                         importEnv = global_start)
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), best_fit_anqi_polar)

# rcw
global_start$start_params <- rbind(best_fit_rcw_uniform$optim_results$member$pop,best_fit_rcw_uniform$optim_results$member$pop)
#global_start$start_params[,7] <- NULL
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_rcw_uniform.R',
                         'best_fit_rcw_uniform',
                         exportEnv = "best_fit_rcw_uniform",
                         importEnv = global_start)
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), best_fit_rcw_uniform)

global_start$start_params <- NULL
#global_start$start_params[,7] <- .79
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_rcw_polar.R', 'best_fit_rcw_polar',
                         exportEnv = "best_fit_rcw_polar",
                         importEnv = global_start)
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), best_fit_rcw_polar)

# can uniform
global_start <- new.env()
global_start$start_params <- best_fit_can_uniform$optim_results$member$pop
#best_fit_can_uniform_script <- system.file("scripts", "fitting_scripts", "best_fit_can_uniform.R", package = "attentionmapsR")
best_fit_can_uniform_script <- './inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_can_uniform.R'
rstudioapi::jobRunScript(best_fit_can_uniform_script, 'best_fit_can_uniform',
                         exportEnv = "best_fit_can_uniform",
                         importEnv = global_start)

file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/uniformfinalresult', file_code),
     best_fit_can_uniform)

# can polar
global_start              <- new.env()
global_start$start_params <- best_fit_can_polar$optim_results$member$pop
#best_fit_can_polar <- system.file("scripts", "fitting_scripts", "best_fit_can_polar.R", package = "attentionmapsR")
best_fit_can_polar_script <- './inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_can_polar.R'
rstudioapi::jobRunScript(best_fit_can_polar_script, 'best_fit_can_polar',
                         exportEnv = "best_fit_can_polar",
                         importEnv = global_start)
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/polarfinalresult', file_code), best_fit_can_polar)

## arw Uniform
global_start              <- new.env()
global_start$start_params <- best_fit_arw_uniform$optim_results$member$pop
#best_fit_can_uniform_script <- system.file("scripts", "fitting_scripts", "best_fit_can_uniform.R", package = "attentionmapsR")
best_fit_arw_uniform_script <- './inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_arw_uniform.R'
rstudioapi::jobRunScript(best_fit_arw_uniform_script, 'best_fit_arw_uniform',
                         exportEnv = "best_fit_arw_uniform",
                         importEnv = global_start)

file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/uniformfinalresult', file_code),
     best_fit_arw_uniform)

# arw polar
global_start <- new.env()
global_start$start_params <- best_fit_arw_polar$optim_results$member$pop
#best_fit_can_polar <- system.file("scripts", "fitting_scripts", "best_fit_can_polar.R", package = "attentionmapsR")
best_fit_arw_polar_script <- './inst/scripts/fitting_scripts/subject_fit_scripts/best_fit_arw_polar.R'
rstudioapi::jobRunScript(best_fit_arw_polar_script, 'best_fit_arw_polar',
                         exportEnv = "best_fit_arw_polar",
                         importEnv = global_start)

file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/polarfinalresult', file_code), best_fit_arw_polar)

## Comment out.
best_fits <- NULL
best_fits$rcw_polar    <- best_fit_rcw_polar
best_fits$rcw_uniform  <- best_fit_rcw_uniform
best_fits$anqi_polar   <- best_fit_anqi_polar
best_fits$anqi_uniform <- best_fit_anqi_uniform
best_fits$can_polar    <- best_fit_can_polar
best_fits$can_uniform  <- best_fit_can_uniform
best_fits$arw_polar    <- best_fit_arw_polar
best_fits$arw_uniform  <- best_fit_arw_uniform
best_fits$optimal_arw_uniform <- optimal_arw_uniform
best_fits$optimal_arw_polar <- optimal_arw_polar
best_fits$optimal_can_uniform <- optimal_can_uniform
best_fits$optimal_can_polar <- optimal_can_polar


best_fits$rcw_uniform_max   <- all_fits[[16]]$optim_results$optim
best_fits$rcw_polar_concave <- all_fits$rcw_polar_concave

all_fits_format                     <- purrr::map(best_fits, attentionmapsR::format_optimizer_results)
all_fits_format$rcw_polar$label     <- "fit"
all_fits_format$rcw_uniform$label   <- "fit"
all_fits_format$anqi_polar$label    <- "fit"
all_fits_format$anqi_uniform$label  <- "fit"
all_fits_format$can_polar$label    <- "fit"
all_fits_format$can_uniform$label  <- "fit"
all_fits_format$arw_polar$label    <- "fit"
all_fits_format$arw_uniform$label  <- "fit"

all_fits_format$flat_rcw_uniform$label  <- "flat"
all_fits_format$flat_rcw_polar$label    <- "flat"
all_fits_format$flat_anqi_uniform$label <- "flat"
all_fits_format$flat_anqi_polar$label   <- "flat"

all_fits_format$optimal_rcw_polar$label     <- "opt"
all_fits_format$optimal_rcw_uniform$label   <- "opt"
all_fits_format$optimal_anqi_polar$label    <- "opt"
all_fits_format$optimal_anqi_uniform$label  <- "opt"
all_fits_format$optimal_arw_polar$label    <- "opt"
all_fits_format$optimal_arw_uniform$label  <- "opt"
all_fits_format$optimal_can_polar$label    <- "opt"
all_fits_format$optimal_can_uniform$label  <- "opt"


all_fits_format$rcw_uniform_max$label <- "max_acc_unif"
all_fits_format$rcw_polar_bump$label  <- "max_acc_bump"
all_fits_format$rcw_polar_concave$label <- "max_acc_concave"

efficiency_map_1D <- do.call(rbind, all_fits_format)

## Comment out
# Rcw
#global_start$start_params <- best_fit_rcw_polar$optim_results$member$pop
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/flat_rcw_polar.R', 'flat_rcw_polar',
                         exportEnv = "flat_rcw_polar")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), flat_rcw_polar)

rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/flat_rcw_uniform.R', 'flat_rcw_uniform',
                         exportEnv = "flat_rcw_uniform")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), flat_rcw_uniform)

rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/optimal_rcw_polar.R', 'optimal_rcw_polar',
                         exportEnv = "optimal_rcw_polar")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_rcw_polar)

rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/optimal_rcw_uniform.R', 'optimal_rcw_uniform',
                         exportEnv = "optimal_rcw_uniform")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_rcw_uniform)


## Anqi
#global_start$start_params <- best_fit_anqi_polar$optim_results$member$pop
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/flat_anqi_polar.R', 'flat_anqi_polar',
                         exportEnv = "flat_anqi_polar")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), flat_anqi_polar)

rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/flat_anqi_uniform.R', 'flat_anqi_uniform',
                         exportEnv = "flat_anqi_uniform")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), flat_anqi_uniform)

rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/optimal_anqi_polar.R', 'optimal_anqi_polar',
                         exportEnv = "optimal_anqi_polar")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_anqi_polar)

rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/optimal_anqi_uniform.R', 'optimal_anqi_uniform',
                         exportEnv = "optimal_anqi_uniform")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_anqi_uniform)

## Arw
global_start <- new.env()
global_start$start_params <- NULL
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/optimal_arw_polar.R', 'optimal_arw_polar',
                         exportEnv = "optimal_arw_polar")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_arw_polar)

rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/optimal_arw_uniform.R', 'optimal_arw_uniform',
                         exportEnv = "optimal_arw_uniform")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_arw_uniform)

## Can
global_start <- new.env()
global_start$start_params <- NULL
rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/optimal_can_polar.R', 'optimal_can_polar',
                         exportEnv = "optimal_can_polar")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_can_polar)

rstudioapi::jobRunScript('./inst/scripts/fitting_scripts/optimal_can_uniform.R', 'optimal_can_uniform',
                         exportEnv = "optimal_can_uniform")
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), optimal_can_uniform)

