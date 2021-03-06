# Uniform
global_start <- new.env()
global_start$start_params[, 2] <- max_accuracy_rcw_uniform$optim_results$optim$bestmem[2]
#global_start$start_params[,6] <- 1
#global_start$start_params <- rbind(max_acc_rcw_uniform$optim_results$member$pop,max_acc_rcw_uniform$optim_results$member$pop)
global_start$start_params <- NULL
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/maximum_accuracy_uniform_rcw.R', 'max_accuracy_rcw_uniform', 
                         exportEnv = "max_accuracy_rcw_uniform", importEnv = global_start)

file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), max_accuracy_rcw_uniform)

#Bump
global_start <- new.env()
global_start$start_params <- max_accuracy_rcw_polar_bump$optim_results$member$pop
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/maximum_accuracy_polar_bump_rcw.R', 'max_accuracy_rcw_polar_bump', 
  exportEnv = "max_accuracy_rcw_polar_bump", importEnv = global_start)
  
file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), max_accuracy_rcw_polar_bump)

# Decreasing
global_start <- new.env()
global_start$start_params <- max_accuracy_rcw_polar_concave$optim_results$member$pop
#global_start$start_params[,7] <- .777
rstudioapi::jobRunScript('~/Dropbox/Calen/Work/search/modeling/_analysis/_code/gain_map/maximum_accuracy_polar_rcw.R', 'max_accuracy_rcw_polar_concave', 
                         exportEnv = "max_accuracy_rcw_polar_concave", 
                         importEnv = global_start)

file_code <- stringi::stri_rand_strings(1, 16)
save(file = paste0('/tmp/', file_code), max_accuracy_rcw_polar_concave)
