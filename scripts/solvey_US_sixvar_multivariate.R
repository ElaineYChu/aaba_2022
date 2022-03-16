# Six of the response variables in the Subadult Virtual Anthropology Database
# (SVAD) are used. The response variables are:
#
# HME_EF - Humerus medial epicondyle epiphyseal fusion
# TC_Oss - Tarsal count
# max_M1 - Maxillary first molar development (permanent)
# man_I2 - Mandibular second incisor development (permanent)
# FDL    - Femur diaphyseal length
# RDL    - Radius diaphyseal length

# Load libraries
library(yada)
library(doParallel)

# Clear the workspace
rm(list=ls())

# Check that a models folder exists in the working directory
if(! ("models" %in% dir()) ) {
  stop("There is no 'models' folder in the working directory")
}

# Use all available cores for parallel processing
registerDoParallel(detectCores())

# The data directory is /models
data_dir <- "models"
# The "ID" that uniquely identifies this analysis:
analysis_name <- "US_sixvar"

# A wrapper to define the problem and do the fit. The wrapper works for either
# the main problem or a fold.
multivariate_fit_wrapper <- function(data_dir,
                                     analysis_name,
                                     fold=NA) {
  # A save file to track the progress of the optimization with dfOptim::hjk
  hjk_progress_file <- build_file_path(data_dir, 
                                       analysis_name, 
                                       "hjk_progress", 
                                       fold=fold)
  # A save file for the final result
  optim_save_file <- build_file_path(data_dir, 
                                     analysis_name,
                                     "cdep_model", 
                                     fold=fold)
  if (is.na(fold)) {
    problem_file <- build_file_path(data_dir,
                                    analysis_name,
                                    "main_problem")
  } else {
    problem_file <- build_file_path(data_dir,
                                    analysis_name,
                                    "training_problem",
                                    fold=fold)
  }

  # The following conditionally independent model is generated using the main
  # problem or folds.
  cindep_model <- build_cindep_model(data_dir,
                                     analysis_name,
                                     fold=fold,
                                     calc_se=T,
                                     save_file=T,
                                     allow_corner=T)

  problem <- readRDS(problem_file)
  mod_spec <- cindep_model$mod_spec
  # Use four correlation groups -- this may be edited by the user
  # Group 1	EF variables
  # Group 2	Oss variables
  # Group 3	Dental variables
  # Group 4	Continuous variables
  J <- get_J(mod_spec)
  K <- get_K(mod_spec)

  ind_EF   <- which(  endsWith(problem$var_names,'EF' ))
  ind_Oss  <- which(  endsWith(problem$var_names,'Oss'))
  ind_Dent <- which(startsWith(problem$var_names,'ma'))
  ind_LB   <- J + (1:K)

  cdep_groups <- rep(NA,J+K)
  cdep_groups[ind_EF]   <- 1
  cdep_groups[ind_Oss]  <- 2
  cdep_groups[ind_Dent] <- 3
  cdep_groups[ind_LB]   <- 4

  mod_spec$cdep_groups <- cdep_groups
  mod_spec$cdep_spec = "dep"

  ## Fit the conditionally dependent multivariate model
  cdep_model <- fit_multivariate(problem$x,
                              problem$Y,
                              mod_spec,
                              cindep_model,
                              save_file=hjk_progress_file,
                              hjk_control=list(info=T))

  output <- list(th_y=cdep_model$th_y,
                 mod_spec=cdep_model$mod_spec,
                 th_y_bar=cdep_model$th_y_bar,
                 hjk_output=cdep_model$hjk_output)
  saveRDS(output, optim_save_file)
  return(output)
}

# Solve the main problem
main_fit <- multivariate_fit_wrapper(data_dir,analysis_name)

fold_fit_list <- list()
num_folds <- get_num_training_problems(data_dir,analysis_name)
for (fold in 1:num_folds) {
 fold_fit_list[[fold]] <- multivariate_fit_wrapper(data_dir,analysis_name,fold)
}

# Stop clusters from parallel processing
stopImplicitCluster()
