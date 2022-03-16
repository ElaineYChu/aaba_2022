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
analysis_name <- 'US_sixvar'

# Load the main problem from file
problem0 <- readRDS(build_file_path(data_dir, analysis_name, "main_problem"))

# Build ordinal problems (main problem and cross-validation folds)
ord_prob_list <- build_univariate_ord_problems(data_dir,
                                               analysis_name,
                                               add_folds=TRUE)

# From random.org between 1 and 1,000,000
base_seed <- 264528
set.seed(base_seed)
seed_vect <- sample.int(1000000, length(ord_prob_list), replace=F)

# Solve the ordinal problems in parallel and save to user-defined directory
ord_success <-
  foreach::foreach(i=1:length(ord_prob_list), .combine=cbind) %dopar% {
    yada::solve_ord_problem(data_dir,
                            analysis_name,
                            ord_prob_list[[i]],
                            anneal_seed=seed_vect[i])
  }

# Build continuous problems (main problem and cross-validation folds)
cont_prob_list <- build_univariate_cont_problems(data_dir,
                                                 analysis_name,
                                                 add_folds=TRUE)

# Solve the continuous problems in parallel and save to user-defined directory
cont_success <-
  foreach::foreach(i=1:length(cont_prob_list), .combine=cbind) %dopar% {
    yada::solve_cont_problem(data_dir, analysis_name, cont_prob_list[[i]])
  }

# Stop clusters from parallel processing
stopImplicitCluster()
