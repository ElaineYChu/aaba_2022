# Load libraries
library(yada)

# Clear the workspace
rm(list=ls())

# Check that aaba_2022 is the working directory
if(grepl('aaba_2022',getwd())==FALSE) {
  stop(paste0("Please check that your working directory is the ",
              "cloned directory 'aaba_2022'"))
}

# Load the variable information file. var_info is a data frame where rows are
# variables and columns are variable specifications.
var_info <-  yada::load_var_info('data/US_var_info.csv')
data_file <- 'data/SVAD_US.csv'
cp_data <- load_cp_data(data_file, var_info)

# Extract the main problem (that is, not a cross-validation fold) from cp_data,
# then save it to aaba_2022/models.
data_dir <- "models"
analysis_name <- "US_sixvar"
main_problem <- cp_data$problem
save_problem(data_dir, analysis_name, main_problem)

# Generate four cross-validation problems. Use a random number seed (from
# random.org) for reproducibility.
# 4 training and 4 test folds in a list format
cv_problems <- generate_cv_problems(main_problem, K=4, seed=234227327)

# Save the cross-validation problems to file.
# NOTE: is_folds=T here
save_problem(data_dir, analysis_name, cv_problems, is_folds=T)
