# Load libraries
library(yada)

# Clear the workspace
rm(list=ls())

# Check that a models folder exists in the working directory
if(! ("models" %in% dir()) ) {
  stop("There is no 'models' folder in the working directory")
}

# Use all available cores for parallel processing
# registerDoParallel(detectCores(logical=FALSE))

# The data directory is /models
data_dir <- "models"

# The "ID" that uniquely identifies this analysis:
analysis_name <- "US_sixvar"

# Build the ordinal and continuous model vectors
mean_ord <- c("pow_law_ord","log_ord","lin_ord")  # 3 mean specification options
mean_cont <- c("pow_law")  # 1 mean specification option
noise <- c("const","lin_pos_int")  # 2 noise specification options

ord_models <- build_model_vec(mean_ord, noise)
cont_models <- build_model_vec(mean_cont, noise)

# Call evaluate_univariate_models to do the cross validation. 
eval_data <- evaluate_univariate_models(data_dir, analysis_name, 
                                        eval_type="cv",
                                        ord_models, cont_models,
                                        cand_tol=.05,
                                        scale_exp_min=.01,
                                        beta2_max=5)
                                      
# Write a cross validation report for each ordinal variable, which is stored
# in aaba_2022/models
for(j in 1:length(eval_data$mod_select_ord)) {
  write_ordinal_report(data_dir, analysis_name, j, line_width=200)
}

# Write a cross validation report for each continuous variable, which is stored
# in aaba_2022/models
for(k in 1:length(eval_data$mod_select_cont)) {
  write_continuous_report(data_dir, analysis_name, k, line_width=200)
}

# Extract best univariate parameters for each response variable into a dataframe.
# Dataframe is stored as aaba_2022/models/US_sixvar_univariate_model_parameters.rds
get_best_univariate_params(data_dir, analysis_name, save_file=TRUE)
