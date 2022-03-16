#############################################
##
##       AABA 2022 Functions
##
#############################################

## Remove individuals with all NA
idx_all_na <- function(df, columns) {
     tmp_df <- df[columns]  # create temporary df
     
     idx <- which(rowSums(tmp_df, na.rm=TRUE)==0)  # rows that have all NA
     
     if(length(idx)==0) {  # if there are none, recode idx as NA
          idx=NA
     }
     
     return(idx)
}

## Univariate Model Posterior Age Prediction
mcp_univariate_pred <- function(data_dir, analysis_name, 
                                test_samp, response_cols, 
                                id_col, age_col,
                                xcalc, seed, save_file=T) {
  
  ## Initialize empty data frame with:
  ## SVAD_identifier, location, known_age, point_estimate, lower95, upper95
  df0 <- as.data.frame(matrix(data=NA, nrow=nrow(test_samp), ncol=5))
  
  # Load problem file for current analysis
  problem <- readRDS(build_file_path(data_dir, analysis_name, 'main_problem'))
  
  # Calculate parameterization for prior on x (age)
  set.seed(seed)
  weib_offset <- 0.002
  weib_fit <- mixtools::weibullRMM_SEM(problem$x + weib_offset,
                                       k=3,
                                       maxit=2000)
  th_x <- list(fit_type='offset_weib_mix',
               fit=weib_fit,
               weib_offset=weib_offset)
  
  ## Loop through available response variables in test sample
  for(i in response_cols) {
    var_name <- colnames(test_samp)[i]
    best_model <- tryCatch({
      load_best_univariate_model(data_dir, analysis_name, var_name=var_name)},
      error=function(cond) {
      return(NA)
    })
    
    if (length(best_model) != 2) {
      print(paste0("Best model not selected for ",var_name))
      next
    }
    
    print(paste0("---Starting batch processing for ",var_name,"---"))
    
    ## Initialize best model parameters
    th_y <- best_model$th_y
    mod_spec <- best_model$mod_spec
    
    current_col <- test_samp[[i]]  # vector of current variable values
    
    ## Loop through values in current_col and estimate point and CI
    for (n in 1:length(current_col)) {
      current_val <- current_col[[n]]
      df0[n,1] <- test_samp[[n,id_col]]
      df0[n,2] <- test_samp[[n,age_col]]
      print(paste0("Calculating for individual ",test_samp[[n,id_col]]))
      
      ## Check if current_val is NA
      if (is.na(current_val)) {
        df0[n,3] <- NA
        df0[n,4] <- NA
        df0[n,5] <- NA
        next
      }
      
      ## Calculate posterior density, point estimate, and confidence interval
      x_post <- calc_x_posterior(current_val, th_x, th_y, 
                                 mod_spec, xcalc, seed)
      x_analyze <- analyze_x_posterior(x_post$x, x_post$density)
      df0[n,3] <- round(x_analyze$xmean,2)
      df0[n,4] <- round(x_analyze$xlo,2)
      df0[n,5] <- round(x_analyze$xhi,2)

    }
    
    df <- df0
    colnames(df) <- c("SVAD_identifier","known_age","point_est",
                      "lower95","upper95")
    
    if(save_file) {
      write.csv(df, paste0("uni_results/",var_name,"_test_predictions.csv"),
                row.names=F)
    }
    
    print(paste0('Prediction Table for ',var_name))
    print(head(df))
  }
}



## Multivariate Model Posterior Age Prediction
mcp_multivariate_pred <- function(data_dir, analysis_name, 
                                  test_samp, id_col, 
                                  age_col, xcalc, seed, save_file=T) {
  
  # Load problem file for current analysis
  problem <- readRDS(build_file_path(data_dir, analysis_name,
                                     'main_problem'))

  
  # Calculate parameterization for prior on x (age)
  set.seed(seed)
  weib_offset <- 0.002
  weib_fit <- mixtools::weibullRMM_SEM(problem$x + weib_offset,
                                       k=3,
                                       maxit=2000)
  th_x <- list(fit_type='offset_weib_mix',
               fit=weib_fit,
               weib_offset=weib_offset)
  
  test_samp0 <- test_samp[c(1:3,which(names(test_samp) %in% problem$var_names))]
  idx <- idx_all_na(test_samp0,4:ncol(test_samp0))
  if (is.na(idx)) {
       test_samp1 <- test_samp0
  } else {
       test_samp1 <- test_samp0[-idx,]
  }
  
  # Extract response variables as matrix Y in order expected by multivariate model
  ef_vars <- grep("_EF|_Oss", names(test_samp1))
  dent_vars <- grep("man_|max_", names(test_samp1))
  lb_vars <- grep("DL|PB|MSB|DB", names(test_samp1))
  Y <- t(test_samp1[c(ef_vars, dent_vars, lb_vars)])
  if (!all(rownames(Y) == problem$var_names)) {
    stop("Problem with organizing response variables")
  }
  
  # Loop through process twice, one for cindep and one for cdep
  for (m in c('cindep_model','cdep_model')) {
    
    ## Initialize empty data frame with:
    ## SVAD_identifier, location, known_age, point_estimate, lower95, upper95
    df0 <- as.data.frame(matrix(data=NA, nrow=nrow(test_samp1), ncol=5))
    
    ## Load multivariate model
    model <- readRDS(build_file_path(data_dir, analysis_name, m))
    th_y <- model$th_y
    mod_spec <- model$mod_spec
    
    for(i in 1:ncol(Y)) {
         # if ((5 %% i) == 5) {
         #      print(paste0('Calculating age posterior for id=',test_samp1[i,id_col]))
         # }
      
      print(paste0('Calculating age posterior for id=',test_samp1[i,id_col]))

      df0[i,1] <- test_samp1[i,id_col]
      df0[i,2] <- test_samp1[i,age_col]
      
      x_post <- calc_x_posterior(Y[,i], th_x, th_y, mod_spec, xcalc)
      x_analyze <- analyze_x_posterior(x_post$x, x_post$density)
      
      df0[i,3] <- round(x_analyze$xmean,2)
      df0[i,4] <- round(x_analyze$xlo,2)
      df0[i,5] <- round(x_analyze$xhi,2)
      
      write.csv(df0,paste0('results/temp_',analysis_name,'_',m,
                         '_test_predictions.csv'), row.names=F)
    }
    
    df <- df0
    colnames(df) <- c('SVAD_identifier','known_age',
                      'point_est','lower95','upper95')
    
    if (save_file) {
      write.csv(df, paste0('multi_results/',analysis_name,'_',m,
                           '_test_predictions.csv'), row.names=F)
    }
    print(paste0('Prediction Table for ',analysis_name,'-',m))
    print(head(df))
    
  }
}

## Calculate Model Performance
calc_perf <- function(data_dir, file_name, type) {
  df <- read.csv(paste0(data_dir,"/",file_name))
  
  if (type=="%accuracy") {  # calculate prediction accuracy
    df_new <- df %>% mutate(accurate=ifelse(known_age <= upper95 & 
                                              known_age >= lower95,
                                            T,F))
    n_samp <- df_new %>% drop_na() %>% nrow()
    accuracy <- length(which(df_new$accurate==T))/n_samp
    out <- accuracy
  }
  
  if (type=="rmse") {
    rmse <- RMSE(df$point_est, df$known_age, na.rm=TRUE)
    out <- rmse
  }
  
  if (type=="SEE") {
    see <- sd(df$resid, na.rm=TRUE)
    out <- paste0("+/- ",round(see*1.96,2))
  }
  
  return(out)
}


