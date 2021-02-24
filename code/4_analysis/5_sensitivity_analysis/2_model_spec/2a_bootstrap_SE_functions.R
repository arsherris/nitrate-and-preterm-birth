## bootstrapped standard errors functions
library(data.table)


# get standard errors by bootstrap 
bootstrap_output <- function(data, model, n_iters, outcome, exposure, carryover)  {
  
  
  iters_store <- tibble(beta_med = numeric(n_iters),
                        beta_high = numeric(n_iters))
  
  for(i in 1:n_iters) {
    
      Sys.time()
      print(paste("iteration", i))
   
      #set.seed(i)
      
      # generate bootstrap sample
      setDT(data, key = "mat_id")
      boot_sample <- data[.(sample(unique(mat_id), replace = T))]
      
      # run model with bootstrap sample
      mod_iter <- broom::tidy(model(data = boot_sample, outcome = outcome, exposure, carryover))
      
      # store model output
      iters_store$beta_med[i] <- mod_iter$estimate[mod_iter$term == "data[[exposure]]medium"]
      iters_store$se_med[i] <- mod_iter$std.error[mod_iter$term == "data[[exposure]]medium"]
      iters_store$beta_high[i] <- mod_iter$estimate[mod_iter$term == "data[[exposure]]high"]
      iters_store$se_high[i] <- mod_iter$std.error[mod_iter$term == "data[[exposure]]high"]
      
  }
  
  return(iters_store)

}



# Results table for mixed source model - bootstrapped errors

results_table_boot <- function(data, model, n_iters) {
  
  # define exposure and outcomes
  exposure <- "exp_cat"
  outcomes <- c("prem_20_to_31", "prem_32_to_36")
  
  # set referent level as "low" exposure
  data[[exposure]] <- factor(data[[exposure]], levels = c("low", "medium", "high"))
  data[[carryover]] <- factor(data[[carryover]], levels = c("low", "medium", "high"))
  
  # initialize results tables
  res_tbl_med <- tibble(outcome = outcomes,
                        exposure_cat = "Medium",
                        estimate = double(2),
                        boot_estimate =double(2),
                        lower = double(2),
                        upper = double(2))
  
  res_tbl_high <- tibble(outcome = outcomes,
                         exposure_cat = "High",
                         estimate = double(2),
                         boot_estimate =double(2),
                         lower = double(2),
                         upper = double(2))
  
  
  # loop through functions for each outcome
  for (i in 1:2) {
    
    mod <- model(data, outcomes[i], exposure, carryover)
    beta_mod <- fixef(mod) 
  
    # generate bootstrapped estimates
    se_boot_store <- bootstrap_output(data, model, n_iters, outcomes[i], exposure, carryover)
    # se_med = sd(se_boot_store$beta_med)
    # se_high = sd(se_boot_store$beta_high)
    
    beta_boot_med <- mean(se_boot_store$beta_med)
    lower_med <- quantile(se_boot_store$beta_med, seq(0, 1, .025))[2]
    upper_med <- quantile(se_boot_store$beta_med, seq(0, 1, .025))[40]
    
    
    beta_boot_high <- mean(se_boot_store$beta_high)
    lower_high <- quantile(se_boot_store$beta_high, seq(0, 1, .025))[2]
    upper_high <- quantile(se_boot_store$beta_high, seq(0, 1, .025))[40]
    
    res_tbl_med$estimate[i] <- exp(beta_mod[2])
    res_tbl_med$boot_estimate[i] <- exp(beta_boot_med)
    res_tbl_med$lower[i] <- exp(lower_med)
    res_tbl_med$upper[i] <- exp(upper_med)

    res_tbl_high$estimate[i] <- exp(beta_mod[3])
    res_tbl_high$boot_estimate[i] <- exp(beta_boot_high)
    res_tbl_high$lower[i] <- exp(lower_high)
    res_tbl_high$upper[i] <- exp(upper_high)

  }
  
  
  # combine and return results tables
  res_tbl <- rbind(res_tbl_med, res_tbl_high)
  
  return(res_tbl)
}

