## FUNCTIONS FOR MODEL OUTPUT
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

## function: clean output of model with categorical exposure
  # input:
    # mod_object: survival or glmer model object
    # outcome: model outcome (as a string)
  # output: tbl_df with model results and the following columns
    # outcome: model outcome
    # exposure_cat: exposure category
    # n_preterm: number of preterm births with the specified gestational age outcome
    # n_term: number of term births
    # result: formatted result as string "OR (lower 95% CI, upper 95% CI)"
    # estimate: original model estimate
    # OR: odds ratio
    # lower: lower 95% Wald CI
    # upper: upper 95% Wald CI
    # exposure: study exposure 

clean_model_output <- function(mod_object, outcome) {
  
  # extract dataframe used for model (excludes missing data)
  mod_frame  <- model.frame(mod_object)
  
  # extract outcomes (will be used to calculate sample size per outcome and exposure category)
  if(data.class(mod_object) == "glmerMod") {
    mod_outcome <- mod_frame$`data[[outcome_var]]`
  } else {
    mod_outcome <- mod_object$y[,2]
  }
  
  # clean output
  tidy(mod_object) %>% 
    filter(grepl("exp", term)) %>% 
    mutate(exposure_cat = case_when(
      grepl("medium", term) ~ "medium",
      grepl("high", term) ~ "high")) %>% 
    mutate(
      OR = exp(estimate),
      lower = exp(estimate - 1.96*std.error),
      upper = exp(estimate + 1.96*std.error),
      result = paste(round(OR, 2), " (", round(lower, 2), 
                     ", ", round(upper, 2), ")", sep = "")) %>% 
    select(exposure_cat, estimate, OR:result) %>% 
    bind_rows(tibble(exposure_cat = "low",
                     estimate = NA, OR = NA, lower = NA, upper = NA,
                     result = "Ref")) %>% 
    rowwise() %>% 
    mutate(n_preterm = sum(mod_outcome[mod_frame$exp_cat == exposure_cat] == 1),
           n_term =    sum(mod_outcome[mod_frame$exp_cat == exposure_cat] == 0)) %>% 
    ungroup %>% 
    mutate(outcome = outcome,
           exposure = study_exposure,
           exposure_cat = factor(exposure_cat, 
                                 levels = c("low", "medium", "high"),
                                 labels = c("Low", "Medium", "High"))) %>%
    arrange(exposure_cat) %>% 
    select(outcome, exposure_cat, n_preterm, n_term, result, estimate:upper, exposure) %>% 
    ungroup %>% 
    return()
}


## function to clean output of model with continuous exposure
  # input and output parameters are the same as above, with the exception of the "exposure_cat" variable
  # OR is interpreted as change in odds with unit increase in concentration of nitrate

clean_model_output_conc <- function(mod_object, outcome) {
  
  # extract dataframe used for model (excludes missing data)
  mod_frame   <- model.frame(mod_object)
  
  # extract outcomes (will be used to calculate sample size per outcome and exposure category)
  if(data.class(mod_object) == "glmerMod") {
    mod_outcome <- mod_frame$`data[[outcome_var]]`
  } else {
    mod_outcome <- mod_object$y[,2]
  }
  
  # clean output
  tidy(mod_object) %>% 
    filter(grepl("conc", term)) %>% 
    mutate(OR = exp(estimate),
           lower = exp(estimate - 1.96*std.error),
           upper = exp(estimate + 1.96*std.error),
           result = paste(round(OR, 3), " (", round(lower, 3), 
                          ", ", round(upper, 3), ")", sep = "")) %>% 
    mutate(n_preterm = sum(mod_outcome == 1),
           n_term =    sum(mod_outcome == 0),
           outcome = outcome,
           exposure = study_exposure) %>%
    select(outcome, n_preterm, n_term, result, estimate:upper, exposure) %>% 
    ungroup %>% 
    return()
}

