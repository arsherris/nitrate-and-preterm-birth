# functions for sensitivity analses

### model specifications

# adjusted except race
glmer_no_race <-  function(data, outcome_var) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome_var]] ~ exp_cat +
          factor(parity_cat) +
          payer + precare +
          mat_educ + mat_age_5cat +
          (1|pwsid),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}


# adjusted except education
glmer_no_educ <-  function(data, outcome_var) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome_var]] ~ exp_cat +
          factor(parity_cat) +
          payer + precare +
          mat_race + mat_age_5cat +
          (1|pwsid),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}





### model specifications

# adjusted except race
glmer_crossover_no_race <-  function(data, outcome_var) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  data$exp_carryover <- factor(data$exp_carryover, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome_var]] ~ exp_cat +
          exp_carryover +
          factor(parity_cat) +
          payer + precare +
          mat_educ + mat_age_5cat +
          (1|mat_id) +
          (1|pwsid),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}


# adjusted except education
glmer_crossover_no_educ <-  function(data, outcome_var) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  data$exp_carryover <- factor(data$exp_carryover, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome_var]] ~ exp_cat +
          exp_carryover + 
          factor(parity_cat) +
          payer + precare +
          mat_race + mat_age_5cat +
          (1|mat_id) +
          (1|pwsid),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}

# adjusted model excluding parity adjustment (for stratification)

glmer_case_control_adj_noparity <-  function(data, outcome) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome]] ~ exp_cat +
          payer + precare +
          mat_race + mat_educ + mat_age_5cat +
          (1|pwsid) + (1|mat_id),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
  
# continuous exposure excluding parity adjustment (for parity stratification)

glmer_case_control_conc_noparity <-  function(data, outcome) {
  
  glmer(data[[outcome]] ~ conc +
          payer + precare +
          mat_race + mat_educ + mat_age_5cat +
          (1|pwsid) + (1|mat_id),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}


}
