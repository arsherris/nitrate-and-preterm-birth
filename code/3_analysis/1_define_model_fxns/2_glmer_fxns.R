## FUNCTIONS FOR INDIVIDUAL-LEVEL ANALYSES
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# define glmer controls to allow faster model runtime
fast_control <- glmerControl(optimizer="nloptwrap", calc.derivs = F)

# functions to run mixed models for individual-level analyses
  # input: 
    # data: data frame for analysis
    # outcome: study outcome as a string identical to column name ("prem_20_to_31" or "prem_32_to_36)
  # output: survival model object

# adjusted individual-level analysis 
  # mother and pwsid intercepts
  # adjusted for parity category, payer type, prenatal care,
  # maternal race, education, and age
  
glmer_case_control_adj <-  function(data, outcome) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome]] ~ exp_cat +
          factor(parity_cat) +
          payer + precare +
          mat_race + mat_educ + mat_age_5cat +
          (1|pwsid) + (1|mat_id),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}

# adjusted individual-level analysis: continuous exposure

glmer_case_control_conc <-  function(data, outcome) {
  
  glmer(data[[outcome]] ~ conc +
          factor(parity_cat) +
          payer + precare +
          mat_race + mat_educ + mat_age_5cat +
          (1|pwsid) + (1|mat_id),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}

# end
