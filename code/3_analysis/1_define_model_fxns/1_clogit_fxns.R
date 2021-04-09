## FUNCTIONS FOR WITHIN-MOTHER ANALYSES
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# functions to run conditional logistic regression models for within-mother analysis
  # input: 
    # data: data frame for analysis
    # outcome: study outcome as a string identical to column name 
      # "prem_20_to_31" or "prem_32_to_36"
  # output: survival model object


# unadjusted model --------------------------------------------------------------------
  # conditional on mother ID

clog_unadj <- function(data, outcome) {
  
  # redefine exposure category factor levels
    # ensures that "low" exposure is reference group
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))

  # specify model formula, method, and data
  clogit(formula = data[[outcome]] ~ exp_cat +
           strata(mat_id),
         method = "exact",
         data = data)
  
}

# adjusted model ---------------------------------------------------------------------
  # conditional on mother ID
  # adjusted for parity category, IPI dummy, and maternal age

clog_adj <- function(data, outcome) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))

  clogit(data[[outcome]] ~ exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) + 
           strata(mat_id),
         method =  "exact",
         data = data)
}

# adjusted model: continuous exposure --------------------------------------------------

clog_adj_conc <- function(data, outcome) {
  
    clogit(data[[outcome]] ~ conc +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) + 
           strata(mat_id),
         method =  "exact",
         data = data)
}

# end

