# models to identify interaction terms

# glmer
glmer_interaction <-  function(data, interaction_factor, outcome) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  glmer(data[[outcome]] ~ exp_cat +
          data[[interaction_factor]]*exp_cat +
          factor(parity_cat) +
          payer + precare +
          mat_race + mat_educ + mat_age_5cat +
          (1|pwsid) + (1|mat_id),
        family = binomial, 
        control = fast_control,
        nAGQ = 0,
        data = data)
}

# clogit

clog_interaction <-  function(data, interaction_factor, outcome) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  clogit(data[[outcome]] ~ exp_cat +
           data[[interaction_factor]]*exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) + 
           strata(mat_id),
         method =  "exact",
         data = data)
}

# table to call functions and generate results table

# res_table_interactions <- function(mod_early, mod_late, interaction_factor) { #data, mod_fxn
#   
#   # mod_early <- mod_fxn(data, interaction_factor, "prem_20_to_31")
#   # mod_late  <- mod_fxn(data, interaction_factor, "prem_32_to_36")
#   
#   res_early <- tidy(mod_early) %>% 
#     filter(grepl("interaction_factor", term)) %>% 
#     mutate(outcome = "20-31 weeks",
#            factor = interaction_factor,
#            term = str_replace(term, "data", ""),
#            term = str_replace(term, "exp_cat", "")) %>% 
#     select(outcome, factor, term, estimate, p.value)
#   
#   res_late <- tidy(mod_late) %>% 
#     filter(grepl("interaction_factor", term)) %>% 
#     mutate(outcome = "32-36 weeks",
#            factor = interaction_factor,
#            term = str_replace(term, "data", ""),
#            term = str_replace(term, "exp_cat", "")) %>% 
#     select(outcome, factor, term, estimate, p.value)
#   
#   res_early %>% bind_rows(res_late) %>% return()
#   
# }
# 

res_table_interactions <- function(data, mod_fxn, interaction_factor) {
  
  mod_early <- mod_fxn(data, interaction_factor, "prem_20_to_31")
  mod_late  <- mod_fxn(data, interaction_factor, "prem_32_to_36")

  res_early <- tidy(mod_early) %>% 
    filter(grepl("interaction_factor", term)) %>% 
    mutate(outcome = "20-31 weeks",
           factor = interaction_factor,
           term = str_replace(term, "data", ""),
           term = str_replace(term, "exp_cat", "")) %>% 
    select(outcome, factor, term, estimate, p.value)
  
  res_late <- tidy(mod_late) %>% 
    filter(grepl("interaction_factor", term)) %>% 
    mutate(outcome = "32-36 weeks",
           factor = interaction_factor,
           term = str_replace(term, "data", ""),
           term = str_replace(term, "exp_cat", "")) %>% 
    select(outcome, factor, term, estimate, p.value)
  
  res_early %>% bind_rows(res_late) %>% return()
  
}

