## sibling sample

  ## stratify by race
  
    # split
    data_race_sibs <- split(sibs_discordant, sibs_discordant$mat_race)
    
    # results by race
    res_race_sibs <- map_df(.x = data_race_sibs, 
                            .f = function(x) res_table_primary(x, clog_adj), 
                            .id = "Race") 
    
    # save
    write_output(res_race_sibs, "output/results/sensitivity/3_stratification/maternal_demo/")
  
  ## interaction
    
    mod_race_early <- clog_interaction(sibs_discordant, "mat_race", "prem_20_to_31")
    mod_race_late <-  clog_interaction(sibs_discordant, "mat_race", "prem_32_to_36")
    
    res_interaction_race <- res_table_interactions(mod_race_early, mod_race_late, "Race")
    write_output(res_interaction_race, "output/results/sensitivity/3_stratification/maternal_demo/")
  
    # compare models with and without interaction
    
    mod_sibs_early_race <- clogit(prem_20_to_31 ~ exp_cat +
                                    mat_race + 
                                    factor(parity_cat) +
                                    factor(ipi_less_1yr) +
                                    factor(mat_age_5cat) + 
                                    strata(mat_id),
                                  method =  "exact",
                                  data = sibs_discordant)
    
    mod_sibs_late_race  <- clogit(prem_32_to_36 ~ exp_cat +
                                    mat_race + 
                                    factor(parity_cat) +
                                    factor(ipi_less_1yr) +
                                    factor(mat_age_5cat) + 
                                    strata(mat_id),
                                  method =  "exact",
                                  data = sibs_discordant)
    
    
    mod_sibs_early_race_inc <- clogit(prem_20_to_31 ~ exp_cat +
                                        mat_race*exp_cat + 
                                        factor(parity_cat) +
                                        factor(ipi_less_1yr) +
                                        factor(mat_age_5cat) + 
                                        strata(mat_id),
                                      method =  "exact",
                                      data = sibs_discordant)
    
    mod_sibs_late_race_inc  <- clogit(prem_32_to_36 ~ exp_cat +
                                        mat_race*exp_cat + 
                                        factor(parity_cat) +
                                        factor(ipi_less_1yr) +
                                        factor(mat_age_5cat) + 
                                        strata(mat_id),
                                      method =  "exact",
                                      data = sibs_discordant)  
    
    save(mod_sibs_early_race, mod_sibs_late_race,
         mod_sibs_early_race_int, mod_sibs_late_race_int,
         file = "output/results/sensitivity/3_stratification/maternal_demo/race_int_mods.RData")
    
    lrtest(mod_sibs_early_race, mod_sibs_early_race_inc)
    lrtest(mod_sibs_late_race, mod_sibs_late_race_inc)
    
    
  
## Education  

  ## stratify by education
  
    # split
    data_education_sibs <- split(sibs_discordant, sibs_discordant$mat_educ)
    
    educ1 <- res_table_primary(data_education_sibs[[1]], clog_adj)
    educ2 <- res_table_primary(data_education_sibs[[2]], clog_adj)
    educ3 <- res_table_primary(data_education_sibs[[3]], clog_adj)
    
    # # results by education
    # res_education_sibs <- map_df(.x = data_education_sibs, 
    #                              .f = function(x) res_table_primary(x, clog_adj), 
    #                              .id = "Education") 
    # 
    
  res_educ_sibs <- bind_rows(mutate(educ1, education = names(data_education_sibs)[1]), 
                             mutate(educ2, education = names(data_education_sibs)[2]), 
                             mutate(educ3, education = names(data_education_sibs)[3]))
    # save
    write_output(res_educ_sibs, "output/results/sensitivity/3_stratification/maternal_demo/")
    
# compare models with and without interaction
    
    mod_sibs_early_educ <- clogit(prem_20_to_31 ~ exp_cat +
                                    mat_educ + 
                                    factor(parity_cat) +
                                    factor(ipi_less_1yr) +
                                    factor(mat_age_5cat) + 
                                    strata(mat_id),
                                  method =  "exact",
                                  data = sibs_discordant)

    mod_sibs_late_educ  <- clogit(prem_32_to_36 ~ exp_cat +
                                    mat_educ + 
                                    factor(parity_cat) +
                                    factor(ipi_less_1yr) +
                                    factor(mat_age_5cat) + 
                                    strata(mat_id),
                                  method =  "exact",
                                  data = sibs_discordant)
    
 
    mod_sibs_early_educ_inc <- clogit(prem_20_to_31 ~ exp_cat +
                                    mat_educ*exp_cat + 
                                    factor(parity_cat) +
                                    factor(ipi_less_1yr) +
                                    factor(mat_age_5cat) + 
                                    strata(mat_id),
                                  method =  "exact",
                                  data = sibs_discordant)
    
    mod_sibs_late_educ_inc  <- clogit(prem_32_to_36 ~ exp_cat +
                                    mat_educ*exp_cat + 
                                    factor(parity_cat) +
                                    factor(ipi_less_1yr) +
                                    factor(mat_age_5cat) + 
                                    strata(mat_id),
                                  method =  "exact",
                                  data = sibs_discordant)  
    
    save(mod_sibs_early_educ, mod_sibs_late_educ,
         mod_sibs_early_educ_int, mod_sibs_late_educ_int,
         file = "output/results/sensitivity/3_stratification/maternal_demo/education_int_mods.RData")
    
  lrtest(mod_sibs_early_educ, mod_sibs_early_educ_inc)
  lrtest(mod_sibs_late_educ, mod_sibs_late_educ_inc)
  
    #   ## interaction
#     
#     mod_education_early <- clog_interaction(sibs_discordant, "mat_education", "prem_20_to_31")
#     mod_education_late <-  clog_interaction(sibs_discordant, "mat_education", "prem_32_to_36")
#     
#     res_interaction_education <- res_table_interactions(mod_education_early, mod_education_late, "Education")
#     write_output(res_interaction_education, "output/results/sensitivity/3_stratification/maternal_demo/")
#     
#     
#     
# ## full cohort
#     
#     ## stratify by race
#     
#     # split
#     data_race_cohort <- split(births_case_control, births_case_control$mat_race)
#     
#     # results by race
#     res_race_cohort <- map_df(data_race_cohort, 
#                               function(x) res_table_primary(x, glmer_case_control_adj_norace),
#                               .id = "Race") 
#     
#     # save
#     write_output(res_race_cohort, "output/results/sensitivity/3_stratification/maternal_demo/")
#     
#     ## interaction
#     
#     mod_race_early <- glmer_interaction(births_case_control, "mat_race", "prem_20_to_31")
#     mod_race_late <-  glmer_interaction(births_case_control, "mat_race", "prem_32_to_36")
#     
#     res_interaction_race_cohort <- res_table_interactions(mod_race_early, mod_race_late, "Race")
#     write_output(res_interaction_race, "output/results/sensitivity/3_stratification/maternal_demo/")
#     
#     
#   