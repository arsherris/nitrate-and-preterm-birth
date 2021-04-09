## PREP DATA FOR ANALYSIS: SIBLING SAMPLE 
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# generate dataframe for siblings eligible for crossover trial

load("data/processed/births_exposure/births_case_control.RData")

# find consecutive siblings

siblings_ipi <- births_case_control  %>% 
  
  # select only duplicate ids (all siblings)
  filter(mat_id %in% mat_id[duplicated(mat_id)]) %>% 
  
  # find IPI 
  arrange(bdate) %>% 
  group_by(mat_id) %>% 
  mutate(ipi_days = doc - lag(bdate)) %>% 
  ungroup()

# find number of siblings with IPI < 36
  
sum(siblings_ipi$ipi_days < 36, na.rm=T)

# filter out IPI < 36 days and create dummy variable for IPI < 1 year

sibs_consecutive <- siblings_ipi %>% 
  mutate(ipi_less_1yr = replace_na(ifelse(ipi_days < 365, "yes", "no"), "no")) %>% 
  filter(ipi_days >= 36 | is.na(ipi_days)) %>%
  group_by(mat_id) %>%

  # identify and filter to consecutive siblings
  mutate(parity_lag_diff = parity - lag(parity),
         parity_lead_diff = lead(parity) - parity) %>%
  ungroup() %>%
  filter(parity_lag_diff == 1 | parity_lead_diff == 1) %>%
  select(-parity_lag_diff, -parity_lead_diff) %>%
  
  # define covariates
  mutate(
    # relevel exposure variable
    exp_cat = factor(exp_cat, levels = c("low", "medium", "high")),
    # define IPI covariate
    ipi_less_1yr =  factor(case_when(ipi_days < 365 ~ "yes", 
                                     ipi_days >= 365 ~ "no",
                                     parity == 1 ~ "no",
                                     is.na(ipi_days) ~ "unknown"),
                           levels = c("yes", "no", "unknown")))

# write output

save(siblings_ipi, file = "data/processed/births_exposure/sibs_ipi_rev.RData")
save(sibs_consecutive, file = "data/processed/births_exposure/sibs_consecutive.RData")

