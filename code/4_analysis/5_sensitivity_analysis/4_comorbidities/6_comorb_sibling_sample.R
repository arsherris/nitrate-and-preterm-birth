# generate dataframe for siblings eligible for crossover trial

load("data/processed/births_exposure/births_case_control.RData")

births_case_control$`==...` <- NULL

births_comorb_cohort <- births_comorb_exposure %>% 
  filter(!is.na(exp_cat)) %>% 
  mutate(parity_cat=  case_when(
    parity == 1 ~ 1,
    parity == 2 ~ 2,
    parity > 2 ~ 3)) %>% 
  bind_rows(births_case_control)
  
  

# find consecutive siblings

sibs_comorb_ipi <- births_comorb_cohort  %>% 
  
  # select only duplicate ids (all siblings)
  filter(mat_id %in% mat_id[duplicated(mat_id)]) %>% 
  
  # find IPI 
  arrange(bdate) %>% 
  group_by(mat_id) %>% 
  mutate(ipi_days = doc - lag(bdate)) %>% 
  ungroup()

# find number of siblings with IPI < 36
  
sum(sibs_comorb_ipi$ipi_days < 36, na.rm=T)

# filter out IPI < 36 days and create dummy variable for IPI < 1 year

siblings_comorb <- sibs_comorb_ipi %>% 
  mutate(ipi_less_1yr = replace_na(ifelse(ipi_days < 365, "yes", "no"), "no")) %>% 
  filter(ipi_days >= 36 | is.na(ipi_days)) %>%
  group_by(mat_id) %>%

  # identify and filter to consecutive siblings
  mutate(parity_lag_diff = parity - lag(parity),
         parity_lead_diff = lead(parity) - parity) %>%
  ungroup() %>%
  filter(parity_lag_diff == 1 | parity_lead_diff == 1) %>%
  select(-parity_lag_diff, -parity_lead_diff) %>%
  mutate(
    exp_cat = factor(exp_cat, levels = c("low", "medium", "high")),
    parity_cat = case_when(
      parity == 1 ~ 1,
      parity == 2 ~ 2,
      parity > 2 ~ 3))


# write output

save(sibs_comorb_ipi, file = "data/processed/births_exposure/sibs_comorb_ipi.RData")
save(siblings_comorb, file = "data/processed/births_exposure/siblings_comorb.RData")


