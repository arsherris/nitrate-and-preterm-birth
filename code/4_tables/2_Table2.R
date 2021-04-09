## GENERATE TABLE 2
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

source("code/0_config.R")
source("code/4_visualization/1_manuscript/0_Table_fxns.R")

# load datasets to be summarised in Table 1
  
  # study population
  load("data/processed/clean_input/births_study_pop.RData")
  # individual-level sample
  load("data/processed/births_exposure/births_case_control.RData")
  # sibling sample
  load("data/processed/births_exposure/sibs_consecutive.RData")
  # discordant siblings
  load("data/processed/births_exposure/sibs_discordant.RData")

# generate table 2 for each sample
  
table2_births  <- make_table2(births_case_control) %>% 
  mutate(sample = "Case-control")

table2_sibs    <- make_table2(sibs_consecutive) %>% 
  mutate(sample = "All siblings")

table2_concord <- make_table2(filter(sibs_consecutive, !mat_id %in% sibs_discordant$mat_id))%>% 
  mutate(sample = "Concordant siblings")

table2_discord <- make_table2(sibs_discordant)%>% 
  mutate(sample = "Discordant siblings")

# bind tables for each sample

table2 <- bind_rows(table2_births, table2_sibs, table2_concord, table2_discord)

write_output(table2, "output/visualizations/1_manuscript/")

## end