## GENERATE TABLE 1
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

source("code/0_config.R")
source("code/4_visualization/1_manuscript/0_Table_fxns.R")

## load datasets to be summarised in Table 1

# study population
load("data/processed/clean_input/births_study_pop.RData")
# individual-level sample
load("data/processed/births_exposure/births_case_control.RData")
# sibling sample
load("data/processed/births_exposure/sibs_consecutive.RData")
# discordant siblings
load("data/processed/births_exposure/sibs_discordant.RData")

# prepare datasets for generating table 

tbl_study_pop    <- prep_tbl_data(births_study_pop)
tbl_case_control <- prep_tbl_data(births_case_control)
tbl_sibs         <- prep_tbl_data(sibs_consecutive)
tbl_sibs_disc    <- prep_tbl_data(sibs_discordant)
# concordant siblings: siblings not included in discordant sibling set
tbl_sibs_con     <- prep_tbl_data(filter(sibs_consecutive, !mat_id %in% sibs_discordant$mat_id))
  
# generate descriptive table for each sample
  
options(scipen = 999)

table1_study_pop    <- make_table1(tbl_study_pop)
table1_case_control <- make_table1(tbl_case_control)
table1_sibs         <- make_table1(tbl_sibs)
table1_disc         <- make_table1(tbl_sibs_disc)
table1_con          <- make_table1(tbl_sibs_con)
  
# merge descriptive tables
  
table1 <- table1_study_pop %>% 
  select(variable, value, study_pop = percent) %>% 
  left_join(select(table1_case_control, variable, value, case_control = percent), by = c("variable", "value")) %>% 
  left_join(select(table1_sibs, variable, value, crossover = percent), by = c("variable", "value")) %>% 
  left_join(select(table1_con, variable, value, concordant = percent), by = c("variable", "value")) %>% 
  left_join(select(table1_disc, variable, value, discordant = percent), by = c("variable", "value")) %>% 
  arrange(variable)

  
# calculate number of mothers in each sample
n_mat <- c(
  length(unique(tbl_study_pop$mat_id)),
  length(unique(tbl_case_control$mat_id)),
  length(unique(tbl_sibs$mat_id)),
  length(unique(tbl_sibs_con$mat_id)),
  length(unique(tbl_sibs_disc$mat_id)))
  
  # append to table1
  table1[1,3:7] <- n_mat
  
# determine sample size of each sample

n_samp <- c("Sample_size",
            "N",
            nrow(tbl_study_pop),
            nrow(tbl_case_control),
            nrow(tbl_sibs),
            nrow(tbl_sibs_con),
            nrow(tbl_sibs_disc))
  
  names(n_samp) <- names(table1)
  
  # append to table1  
  table1 <- rbind(n_samp, table1)
  
# save output
write_output(table1, "output/visualizations/1_manuscript/")
  

## end