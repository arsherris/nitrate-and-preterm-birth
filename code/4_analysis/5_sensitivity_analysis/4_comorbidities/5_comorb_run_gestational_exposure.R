## RUN FUNCTIONS TO ASSIGN GESTATIONAL EXPOSURE
## Drinking water and PTB sybling analysis
## Author: A. Sherris

# load study population with pws assignments

# load water quality data
#load("data_processed/clean_input/nitrate_wq_data.RData")

# load functions

source("code/4_exposure_assessment/2_assign_gestational_exposure_no_dist.R")

# load data

load("data/processed/clean_input/nitrate_wq_data.RData")

# generate sample

# generate exposure intervals for each birth
  # primary interval is from conception to birth;
  # secondary long interval is from 15 months before conception to 12 months after birth
  
  births_interval <- births_comorb_sp_pws_final %>% 
    mutate(before_doc = doc - 450, # 15 months before conception
           after_bdate = bdate + 360, # 12 months after birth
           preg_int = interval(doc, bdate), # interval of pregnancy
           preg_int_long = interval(before_doc, after_bdate)) # longer interval before and after pregnancy

# generate list of counties in birth data;
  # these will be used to run county-by-county exposure assessment functions
  county_list <- unique(births_interval$county)

# assign nitrate exposure (maps over counties)
  
  Sys.time()
  
  # initiate progress bar
  pb <- progress_estimated(length(county_list))
  
  # assign exposure
  exposure <- county_list %>% map_df(~assign_exp_with_progress(.))

  
# join to births covariates
births_comorb_exposure <- exposure %>% 
  select(-pwsid) %>% 
  separate(conc, into = c("conc", "sample_n", "source_n", "interval"), #"uncertainty_SD", "uncertainty_SE", 
           sep = " ", convert = T) %>% 
  mutate(mcl = first(wq_data$mcl),
         exp_cat = factor(case_when(
           conc < mcl / 2 ~ "low",
           conc >= mcl ~ "high",
           conc >= mcl/2 ~ "medium", # medium only includes less than MCL
           TRUE ~ NA_character_), 
           levels = c("low", "medium", "high")),
         exp_cat_bin = factor(case_when(
           exp_cat == "low" ~ 1,
           exp_cat %in% c("medium", "high") ~ 2)),
         chemical = chem_of_interest) %>% 
  left_join(births_comorb_sp_pws_final, by = "birth_id") %>%
  unique()

Sys.time()

save(births_comorb_exposure, file =  "data/processed/births_exposure/births_comorb_exposure.RData")

rm(births_interval, exposure)
