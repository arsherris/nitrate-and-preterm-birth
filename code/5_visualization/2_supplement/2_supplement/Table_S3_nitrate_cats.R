source('code/6_visualization/1_manuscript/table_functions.R')

## Descriptive stats of births in each nitrate cat: Siblings

  # prepare datasets for table
  tbl_sibs <- prep_tbl_data(sibs_consecutive)

  # split by nitrate cate
  sibs_nitrate <- split(tbl_sibs, tbl_sibs$exp_cat)

## Descriptive stats of births in each nitrate cat: Cohort
  
  # prepare datasets for table
  tbl_births <- prep_tbl_data(births_case_control)
  
  # split by nitrate cate
  births_nitrate <- split(tbl_births, tbl_births$exp_cat)
  
# generate descriptive table

  options(scipen = 999)
  tbl_nitrate_sibs   <- lapply(sibs_nitrate, tab_descriptive)
  tbl_nitrate_births <- lapply(births_nitrate, tab_descriptive)
  
  table_s3_nitrate_cats <-  tbl_nitrate_sibs[[1]] %>% 
    select(variable, value, sibs_low = percent) %>% 
    left_join(select(tbl_nitrate_sibs[[2]], variable, value, sibs_medium = percent), by = c("variable", "value")) %>% 
    left_join(select(tbl_nitrate_sibs[[3]], variable, value, sibs_high = percent), by = c("variable", "value")) %>% 
    left_join(select(tbl_nitrate_births[[1]], variable, value, births_low = percent), by = c("variable", "value")) %>% 
    left_join(select(tbl_nitrate_births[[2]], variable, value, births_medium = percent), by = c("variable", "value")) %>% 
    left_join(select(tbl_nitrate_births[[3]], variable, value, births_high = percent), by = c("variable", "value")) %>% 
    filter(variable != "Count")
  
write_output(table_s3_nitrate_cats, "output/visualizations/2_supplement/")  
  
  
  
