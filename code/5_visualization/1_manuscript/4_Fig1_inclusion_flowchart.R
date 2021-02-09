## QUANTIFY INCLUDED AND EXCLUDED POPULATION (FIGURE 1)
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

## Define inclusion criteria and find number of births included at each step

# raw birth data starts with 6,226,183 births 
  # see "Data flow chart_for_Allie_20Jun2019.doc" for exclusion criteria applied prior to data extract

births_raw <- read_csv("E:/Projects/WaterExposure/Data/For_allie_CA_yr0011_05JUL2019.csv")

# determine counties in which maternal residences are located 
  # births outside CA will be excluded 
    
    births_counties <- births_raw %>% 
      select(brthid, lat, long) %>% 
      # remove invalid lat/long (required to convert to spatial)
      filter(!is.na(lat)) %>% 
      # convert to spatial object
      st_as_sf(coords = c("long", "lat"), crs = crs_geo) %>% 
      st_transform(crs_projected) %>% 
      # join with county polygon
      st_join(ca_counties, 
              join = st_intersects)  %>% 
      st_drop_geometry() %>% 
      # a few births are on border of two counties, resulting in duplicates
      # remove duplicates
      distinct(brthid, .keep_all = T)

    # join county assignments to raw data
    births_flowchart <- births_raw %>% 
      left_join(births_counties) 
    
    write_output(births_counties, "1_data_clean")
      
# define starting population
    # all singleton births
    # joined with county data
    # exclude one birth with plural indicated by birth certificate

    
    # starting population value for figure 1 includes births excluded by Wei
    pop_start <- nrow(births_flowchart) +
      13965 + # invalid sfn 
      27758 - # excluded by wei because invalid lat/long
      1 # plural indicated by birth certificate PLURA == 1
    
    pop1_valid_sfn <- pop1_start - 13965

 # births with valid lat/long 
    # 27258 excluded by Wei
    # 3990 additionally excluded

    attach(births_flowchart)
    pop2_valid_lat <- sum(PLURA == 0 & 
                            !is.na(long)) # 6,222,192
    
 # births with valid geocode to Street or ZIP level
    # sufficient for exposure assessment to public water system
    
    pop3_valid_geocode <- sum(PLURA == 0 &
                                !is.na(long) &
                                MATCHED != "City") # 6,221,558
    
 # births in California
    
    pop4_in_CA <-  sum(PLURA == 0 &
                         !is.na(long) &
                         MATCHED != "City" &
                         !is.na(county)) # 6,181,160
       
  
 # births with valid gestational age and birth weight
    
    pop5_valid_ga_bw <- sum(PLURA == 0 &
                              !is.na(long) &
                              MATCHED != "City" &
                              !is.na(county) &
                              Final_gestwk_valid == 1 &
                              bwt_range == 1) # 5,678,842
    
 # better link between OSHPD and birth certificate
    # and bettter match with VSB, PDDI, and PDDM for maternal ICD9 
    # codes important for Diabetes, HTN, and PTB subtyping

   pop6_linked <- sum(PLURA == 0 &
                        !is.na(long) &
                        MATCHED != "City" &
                        !is.na(county) &
                        Final_gestwk_valid == 1 &
                        bwt_range == 1 &
                        link == 1 &
                        linkedB %in% c("Y", "M")) # 5,551,051
    
 # spontaneous PTB no complications
   
   pop7_spont_PTB <- sum(PLURA == 0 &
                           !is.na(long) &
                           MATCHED != "City" &
                           !is.na(county) &
                           Final_gestwk_valid == 1 &
                           bwt_range == 1 &
                           link == 1 &
                           linkedB %in% c("Y", "M") &
                           !is.na(prem_5cat_spon_nocomp)) # 4,698,830
  
    # 4,698,830 births in study population

# find number excluded due to no exposure assessment

load("data/processed/maternal_pwsid/births_study_pop_pws_final.RData")
load("data/processed/births_exposure/births_case_control.RData")

n_with_pwsid <- sum(!is.na(births_study_pop_pws_final$pwsid))
n_with_exposure <- nrow(births_case_control)

## find number of siblings 

load("data/processed/births_exposure/sibs_ipi.RData")
load("data/processed/births_exposure/sibs_consecutive.RData")

sibs_all <- births_case_control %>% 
  filter(mat_id %in% mat_id[which(duplicated(mat_id))])
n_sibs <- nrow(sibs_all)
n_good_ipi <- n_sibs - sum(siblings_ipi$ipi_days<36, na.rm=T)
n_consecutive_sibs <- nrow(sibs_consecutive)

# combine and save

figure1_data <- inclusion_table %>% 
  bind_rows(tibble(criteria =  c("start", "valid_sfn", 
                                 "valid_lat", "valid_geocode", 
                                 "pop_in_CA", "valid_ga_bw", 
                                 "linked", "spont_PTB",
                                 "with pwsid", "with exposure",
                                 "all sibs", "good ipi",
                                 "conscutive sibs"),
                   included = c(pop_start, pop1_valid_sfn,
                                pop2_valid_lat, pop3_valid_geocode, 
                                pop4_in_CA, pop5_valid_ga_bw,
                                pop6_linked, pop7_spont_PTB,
                                n_with_pwsid, n_with_exposure,
                                n_sibs, n_good_ipi,
                                n_consecutive_sibs
                                ))) %>% 
  mutate(excluded = included - lag(included))

# save figure data

write_output(figure1_data, "output/visualizations/1_manuscript/")
