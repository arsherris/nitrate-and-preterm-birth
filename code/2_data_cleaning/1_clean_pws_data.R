## DEFINE FUNCTION: CLEAN DATA FROM PUBLIC WATER SYSTEM
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# function: clean data from public water systems (PWS) service area boundaries
  # input: raw PWS data from Water Boundary Tool
  # output: cleaned PWS data spatial polygon

clean_pws <- function(data_pws_raw) {
  
  data_pws_raw %>%
    
    # transform to projected coordinate reference system
    st_transform(crs = crs_projected) %>% 
    
    # join to PWS info from Open Data Portal (fee codes and type codes)
    left_join(select(pws_info, 
                     pwsid =     `Water System No`,
                     type_code = `Federal Water System Type -CODE`,
                     fee_code =  `Fee Code`)
              ) %>%
    
    # include only community water systems
    # exclude wholesale systems
    filter(d_pws_fed_ == "C", 
           fee_code != "WH") %>%
    unique() %>%
    group_by(pwsid) %>%
    
    # retain one polygon per PWS (by date; newest retained)
    top_n(-1, dt_created) %>%
    ungroup() %>% 
    
    # calculate area of each water system
    mutate(area = as.numeric(st_area(.))) %>% 
    select(
      pwsid, county = d_prin_cnt, activity_s, activity_d, owner_type, 
      svc_connec, population = d_populati, type_code, fee_code, area
      ) %>% 
    return()
}

# end
