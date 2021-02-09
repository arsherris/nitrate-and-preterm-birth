## LINK MATERNAL IDS TO PUBLIC WATER SYSTEM IDS
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# function to divide data into counties and assign maternal pwsid

assign_pws_by_county <- function(births_data, pws_boundaries, county_name) {
  
  # restrict all data to county of interest
  births_county <- filter(births_data, county == county_name)
  pws_county <- filter(pws_boundaries, county == county_name)
  
  # assign maternal pwsid
  births_pws <- assign_pws(births_county, pws_county) %>% 
    return()
  
}

# function to assign maternal addresses to PWS using boundaries
# of water systems (from Water Boundary Tool)

assign_pws <- function(births_data, pws_boundaries) {
  
  # determine PWS in which maternal residences are located 
  births_pws <- st_join(births_data, 
                        select(pws_boundaries, pwsid, area, population), 
                        join = st_intersects) 
  
  # preserve births not matched to pwsid
  births_pws_na <- births_pws %>% 
    filter(is.na(pwsid)) %>% 
    st_drop_geometry()
  
  # for births matched to a pwsid:
  # if multiple matches, selects the smallest PWS;
  # if two overlapping with same area, selects that with larger population
  births_pws_match <- births_pws %>% 
    group_by(birth_id) %>%
    top_n(-1, area) %>%
    top_n(1, population) %>% 
    ungroup() %>%
    st_drop_geometry() %>% 

    # bind with births not matched to PWSID
    bind_rows(births_pws_na) %>% 
    return()
  
}
