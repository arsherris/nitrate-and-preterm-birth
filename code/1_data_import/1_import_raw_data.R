## IMPORT RAW DATA
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# data to be cleaned 
  
  # birth data
births_raw <- read_csv("E:/Projects/WaterExposure/Data/For_allie_CA_yr0011_05JUL2019.csv")

  # water quality data
    wq_data_raw <- read_csv("data/raw/tabular/PWS monitoring results/wq_data_2020-05-27.csv") 
    
  # water boundary tool
    pws_sp_raw <- st_read("data/raw/polygon/WBT/May 2019/service_areas.shp", quiet = TRUE)
    
    # descriptive data for PWS and sources 
    flow_paths_raw <- read_csv("data/raw/tabular/PWS information/Flow_Supply to Receiving_8-11-2020.csv") 
    source_info <- read.dbf("data/raw/tabular/PWS information/siteloc.dbf")
    pws_info <- read_csv("data/raw/tabular/PWS information/Public Potable Water Systems FINAL 06-22-2018_0.csv")
    mcls <- read_csv("data/raw/tabular/PWS monitoring results/mcls_dlrs_phgs.csv") %>% 
      mutate_at(vars(MCL:PHG), as.numeric)
    
    # geospatial data
    
    # counties in CA
    ca_counties <- st_read("data/raw/polygon/CA_counties/ca_counties.shp", quiet = TRUE) %>% 
      mutate(county = toupper(NAME)) %>% 
      select(county) %>% 
      st_transform(crs_projected)
    
    ca_counties_geo <- ca_counties %>% st_transform(crs_geo)
    
    # CA state outline
    ca_state_proj <- st_union(ca_counties)
    ca_state_geo <- ca_state_proj %>% st_transform(crs_geo)
    
    
    