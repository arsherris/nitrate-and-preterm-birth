## IMPORT RAW DATA
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# data to be cleaned (see README.Rmd for data sources)
  
# birth data
births_raw <- read_csv("E:/Projects/WaterExposure/Data/For_allie_CA_yr0011_05JUL2019.csv")

# water quality data
wq_data_raw <- read_csv("data/raw/tabular/PWS monitoring results/nitrate_data_raw.csv") 
    
# water boundary tool (PWS service areas)
pws_sp_raw <- st_read("data/raw/polygon/WBT/May 2019/service_areas.shp", quiet = TRUE)
    
# descriptive data for PWS and sources 
flow_paths_raw <- read_csv("data/raw/tabular/PWS information/Flow_Supply to Receiving_8-11-2020.csv") 
source_status <- read_csv("data/raw/tabular/PWS information/source_status_09-06-2017.csv")
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
    
    
    