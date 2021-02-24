## INPORT RAW DATA
## Drinking water and PTB sybling analysis
## Author: A. Sherris

# data to be cleaned 
  
  # birth data
    births_raw <- read_csv("E:/Projects/WaterExposure/Data/For_allie_CA_yr0011_05JUL2019.csv")
  
# geospatial data
  
  # counties in CA
    ca_counties <- st_read("data/raw/polygon/CA_counties/ca_counties.shp", quiet = TRUE) %>% 
      mutate(county = toupper(NAME)) %>% 
      select(county) %>% 
      st_transform(crs_projected)
    
    # ca_counties_geo <- ca_counties %>% st_transform(crs_geo)
    # 
    # # CA state outline
    # ca_state_proj <- st_union(ca_counties)
    # ca_state_geo <- ca_state_proj %>% st_transform(crs_geo)
    # 
    # 
