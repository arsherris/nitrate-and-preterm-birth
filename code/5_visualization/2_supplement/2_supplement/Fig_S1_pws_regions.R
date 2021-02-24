## map pwsids with "exposed" residents

library(RColorBrewer)

# ggplot tuning

ditch_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.spacing = unit(-0.4, "lines"),
  panel.grid.major = element_line(colour = 'transparent'),
  strip.text = element_blank(),
  strip.background = element_blank()
)


# load data

  # births and water systems
  load("data/processed/clean_input/pws_clean.RData")

  # spatial data
  
    # counties in CA
    ca_counties <- st_read("data/raw/polygon/CA_counties/ca_counties.shp", quiet = TRUE) %>% 
      mutate(county = toupper(NAME)) %>% 
      select(county) %>% 
      st_transform(crs_projected)
    
    ca_counties_geo <- ca_counties %>% 
      st_transform(crs_geo) %>% 
      mutate(district = case_when(
        county %in% c("BUTTE", "COLUSA",  "GLENN","SHASTA",  "TEHAMA", 
                      "EL DORADO","PLACER","YOLO", "YUBA","SUTTER", "SACRAMENTO",
                      "DEL NORTE", "HUMBOLDT" ,"SISKIYOU",   "SIERRA",  "NEVADA",
                      "LASSEN", "MODOC", "PLUMAS","TRINITY",
                      "AMADOR", "CALAVERAS") ~ "1. Northern California",
        county %in% c("MENDOCINO","LAKE", "ALAMEDA", "CONTRA COSTA", "MARIN",  "SONOMA",
                      "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO",
                      "MONTEREY",  "NAPA","SAN BENITO", "SANTA CRUZ") ~ "2. San Francisco Bay Area",
        county %in% c("FRESNO", "KERN", 'KINGS',"MERCED","SAN JOAQUIN", "MADERA",  
                      "STANISLAUS", "TULARE", "MARIPOSA", "TUOLUMNE") ~ "3. San Joaquin Valley",
        county %in% c("MONO", "INYO", "ALPINE", "RIVERSIDE", "SAN BERNARDINO", 
                      "ORANGE", "IMPERIAL", "SAN DIEGO") ~ "5. Inland Empire",
        county %in% c("SAN LUIS OBISPO", "SANTA BARBARA", "VENTURA", "LOS ANGELES") ~ "4. South Coast"))
    
    # CA state outline
    ca_state_geo <- st_union(ca_counties_geo)

    library(ggsci)
pws_map <- 
  ggplot(ca_counties_geo) +
  geom_sf(aes(fill = district, col = district), lwd = 0.1) +
  scale_fill_discrete(h = c(40, 250), c = 35, l = 90, name = "District") +
  scale_color_discrete(h = c(40, 250), c = 35, l = 55, name = "District") +
  geom_sf(data = pws_sp, fill = "grey65", col = "grey40", alpha = 0.4, size = 0.1) +
  geom_sf(data = ca_state_geo,
          col = "grey30", size = 0.4,
          fill = NA) +  theme_bw() +
  theme_void() +
  ditch_axes 

pws_map

ggsave(plot = pws_map, device = pdf(),
       filename =  "output/visualizations/2_supplement/FigS1_pws_districts.pdf",
       height =7.5, width = 7.5, units = "in")

ggsave(plot = pws_map, 
       filename =  "output/visualizations/2_supplement/FigS1_pws_districts.png",
       dpi = 300,
       height =7.5, width = 7.5, units = "in")

# characteristics of PWS in different regions

pws_info <- read_csv("data/raw/tabular/PWS information/Public Potable Water Systems FINAL 06-22-2018_0.csv") %>% 
  rename(pwsid = `Water System No`)

pws_regions <- pws_sp %>% 
  left_join(pws_info) %>% 
  mutate(district = case_when(
    county %in% c("BUTTE", "COLUSA",  "GLENN","SHASTA",  "TEHAMA", 
                  "EL DORADO","PLACER","YOLO", "YUBA","SUTTER", "SACRAMENTO",
                  "DEL NORTE", "HUMBOLDT" ,"SISKIYOU",   "SIERRA",  "NEVADA",
                  "LASSEN", "MODOC", "PLUMAS","TRINITY",
                  "AMADOR", "CALAVERAS") ~ "1. Northern California",
    county %in% c("MENDOCINO","LAKE", "ALAMEDA", "CONTRA COSTA", "MARIN",  "SONOMA",
                  "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO",
                  "MONTEREY",  "NAPA","SAN BENITO", "SANTA CRUZ") ~ "2. San Francisco Bay Area",
    county %in% c("FRESNO", "KERN", 'KINGS',"MERCED","SAN JOAQUIN", "MADERA",  
                  "STANISLAUS", "TULARE", "MARIPOSA", "TUOLUMNE") ~ "3. San Joaquin Valley",
    county %in% c("MONO", "INYO", "ALPINE", "RIVERSIDE", "SAN BERNARDINO", 
                  "ORANGE", "IMPERIAL", "SAN DIEGO") ~ "5. Inland Empire",
    county %in% c("SAN LUIS OBISPO", "SANTA BARBARA", "VENTURA", "LOS ANGELES") ~ "4. South Coast")) %>% 
  st_drop_geometry()

table(pws_regions$`Primary Water Source Type -CODE`)

pws_regions %>% 
  group_by(district) %>% 
  summarise(perc_gw = sum(`Primary Water Source Type -CODE` == "GW") / n(),
            med_connections = median(`Number of Residential Service Connections`, na.rm=T))

data_region <- data_region %>% 
  left_join(pws_info, by = "pwsid")

data_region %>% 
  group_by(district) %>% 
  summarise(perc_gw = sum(`Primary Water Source Type -CODE` %in% c("GW", "GWP", "GU")) / n(),
            med_connections = median(`Number of Residential Service Connections`, na.rm=T),
            perc_high4 = sum(`Number of Residential Service Connections` > 4000, na.rm=T),
            perc_high2 = sum(`Number of Residential Service Connections` > 2000, na.rm=T))
