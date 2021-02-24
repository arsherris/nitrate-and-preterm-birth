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
  load("data_processed/births_exposure/births_case_control.RData")
  load("data_processed/clean_input/pws_clean.RData")

  # spatial data
  
    # counties in CA
    ca_counties <- st_read("./data_in/polygon/CA_counties/ca_counties.shp", quiet = TRUE) %>% 
      mutate(county = toupper(NAME)) %>% 
      select(county) %>% 
      st_transform(crs_projected)
    ca_counties_geo <- ca_counties %>% st_transform(crs_geo)
    
    # CA state outline
    ca_state_proj <- st_union(ca_counties)
    ca_state_geo <- ca_state_proj %>% st_transform(crs_geo)
    
    # SJV counties and outline
    sjv_geo <-filter(ca_counties_geo, county %in% sjv_counties)
    sjv_outline <- st_union(sjv_geo)
  
# identify PWS with exceedances
    
  pws_high <- unique(births_case_control$pwsid[births_case_control$exp_cat == "high"])
  pws_med <- unique(births_case_control$pwsid[births_case_control$exp_cat == "medium"])
  pws_low <- unique(births_case_control$pwsid[births_case_control$exp_cat == "low"])
  
  # classify pwsids
  pws_exceeds <- pws_sp %>% 
    mutate(category = case_when(
      pwsid %in% pws_high ~ "High",
      pwsid %in% pws_med ~ "Medium",
      pwsid %in% pws_low ~ "None",
      TRUE ~ NA_character_)) %>% 
    mutate(category = factor(category, levels = c( "Medium", "High", "None")))

# plot
  
# pws_map <- 
#   ggplot(pws_exceeds) +
#   geom_sf(data = ca_counties_geo,
#           col = "grey",
#           fill = "white", lwd = 0.1) +
#   geom_sf(aes(fill = category, col = category), size = 0.3) + 
#   scale_fill_brewer(palette = "Set2", na.value = "grey67") +
#   scale_color_brewer(palette = "Dark2", na.value = "grey57") +
#   # geom_sf(data = sjv_outline,
#   #         col = "grey30", size = 0.7,
#   #         fill = NA) +  theme_bw() + 
#   geom_sf(data = ca_state_geo,
#           col = "grey30", size = 0.6,
#           fill = NA) +  theme_bw() + 
#   labs(fill = "", col = "") +
#   ditch_axes 
# 
# pws_map
# 
# ggsave(plot = pws_map, device = pdf(),
#        filename = "data_out/visualizations/2_supplement/pws_nitrate_counties_map.pdf", 
#        height = 7.5, width = 7.5, units = "in")



# plot (with SJV outline)

pws_map <- 
  ggplot(pws_exceeds) +
   geom_sf(data = sjv_outline,
          col = "grey50", size = 0.4,
          fill = "grey95") +  theme_bw() +
  geom_sf(data = ca_counties_geo,
          col = "grey",
          fill = NA, lwd = 0.1) +
  geom_sf(aes(fill = category, col = category), size = 0.1) +
  scale_fill_brewer(palette = "Set2", na.value = "grey67") +
  scale_color_brewer(palette = "Dark2", na.value = "grey57") +
  geom_sf(data = ca_state_geo,
          col = "grey30", size = 0.4,
          fill = NA) +  theme_bw() + 
  labs(fill = "", col = "") +
  ditch_axes 

pws_map

ggsave(plot = pws_map, device = pdf(),
       filename =  "data_out/visualizations/2_supplement/pws_nitrate_counties_map.pdf",
       height =7.5, width = 7.5, units = "in")


## SJV inset


pws_exceeds_sjv <- pws_exceeds %>% 
  filter(county %in% sjv_counties)
  # filter(st_is_valid(.)) %>% 
  # st_join(sjv_proj, join = st_within)

pws_map_sjv <- 
  ggplot(pws_exceeds_sjv) +
  # geom_sf(data = ca_counties_geo,
  #         col = "grey",
  #         fill = "white", lwd = 0.1) +
  geom_sf(data = sjv_geo,
          col = "grey", 
          fill = "grey95") +  
  theme_bw() + 
  geom_sf(aes(fill = category, col = category), size = 0.2) + 
  scale_fill_brewer(palette = "Set2", na.value = "grey67") +
  scale_color_brewer(palette = "Dark2", na.value = "grey57") +
  geom_sf(data = sjv_outline,
          col = "grey50", size = 0.5,
          fill = NA) +  theme_bw() + 
  labs(fill = "", col = "") +
  ditch_axes 

ggsave(plot = pws_map_sjv, device = png(),
       filename =  "data_out/visualizations/2_supplement/pws_nitrate_counties_sjv.pdf",
       height = 5, units = "in")


# # how many exceedances by county?
# 
# counties_exceeds <- pws_exceeds %>% 
#   st_drop_geometry() %>% 
#   count(county, category)
# 
# 
# sjv_exceeds <- pws_exceeds %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(
#     county %in% c("LOS ANGELES", "ORANGE", "VENTURA") ~ "LA",
#     county %in% sjv_counties ~ "SJV",
#     TRUE ~ "other")) %>% 
#   count(region, category) 
# 
# table(pws_exceeds$category, useNA = "a")
# 
# 
# 
# 
# births_exceeds <- births_case_control %>% 
#   mutate(region_sjv = case_when(
#     county %in% c("LOS ANGELES", "ORANGE", "VENTURA", 
#                   "SAN BERNARDINO", "RIVERSIDE") ~ "LA",
#     county %in% sjv_counties ~ "SJV",
#     TRUE ~ "other"),
#     region = case_when(
#       county %in% c("BUTTE", "COLUSA",  "GLENN","SHASTA",  "TEHAMA") ~ "Shasta",
#       county %in% c("EL DORADO","PLACER","YOLO", "YUBA","SUTTER", "SACRAMENTO") ~ "Sacramento",
#       county %in% c("DEL NORTE", "HUMBOLDT", "LAKE","SISKIYOU", "MENDOCINO",  "SIERRA",  "NEVADA",
#                     "SONOMA", "LASSEN", "MODOC", "PLUMAS","TRINITY") ~ "Northern",
#       county %in% c("ALAMEDA", "CONTRA COSTA", "MARIN", "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO",
#                     "NAPA","SAN BENITO", "SANTA CRUZ") ~ "Bay Area",
#       county %in% c("INYO", "ALPINE", "AMADOR", "CALAVERAS","MARIPOSA",  "MONO", "TUOLUMNE") ~ "Sierra",
#       county %in% c("MONTEREY",  "SAN LUIS OBISPO", "SANTA BARBARA") ~ "Central Coast",
#       county %in% c("FRESNO", "KERN", 'KINGS',"MERCED","SAN JOAQUIN", "MADERA",  "STANISLAUS", "TULARE") ~ "SJV",
#       county %in% c("RIVERSIDE", "SAN BERNARDINO") ~ "Inland Empire",
#       county %in% c("VENTURA", "LOS ANGELES", "ORANGE") ~ "South Coast",
#       county %in% c("IMPERIAL", "SAN DIEGO") ~ "Imperial",
#       TRUE ~ "none"),
#     district = case_when(
#       county %in% c("BUTTE", "COLUSA",  "GLENN","SHASTA",  "TEHAMA", 
#                     "EL DORADO","PLACER","YOLO", "YUBA","SUTTER", "SACRAMENTO",
#                     "DEL NORTE", "HUMBOLDT" ,"SISKIYOU",   "SIERRA",  "NEVADA",
#                     "SONOMA", "LASSEN", "MODOC", "PLUMAS","TRINITY",
#                     "AMADOR", "CALAVERAS") ~ "1. Northern",
#       county %in% c("MENDOCINO","LAKE", "ALAMEDA", "CONTRA COSTA", "MARIN", 
#                     "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO",
#                     "MONTEREY",  "NAPA","SAN BENITO", "SANTA CRUZ") ~ "2. Bay Area",
#       county %in% c("FRESNO", "KERN", 'KINGS',"MERCED","SAN JOAQUIN", "MADERA",  
#                     "STANISLAUS", "TULARE", "MARIPOSA", "TUOLUMNE") ~ "3. SJV",
#       county %in% c("MONO", "INYO", "ALPINE", "RIVERSIDE", "SAN BERNARDINO", 
#                     "ORANGE", "IMPERIAL", "SAN DIEGO") ~ "5. Inland Empire",
#       county %in% c("SAN LUIS OBISPO", "SANTA BARBARA", "VENTURA", "LOS ANGELES") ~ "4. LA")) %>% 
#   count(region, exp_cat) 
# 
# births_exceeds_county <- births_case_control %>% 
#   count(county, exp_cat) 
# 
# table(pws_exceeds$category, useNA = "a")
# 
# table(pws_exceeds$category, useNA = "a")
