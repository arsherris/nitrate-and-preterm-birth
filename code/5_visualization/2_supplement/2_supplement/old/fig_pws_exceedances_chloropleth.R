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
  births_spon_nocomp_no3 <- read_csv("./data_out/2_exposure_assessment/births_spon_nocomp_no3_2020-04-11.csv")
  pws <- st_read("data_out/1_data_clean/pws_clean", "pws_clean")

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
  
# identify percentage of women in each PWS with "exposure"
    
   births_exposed_by_pwsid <-  births_spon_nocomp_no3 %>% 
      group_by(pwsid) %>% 
      summarise(count = n(),
                exposed = sum(cat_no3_bin == 2),
                percent_exposed = exposed*100/count)
 
  # classify pwsids
  pws_exposed <- pws %>% 
    left_join(births_exposed_by_pwsid, by = "pwsid")

# plot
  
pws_map <- 
  ggplot(pws_exposed) +
  geom_sf(data = ca_counties_geo,
          col = "grey",
          fill = "white", lwd = 0.1) +
  geom_sf(aes(fill = percent_exposed, col = percent_exposed), size = 0.3) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # scale_fill_brewer(palette = "Set2") +
  # scale_color_brewer(palette = "Dark2") +
  # geom_sf(data = sjv_outline,
  #         col = "grey30", size = 0.7,
  #         fill = NA) +  theme_bw() + 
  geom_sf(data = ca_state_geo,
          col = "grey30", size = 0.6,
          fill = NA) +  theme_bw() + 
  labs(fill = "", col = "") +
  ditch_axes 

pws_map

ggsave(plot = pws_map, device = png(),
       filename = "data_out/4_visualizations/2_supplement/pws_nitrate_counties_sjv_map.png", 
       height = 10, width = 10, units = "in")


## SJV inset


pws_exceeds_sjv <- pws_exceeds %>% 
  filter(county %in% sjv_counties)
  # filter(st_is_valid(.)) %>% 
  # st_join(sjv_proj, join = st_within)

pws_map_sjv <- 
  ggplot(pws_exceeds_sjv) +
  geom_sf(aes(fill = category, col = category), size = 0.3) + 
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  geom_sf(data = sjv_outline,
          col = "grey30", size = 0.7,
          fill = NA) +  theme_bw() + 
  labs(fill = "", col = "") +
  ditch_axes 

ggsave(plot = pws_map_sjv, device = png(),
       filename = "data_out/4_visualizations/2_supplement/pws_nitrate_sjv_map.png", 
       height = 8, units = "in")


# what are the PWS missing from spon_nocomp population that are included in study pop?
pws_study_pop <- unique(births_study_pop_pws_final$pwsid)
pws_spnc <- unique(births_spon_nocomp_no3$pwsid)

pws_excluded <- pws_study_pop[!pws_study_pop %in% pws_spnc]
sum(pws_excluded %in% wq_data$pwsid)

wq_data_excluded <- wq_data %>% filter(pwsid %in% pws_excluded)
length(unique(wq_data_excluded$pwsid))
