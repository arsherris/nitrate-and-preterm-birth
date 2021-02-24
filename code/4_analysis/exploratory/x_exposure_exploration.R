
births_exp <-  exp_all %>% 
  left_join(births, by = "birth_id") %>% 
  #select(birth_id:tract_poverty, county, lat, long) %>%
  unique() %>%
  mutate(chem = case_when(
    chem == "ARSENIC" ~ "Arsenic",
    chem == "NITRATE" ~ "Nitrate",
    chem == "URANIUM" ~ "Uranium",
    chem == "CHROMIUM, HEXAVALENT" ~ "Chromium",
    chem == "SELENIUM" ~ "Selenium",
    chem == "GROSS ALPHA" ~ "GrossAlpha"),
    mcl = case_when(
      chem == "Arsenic" ~ 10,
      chem == "Nitrate" ~ 45,
      chem == "Chromium" ~ 10,
      chem == "GrossAlpha" ~ 15,
      chem == "Selenium" ~ 50,
      chem == "Uranium" ~ 20),
    over_mcl = ifelse(conc > mcl, 1, 0),
    over_half_mcl = ifelse(conc > mcl/2, 1, 0),
    mat_race = case_when(
        mat_race == 1 ~ "White",
        mat_race == 2 ~ "Hispanic",
        mat_race == 3 ~ "Black",
        mat_race == 4 ~ "Asian",
        mat_race == 5 ~ "Other")) 

births_exp_save <- births_exp
births_exp <- births_exp %>% filter(!chem %in% c("GrossAlpha", "Selenium"))

save.image(file = "data091219.RData")

  
## missing data in full dataset 

  # how many women with missing data?
  sum(is.na(births$lat))
  
  sum(is.na(births$final_gest_wk))
  
  # lat/long - 3990
  # gestage - 493,107
  
  # county
  births_noc <- births_sp %>% filter(is.na(county))
  #37885 not matched to county
  ggplot(births_noc) + geom_sf()

# 1,500,000 births sample
  
  # how many with successful PWS match?
  length(unique(births_samp$birth_id)) - length(unique(births_pws$birth_id))
  # 119811 not matched to PWS (8%)
  
  dup_ids <- births_pws$birth_id[which(duplicated(births_pws$birth_id))]
  View(filter(births_pws, birth_id %in% dup_ids))
  # CA3200150 and CA3210010 overlap and have same area;
  # use 3210010 because serves more people
  # need to fix this in pws assignment - top n by area -1 then by pop +1
  

    # how many with multiple PWS? (from prior analysis)
  pwsids <- births_pws %>% group_by(birth_id) %>% 
    mutate(count = n(),
           order = 1:n()) %>% 
    filter(order == 1) %>% 
    group_by(count) %>% 
    summarise(total = n())
  # one pwsid: 91.5 
  # two: 7.7 
  # three: 0.107 
  # four: 0.0174%
  
  multi_pws <-  births_exp_final %>% 
    group_by(birth_id) %>% 
    arrange(area) %>% 
 #   select(-nitrate, -area, -pwsid) %>% 
    mutate(count = n(),
           order = 1:n()) %>% 
    filter(count == 2) #%>% 
  #  spread(key = order, value = arsenic)


  multi_pws <- multi_pws %>% 
    mutate(over1 = ifelse(`1` > 10, 1, 0),
           over2 = ifelse(`2` > 10, 1, 0))  
  t.test(multi_pws$`1`, multi_pws$`2`, paired = T)
  
  
  table(multi_pws$over1, multi_pws$over2, useNA = "a")
  sum(is.na(multi_pws$over1))
  sum(is.na(multi_pws$over2))
  
  # 2nd (larger) pwsid has many more that are null
  # first (smaller) pwsid has more that are over MCL
  497/(33620+497)
  86/(26332 + 86)
  
  plot(multi_pws$`1`, multi_pws$`2`)

  ## sample of 500000
    
  births_pws %>% 
    mutate(pws_match = ifelse(is.na(pwsid), 0, 1)) %>% 
    group_by(mat_race) %>% 
    summarise(match_perc = mean(pws_match))
  
table(births_pws_comp$mat_race, births_pws_comp$pws_match)

table(births_as_samp$mat_race, births_as_samp$conc_match)

births_as_samp %>% 
  mutate(conc_match = ifelse(is.na(conc), 0, 1)) %>% 
  group_by(mat_race) %>% 
  summarise(conc_perc = mean(conc_match))

  # how many have data for each contaminant?
  
  births_exp %>% 
    group_by(chem) %>% 
    summarise(count = n()) %>% 
    mutate(percent = count / 1380201)

  
  # 1 ARSENIC              1270730   0.921
  # 2 CHROMIUM, HEXAVALENT  826778   0.599
  # 3 GROSS ALPHA          1207826   0.875
  # 4 NITRATE              1285558   0.931
  # 5 SELENIUM             1269186   0.920
  # 6 URANIUM               977682   0.708
  
  
# how many women have all contaminants?
  
  births_exp %>% 
    group_by(birth_id) %>% 
    summarise(count = n()) %>% 
    group_by(count) %>% 
    summarise(final = n())
  # 675482 have 6 contaminants  

  
# tap water by county
  
  # ggplot() +
  #   geom_sf(data = ca_counties) +
  #   geom_sf(data = select(births_no_county, birth_id))
  
  # chem = case_when(
  #   chem == "ARSENIC" ~ "Arsenic",
  #   chem %in% c("NITRATE (AS NO3)", "NITRATE (AS N)") ~ "Nitrate",
  #   chem == "NITRITE (AS N)" ~ "Nitrite",
  #   chem == "URANIUM (PCI/L)" ~ "Uranium",
  #   chem == "CHROMIUM, HEXAVALENT" ~ "Chromium",
  #   chem == "GROSS ALPHA" ~ "GrossAlpha",
  #   chem == "SELENIUM" ~ "Selenium",
  #   chem == "MERCURY" ~ "Mercury",
  #   chem == "RADIUM 228" ~ "Radium228",
  #   chem == "RADIUM 226" ~ "Radium226",
  #   TRUE ~ chem)) 
  #   
    
  births_exp <- births_exp_save %>% 
    select(birth_id:tract_poverty, county, lat, long) %>%
    unique()
  
  ## a few women excluded because just outside coast border; can be included manually
  
  # how many in each county
  table(births_exp$county, useNA = "a")
  table(births_sjv$county, useNA = "a")
  
  
  # plot concentrations
  ggplot(births_sjv, aes(y = log(conc), x = chem, fill = county)) + 
    geom_boxplot() 
  
  county_means <- births_exp %>% 
    group_by(county, chem) %>% 
    summarise(count = n(),
              mean = mean(conc),
              median = median(conc))
      
  # how many in each year?
  years <-  births_exp %>% 
    mutate(year = year(bdate)) %>% 
    group_by(year, chem) %>% 
    summarise(count = n())
  
  years_births <-  births %>% 
    mutate(year = year(bdate)) %>% 
    group_by(year) %>% 
    summarise(count = n())
  
  
# how many are over the MCL for each contaminant?
  #n_pop <- 178778
  n_pop <- 1500000

exceeds <- births_exp %>% 
  #filter(county %in% sjv_counties) %>% # remove if statewide
  group_by(chem) %>% 
  summarise(count = n(),
            exceeds_mcl = sum(over_mcl),
            exceeds_half = sum(over_half_mcl),
            perc_data = count / n_pop) %>% 
  ungroup %>% 
  mutate(mcl_perc_all = exceeds_mcl*100 / n_pop,
         half_perc_all = exceeds_half*100 / n_pop,
         mcl_perc_data = exceeds_mcl*100 / count,
         half_perc_data = exceeds_half*100 / count) 

exceeds_any <- births_exp_multi %>%
#  filter(county %in% sjv_counties) %>% # remove if statewide
  summarise(count = n(),
            exceeds_mcl = sum(n_over_mcl > 0),
            exceeds_half = sum(n_over_half > 0),
            perc_data = count / n_pop) %>% 
  mutate(chem = "Any Chemical",
         mcl_perc_all = exceeds_mcl*100 / n_pop,
         half_perc_all = exceeds_half*100 / n_pop,
         mcl_perc_data = exceeds_mcl*100 / count,
         half_perc_data = exceeds_half*100 / count) %>% 
  select(chem, everything())
  
exceeds2 <- rbind(exceeds, exceeds_any) %>% 
  select(chem, mcl_perc_all, half_perc_all) %>% 
  gather(ends_with("all"), key = "category", value = "percent") %>% 
  mutate(facet = chem == "Any Chemical",
         Chemical = factor(chem, levels = c("Chromium", "Arsenic", "Nitrate", "Uranium", "Any Chemical")),
         Category = factor(category, 
                           levels = c("mcl_perc_all", "half_perc_all"), 
                           labels = c("Over the MCL", "Over half the MCL")))

# ggplot(exceeds) +
#   geom_bar(aes(x = reorder(chem, -mcl_perc_all), weight = half_perc_all)) +
#   geom_bar(aes(x = reorder(chem, -mcl_perc_all), weight = mcl_perc_all), fill = "blue")


ggplot(exceeds2, aes(x = Chemical, fill = Category, weight = percent)) +
  geom_bar(position = "dodge") +
  facet_grid(~facet, scales = "free_x", space = "free_x") +   
  labs(y = "Percent",
       fill = "") +
  scale_fill_manual(values = c("blue", "grey60")) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25))+
  theme_classic()+
  theme(text = element_text(size = 16),
        strip.text = element_blank()) 
  
## individual plots

ggplot(filter(exceeds2, Chemical != "Any Chemical"), aes(x = chem, fill = Category, weight = percent)) +
  geom_bar(position = "dodge") +
  labs(y = "Percent",
       x = "Chemical",
       fill = "") +
  scale_fill_manual(values = c("blue", "grey60")) +
  #scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25))+
  theme_classic()+
  theme(text = element_text(size = 16),
        strip.text = element_blank()) 


## Pop exposed frequency vs number of contaminants in tap water
n_pop <- 178778
n_pop <- 1500000

over_mcl_freq <- births_exp_multi %>% 
#  filter(county %in% sjv_counties) %>% # remove if statewide
  group_by(n_over_mcl) %>% 
  summarise(count = n()) %>% 
  rename(n_over = n_over_mcl) %>% 
  mutate(category = "mcl")
  
over_half_freq <- births_exp_multi %>% 
#  filter(county %in% sjv_counties) %>% # remove if statewide
  group_by(n_over_half) %>% 
  summarise(count = n())%>% 
  rename(n_over = n_over_half) %>% 
  mutate(category = "half")

over_freq <- bind_rows(over_mcl_freq, over_half_freq) %>% 
  filter(n_over > 0) %>% 
  mutate(frequency = count*100 / n_pop,
         category = factor(category, labels = c("Over half the MCL", "Over the MCL")))

over_freq$cum_freq <- c(8.3055333,
                        0.1266,
                        27.6191334,
                        4.4414667,
                        0.4326667,
                        0.0066)

ggplot(over_freq, aes(x = n_over, y = frequency, col = category)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  #scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25))+
  scale_color_manual(values = c("grey60", "blue")) +
  labs(x = "Number of contaminants above threshold",
       y = "Frequency of occurence in \nstudy population tap water",
       col = "Category") +
  theme(text = element_text(size = 14))
  










# how many pwsids are mapped?
length(unique(pws_sp$pwsid))
length(unique(wq_data_raw$pwsid))
sum(!unique(wq_data_raw$pwsid) %in% pws_sp$pwsid)
# 338 out of 2809 are excluded that are CWS based on drinc (may be wholesaler)
excluded <- unique(wq_data_raw$pwsid[which(!wq_data_raw$pwsid %in% pws_sp$pwsid)])




## multiple exceedances for births
births_exp_multi <- births_exp %>%
  arrange(chem) %>% 
  group_by(birth_id) %>% 
  summarise(mat_race = first(mat_race),
            mat_age = first(mat_age),
            mat_educ = first(mat_educ),
            mat_id = first(mat_id),
            county = first(county),
            lat = first(lat),
            long = first(long),
            n_over_mcl = sum(over_mcl),
            n_over_half = sum(over_half_mcl),
            list_over_mcl = list(chem[over_mcl == 1]),
            list_over_half = list(chem[over_half_mcl == 1])) %>% 
  ungroup


table(births_exp_multi$n_over_mcl)
table(births_exp_multi$n_over_half)

View(births_exp_multi$list_over_mcl[births_exp_multi$n_over_mcl == 2])

list_mcl <- data.frame(table(as.character(births_exp_multi$list_over_mcl)))
names(list_mcl) <- c("Chemical", "Frequency")

ggplot(list_mcl[2:7,]) +
  geom_bar(aes(x = reorder(Chemical, -Frequency), weight = Frequency))

list_over_half <- data.frame(table(as.character(births_exp_multi$list_over_half)))
names(list_over_half) <- c("Chemical", "Frequency")










# how does exposure change over time?
births_over_time <- births %>% 
  mutate(year = year(bdate)) %>% 
  group_by(year) %>% 
  summarise(count = n())
            
ggplot(births_over_time, aes(x = year, y = count)) + geom_line()

births_samp_over_time <- births_samp_ids %>%
  select(-geometry) %>% 
  left_join(select(births, birth_id, bdate)) %>% 
  unique() %>% 
  mutate(year = year(bdate)) %>% 
  group_by(year) %>% 
  summarise(count = n())

exp_over_time2 <- births_exp %>%
  mutate(year = year(bdate)) %>% 
  group_by(year, chem) %>% 
  summarise(mean = mean(conc),
            sd = sd(conc),
            over_mcl = sum(over_mcl),
            over_half = sum(over_half_mcl)) %>% 
  left_join(births_samp_over_time, by = "year") %>% 
  mutate(prop_over_mcl = over_mcl / count,
         prop_over_half = over_half / count,
         se = sd / sqrt(count))
  
ggplot(filter(exp_over_time2, !chem %in% c("GrossAlpha", "Selenium")), aes(x = year, y = prop_over_mcl, col = chem)) +
  geom_line() +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  labs(x = "Year", y = "Proportion over MCL", col = "Chemical")

ggplot(filter(exp_over_time2, !chem %in% c("GrossAlpha", "Selenium")), 
       aes(x = year, y = prop_over_half, col = chem)) +
  geom_line() +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  labs(x = "Year", y = "Proportion over half MCL", col = "Chemical")

ggplot(exp_over_time, aes(x = year, y = count, col = chem)) +
  geom_line()







# who so few arsenic exposed in sibling dataset compared with random sample?
births_exp_as <- filter(births_exp, chem == "Arsenic")

table(sibs_exp_all$conc)
table(sibs_exp_all$parity)*100/nrow(sibs_exp_all)
table(births_exp_as$parity)*100/nrow(births_exp_as)

hist(births_exp_as$conc, xlim = c(0, 10), breaks = 400)
hist(sibs_exp_all$conc)

sum(sibs_exp_all$conc>10, na.rm = T)


#compare arsenic estimates between births samp and sib datset

sibs_comp <- select(sibs_exp_all, birth_id, pwsid, conc) %>% 
  left_join(select(births_exp_as, birth_id, conc_orig = conc)) %>%  
  unique

sibs_comp$same = ifelse(sibs_comp$conc == sibs_comp$conc_orig, 1, 0)

# why is B20052nPaIVtHTe so different?

View(wq_data %>% filter(pwsid == "CA1610003", 
                        date >= "2004-08-04" & date <= "2005-04-19", 
                        chem == "Arsenic"))


View(wq_data_raw %>% filter(pwsid == "CA1610003", 
                        SAMP_DATE >= "2004-08-04" & SAMP_DATE <= "2005-04-19", 
                        CHEMICAL == "ARSENIC"))

View(flow_paths_raw %>% filter(pwsid == "CA1610003"))
