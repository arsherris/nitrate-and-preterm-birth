births_exp <- exp_all %>% 
  bind_rows(tcp_cat) %>% 
  left_join(select(births, birth_id, starts_with("mat_"), precare, payer, tract_poverty, county), by = "birth_id")

  
# 1,000,000 births sample, 2005-2011
  
  # how many with successful PWS match?
  sum(is.na(births_samp_pws$pwsid)) / nrow(births_samp_pws)
  # 75,783 not matched to PWS (7.6%)
  
  # what percent matched by race?
  births_samp_pws %>% 
    mutate(pws_match = ifelse(is.na(pwsid), 0, 1)) %>% 
    group_by(mat_race) %>% 
    summarise(match_perc = mean(pws_match))
  
  # what percent can estimate exposure by chem?
  chem_summary <- births_exp %>% 
    mutate(conc_nona = ifelse(is.na(category), 0, 1)) %>% 
    group_by(chem) %>% 
    summarise(perc_exp = mean(conc_nona),
              cat_high = sum(category == "high", na.rm = T),
              cat_med = sum(category == "medium", na.rm = T),
              cat_low = sum(category == "low", na.rm = T),
              perc_high = cat_high / n(),
              perc_med = cat_med / n(),
              perc_high_exp = perc_high / perc_exp,
              perc_med_exp = perc_med / perc_exp) 
    

  
# how many chemicals are births exposed to?

births_exp_rev <- births_exp %>% 
  filter(!chem %in% c("Chromium (VI)", "Manganese")) 
  
mcl_by_birth <- births_exp_rev %>% 
  filter(category=="high") %>% 
  group_by(birth_id) %>% 
  mutate(chem_mcl = case_when(
    n() == 1 ~ chem,
    n() > 1 ~ "Multiple"),
    order = 1:n()) %>% 
  ungroup() %>% 
  filter(order == 1) %>% 
  select(birth_id, chem_mcl)

half_by_birth <- births_exp_rev %>% 
  filter(category!="low") %>% 
  group_by(birth_id) %>% 
  mutate(chem_half = case_when(
    n() == 1 ~ chem,
    n() > 1 ~ "Multiple"),
    order = 1:n()) %>% 
  ungroup() %>% 
  filter(order == 1) %>% 
  select(birth_id, chem_half)


table(mcl_by_birth$chem_mcl)
table(mcl_by_birth$chem_mcl)

mcl_summary <- mcl_by_birth %>% 
  group_by(chem_mcl) %>% 
  summarise(count = n(),
            percent = n() / 10000,
            x = "A")

mcl_summary$chem_mcl <- factor(mcl_summary$chem_mcl, 
                               levels = c("Multiple", "Uranium", "Nitrate", "Fluoride",
                                          "Gross Alpha", "Arsenic", "TCP"))                                   

plot_mcl <- ggplot(mcl_summary, aes(x = x, y = percent, fill = chem_mcl)) +
  geom_bar(stat='identity', position = "stack") +
  coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = guide_legend(reverse=T)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x = "", y = "Percent", fill = "Chemical")
plot_mcl

ggsave("./data_out/4_figures/mcl_bar.png", plot_mcl)


exp_by_birth <- births_exp %>%
  filter(!chem %in% c("Chromium (VI)", "Manganese")) %>% 
  group_by(birth_id) %>% 
  summarise(pwsid = first(pwsid),
            chems_est = sum(!is.na(category)),
            n_exceeds_mcl = sum(category == "high", na.rm=T),
            n_exceeds_half = sum(category %in% c("high", "medium"), na.rm=T)) %>% 
  ungroup %>% 
  mutate(exceeds_mcl = ifelse(n_exceeds_mcl > 0, 1, 0),
         exceeds_half = ifelse(n_exceeds_half > 0, 1, 0)) %>% 
  left_join(select(births, birth_id, starts_with("mat_"), precare, payer, tract_poverty, county), by = "birth_id") %>% 
  left_join(mcl_by_birth) %>% 
  left_join(half_by_birth) %>% 
  mutate(chem_mcl2 = case_when(
    chems_est == 0 ~ "Unknown",
    n_exceeds_mcl == 0 ~ "None",
    TRUE ~ chem_mcl),
    chem_half2 = case_when(
      chems_est == 0 ~ "Unknown",
      n_exceeds_half == 0 ~ "None",
      TRUE ~ chem_half))

View(head(exp_by_birth, 100))
table(exp_by_birth$chems_est)
table(exp_by_birth$exceeds_mcl)
table(exp_by_birth$n_exceeds_mcl)
table(exp_by_birth$exceeds_half)
table(exp_by_birth$n_exceeds_half)
table(exp_by_birth$chem_mcl2)
table(exp_by_birth$chem_mcl)

mcl_summary2 <- exp_by_birth %>% 
  group_by(chem_mcl2) %>% 
  summarise(count = n(),
            percent = n() / 10000,
            x = "A")

mcl_summary2$chem_mcl2 <- factor(mcl_summary2$chem_mcl2, 
                                 levels = c("Unknown", "None", "Multiple", "Uranium", "Nitrate", "Fluoride",
                                            "Gross Alpha", "Arsenic", "TCP"))                                   


(plot_mcl2 <- ggplot(mcl_summary2, aes(x = x, y = percent, fill = chem_mcl2)) +
  geom_bar(stat='identity', position = "stack") +
  coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = guide_legend(reverse=T)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x = "", y = "Percent", fill = "Chemical"))

ggsave("./data_out/4_figures/mcl_bar2.png", plot_mcl2)



# same for half


half_summary <- half_by_birth %>% 
  group_by(chem_half) %>% 
  summarise(count = n(),
            percent = n() / 10000,
            x = "A")

half_summary$chem_half <- factor(half_summary$chem_half, 
                               levels = c("Multiple", "Uranium", "Nitrate", "Fluoride",
                                          "Gross Alpha", "Arsenic", "TCP"))                                   

plot_half <- ggplot(half_summary, aes(x = x, y = percent, fill = chem_half)) +
  geom_bar(stat='identity', position = "stack") +
  coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = guide_legend(reverse=T)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x = "", y = "Percent", fill = "Chemical")
plot_half


ggsave("./data_out/4_figures/half_bar.png", plot_half)



half_summary2 <- exp_by_birth %>% 
  group_by(chem_half2) %>% 
  summarise(count = n(),
            percent = n() / 10000,
            x = "A")

half_summary2$chem_half2 <- factor(half_summary2$chem_half2, 
                                 levels = c("Unknown", "None", "Multiple", "Uranium", "Nitrate", "Fluoride",
                                            "Gross Alpha", "Arsenic", "TCP"))                                   


(plot_half2 <- ggplot(half_summary2, aes(x = x, y = percent, fill = chem_half2)) +
    geom_bar(stat='identity', position = "stack") +
    coord_flip() +
    scale_fill_viridis_d(direction = -1) +
    guides(fill = guide_legend(reverse=T)) +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 18)) +
    labs(x = "", y = "Percent", fill = "Chemical"))

ggsave("./data_out/4_figures/half_bar2_rev.png", plot_half2, width = 10.4, height = 1.86, units = "in")


## urban areas

urban_sp <- st_read("./data_in/polygon/CA_urban/Urban_CA.shp") %>% 
  mutate(urban = 1) %>% 
  select(urban) %>% 
  st_transform(crs_projected)

births_samp_urban <- births_samp_sp %>% 
  st_join(urban_sp, 
        join = st_intersects) %>%
  select(birth_id, urban) %>% 
  mutate(urban = replace_na(urban, 0)) %>% 
  st_drop_geometry %>% 
  # a few births are on border of two counties; select first
  group_by(birth_id) %>%
  mutate(count = 1:n()) %>%
  filter(count == 1) %>%
  select(-count) %>%
  ungroup









# find percentage over half for each 
births_half <- births_exp %>% 
  filter(!chem %in% c("Chromium (VI)", "Manganese")) %>% 
  mutate(exceeds_half = ifelse( category %in% c("medium", "high"), 1, 0)) %>% 
  select(birth_id, chem, exceeds_half) %>% 
  group_by(birth_id) %>% 
  mutate(sum_over_half = sum(exceeds_half, na.rm=T))  %>% 
  ungroup

table(births_half$sum_over_half) / 9

births_half_2 <- births_half %>% 
  filter(sum_over_half == 2, 
         exceeds_half == 1) %>%
  group_by(birth_id) %>% 
  mutate(order = 1:n()) %>% 
  ungroup %>% 
  select(-exceeds_half, -sum_over_half) %>% 
  spread(key = order, value = chem, fill = NA) %>% 
  rename(chem1 = `1`,
         chem2 = `2`) 

births_half_3 <- births_half %>% 
  filter(sum_over_half == 3, 
         exceeds_half == 1) %>% 
  group_by(birth_id) %>% 
  mutate(order = 1:n()) %>% 
  ungroup %>% 
  select(-exceeds_half, -sum_over_half) %>% 
  spread(key = order, value = chem, fill = NA) %>% 
  rename(chem1 = `1`,
         chem2 = `2`,
         chem3 = `3`) 

births_half_4 <- births_half %>% 
  filter(sum_over_half == 4, 
         exceeds_half == 1) %>% 
  group_by(birth_id) %>% 
  mutate(order = 1:n()) %>% 
  ungroup %>% 
  select(-exceeds_half, -sum_over_half) %>% 
  spread(key = order, value = chem, fill = NA) %>% 
  rename(chem1 = `1`,
         chem2 = `2`,
         chem3 = `3`,
         chem4 = `4`) 

births_half_5 <- births_half %>% 
  filter(sum_over_half == 5, 
         exceeds_half == 1) %>%
  group_by(birth_id) %>% 
  mutate(order = 1:n()) %>% 
  ungroup %>% 
  select(-exceeds_half, -sum_over_half) %>% 
  spread(key = order, value = chem, fill = NA) %>% 
  rename(chem1 = `1`,
         chem2 = `2`,
         chem3 = `3`,
         chem4 = `4`,
         chem5 = `5`) 

births_half_6 <- births_half %>% 
  filter(sum_over_half == 6, 
         exceeds_half == 1) %>% 
  group_by(birth_id) %>% 
  mutate(order = 1:n()) %>%
  ungroup %>% 
  select(-exceeds_half, -sum_over_half) %>% 
  spread(key = order, value = chem, fill = NA) %>% 
  rename(chem1 = `1`,
         chem2 = `2`,
         chem3 = `3`,
         chem4 = `4`,
         chem5 = `5`,
         chem6 = `6`) 

births_half_combos <- births_half_2 %>% 
  # 3 chems
  bind_rows(select(births_half_3, birth_id, chem1, chem2)) %>% 
  bind_rows(select(births_half_3, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(births_half_3, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  # 4 chems
  bind_rows(select(births_half_4, birth_id, chem1, chem2)) %>% 
  bind_rows(select(births_half_4, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(births_half_4, birth_id, chem1, chem2 = chem4)) %>% 
  bind_rows(select(births_half_4, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  bind_rows(select(births_half_4, birth_id, chem1 = chem2, chem2 = chem4)) %>% 
  bind_rows(select(births_half_4, birth_id, chem1 = chem3, chem2 = chem4)) %>% 
  # 5 chems
  bind_rows(select(births_half_5, birth_id, chem1, chem2 = chem2)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1, chem2 = chem4)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1, chem2 = chem5)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1 = chem2, chem2 = chem4)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1 = chem2, chem2 = chem5)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1 = chem3, chem2 = chem4)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1 = chem3, chem2 = chem5)) %>% 
  bind_rows(select(births_half_5, birth_id, chem1 = chem4, chem2 = chem5)) %>% 
  # 6 chems
  bind_rows(select(births_half_6, birth_id, chem1, chem2)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1, chem2 = chem4)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1, chem2 = chem5)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1, chem2 = chem6)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem2, chem2 = chem4)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem2, chem2 = chem5)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem2, chem2 = chem6)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem3, chem2 = chem4)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem3, chem2 = chem5)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem3, chem2 = chem6)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem4, chem2 = chem5)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem4, chem2 = chem6)) %>% 
  bind_rows(select(births_half_6, birth_id, chem1 = chem5, chem2 = chem6)) %>% 
  mutate(chem1_cat = ifelse(chem1 %in% c("Nitrate", "TCP"), "Anthro", "Geo"),
         chem2_cat = ifelse(chem2 %in% c("Nitrate", "TCP"), "Anthro", "Geo"),
         mix = case_when(chem1_cat == "Anthro" & chem2_cat == "Anthro" ~ "Anthropogenic",
                         chem1_cat == "Geo" & chem2_cat == "Geo" ~ "Geogenic",
                         chem1_cat != chem2_cat ~ "Both"))

# births_half_combos$chem1 <- factor(births_half_combos$chem1_final, levels = c("1,2,3-TCP",  "Nitrate",  "Other Anthropogenic",  "Arsenic", 
#                                                               "Chromium (VI)", "Fluoride", "Gross Alpha/Beta", "Uranium", "Other Geogenic"))
# births_half_combos$chem2 <- factor(births_half_combos$chem2_final, levels = c("1,2,3-TCP",  "Nitrate",  "Other Anthropogenic",  "Arsenic", 
#                                                    "Chromium (VI)", "Fluoride", "Gross Alpha/Beta", "Uranium", "Other Geogenic"))


births_half_combos$mix <- factor(births_half_combos$mix, levels = c("Anthropogenic", "Geogenic", "Both"))         
births_half_combos$chem1 <- factor(births_half_combos$chem1_final, levels = c("1,2,3-TCP",  "Arsenic", 
                                                                              "Fluoride", "Gross Alpha",  "Nitrate",   
                                                                              "Uranium",   "Other Anthropogenic", "Other Geogenic"))

births_half_combos$chem2 <- factor(births_half_combos$chem2_final, levels = c("1,2,3-TCP",  "Arsenic",  
                                                                              "Fluoride", "Gross Alpha",  "Nitrate",   
                                                                              "Uranium",   "Other Anthropogenic", "Other Geogenic"))

ggplot(births_half_combos) + 
  geom_count(mapping = aes(x = chem1, y = reorder(chem2, desc(chem2)), col = mix)) +
  labs(size = "Count",
       col = "Type of contaminants") +
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c("#F8766D", "#619CFF", "purple")) +
  #scale_size(limits = c(1,100), range = c(1,7), breaks = c(10, 50, 100)) +
  #guides(color = guide_legend(override.aes = list(size=4), order = 2)) +
  #scale_size_continuous(limits = c(1,120)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = -.1),
        axis.title = element_blank(),
        text = element_text(size = 18)) 

# summary of combinations

combo_summary <- births_half_combos %>% 
  mutate(combo = paste(chem1, "+", chem2)) %>% 
  group_by(combo) %>% 
  summarise(count = n(),
            percent = count / nrow(births_half_combos),
            x = "A") %>% 
  ungroup

(plot_combos <- ggplot(combo_summary, aes(x = x, y = percent, fill = reorder(combo, percent))) +
  geom_bar(stat='identity', position = "stack") +
  coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = guide_legend(reverse=T)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x = "", y = "Percent", fill = "Chemical combination"))



# what factors are associated with exposure?

exposure_by_birth$mat_race <- factor(exposure_by_birth$mat_race, levels = c("White", "Hispanic", "Black", "Asian", "Other" ))
exposure_by_birth$mat_educ <- factor(exposure_by_birth$mat_educ, levels = c("12 years", "<12 years", ">12 years"))

glm_mcl <- glm(exceeds_mcl ~ county + precare + mat_race + mat_educ, 
            data = exposure_by_birth, 
            family = "binomial")

glm_half <- glm(exceeds_half ~ county + precare + mat_race + mat_educ, 
               data = exposure_by_birth, 
               family = "binomial")

summary(glm_mcl)
summary(glm_half)

pws_chars <- read_csv("./data_in/tabular/PWS information/PWS_characteristics.csv") %>% 
  group_by(pwsid) %>% 
  mutate(order = 1:n()) %>% 
  ungroup() %>% 
  filter(order ==1) %>% 
  select(-order)

library(caret)
library(pdp)
library(vip)
library(randomForest)
library(party)


# randomforest 
data <- exposure_by_birth %>% 
  left_join(pws_chars, by = "pwsid") %>% 
  select(exceeds_mcl, exceeds_half, mat_race, mat_educ, precare, fee_code:sources, avg_hh_size, 
         pHwater2m25:region) %>% 
  mutate_at(vars(exceeds_mcl:land, region), .funs = as.factor) %>% 
  na.omit

data_mcl <- data %>% 
  select(-exceeds_half)

data_half <- data %>% 
  select(-exceeds_mcl)

set.seed(1)
train_index <- createDataPartition(data_mcl$exceeds_mcl, p = .8, 
                                   list = FALSE, 
                                   times = 1)

train_data <- data_mcl[train_index,]
test_data <- data_mcl[-train_index, ]

randfo_anymcl <- randomForest(factor(exceeds_mcl) ~ . , data = train_data,  ntree = 500, mtry = 5, importance = TRUE)

randfo_anymcl
# predictive power
any_mcl_predict <- predict(randfo_anymcl, test_data)
confusionMatrix(any_mcl_predict, as.factor(test_data$exceeds_mcl))


# variable importance

importance(randfo_anymcl)

# partial dependency
partialPlot(randfo_anymcl, pred.data = train_data, x.var = "coarsefract")


# conditional tree
library(party)
ctree_anymcl <- ctree(factor(exceeds_mcl) ~ . , data = train_data, )
Sys.time()
plot(ctree_anymcl)
# predictive power
ctree_anymcl_predict <- predict(ctree_anymcl, test_data)
confusionMatrix(ctree_anymcl_predict, as.factor(test_data$exceeds_mcl))


# exceeds half

set.seed(1)
train_index <- createDataPartition(data_half$exceeds_half, p = .8, 
                                   list = FALSE, 
                                   times = 1)

train_data <- data_half[train_index,]
test_data <- data_half[-train_index, ]

randfo_anyhalf <- randomForest(factor(exceeds_half) ~ . , data = train_data,  ntree = 500, mtry = 5, importance = TRUE)

randfo_anyhalf
# predictive power
any_half_predict <- predict(randfo_anyhalf, test_data)
confusionMatrix(any_half_predict, as.factor(test_data$exceeds_half))


# variable importance
varImp(randfo_anyhalf)

# partial dependency
partialPlot(randfo_anyhalf, pred.data = train_data, x.var = "coarsefract")


# conditional tree
ctree_anyhalf <- ctree(factor(exceeds_half) ~ . , data = train_data, )
Sys.time()
plot(ctree_anyhalf)
# predictive power
ctree_anyhalf_predict <- predict(ctree_anyhalf, test_data)
confusionMatrix(ctree_anyhalf_predict, as.factor(test_data$exceeds_half))








# what is the association between chemical concentrations
corr_dat <- exp_all_p1 %>% 
  select(birth_id, conc, chem) %>% 
  spread(key = "chem", value = conc) 

cor(corr_dat[,-1], use = "pairwise.complete.obs")
var(corr_dat[,-1], use = "pairwise.complete.obs")

