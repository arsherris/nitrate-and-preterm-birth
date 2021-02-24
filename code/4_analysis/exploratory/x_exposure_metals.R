metal_summary <- exp_metals %>% 
  group_by(chem) %>% 
  summarise(Low = sum(category == "low", na.rm = T),
            Medium = sum(category == "medium", na.rm = T),
            High = sum(category == "high", na.rm = T),
            Unknown = sum(is.na(category)),
            preg = sum(interval == "pregnancy", na.rm = T),
            med_samps = median(sample_n, na.rm = T)) %>% 
  ungroup() %>% 
  gather(Low:Unknown, key = category, value = count) %>% 
  filter(!chem %in% c("Chromium (Total)", "Selenium")) %>% 
  mutate(category = factor(category, 
                           levels = c("Unknown", "Low", "Medium", "High")),
         chem = factor(chem,
                       levels = c("Manganese", "Iron", "Chromium (VI)", "Arsenic", "Uranium",
                                  "Vanadium"))) 


ggplot(metal_summary, aes(x = reorder(chem, desc(chem)), y = count/10000, fill = category)) +
  geom_bar(stat='identity', position = "stack") +
  scale_fill_viridis_d(direction = -1) +
  coord_flip() +
  labs(x = "Chemical", y = "Percent", fill = "Exposure category") +
  theme_classic() +
  theme(text = element_text(size= 16))



# combinations of exposure


metal_half <- exp_metals %>% 
  filter(!chem %in% c("Chromium (Total)", "Selenium")) %>% 
  mutate(exceeds_half = ifelse( category %in% c("medium", "high"), 1, 0)) %>% 
  select(birth_id, chem, exceeds_half) %>% 
  group_by(birth_id) %>% 
  mutate(sum_over_half = sum(exceeds_half, na.rm=T))  %>% 
  ungroup

table(metal_half$sum_over_half) / 6

metal_half_2 <- metal_half %>% 
  filter(sum_over_half == 2, 
         exceeds_half == 1) %>%
  group_by(birth_id) %>% 
  mutate(order = 1:n()) %>% 
  ungroup %>% 
  select(-exceeds_half, -sum_over_half) %>% 
  spread(key = order, value = chem, fill = NA) %>% 
  rename(chem1 = `1`,
         chem2 = `2`) 

metal_half_3 <- metal_half %>% 
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

metal_half_4 <- metal_half %>% 
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

metal_half_5 <- metal_half %>% 
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

metal_half_combos <- metal_half_2 %>% 
  # 3 chems
  bind_rows(select(metal_half_3, birth_id, chem1, chem2)) %>% 
  bind_rows(select(metal_half_3, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(metal_half_3, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  # 4 chems
  bind_rows(select(metal_half_4, birth_id, chem1, chem2)) %>% 
  bind_rows(select(metal_half_4, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(metal_half_4, birth_id, chem1, chem2 = chem4)) %>% 
  bind_rows(select(metal_half_4, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  bind_rows(select(metal_half_4, birth_id, chem1 = chem2, chem2 = chem4)) %>% 
  bind_rows(select(metal_half_4, birth_id, chem1 = chem3, chem2 = chem4)) %>% 
  # 5 chems
  bind_rows(select(metal_half_5, birth_id, chem1, chem2 = chem2)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1, chem2 = chem3)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1, chem2 = chem4)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1, chem2 = chem5)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1 = chem2, chem2 = chem3)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1 = chem2, chem2 = chem4)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1 = chem2, chem2 = chem5)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1 = chem3, chem2 = chem4)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1 = chem3, chem2 = chem5)) %>% 
  bind_rows(select(metal_half_5, birth_id, chem1 = chem4, chem2 = chem5))

# metal_half_combos$chem1 <- factor(metal_half_combos$chem1_final, levels = c("1,2,3-TCP",  "Nitrate",  "Other Anthropogenic",  "Arsenic", 
#                                                               "Chromium (VI)", "Fluoride", "Gross Alpha/Beta", "Uranium", "Other Geogenic"))
# metal_half_combos$chem2 <- factor(metal_half_combos$chem2_final, levels = c("1,2,3-TCP",  "Nitrate",  "Other Anthropogenic",  "Arsenic", 
#                                                    "Chromium (VI)", "Fluoride", "Gross Alpha/Beta", "Uranium", "Other Geogenic"))


ggplot(metal_half_combos, aes(x = chem1, y = reorder(chem2, desc(chem2)))) + 
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend') +
  labs(size = "Count", color = "Count") +
  scale_x_discrete(position = "top") +
  scale_size(range = c(1,10)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = -.1),
        axis.title = element_blank(),
        text = element_text(size = 18)) 

# summary of combinations

combo_summary <- metal_half_combos %>% 
  mutate(combo = paste(chem1, "+", chem2)) %>% 
  group_by(combo) %>% 
  summarise(count = n(),
            percent = count / nrow(metal_half_combos),
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

