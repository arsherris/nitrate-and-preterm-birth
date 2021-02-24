sum(is.na(sibs_all$parity_cat))


# how often does prenatal care use change between siblings?

table(sibs_all$precare, useNA = "a")

sibs_precare_disc <- sibs_discordant %>% 
  filter(mat_id %in% mat_id[precare == "Yes"],
         mat_id %in% mat_id[precare == "No"])

length(unique(sibs_discordant$mat_id))
length(unique(sibs_payer_disc$mat_id))

51300/689895
7738/87518

# how often does payer delivery care use change between siblings?

table(sibs_all$payer, useNA = "a")

sibs_payer_disc <- sibs_discordant %>% 
  group_by(mat_id) %>% 
  summarise(n_payer = length(unique(payer))) %>% 
  filter(n_payer > 1)

length(unique(sibs_discordant$mat_id))
length(unique(sibs_payer_disc$mat_id))

51300/689895
7738/87518

# are these changes correlated to niitrate?


# what percent of discordant sibs are mixed early/late

ids_term  <- unique(sibs_discordant$mat_id[sibs_discordant$prem_5cat_spon_nocomp == 5])
ids_late  <- unique(sibs_discordant$mat_id[sibs_discordant$prem_32_to_36 == 1])
ids_early <- unique(sibs_discordant$mat_id[sibs_discordant$prem_20_to_31 == 1])

sets_term_only <- sibs_discordant %>% 
  filter(mat_id %in% ids_term,
         !mat_id %in% ids_late,
         !mat_id %in% ids_early)

sets_term_late  <- sibs_discordant %>% 
  filter(mat_id %in% ids_term,
         mat_id %in% ids_late)

sets_term_early <- sibs_discordant %>% 
  filter(mat_id %in% ids_term,
         mat_id %in% ids_early)

length(unique(sets_term_early$mat_id))
sum(unique(sets_term_early$mat_id) %in% sets_term_late$mat_id)

sets_term_early_late <- sibs_discordant %>% 
  filter(mat_id %in% ids_term,
         mat_id %in% ids_late,
         mat_id %in% ids_early)

length(unique(sets_term_early_late$mat_id)) / length(unique(sibs_discordant$mat_id))

set_early_late <-  sibs_discordant %>% 
  filter(!mat_id %in% ids_term,
         mat_id %in% ids_late,
         mat_id %in% ids_early)

length(unique(set_early_late$mat_id)) 


sets_term_early_late %>% 
  group_by(mat_id) %>% 
  summarise(kids=n()) %>% 
  ungroup %>% 
  count(kids)


