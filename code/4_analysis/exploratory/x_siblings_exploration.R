# Change in exposure between siblings

#how many women and births in whole sample?
length(unique(births$birth_id)) #5729972
length(unique(births$mat_id)) #4368706


# how many siblings eligible?

sibs_exp %>% group_by(mat_id) %>% 
  summarise(count = n()) %>% 
  group_by(count) %>% 
  summarise(total = n())

# how many siblings with usable data?

sibs_exp <- sibs_exp %>% 
  group_by(mat_id) %>% 
  mutate(n_births = n(),
         o_births = 1:n()) %>% 
  ungroup() %>% 
  filter(n_births != 1)
  
  # how many with PWS match?
  sum(!is.na(sibs_exp$pwsid)) / nrow(sibs_exp) # 92.1%
  # how many sibs witha arsenic data?
  sum(!is.na(sibs_exp$conc)) / nrow(sibs_exp) # 92.1%
  # what percent of matched have arsenic data?
  sum(!is.na(sibs_exp$conc)) / sum(!is.na(sibs_exp$pwsid)) # 92.1%
  

# how many mothers move between births?
sibs_exp_change <- sibs_exp %>% 
  arrange(mat_id, bdate) %>% 
  mutate(as_cat = case_when(
    conc >= mcl ~ "high",
    conc >= mcl/2 ~ "medium",
    conc < mcl / 2 ~ "low",
    TRUE ~ NA_character_)) %>%
  group_by(mat_id) %>% 
  mutate(n_births = n(),
         o_births = 1:n(),
         moved_pws = case_when(
           o_births == 1 ~ "first birth",
           is.na(lag(pwsid)) & is.na(pwsid) ~ "off grid",
           is.na(lag(pwsid)) & !is.na(pwsid) ~ "moved to grid",
           !is.na(lag(pwsid)) & is.na(pwsid) ~ "moved off grid",
           lag(pwsid) != pwsid ~ "changed PWS",
           lag(pwsid) == pwsid ~ "stayed in PWS"),
         moved_county = case_when(
           o_births == 1 ~ "first birth",
           lag(county) != county ~ "moved counties",
           lag(county) == county ~ "stayed in county"),
         as_cat_change = case_when(
           o_births == 1 ~ "first birth",
           is.na(lag(as_cat)) & is.na(as_cat) ~ "Missing both",
           is.na(lag(as_cat)) | is.na(as_cat) ~ "Missing one",
           lag(as_cat) == "high" & as_cat =="medium" ~ "Decrease",
           lag(as_cat) == "medium" & as_cat =="low" ~ "Decrease",
           lag(as_cat) == "high" & as_cat =="low" ~ "Decrease",
           lag(as_cat) == "medium" & as_cat =="high" ~ "Increase",
           lag(as_cat) == "low" & as_cat =="medium" ~ "Increase",
           lag(as_cat) == "low" & as_cat =="high" ~ "Increase",
           lag(as_cat) == as_cat ~ "No Change"))
  
  table(sibs_exp_change$moved_pws, sibs_exp_change$o_births)
  table(sibs_exp_change$moved_county)
  table(sibs_exp_change$o_births, sibs_exp_change$moved_county)
  table(sibs_exp_change$as_cat_change)
  View(table(sibs_exp_change$o_births, sibs_exp_change$as_cat_change))
  table(sibs_exp_change$as_cat_change, sibs_exp_change$moved_pws)
  table(sibs_exp_change$as_cat_change, sibs_exp_change$moved_county)
  
  sibs_any_change <- sibs_exp_change %>% 
    filter(!is.na(conc)) %>% 
    summarise(count = n(),
              mat_age = first(mat_age),
            mat_race = first(mat_race),
            mat_educ = first(mat_educ),
            county = first(county),
            n_cats = length(unique(as_cat))) %>% 
    ungroup

  table(sibs_any_change$n_cats)
  table(sibs_any_change$count)
  
  
  
# sibs_exp_wide <- sibs_exp_as %>% 
#   filter(o_births == 1 | o_births == n_births) %>% 
#   mutate(first_birth = ifelse(o_births == n_births, "first", "last")) %>% 
#   select(mat_id, mat_race, mat_age, mat_educ, sex, bdate, tract_poverty, county, 
#          lat, long, conc, over_mcl, over_half_mcl, first_birth) %>% 
#   unite(all, mat_age:over_half_mcl) %>% 
#   spread(key = first_birth, value = all)
  
  

# consecutive births

sibs_exp_comp <- sibs_exp_as %>%
  left_join(select(sibs_pws, birth_id, pwsid)) %>% 
  ungroup() %>% 
  filter(o_births == 1 | o_births == 2) %>% 
  # mutate(first_birth = ifelse(o_births == n_births, "first", "last"),
  mutate(year =  year(bdate),
         as_cat = cut(conc, c(0,5,10,500), labels = c("low","medium","high"))) %>%
  group_by(mat_id) %>% 
  arrange(o_births) %>% 
  mutate(
    lat2 = lat[2], 
    long2 = long[2],
    pwsid2 = pwsid[2],
    county2 = county[2],
    conc2 = conc[2],
    as_cat2 = as_cat[2],
    as_change = conc2 - conc,
    as_cat_diff = ifelse(as_cat==as_cat2, "Same As Category", "Different As Category"),
    as_cat_change = case_when(
      as_cat == "high" & as_cat2 =="medium" ~ "Decrease",
      as_cat == "medium" & as_cat2 =="low" ~ "Decrease",
      as_cat == "high" & as_cat2 =="low" ~ "Decrease",
      as_cat == "medium" & as_cat2 =="high" ~ "Increase",
      as_cat == "low" & as_cat2 =="medium" ~ "Increase",
      as_cat == "low" & as_cat2 =="high" ~ "Increase",
      as_cat == as_cat2 ~ "No Change",
      TRUE ~ NA_character_),
    IPI = (bdate[2] - bdate[1])/365) %>% # evalute interpregnancy interval
  filter(o_births == 1)


# how many women change counties?
sum(sibs_exp_comp$county != sibs_exp_comp$county2) / nrow(sibs_exp_comp)

# how many women change pwsids?
sum(sibs_exp_comp$pwsid != sibs_exp_comp$pwsid2) / nrow(sibs_exp_comp)


table(sibs_exp_change$mat_race, sibs_exp_change$as_cat_change)


table(sibs_exp_wide$as_cat_change)
table(sibs_exp_wide$mat_race, sibs_exp_wide$as_cat_change)

table(sibs_exp_wide$as_cat, sibs_exp_wide$as_cat2)

hist(sibs_exp_wide$as_change, breaks = 400, xlim = c(-20,20),
     col = "lightsteelblue3",
     xlab = "Change in arsenic concentration in tap water between pregnancies (ug/L)",
     main = "")





ggplot(sibs_exp_wide) +
  geom_density(aes(x =as_change), alpha = 0.5, col = "red", fill =  "red") +
  xlim(-20,20) +
  labs(x= "",
       y ="Density") +
  theme_classic() + 
  theme(text = element_text(size = 20))

ggplot(sibs_exp_wide) +
  geom_density(aes(x =NO3Change), alpha = 0.5, col = "blue", fill =  "blue") +
  xlim(-40,40) +
  labs(x= "",
       y ="Density") +
  theme_classic() + 
  theme(text = element_text(size = 20))

sibs_exp_wide$year <- year(sibs_exp_wide$bdate)

summary(lm(as_change ~ year + race + medu_3cat + mothage + IPI, data = sibs_exp_wide))

sibs_exp_wide %>% group_by(race) %>% summarise(as_changeM = mean(as_change, na.rm=T))

# differences in outcomes betwreen first and second 


ggplot(sibs_exp_wide, aes(x = no3_change, y = gest_change)) + geom_point()
ggplot(sibs_exp_wide, aes(x = medu_3cat, y = gest_change, col = as_cat_change)) + geom_boxplot()
ggplot(sibs_exp_wide, aes(x = race, y = no3_change)) + geom_boxplot()

# table(sibs_exp_wide$premcat, sibs_exp_wide$premcat2)
# table(sibs_exp_wide$ispreterm, sibs_exp_wide$ispreterm2)
# hist(sibs_exp_wide$gest_change, xlim = c(-150, 150), breaks= 30)
# hist(sibs_exp_wide$gest_change[sibs_exp_wide$ispreterm==1], xlim = c(-150, 150), breaks= 100)
# 
# library(ggplot2)
# sibs_exp_wide$anypreterm <- ifelse(sibs_exp_wide$ispreterm == 1 | sibs_exp_wide$ispreterm2 == 1, 1, 0)
# sibs_exp_wide$bothpreterm <- ifelse(sibs_exp_wide$ispreterm == 1 & sibs_exp_wide$ispreterm2 == 1, 1, 0)
# 
# 
# ggplot(sibs_exp_wide, aes(x =gest_change, color = as.factor(bothpreterm), fill = as.factor(bothpreterm))) +
#   geom_density(alpha = 0.5) +
#   labs(x="Change in length of gestation between pregnancies",
#        y ="Density") +
#   theme_classic() + 
#   theme(text = element_text(size = 14)) +
#   scale_fill_manual(values = c("red", "blue"),
#                     name = "",
#                     labels = c("Neither sampling preterm", "Both samplings preterm")) +
#   scale_color_manual(values = c("red", "blue"),
#                      name = "",
#                      labels = c("Neither sampling preterm", "Both samplings preterm")) 
# 
# ggplot(sibs_exp_wide, aes(x =gest_change, color = as.factor(premcat), fill = as.factor(premcat))) +
#   geom_density(alpha = 0.5) +
#   labs(x="Change in length of gestation between pregnancies",
#        y ="Density") +
#   theme_classic() + 
#   theme(text = element_text(size = 14)) 
# 
# ggplot(sibs_exp_wide, aes(x =gest_change, color = as.factor(pretermDiff), fill = as.factor(pretermDiff))) +
#   geom_density(alpha = 0.4) +
#   labs(x="Change in length of gestation between pregnancies",
#        y ="Density") +
#   theme_classic() + 
#   theme(text = element_text(size = 14)) + 
#   scale_fill_manual(values = c("red", "purple", "orange", "green3")) +
#   scale_color_manual(values = c("red", "purple", "orange", "green3"))
# 
# ggplot(sibs_exp_wide, aes(x =gest_change)) +
#   geom_density(fill = "grey") +
#   labs(x="Change in length of gestation between pregnancies",
#        y ="Density") +
#   theme_classic() + 
#   theme(text = element_text(size = 14)) 
# 
# mean(sibs_exp_wide$gest_change[sibs_exp_wide$bothpreterm==1], na.rm=T)
# mean(sibs_exp_wide$gest_change[sibs_exp_wide$bothpreterm==0], na.rm=T)
# mean(sibs_exp_wide$gest_change, na.rm=T)
# 
# sibs_exp_wide %>% group_by(premcat2) %>% 
#   summarise(meangest_change = mean(gest_change, na.rm = T))
# 
# change in As and Preterm cat
table(sibs_exp_wide$premcat_diff, sibs_exp_wide$as_cat_diff)

# change in address between pregnancies
samplat <- sibs_exp_wide %>% 
  mutate(latdiff = abs(lat2 - lat),
         longdiff = abs(long2 - long),
         moved = ifelse(latdiff > 0.05 | longdiff > 0.05, "Moved", "No Move"))


table(samplat$As>15, samplat$moved)
table(samplat$as_cat, samplat$moved)
table(samplat$as_cat)

library(lme4)
mix <- lmer(gestage ~ no3_cat + mothage + sex + obirths + parity + race + precare_5 + (1|id), 
            data = births_exp_cat)

summary(mix)

library(gee)
geefit <- gee(ispreterm ~ as_cat + year, #+ mothage + sex + obirths + parity + race + precare_5,
              id = id, data = births_exp_cat, family="binomial",
              corstr="unstructured")

summary(geefit)


write_csv(births_exp_cat, "./data_out/births_exp_cat.csv")
write_csv(sibs_exp_wide, "./data_out/sibs_exp_wide.csv")


# figures

table(births_exp_cat$as_cat, useNA = "a") /nrow(births_exp_cat)
table(allbirths$race, useNA = "a") / nrow(allbirths)
table(births_exp_cat$race, useNA = "a") / nrow(births_exp_cat)
table(births_exp_cat$race[births_exp_cat$as_cat == "high"]) / sum(table(births_exp_cat$race[births_exp_cat$as_cat == "high"]))

ggplot(births_exp_cat) + 
  geom_density(aes(x = conc_as), fill = "slategray3") +
  theme_classic() +
  xlim(0,25) +
  labs(x = "Tap water arsenic concentration during pregnancy",
       y = "Density") +
  theme(text = element_text(size = 18))

ggplot(births_exp_cat, aes(x = as_cat)) + 
  geom_bar(fill  ="slategray4", aes(y = (..count..)*100/sum(..count..))) + 
  scale_y_continuous() +
  theme_classic() +
  #xlim(0,25) +
  labs(x = "Tap water arsenic category \nduring pregnancy",
       y = "Percent") +
  coord_flip() +
  theme(text = element_text(size = 18))

ggplot(births_exp_cat, aes(x = as_cat, fill = medu_3cat)) + 
  geom_bar(position = "fill") +
  theme_classic() +
  #xlim(0,25) +
  labs(x = "Tap water arsenic concentration during pregnancy",
       y = "Density") +
  theme(text = element_text(size = 18))






# difference between long and short pregnancy intervals
plot(births_exp$preg_as_long, births_exp$preg_as, ylim = c(0,50))
summary(lm(preg_as_long ~ preg_as, data = births_exp[births_exp$preg_as < 10,]))


# # map percent with exceedances
# # make hex binned map of exceedances
# 
# # initialize raster
# library(raster)
# library(maptools)
# 
#   rast <- raster()
#   ca_extent <- extent(ca_state)
#   extent(rast) <- ca_state 
#   cell_size <- 20000
#   nrow(rast) <- round((ca_extent@ymax - ca_extent@ymin) / cell_size)
#   ncol(rast) <- round((ca_extent@xmax - ca_extent@xmin) / cell_size)
# 
# 
# # Creates grid version of points using the cells of rast, values from the IP field:
#   rast_mcl_exc <- rasterize(mcl_exc, rast, 
#                           mcl_exc$arsenic_mcl, 
#                           fun = mean) 
# 
# #writeRaster(rast2, "tcprast.tif")
# plot(ca_state, col = "grey")
# plot(rast_mcl_exc, add = T)
# 
