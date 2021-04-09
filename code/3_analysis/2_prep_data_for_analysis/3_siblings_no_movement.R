## PREP DATA FOR ANALYSIS: SIBLING SAMPLE WITHOUT MATERNAL MOVEMENT
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# identify movement between consecutive pregnancies

sibs_moves <- sibs_consecutive %>% 
  group_by(mat_id) %>% 
  mutate(
    move_pwsid = case_when(
      parity == 1 ~ "First birth",
      lag(pwsid) == pwsid ~ "No move",
      lag(pwsid) != pwsid ~ "Move"),
    exp_change = case_when(
      parity == 1 ~ "First birth",
      lag(exp_cat) == exp_cat ~ "No Change",
      lag(exp_cat) == "high" & exp_cat =="medium" ~ "Decrease",
      lag(exp_cat) == "medium" & exp_cat =="low" ~ "Decrease",
      lag(exp_cat) == "high" & exp_cat =="low" ~ "Decrease",
      lag(exp_cat) == "medium" & exp_cat =="high" ~ "Increase",
      lag(exp_cat) == "low" & exp_cat =="medium" ~ "Increase",
      lag(exp_cat) == "low" & exp_cat =="high" ~ "Increase")) %>% 
  ungroup() %>% 
  mutate(move_pwsid = replace_na(move_pwsid, "First observed"))


# restrict to women who do not move PWS

sibs_same_pws <- sibs_moves %>% 
  filter(move_pwsid != "Move") %>% 
  filter(mat_id %in% mat_id[duplicated(mat_id)])

save(sibs_moves,  file = "data/processed/births_exposure/sibs_consec_moves.RData")
save(sibs_same_pws,  file = "data/processed/births_exposure/sibs_consec_same_pws.RData")


# descriptive statistics: movement between pregnancies

  # percentage of ipis with move
  table(sibs_moves$move_pwsid)
  
  sibs_moves %>% 
    filter(move_pwsid %in% c("Move", "No move")) %>% 
    summarise(sum(move_pwsid == "Move") / n())
  
  # what percent of births with exp change also have moved
  sibs_moves %>% 
    mutate(exp_change_cat = 
             case_when(exp_change %in% c("Increase", "Decrease") ~ "Change",
                       T ~ exp_change)) %>%
    filter(parity != 1) %>% 
    group_by(exp_change_cat) %>% 
    summarise(sum(move_pwsid == "Move") / n())



