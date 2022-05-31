## script to get historical jump data and create a master file for 
## use with determining percentiles

d.all <- read.csv('../forceDecksHou/allHouForceDecksData.csv', 
                  stringsAsFactors = F) %>% 
  rename(Name = athlete,
         type = test_type,
         date = test_date) %>%
 # filter(Name %in% roster) %>%
  filter(type %in% c("Countermovement Jump","Drop Jump" , "Single Leg Jump" )) %>%
  mutate(date = as.Date(date, '%Y-%m-%d')) %>%
## find mean values for each variable by jump type
  group_by(Name, type, date, varName) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup()

# val_CM_conImp
# pct_CM_comImp
# val_CM_fzero
# pct_CM_fzero
# val_CM_relPP
# pct_CM_relPP
# val_CM_PP
# pct_CM_PP
# val_CM_RSImod
# pct_CM_RSImod

d_c <- d.all %>%
  filter(type == "Countermovement Jump",
         varName %in% c("body_weight_kg","concentric_impulse_ns","force_at_zero_velocity_n",
                        "peak_power_bm_w_kg","peak_power_w" ,"rsi_modified_m_s")
  ) %>%
  spread(varName, value) %>%
  mutate(body_wt_kg2 = ifelse(is.na(body_weight_kg), peak_power_w / peak_power_bm_w_kg,
                              body_weight_kg)) %>%
  select(-body_weight_kg) %>%
  na.omit() %>%
  gather(varName, value, -Name, -type, -date)

## val_DJ_CT
## pct_DJ_CT
## val_DJ_RSI
## pct_DJ_RSI

d_dj <- 
  d.all %>%
  filter(type == "Drop Jump",
         varName %in% c("flight_time_contraction_time", "rsi_jh_flight_time_contact_time_m_s")) %>%
  na.omit() 
  
## val_SL_EccMean
## pct_SL_EccMean
##
## get slj from 2021-2022 folder

d_slj <- 
  d.all %>%
  filter(type == "Single Leg Jump",
         varName %in% c("eccentric_mean_force_n")) %>%
  na.omit() 
