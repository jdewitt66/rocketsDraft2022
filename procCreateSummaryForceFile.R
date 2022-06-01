## script to get historical jump data and create a master file for 
## use with determining percentiles
##
## creates 
##    write.csv(d_all, file = 'allHistoricalJumpData.csv', row.names = F)

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
  mutate(body_wt_kg = ifelse(is.na(body_weight_kg), peak_power_w / peak_power_bm_w_kg,
                              body_weight_kg)) %>%
  na.omit() %>%
  mutate(concentric_impulse_ns_kg = concentric_impulse_ns / body_weight_kg,
         force_at_zero_velocity_n_kg = force_at_zero_velocity_n / body_weight_kg)
  gather(varName, value, -Name, -type, -date)

## val_DJ_CT
## pct_DJ_CT
## val_DJ_RSI
## pct_DJ_RSI

d_dj <- 
  d.all %>%
  filter(type == "Drop Jump",
         varName %in% c("contact_time_s","rsi_flight_time_contact_time")) %>%
  na.omit() 
  
d_dj %>% group_by(varName) %>% summarize(n= n())
## val_SL_EccMean
## pct_SL_EccMean
##
## get slj from 2021-2022 folder
inP <- '../../data/2021_2022 Data/2021_2022 Force Decks Data'
sljFile = "SLJ_TEAM_Historical.xlsx"
read.xlsx(file.path(inP, "SLJ_TEAM_Historical.xlsx"  ), detectDates = T)

d_slj_all <- NULL
for(sljFile in dir(inP, pattern = 'SLJ')) {
d_slj <- 
  read.xlsx(file.path(inP, sljFile), detectDates = T,
            startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
  select(athlete, test_type, test_date, body_weight_kg, trial,
         eccentric_mean_force_n) %>%
  mutate(body_weight_kg = as.numeric(body_weight_kg)) %>%
  mutate(eccentric_mean_force_n_kg = eccentric_mean_force_n / body_weight_kg) %>%
  separate(trial, c('leg', 'trial')) %>%
  gather(varName, value, -athlete, -test_type, -test_date, -leg) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, leg, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% unite('varName', c(leg,varName)) %>% 
  spread(varName, meanVal) %>%
  rename(Name = athlete, type = test_type, date = test_date) %>%
  mutate(Name = trimws(Name))

d_slj_all <- bind_rows(d_slj_all, d_slj)
} 
d_slj_all <- d_slj_all %>%
  gather(varName, value, -Name, -type, -date)

d_all <- bind_rows(d_c, d_dj, d_slj_all)

write.csv(d_all, file = 'allHistoricalJumpData.csv', row.names = F)
