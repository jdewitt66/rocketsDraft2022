## script to read data from data input file and reformat for mail merge template

require(tidyverse)
require(openxlsx)

## 1) Get the performance data file from the data folder ----
inP <- '../../data/Rockets_Draft_2022'
fn <- "2022_Rockets_Draft_Data_Sheet .xlsx"   

df_anthro <- read.xlsx(file.path(inP, fn), detectDates = T) %>%
  janitor::clean_names()

## create output anthro dataframe with new variable names that match mail merge
df_anthro %>%
  mutate(Fullname = paste(first_name, last_name)) %>%
  rename(r_val_Ht_wo_shoes = height_no_shoes, 
         r_val_StandReach = standing_reach,
         r_val_Wing = wingspan,
         r_val_maxVert = max_vertical,
         r_val_BW_lbs = weight,
         r_val_standMaxVert = vertical_no_step,
         r_val_Bfat_pct = body_fat_pct,
         r_val_HandLen = hand_length,
         r_val_HandWid = hand_width,
         r_val_LaneShut_L = pro_lane_agility_l,
         r_val_LaneShut_R = pro_lane_agility_r,
         r_val_LaneAgility = lane_agility,
         r_val_3qrtRock = x3_4_sprint,
         r_val_10m = x10_m,
         r_val_QB = quickboard_total,
         r_val_QB_L = quickboard_l,
         r_val_QB_R = quickboard_r,
         r_val_LMM = lean_muscle_mass,
         val_ProLaneAgility_R = pro_lane_agility_r,
         val_ProLaneAgility__L = pro_lane_agility_l) %>%
  mutate(r_val_qb_imbalance = ((r_val_QB_R / r_val_QB_L) - 1) *100)



# 
# r_pct_Ht_wo_shoes
# 
# r_pct_StandReach
# 
# r_pct_Wing
# r_val_WingHt
# r_pct_Wing_Ht
# r_val_JumpHt
# r_pct_jumpHt
# 
# r_pct_stantMaxVert
# 
# r_pct_MaxVert
# 
# r_pct_BW_lbs
# 
# r_pct_Bfat_pct
# 
# r_pct_HandLen
# 
# r_pct_HandWid
# 
# r_pct_LaneShut_R
# 
# r_pct_LaneShut_L
# 
# r_pct_LaneAgility
# r_val_3qrtSpeed
# r_pct_3qrtSpeed
# 
# r_pct_3qrtRock
# 
# r_pct_QB
# 
# r_pct_10m
# 
# r_pct_shutLRock
# 
# r_pct_shutRRock

## 2) Get historical combine data
fn <- "nba-combine-historical-through-2021-results.csv"
df_combine <- read.csv(file.path(inP, fn), stringsAsFactors = F)

## 2) Get Jump data -----
forceFolder = 'ForceDecks'

## 2a) CMJ data
## CMJ variables:
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
cmjFile <- dir(paste(inP, forceFolder, sep ='/'), pattern = 'CMJ')

d_cmj <- 
read.xlsx(file.path(inP, forceFolder, cmjFile), detectDates = T,
          startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
  select(athlete, test_type, test_date, body_weight_kg, trial,
         concentric_impulse_ns, force_at_zero_velocity_n, peak_power_bm_w_kg,
         peak_power_w,rsi_modified_m_s) %>%
  filter(rsi_modified_m_s < 2) %>%
  mutate(concentric_impulse_ns_kg = concentric_impulse_ns / body_weight_kg,
         force_at_zero_velocity_n_kg = force_at_zero_velocity_n / body_weight_kg)

d_cmj %>%
  gather(varName, value, -athlete, -test_type, -test_date) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% spread(varName, meanVal)
  
## 2b) DJ data
## Drop Jump variables
## val_DJ_CT
## pct_DJ_CT
## val_DJ_RSI
## pct_DJ_RSI
djFile <- dir(paste(inP, forceFolder, sep ='/'), pattern = 'DJ')
d_dj <- 
  read.xlsx(file.path(inP, forceFolder, djFile), detectDates = T,
          startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
  select(athlete, test_type, test_date, body_weight_kg, trial,
         contact_time_s, rsi_flight_time_contact_time)

d_dj %>%
  gather(varName, value, -athlete, -test_type, -test_date) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% spread(varName, meanVal)

## 2c) SLJ data
## SLJ variables
## val_SL_EccMean
## pct_SL_EccMean
sljFile <- dir(paste(inP, forceFolder, sep ='/'), pattern = 'SLJ')
d_slj <- 
  read.xlsx(file.path(inP, forceFolder, sljFile), detectDates = T,
            startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
    select(athlete, test_type, test_date, body_weight_kg, trial,
           eccentric_mean_force_n) %>%
      mutate(eccentric_mean_force_n_kg = eccentric_mean_force_n / body_weight_kg) %>%
    separate(trial, c('leg', 'trial')) 

d_slj %>%
  gather(varName, value, -athlete, -test_type, -test_date, -leg) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, leg, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% unite('varName', c(leg,varName)) %>% 
  spread(varName, meanVal)
