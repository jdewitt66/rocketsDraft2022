## script to read data from data input file and reformat for mail merge template

require(tidyverse)
require(openxlsx)

## 1) Get the performance data file from the data folder ----
inP <- '../../data/Rockets_Draft_2022'
fn <- "2022_Rockets_Draft_Data_Sheet .xlsx"   

read.xlsx(file.path(inP, fn), detectDates = T) %>%
  janitor::clean_names()

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
