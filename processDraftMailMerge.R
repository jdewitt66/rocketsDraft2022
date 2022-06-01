##script to read data from data input file and reformat for mail merge template

require(tidyverse)
require(openxlsx)

# folder with code and data
inP <- '../../data/Rockets_Draft_2022'

## 1) Get combine data
## 1a) Get historical combine data
source('procGetHistoricalCombine.R')
fn <- "nba-combine-historical-through-2021-results.csv"
df_combine_hist <- getHistoricalCombine(inPath = inP, fileN = fn)

## 1b) Get this year's combine data
df_combine_2022 <- read.csv('combine2022Cleaned.csv', 
                            stringsAsFactors = F)

## 1c) all combine data
df_combine <-
  bind_rows(
    df_combine_2022 %>%
      gather(varName, value, -first_name, -last_name, -position, -testing_year)
    ,
    df_combine_hist %>%
      gather(varName, value, -first_name, -last_name, -position, -testing_year) 
  )


## 2) Get the performance data file from the data folder ----

fn <- "2022_Rockets_Draft_Data_Sheet .xlsx"   

df_anthro_in <- read.xlsx(file.path(inP, fn), detectDates = T) %>%
  janitor::clean_names()

## create output anthro dataframe with new variable names that match mail merge
df_anthro <- df_anthro_in %>%
  mutate(fullname = paste(first_name, last_name),
         age = floor(as.numeric((date - date_of_birth)/365.25)),
         r_val_WingHt = wingspan - height_no_shoes,) %>%
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
    #     val_ProLaneAgility_R = pro_lane_agility_r,
    #     val_ProLaneAgility_L = pro_lane_agility_l,
         r_val_LMM = lean_muscle_mass
       ) %>%
  mutate(r_val_qb_imbalance = ((r_val_QB_R / r_val_QB_L) - 1) *100,
         r_val_JumpHt = r_val_StandReach + r_val_maxVert) 

## 3) go through row by row and add percentile relative to overall combine data

# function to compute percentile of a given score
computePctTile <- function(inCombine, inVal, higher_better = TRUE) {
  inCombine = inCombine[!is.na(inCombine)]
  if(higher_better) {
    # compute %tile where higher value is better
    pctTile = sum(inVal > inCombine) / length(inCombine) * 100
  } else {
    # compute %tile where lower value is better
    pctTile = sum(inVal < inCombine) / length(inCombine) * 100
  }
  return(round(pctTile,2))
}

# percentile computation
allOut <- NULL
for (r in seq(1, nrow(df_anthro))) {
  fName = df_anthro$first_name[r]
  lName = df_anthro$last_name[r]
  this_ath <- df_anthro %>% filter(first_name == fName &
                                     last_name == lName)
  
  inD = df_combine[df_combine$varName == 'c_val_Ht_wo_shoes', 'value']
  r_pct_Ht_wo_shoes = computePctTile(inD, this_ath$r_val_Ht_wo_shoes)
  
  inD = df_combine[df_combine$varName == 'c_val_StandReach', 'value']
  r_pct_StandReach = computePctTile(inD, this_ath$r_val_StandReach)
  
  inD = df_combine[df_combine$varName == 'c_val_Wing', 'value']
  r_pct_Wing = computePctTile(inD, this_ath$r_val_Wing)
  
  inD = df_combine[df_combine$varName == 'c_val_WingHt', 'value']
  r_pct_WingHt = computePctTile(inD, this_ath$r_val_WingHt)
  
  inD = df_combine[df_combine$varName == 'c_val_JumpHt', 'value']
  r_pct_JumpHt = computePctTile(inD, this_ath$r_val_JumpHt)
  
  inD = df_combine[df_combine$varName == 'c_val_standMaxVert', 'value']
  r_pct_standMaxVert = computePctTile(inD, this_ath$r_val_standMaxVert)
  
  inD = df_combine[df_combine$varName == 'c_val_maxVert', 'value']
  r_pct_maxVert = computePctTile(inD, this_ath$r_val_maxVert)
  
  inD = df_combine[df_combine$varName == 'c_val_BW_lbs', 'value']
  r_pct_BW_lbs = computePctTile(inD, this_ath$r_val_BW_lbs)
  
  inD = df_combine[df_combine$varName == 'c_val_Bfat_pct', 'value']
  r_pct_Bfat_pct = computePctTile(inD, this_ath$r_val_Bfat_pct, higher_better = FALSE)
  
  inD = df_combine[df_combine$varName == 'c_val_HandLen', 'value']
  r_pct_HandLen = computePctTile(inD, this_ath$r_val_HandLen)
  
  inD = df_combine[df_combine$varName == 'c_val_HandWid', 'value']
  r_pct_HandWid = computePctTile(inD, this_ath$r_val_HandWid)
  
  inD = df_combine[df_combine$varName == 'c_val_LaneShut_R', 'value']
  r_pct_LaneShut_R = computePctTile(inD, this_ath$r_val_LaneShut_R, higher_better = F)
  
  inD = df_combine[df_combine$varName == 'c_val_LaneShut_L', 'value']
  r_pct_LaneShut_L = computePctTile(inD, this_ath$r_val_LaneShut_L, higher_better = F)
  
  inD = df_combine[df_combine$varName == 'c_val_3qrtSpeed', 'value']
  r_pct_3qrtRock = computePctTile(inD, this_ath$r_val_3qrtRock, higher_better = F)
  
  this_ath <- cbind(
    this_ath,
    data.frame(
      r_pct_Ht_wo_shoes,
      r_pct_StandReach,
      r_pct_Wing,
      r_pct_WingHt,
      r_pct_JumpHt,
      r_pct_standMaxVert,
      r_pct_maxVert,
      r_pct_BW_lbs,
      r_pct_Bfat_pct,
      r_pct_HandLen,
      r_pct_HandWid,
      r_pct_HandWid,
      r_pct_LaneShut_R,
      r_pct_LaneShut_L,
      r_pct_3qrtRock
    )
  )
  allOut <- bind_rows(allOut, this_ath)
}

## write to excel file
write.xlsx(allOut, file = 'docs/draft2022CompiledData.xlsx')

## 2) Get Jump data -----
df_force_hist <- read.csv(file = 'allHistoricalJumpData.csv', stringsAsFactors = F)

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

d_cmj2 <- d_cmj %>%
  gather(varName, value, -athlete, -test_type, -test_date) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% spread(varName, meanVal) %>%
  rename(Name = athlete,
         type = test_type, date = test_date)
  
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

d_dj <- d_dj %>%
  gather(varName, value, -athlete, -test_type, -test_date) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% spread(varName, meanVal) %>%
  rename(Name = athlete, type = test_type, date = test_date)

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

d_slj <- d_slj %>%
  gather(varName, value, -athlete, -test_type, -test_date, -leg) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, leg, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% unite('varName', c(leg,varName)) %>% 
  spread(varName, meanVal) %>%
  rename(Name = athlete,
         type = test_type,
         date - test_date)
