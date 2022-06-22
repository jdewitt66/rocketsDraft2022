##script to read data from data input file and reformat for mail merge template

require(tidyverse)
require(openxlsx)

# folder with code and data
inP <- '../../data/Rockets_Draft_2022'

## 1) Get combine data
getCombineData <- function(inP) {
  ## 1a) Get historical combine data
  source('procGetHistoricalCombine.R')
  fn <- "nba-combine-historical-through-2021-results.csv"
  df_combine_hist <- getHistoricalCombine(inPath = inP, fileN = fn)
  
  ## 1b) Get this year's combine data
  df_combine_2022 <- read.csv('combine2022Cleaned.csv',
                              stringsAsFactors = F)
  
  ## 1c) all combine data
  df_combine_all <-
    bind_rows(
      df_combine_2022 %>%
        gather(
          varName,
          value,
          -first_name,
          -last_name,
          -position,
          -testing_year
        )
      ,
      df_combine_hist %>%
        gather(
          varName,
          value,
          -first_name,
          -last_name,
          -position,
          -testing_year
        )
    ) %>%
    mutate(fullname = paste(first_name, last_name)) %>%
    select(fullname, everything())
  
  return(df_combine_all)
}

df_combine <- getCombineData(inP)

## 1d) get historical quickboard data
d_hist_qb = read.csv(file = 'allHistoricalQuickboardData.csv', 
                     stringsAsFactors = F)

## 2) Get the performance data file from the data folder ----

fn <- "2022_Rockets_Draft_Data_Sheet .xlsx"   

df_anthro_in <- read.xlsx(file.path(inP, fn), detectDates = T) %>%
  janitor::clean_names()

## create output anthro dataframe with new variable names that match mail merge
df_anthro <- df_anthro_in %>%
  mutate(first_name = trimws(first_name), 
         last_name = trimws(last_name),
         fullname = trimws(paste(first_name, last_name)),
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
  mutate(r_val_qb_imbalance = round(((r_val_QB_R / r_val_QB_L) - 1) *100,2),
         r_val_JumpHt = r_val_StandReach + r_val_maxVert) 

## 3) Get Rocket Jump data -----
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

d_cmj_rocket <- 
  read.xlsx(file.path(inP, forceFolder, cmjFile), detectDates = T,
            startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
  select(athlete, test_type, test_date, body_weight_kg, trial,
         concentric_impulse_ns, force_at_zero_velocity_n, peak_power_bm_w_kg,
         peak_power_w,rsi_modified_m_s) %>%
  filter(rsi_modified_m_s < 2) %>%
  mutate(concentric_impulse_ns_kg = concentric_impulse_ns / body_weight_kg,
         force_at_zero_velocity_n_kg = force_at_zero_velocity_n / body_weight_kg) %>%
  gather(varName, value, -athlete, -test_type, -test_date) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% spread(varName, meanVal) %>%
  rename(fullname = athlete,
         type = test_type, date = test_date)

## 3b) DJ data
## Drop Jump variables
## val_DJ_CT
## pct_DJ_CT
## val_DJ_RSI
## pct_DJ_RSI
djFile <- dir(paste(inP, forceFolder, sep ='/'), pattern = 'DJ')
d_dj_rocket <- 
  read.xlsx(file.path(inP, forceFolder, djFile), detectDates = T,
            startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
  select(athlete, test_type, test_date, body_weight_kg, trial,
         contact_time_s, rsi_flight_time_contact_time) %>%
  gather(varName, value, -athlete, -test_type, -test_date) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, varName) %>%
  summarise(meanVal = mean(value)) %>%
  ungroup() %>% spread(varName, meanVal) %>%
  rename(fullname = athlete, type = test_type, date = test_date)

## 3c) SLJ data
## SLJ variables
## val_SL_EccMean
## pct_SL_EccMean
sljFile <- dir(paste(inP, forceFolder, sep ='/'), pattern = 'SLJ')
d_slj_rocket <- 
  read.xlsx(file.path(inP, forceFolder, sljFile), detectDates = T,
            startRow = 8) %>%
  janitor::clean_names() %>%
  mutate(test_date = janitor::excel_numeric_to_date(test_date)) %>%
  select(athlete, test_type, test_date, body_weight_kg, trial,
         eccentric_mean_force_n) %>%
  mutate(eccentric_mean_force_n_kg = eccentric_mean_force_n / body_weight_kg) %>%
  separate(trial, c('leg', 'trial'))  %>%
  gather(varName, value, -athlete, -test_type, -test_date, -leg) %>%
  filter(varName != 'trial') %>%
  mutate(value = as.numeric(value)) %>%
  group_by(athlete, test_type, test_date, leg, varName) %>%
  summarise(meanVal = mean(value, na.rm = T)) %>%
  ungroup() %>% unite('varName', c(leg,varName)) %>% 
  spread(varName, meanVal) %>%
  rename(fullname = athlete,
         type = test_type,
         date = test_date)

## 4) go through row by row and add percentile relative to overall combine data and historical jump data ----

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
  fullName = df_anthro$fullname[r]
  this_ath_rock <- df_anthro %>% filter(first_name == fName &
                                     last_name == lName)
  
  ## get combine data for the current player (if exists)
  this_ath_combine = 
    df_combine %>% filter(first_name == fName,
                          trimws(last_name) == lName) %>%
    spread(varName, value) %>%
    select(-position, testing_year)
  
  ## add combine data to athlete df
  this_ath <- left_join(this_ath_rock, this_ath_combine)
  
  ## anthro (compare with combine and report highest value)
  ## keep three variables - c = combine, r = rocket, s = max of both (for report)
  
  ## Ht wo shoes
  cbnVal = this_ath$c_val_Ht_wo_shoes
  s_val_Ht_wo_shoes = max(cbnVal,this_ath$r_val_Ht_wo_shoes, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_Ht_wo_shoes', 'value']
  s_pct_Ht_wo_shoes = computePctTile(inD,  s_val_Ht_wo_shoes)
  s_val_Ht_wo_shoes_chr <- paste0(s_val_Ht_wo_shoes%/%12,"'",s_val_Ht_wo_shoes%%12)
  
  # StandReach
  cbnVal = this_ath$c_val_StandReach
  s_val_StandReach = max(cbnVal,this_ath$r_val_StandReach, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_StandReach', 'value']
  s_pct_StandReach = computePctTile(inD, s_val_StandReach)
  s_val_StandReach_chr <- paste0(s_val_StandReach%/%12,"'", s_val_StandReach%%12)
  
  # Wing
  cbnVal = this_ath$c_val_Wing
  s_val_Wing = max(cbnVal,this_ath$r_val_Wing, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_Wing', 'value']
  s_pct_Wing = computePctTile(inD, s_val_Wing)
  s_val_Wing_chr <- paste0(s_val_Wing%/%12,"'",s_val_Wing%%12)
  
  # WingHt
  cbnVal =  this_ath$c_val_WingHt
  s_val_WingHt = max(cbnVal, this_ath$r_val_WingHt, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_WingHt', 'value']
  s_pct_WingHt = computePctTile(inD, s_val_WingHt)
  
  # Jump Ht
  cbnVal =  this_ath$c_val_JumpHt
  # check if there was a jump - if not set output to NA
  if((is.na(cbnVal) | is.null(cbnVal)) & is.na(this_ath$r_val_JumpHt)) {
    s_val_JumpHt = '';  s_pct_JumpHt = '';  s_val_JumpHt_chr = ''
  } else {
  s_val_JumpHt = max(cbnVal, this_ath$r_val_JumpHt, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_JumpHt', 'value']
  s_pct_JumpHt = computePctTile(inD, s_val_JumpHt)
  s_val_JumpHt_chr <- paste0(s_val_JumpHt%/%12,"'",s_val_JumpHt%%12)
  }
  
  # Stand Max Vert
  cbnVal =  this_ath$c_val_standMaxVert
  if((is.na(cbnVal) | is.null(cbnVal)) & is.na(this_ath$r_val_standMaxVert)) {
    s_val_standMaxVert = '';  s_pct_standMaxVert = ''
  } else {
  s_val_standMaxVert = max(cbnVal, this_ath$r_val_standMaxVert, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_standMaxVert', 'value']
  s_pct_standMaxVert = computePctTile(inD, s_val_standMaxVert)
  }
  
  # Max Vert
  cbnVal =  this_ath$c_val_maxVert
  if((is.na(cbnVal) | is.null(cbnVal)) & is.na(this_ath$r_val_maxVert)) {
    s_val_maxVert = '';  s_pct_maxVert = ''
  } else {
  s_val_maxVert = max(cbnVal, this_ath$r_val_maxVert, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_maxVert', 'value']
  s_pct_maxVert = computePctTile(inD, s_val_maxVert)
  }
  
  # BW lbs
  cbnVal =  this_ath$c_val_BW_lbs
  s_val_BW_lbs = max(cbnVal, this_ath$r_val_BW_lbs, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_BW_lbs', 'value']
  s_pct_BW_lbs = computePctTile(inD, s_val_BW_lbs)
  
  # Bfat
  cbnVal =  this_ath$c_val_Bfat_pct
  s_val_Bfat_pct = min(cbnVal, this_ath$r_val_Bfat_pct, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_Bfat_pct', 'value']
  s_pct_Bfat_pct = computePctTile(inD, s_val_Bfat_pct, higher_better = FALSE)
  
  # Hand length
  cbnVal =  this_ath$c_val_HandLen
  s_val_HandLen = max(cbnVal, this_ath$r_val_HandLen, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_HandLen', 'value']
  s_pct_HandLen = computePctTile(inD, s_val_HandLen)
  
  # Hand width
  cbnVal =  this_ath$c_val_HandWid
  s_val_HandWid = max(cbnVal, this_ath$r_val_HandWid, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_HandWid', 'value']
  s_pct_HandWid = computePctTile(inD, s_val_HandWid)
  
  # lane shuttle R
  cbnVal =  this_ath$c_val_LaneShut_R
  if((is.na(cbnVal) | is.null(cbnVal)) & is.na(this_ath$r_val_LaneShut_R)) {
    s_val_LaneShut_R = '';  s_pct_LaneShut_R = ''
  } else {
  s_val_LaneShut_R = min(cbnVal, this_ath$r_val_LaneShut_R, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_LaneShut_R', 'value']
  s_pct_LaneShut_R = computePctTile(inD, s_val_LaneShut_R, higher_better = F)
  }
  
  # lane shuttle L
  cbnVal =  this_ath$c_val_LaneShut_L
  if((is.na(cbnVal) | is.null(cbnVal)) & is.na(this_ath$r_val_LaneShut_L)) {
    s_val_LaneShut_L = '';  s_pct_LaneShut_L = ''
  } else {
  s_val_LaneShut_L = min(cbnVal, this_ath$r_val_LaneShut_L, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_LaneShut_L', 'value']
  s_pct_LaneShut_L = computePctTile(inD, s_val_LaneShut_L, higher_better = F)
  }
  
  # 3qrt speed
  cbnVal =  this_ath$c_val_3qrtSpeed
  if((is.na(cbnVal) | is.null(cbnVal)) & is.na(this_ath$r_val_3qrtRock)) {
    s_val_3qrtSpeed = '';  s_pct_3qrtSpeed = ''
  } else {
  s_val_3qrtSpeed = min(cbnVal, this_ath$r_val_3qrtRock, na.rm = T)
  inD = df_combine[df_combine$varName == 'c_val_3qrtSpeed', 'value']
  s_pct_3qrtSpeed = computePctTile(inD, s_val_3qrtSpeed, higher_better = F)
  }
  # quickboard total
  inD <- d_hist_qb$qb_t
  s_pct_QB = computePctTile(inD, this_ath_rock$r_val_QB)
  
  ## force
  
  thisath_CMJ <- d_cmj_rocket %>% filter(fullname == fullName)
  if(nrow(thisath_CMJ) == 0) {thisath_CMJ[1,] = NA}
  
  # val_CM_conImp
  r_val_CM_conImp = thisath_CMJ$concentric_impulse_ns_kg
  inD <- df_force_hist %>% filter(type == 'Countermovement Jump',
                           varName == 'concentric_impulse_ns_kg') %>%
    select(value) %>% pull()
  r_pct_CM_conImp = computePctTile(inD, r_val_CM_conImp)
  
  # val_CM_fzero
  r_val_CM_fzero = thisath_CMJ$force_at_zero_velocity_n_kg
  inD <- df_force_hist %>% filter(type == 'Countermovement Jump',
                                  varName == 'force_at_zero_velocity_n_kg') %>%
    select(value) %>% pull()
  r_pct_CM_fzero = computePctTile(inD, r_val_CM_fzero)
  
  # val_CM_relPP
  r_val_CM_relPP = thisath_CMJ$peak_power_bm_w_kg
  inD <- df_force_hist %>% filter(type == 'Countermovement Jump',
                                  varName == 'peak_power_bm_w_kg') %>%
    select(value) %>% pull()
  r_pct_CM_relPP = computePctTile(inD, r_val_CM_relPP)
  
  # val_CM_PP
  r_val_CM_PP = thisath_CMJ$peak_power_w
  inD <- df_force_hist %>% filter(type == 'Countermovement Jump',
                                  varName == 'peak_power_w') %>%
    select(value) %>% pull()
  r_pct_CM_PP = computePctTile(inD, r_val_CM_PP)
  
  # val_CM_RSImod
  r_val_CM_RSImod = thisath_CMJ$rsi_modified_m_s
  inD <- df_force_hist %>% filter(type == 'Countermovement Jump',
                                  varName == 'rsi_modified_m_s') %>%
    select(value) %>% pull()
  r_pct_CM_RSImod = computePctTile(inD, r_val_CM_RSImod)
  
  ## drop jump
  thisath_DJ <- d_dj_rocket %>% filter(fullname == fullName)
  if(nrow(thisath_DJ) == 0) {thisath_DJ[1,] = NA}
  
  # val_DJ_CT
  r_val_DJ_CT = thisath_DJ$contact_time_s
  inD <- df_force_hist %>% filter(type == 'Drop Jump',
                                  varName == 'contact_time_s') %>%
    select(value) %>% pull()
  r_pct_DJ_CT = computePctTile(inD, r_val_DJ_CT, higher_better = F)
  
  # val_DJ_RSI
  r_val_DJ_RSI = thisath_DJ$rsi_flight_time_contact_time
  inD <- df_force_hist %>% filter(type == 'Drop Jump',
                                  varName == 'rsi_flight_time_contact_time') %>%
    select(value) %>% pull()
  r_pct_DJ_RSI = computePctTile(inD, r_val_DJ_RSI)
  
  ## SLJ jump
  thisath_SLJ <- d_slj_rocket %>% filter(fullname == fullName)
  if(nrow(thisath_SLJ) == 0) {thisath_SLJ[1,] = NA}
  
  ## val_SL_EccMean_L
  r_val_SL_EccMean_L = thisath_SLJ$Left_eccentric_mean_force_n_kg
  inD <- df_force_hist %>% filter(type == 'Single Leg Jump',
                                  varName == 'Left_eccentric_mean_force_n_kg') %>%
    select(value) %>% pull()
  r_pct_SL_EccMean_L = computePctTile(inD, r_val_SL_EccMean_L)
  
  ## val_SL_EccMean_R
  r_val_SL_EccMean_R = thisath_SLJ$Right_eccentric_mean_force_n_kg
  inD <- df_force_hist %>% filter(type == 'Single Leg Jump',
                                  varName == 'Right_eccentric_mean_force_n_kg') %>%
    select(value) %>% pull()
  r_pct_SL_EccMean_R = computePctTile(inD, r_val_SL_EccMean_R)
  

  
  # SL symmetry from SLJ
  r_SLJ_Sym = round(((r_val_SL_EccMean_R / r_val_SL_EccMean_L) - 1) * 100,2)
  
  ## add section and total grades
  avgVertExp = mean(c(s_pct_JumpHt, s_pct_standMaxVert, s_pct_maxVert), na.rm = T)
  avgCOD = mean(c(s_pct_LaneShut_L, s_pct_LaneShut_R), na.rm = T)
  avgAcc = mean(c(s_pct_3qrtSpeed), na.rm = T)
  avgVertForce = mean(c(r_pct_CM_conImp, r_pct_CM_fzero, r_pct_CM_relPP, r_pct_CM_PP, r_pct_CM_RSImod), na.rm = T)
  avg2ndExpl = mean(c(r_pct_DJ_CT, r_pct_DJ_RSI), na.rm = T)
  avgAcc2 = mean(c(s_pct_3qrtSpeed, s_pct_QB), na.rm = T)
  avgLat = mean(c(s_pct_LaneShut_L, s_pct_LaneShut_R), na.rm = T)
  avgStab = NA  # filler until percentiles added
  avgSLJ = mean(c(r_pct_SL_EccMean_L, r_pct_SL_EccMean_R), na.rm = T)
  avgCombine = mean(c(avgVertExp, avgCOD, avgAcc), na.rm = T)
  avgRocket = mean(c(avgVertForce, avg2ndExpl, avgAcc2, avgLat,
                       avgStab, avgSLJ), na.rm = T)
  
  makeRank <- function(x) {
  ## convert grades to 1-5 scale
    rank = ifelse(x < 10, 1,
                ifelse(x >= 10 & x < 30, 2,
                       ifelse(x >=30 & x<70, 3, 
                              ifelse(x>=70 & x < 90, 4, 5))))
    return(rank)
  }
  
  gradeVertExp = makeRank(avgVertExp)
  gradeCOD = makeRank(avgCOD)
  gradeAcc = makeRank(avgAcc)
  gradeVertForce = makeRank(avgVertForce)
  grade2ndExpl = makeRank(avg2ndExpl)
  gradeAcc2 = makeRank(avgAcc2)
  gradeLat = makeRank(avgLat)
  gradeStab= makeRank(avgStab)
  gradeSLJ = makeRank(avgSLJ)
  gradeCombine = makeRank(avgCombine)
  gradeRocket = makeRank(avgRocket)
  
  ## output dataframe
  this_ath <- cbind(
    this_ath,
    data.frame(
      s_val_Ht_wo_shoes,
      s_pct_Ht_wo_shoes,
      s_val_Ht_wo_shoes_chr,
      s_pct_StandReach,
      s_val_StandReach_chr,
      s_val_Wing,
      s_pct_Wing,
      s_val_Wing_chr,
      s_val_WingHt,
      s_pct_WingHt,
      s_val_JumpHt,
      s_pct_JumpHt,
      s_val_JumpHt_chr,
      s_val_standMaxVert,
      s_pct_standMaxVert,
      s_val_maxVert,
      s_pct_maxVert,
      s_val_BW_lbs,
      s_pct_BW_lbs,
      s_val_Bfat_pct,
      s_pct_Bfat_pct,
      s_val_HandLen,
      s_pct_HandLen,
      s_val_HandWid,
      s_pct_HandWid,
      s_val_LaneShut_R,
      s_pct_LaneShut_R,
      s_val_LaneShut_L,
      s_pct_LaneShut_L,
      s_val_3qrtSpeed,
      s_pct_3qrtSpeed,
      s_pct_QB,
      r_val_CM_conImp,
      r_pct_CM_conImp,
      r_val_CM_fzero,
      r_pct_CM_fzero,
      r_val_CM_relPP,
      r_pct_CM_relPP,
      r_val_CM_PP,
      r_pct_CM_PP ,
      r_val_CM_RSImod,
      r_pct_CM_RSImod ,
      r_val_DJ_CT,
      r_pct_DJ_CT,
      r_val_DJ_RSI,
      r_pct_DJ_RSI,
      r_val_SL_EccMean_L,
      r_pct_SL_EccMean_L,
      r_val_SL_EccMean_R,
      r_pct_SL_EccMean_R,
      r_SLJ_Sym,
      gradeVertExp, 
      gradeCOD, 
      gradeAcc, 
      gradeVertForce, 
      grade2ndExpl, 
      gradeAcc2, 
      gradeLat, 
      gradeStab,
      gradeSLJ, 
      gradeCombine, 
      gradeRocket
    ) %>% mutate_if(is.numeric, round, 2)
  )
  
  ## add current athlete data to allOut data frame
  allOut <- bind_rows(allOut, this_ath) 
}

## 5) write to excel file
write.xlsx(allOut, file = 'docs/draft2022CompiledData2022Jun22.xlsx')

