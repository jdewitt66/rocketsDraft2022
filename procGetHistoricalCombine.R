getHistoricalCombine <- function(inPath = inP, fileN = fn) {
  #### proc to read in historical combine data and to clean
  
  require(tidyverse)
  
  ## Get historical combine data
  #inPath <- '../../data/Rockets_Draft_2022'
  #fileN <- "nba-combine-historical-through-2021-results.csv"
  df_combine <- read.csv(file.path(inP, fn), stringsAsFactors = F)
  
  dd <- df_combine %>%
    # rename to combine specific values
    rename(
      c_val_Ht_wo_shoes = height_noshoes,
      c_val_StandReach = standing_reach,
      c_val_Wing = wingspan,
      c_val_standMaxVert = vertical_nostep,
      c_val_maxVert = vertical_max,
      c_val_BW_lbs = weight,
      c_val_Bfat_pct = body_fat_pct,
      c_val_HandLen = hand_length,
      c_val_HandWid = hand_width,
      c_val_LaneShut_R = lane_shuttle_right,
      c_val_LaneShut_L = lane_shuttle_left,
      c_val_LaneAgility = lane_agility,
      c_val_3qrtSpeed = sprint_3_4_court
    ) %>%
    # only keep variables of interest
    select(first_name,
           last_name,
           testing_year,
           position,
           contains('c_val')) %>%
    # convert any data files to numeric
    mutate_at(vars(matches('c_val')), as.numeric) %>%
    mutate(
      c_val_WingHt = c_val_Wing - c_val_Ht_wo_shoes,
      c_val_JumpHt = c_val_standMaxVert - c_val_StandReach
    )
  
  return(dd)
}

