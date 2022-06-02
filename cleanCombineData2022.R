## this script is used to clean the column data from the 2022 combine spreadsheet
## so that it can be combined with historical data.
##
## Cleaning includes creating new columns for first and last name, recoding positions, 
## and rename variables
##
## new output file 'combine2022Cleaned.csv' in the main combine folder
##

require(tidyverse)
library(openxlsx)

## read in this year's combine data
inP <- '../../data/Rockets_Draft_2022'

d1 <- read.xlsx(file.path(inP, "Combine Result (1).xlsx" ), sheet = 1)
d2 <- read.xlsx(file.path(inP, "Combine Result (1).xlsx" ), sheet = 2)
d3 <- read.xlsx(file.path(inP, "Combine Result (1).xlsx" ), sheet = 3)

d <- left_join(left_join(d1,d2),d3) %>%
  janitor::clean_names() 

## clean the data and rename columns so same as historical

## function to take number in format "ft'in''" (str) and convert to in (numeric)
convertToInch <- function(x_in) {
  x1 = str_split(x_in, "'")
  ht = as.numeric(unlist(x1)[1])
  inch = as.numeric(unlist(x1)[2])
  return(as.numeric(ht * 12 + inch))
}

# vectorized function
convertToInch_V <- Vectorize(convertToInch)

d_processed <- 
  d %>%
  # separate player name
  separate(player_name, c('last_name', 'first_name'), sep = ",")  %>% 
  mutate(last_name = trimws(last_name),
         first_name = trimws(first_name)) %>% 
  select(first_name, last_name, everything()) %>% # clean name
  # rename position
  mutate(position = ifelse(position == 'PG', 1,
                           ifelse(position == 'SG', 2,
                                  ifelse(position == 'SF', 3,
                                         ifelse(position == 'PF', 4, 5))))) %>%
  # convert feet - inch to inch as numeric
  mutate(c_val_Ht_wo_shoes	= as.numeric(convertToInch_V(height_with_shoes)),
         height_noshoes	=  as.numeric(convertToInch_V(height_without_shoes)),
         c_val_StandReach	=  as.numeric(convertToInch_V(standing_reach)),
         c_val_Wing	=  as.numeric(convertToInch_V(wingspan_dimensions))) %>%
  # convert attempt / made to separate columns
  separate(mid_side_mid_drill, into = c('mid_side_mid_drill_m', 'mid_side_mid_drill_a'), convert = T) %>%
  separate(side_mid_side_drill, into = c('side_mid_side_drill_m', 'side_mid_side_drill_a'), convert = T) %>%
  separate(spot_up_shooting, into = c('spot_up_shooting_m', 'spot_up_shooting_a'), convert = T) %>%
  separate(off_the_dribble_shooting, into = c('off_the_dribble_shooting_m', 'off_the_dribble_shooting_a'), convert = T) %>%
  separate(free_throws, into = c('free_throws_m', 'free_throws_a'), convert = T) %>%
  separate(x3_point_star_drill, into = c('x3_point_star_drill_m', 'x3_point_star_drill_a'), convert = T) %>%
  separate(mid_range_star_drill, into = c('mid_range_star_drill_m', 'mid_range_star_drill_a'), convert = T) %>%
  # add testing year
  mutate(testing_year = 2022) %>%
  # convert body fat to numeric
  mutate(c_val_Bfat_pct = as.numeric(body_fat)) %>%
  rename(c_val_HandLen = hand_dimensions_length,
         c_val_HandWid	= hand_dimensions_width,
         c_val_3qrtSpeed	= x3_4_court_sprint,
         c_val_LaneAgility	= pro_lane,
         c_val_maxVert	= max_vertical_jump,
         c_val_standMaxVert = standing_vertical) %>%
  ## convert chars to numeric
  mutate(c_val_HandWid = as.numeric(c_val_HandWid),
         c_val_HandLen = as.numeric(c_val_HandLen),
         c_val_BW_lbs = as.numeric(weight),
         c_val_LaneShut_L = as.numeric(lane_shuttle_left),
         c_val_LaneShut_R = as.numeric(lane_shuttle_right),
         c_val_3qrtSpeed = as.numeric(c_val_3qrtSpeed),
         c_val_LaneAgility = as.numeric(c_val_LaneAgility),
         c_val_maxVert = as.numeric(str_remove(c_val_maxVert, pattern = "''")),   #, ## need to strip the quotation mark
         c_val_standMaxVert = as.numeric(str_remove(c_val_standMaxVert, "''"))) %>% ## need to strip the quotation mark
  select(-college_name, -height_with_shoes, -height_without_shoes, -wingspan_dimensions,
         -body_fat, -weight, -standing_reach, -lane_shuttle_left, -lane_shuttle_right,
         -height_noshoes) %>%
  mutate(c_val_WingHt = c_val_Wing - c_val_Ht_wo_shoes,
         c_val_JumpHt = c_val_maxVert + c_val_StandReach)

  ## rename variables to combine specific varnames for consistency with historical combine

write.csv(d_processed, file = 'combine2022Cleaned.csv', row.names = F)


