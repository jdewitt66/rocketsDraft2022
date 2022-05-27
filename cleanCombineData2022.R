## this script is used to clean the column data from the 2022 combine spreadsheet
## so that it can be combined with historical data.
##
## Cleaning includes creating new columns for first and last name, recoding positions, 
## and rename variables

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
d_processed <- 
  d %>%
  separate(player_name, c('last_name', 'first_name'), sep = ",")  %>% 
  select(first_name, last_name, everything()) %>% # clean name
  mutate(position = ifelse(position == 'PG', 1,
                           ifelse(position == 'SG', 2,
                                  ifelse(position == 'SF', 3,
                                         ifelse(position == 'PF', 4, 5))))) %>%
  mutate(testing_year = 2022) %>%
  rename(hand_length = hand_dimensions_length,
         hand_width	= hand_dimensions_width,
         height_shoes	= height_with_shoes,
         height_noshoes	= height_without_shoes,
         standing_reach	= standing_reach,
         wingspan	= wingspan_dimensions,
         sprint_3_4_court	= x3_4_court_sprint,
         lane_agility	= pro_lane,
         vertical_max	= max_vertical_jump,
         vertical_nostep = standing_vertical) %>%
  mutate(hand_width = as.numeric(hand_width),
         hand_length = as.numeric(hand_length),
         weight = as.numeric(weight),
         lane_shuttle_left = as.numeric(lane_shuttle_left),
         lane_shuttle_right = as.numeric(lane_shuttle_right),
         sprint_3_4_court = as.numeric(sprint_3_4_court),
         lane_agility = as.numeric(lane_agility),
         vertical_max = as.numeric(str_remove(vertical_max, '/"' )))    #, ## need to strip the quotation mark
         vertical_nostep = as.numeric(str_reomve(vertical_nostep, '"'))) ## need to strip the quotation mark
  

## read in historical data
d_hist <- read.csv(file.path(inP, "nba-combine-historical-through-2021-results.csv"))

## change names so they are the same in both data frames so they can be combined

