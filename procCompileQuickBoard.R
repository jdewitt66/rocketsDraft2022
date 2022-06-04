## proc to read in and create master quickboard file for use in historical comparisons
##
## writes to file 'allHistoricalQuickboardData.csv'

require(tidyverse)
require(openxlsx)

## data from 2018-2019
d <- bind_rows(
  read.csv('../../data/Force Plate Data/quickBoardData2018/Quick-Board-2018-19_12.9.19.csv') %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  rename(qb_l = Quickboard...Foot.Fire.Left,
         qb_r = Quickboard...Foot.Fire.Right, 
         qb_t = Quickboard...Foot.Fire.Total)
  ,
  read.csv('../../data/Force Plate Data/quickBoardData2018/Quick-Board-2018-19.csv') %>%
    mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
    rename(qb_l = Quickboard...Foot.Fire.Left,
           qb_r = Quickboard...Foot.Fire.Right, 
           qb_t = Quickboard...Foot.Fire.Total)
) %>% distinct()

## data from 2021-22
d_2 <- bind_rows(
  read.xlsx(
    '../../data/2021_2022 Data/QuickBoard 2021-22/Rockets Quickboard-Data_11_11_21 .xlsx',
    detectDates = T
  ) %>%
    mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
    rename(qb_l = `Quickboard.-.Foot.Fire.Left`,
           qb_r = `Quickboard.-.Foot.Fire.Right`,
           qb_t = `Quickboard.-.Foot.Fire.Total`) %>%
    select(Date, Name, contains('qb')) %>%
    mutate(
      qb_l = as.numeric(qb_l),
      qb_r = as.numeric(qb_r),
      qb_t = as.numeric(qb_t)
    )
  ,
  read.csv(
    "../../data/2021_2022 Data/QuickBoard 2021-22/___-QuickBoard-Foot-Fire.csv"
  ) %>%
    mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
    rename(qb_l = Quickboard...Foot.Fire.Left,
           qb_r = Quickboard...Foot.Fire.Right,
           qb_t = Quickboard...Foot.Fire.Total) %>%
    select(Date, Name, contains('qb'))
)

## combine into a single dataframe and remove dups
d_all <- bind_rows(d, d_2) %>% distinct() %>%
  filter(qb_t > 0)

write.csv(d_all, file = 'allHistoricalQuickboardData.csv', row.names = F)
