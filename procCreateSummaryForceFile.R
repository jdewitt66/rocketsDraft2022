## script to get historical jump data and create a master file for 
## use with determining percentiles

d.all <- read.csv('../forceDecksHou/allHouForceDecksData.csv', 
                  stringsAsFactors = F) %>% 
  rename(Name = athlete,
         type = test_type,
         date = test_date) %>%
 # filter(Name %in% roster) %>%
  filter(type %in% c("Countermovement Jump","Drop Jump" , "Single Leg Jump" )) %>%
  mutate(date = as.Date(date, '%Y-%m-%d')) 

## find mean values for each variable by jump type
d.all %>%
  group_by(Name, type, date, varName) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup()

