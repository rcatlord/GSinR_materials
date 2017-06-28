## Tidying football banning orders ##

library(tidyverse)
df <- read_csv("https://raw.githubusercontent.com/rcatlord/GSinR_materials/master/4_tidy/banning_orders.csv", skip = 3) %>% 
  slice(1:24) %>% select(-c(3,6,9,12,15))
premier <- df %>% select(`Premier League`, count = `Banning Orders`) %>% gather(league, club, -count)
championship <- df %>% select(Championship, count =`Banning Orders_1`) %>% gather(league, club, -count)
league1 <- df %>% select(`League One`, count = `Banning Orders_2`) %>% gather(league, club, -count)
league2 <- df %>% select(`League Two`, count = `Banning Orders_3`) %>% gather(league, club, -count)
foot_conf <- df %>% select(`League Two`, count = `Banning Orders_4`) %>% gather(league, club, -count)
other <- df %>% select(`Other Clubs4`, count = `Banning Orders_5`) %>% gather(league, club, -count)
tidy <- bind_rows(premier, championship, league1, league2, foot_conf, other) %>% select(league, club, count) %>% filter(!is.na(count))

