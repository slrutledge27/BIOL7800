##replace NA collecting method slots with 'salvage' based on criteria ##
x <- Arctos_birds_Calif_2000_2020
x1 <- x %>% ifelse(is.na(collectors) , x$collecting_method == salvage)
ifelse(age <= 9 | !is.na(age),"child","adult"))
titanic %>% mutate(child_or_adult = ifelse(age <= 9 | !is.na(age),"child","adult"))

library(tidyverse)
Arctos_birds_Calif_2000_2020$coll_method_2 <- ifelse(is.na(Arctos_birds_Calif_2000_2020$collecting_method), "unknown","known")