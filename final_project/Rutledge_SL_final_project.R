##replace NA collecting method slots with 'salvage' based on criteria ##
x <- Arctos_birds_Calif_2000_2020
x1 <- x %>% ifelse(is.na(collectors) , x$collecting_method == salvage)
ifelse(age <= 9 | !is.na(age),"child","adult"))
titanic %>% mutate(child_or_adult = ifelse(age <= 9 | !is.na(age),"child","adult"))

library(tidyverse)
Arctos_birds_Calif_2000_2020$coll_method_2 <- ifelse(is.na(Arctos_birds_Calif_2000_2020$collecting_method), "unknown","known")

Arctos_birds_Calif_2000_2020$coll_method_3 <- with(Arctos_birds_Calif_2000_2020, ifelse(coll_method_2 == "unknown" & is.na(collectors), collecting_method == "salvage", ifelse(grep("[skull]",parts)),"salvage", ))

\W*((?i)rocket(?-i))\W*
  grep -i '\<rocket\>'

mydata$y = ifelse(mydata$x3 %in% c("A","B") ,mydata$x1*2,
                  ifelse(mydata$x3 %in% c("C","D"), mydata$x1*3,
                         mydata$x1*4))
Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("skull", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", 0))
Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*$", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", 0))
Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*$"|"^skull; wing\\w*", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", 0))
Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*$", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", 0))

Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", 0))
Arctos_birds_Calif_2000_2020$coll_method_3<-gsub("^(skull|skull; wing|skull; tissue).*", "", Arctos_birds_Calif_2000_2020$collecting_method)
df2<-ifelse(Arctos_birds_Calif_2000_2020$coll_method_3 == 0 & Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull; wing\\w*$", Arctos_birds_Calif_2000_2020$parts, value=T)), gsub(0, "salvage", Arctos_birds_Calif_2000_2020$coll_method_3), )