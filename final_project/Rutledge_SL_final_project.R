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

ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*", Arctos_birds_Calif_2000_2020$spec_locality, value=T)), "salvage", 0
       
       
       Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage",ifelse(grep("highway|hwy",
                                                                                                                                                                                                                                                                                                                         Arctos_birds_Calif_2000_2020$spec_locality, 
                                                                                                                                                                                                                                                                                                                         ignore.case = T, value = F),"salvage", 0)))       
       
Arctos_birds_Calif_2000_2020$coll_method_3[Arctos_birds_Calif_2000_2020$coll_method_3 == "0" &
                                                    grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T) &
                                                    !grepl(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality, 
                                                           ignore.case = T)] = "salvage"      
       
       
       
       
       
###Identify rows as salvage based on collectors, parts, and spec_locality ###
Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & Arctos_birds_Calif_2000_2020$spec_locality %in% c(grep("highway|hwy", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T, value = T)),"salvage", ifelse(Arctos_birds_Calif_2000_2020$coll_method_4 == "salvage"), "salvage", 0)))

### Identify rows that are still of unknown collecting method (i.e., unknown and 0 in coll_method_2 and coll_method_3 columns) as salvage based on address
##First, form new column

Arctos_birds_Calif_2000_2020$coll_method_4[Arctos_birds_Calif_2000_2020$coll_method_3 == "0" &
                                             grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T) &
                                             !grepl(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality, 
                                                    ignore.case = T)] = "salvage"  

##Then, sub in coll_meth_3 based on coll_method_4 (i.e., the new column)

Arctos_birds_Calif_2000_2020$coll_method_3<- ifelse(Arctos_birds_Calif_2000_2020$coll_method_4 == "salvage" gsub("0", "salvage", Arctos_birds_Calif_2000_2020$coll_method_3)
Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & is.na(Arctos_birds_Calif_2000_2020$collectors), "salvage", ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage", ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & Arctos_birds_Calif_2000_2020$spec_locality %in% c(grep("highway|hwy", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T, value = T)),"salvage", ifelse(Arctos_birds_Calif_2000_2020$coll_method_4 == "salvage", "salvage", 0))))
                                                    


### now remove some bits ###
df <- Arctos_birds_Calif_2000_2020 %>% filter(!(parts=="egg" | parts  =="egg; nest" | parts == "nest; egg" | parts == "media; egg"))
df2 <- df %>% filter(!(parts=="blood" | parts  =="blood; blood" | parts == "blood; blood; blood" | parts == "blood; blood; blood; blood"))


ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & Arctos_birds_Calif_2000_2020$spec_locality %in% c(grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T)) & Arctos_birds_Calif_2000_2020$spec_locality %in% c(!grep(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality,ignore.case = T)))
Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T) &!grepl(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality,ignore.case = T)
ifelse(Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" & Arctos_birds_Calif_2000_2020$spec_locality[grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T)] & Arctos_birds_Calif_2000_2020$spec_locality[grepl("![' mi | km | block | m | yds ']", Arctos_birds_Calif_2000_2020$spec_locality,ignore.case = T)]),

Arctos_birds_Calif_2000_2020$coll_method_3[Arctos_birds_Calif_2000_2020$coll_method_3 == "0" &
                                             grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T) &
                                             !grepl(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality, 
                                                    ignore.case = T)] = ""

Arctos_birds_Calif_2000_2020$coll_method_3[Arctos_birds_Calif_2000_2020$coll_method_2 == "unknown" &
                                             grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T) &
                                             !grepl(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality, 
                                                    ignore.case = T)] = ""









Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_3 == 0 & Arctos_birds_Calif_2000_2020$parts %in% c(grep("^skull\\w*", Arctos_birds_Calif_2000_2020$parts, value=T)), "salvage",0)

Arctos_birds_Calif_2000_2020$coll_method_3 = ifelse(Arctos_birds_Calif_2000_2020$coll_method_3 == 0 & Arctos_birds_Calif_2000_2020$spec_locality %in% c(grep("highway|hwy", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T, value = F)),"salvage", 0)


ifelse(grep("highway|hwy", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T, value = F),"salvage", 0)))