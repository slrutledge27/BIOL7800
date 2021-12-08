(Arctos_birds_Calif_2000_2020$spec_locality)
xx = unique(filter(Arctos_birds_Calif_2000_2020, is.na(collecting_method))$spec_locality)
grep("highway|hwy", xx, ignore.case = T, value = T)
pp = c("Hwy", )
Arctos_birds_Calif_2000_2020$coll_method_3[grep("highway|hwy",
                                                Arctos_birds_Calif_2000_2020$spec_locality, 
                                                ignore.case = T, value = F)] = "salvage"

Arctos_birds_Calif_2000_2020$coll_method_3[Arctos_birds_Calif_2000_2020$coll_method_3 == "0" &
  grepl("^[0-9]+", Arctos_birds_Calif_2000_2020$spec_locality, ignore.case = T) &
  !grepl(" mi | km | block | m | yds ", Arctos_birds_Calif_2000_2020$spec_locality, 
        ignore.case = T)] = ""
  

xxx = Arctos_birds_Calif_2000_2020$spec_locality
x4 = grep("^[0-9]+", xxx, ignore.case = T, value = T)
x4
grep(" mi | km | block | m | yds")