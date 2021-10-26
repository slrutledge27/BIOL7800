install.packages("palmerpenguins")
library(tidyverse)
library(dplyr)
palmerpenguins::penguins
palmerpenguins::penguins 
%>% group_by(species, island) 
%>% summarise(average_body_mass = mean(body_mass_g, na.rm = TRUE), average_bill_length = mean(bill_length, na.rm = TRUE))
