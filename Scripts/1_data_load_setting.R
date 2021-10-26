if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, jsonlite )

#### Data importing  ------
fromJSON("Data/odor_relations.json", flatten = T)



