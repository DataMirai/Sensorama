if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, jsonlite )

readLines("Data/odor_relations.json")
test1 <- readLines("Data/odor_relations.json")
test1

test2<- str_remove_all(test1, pattern = '(?<=\\})\\,')
test2
read_json(test2)
fromJSON(test2)
str_extract_all(test2, pattern = '[:alnum:]' )

read_json("Data/odor_relations.json" )



json <- do.call(
  rbind, 
  lapply(paste(readLines("Data/odor_relations.json", warn=FALSE), collapse=""), jsonlite::fromJSON))
