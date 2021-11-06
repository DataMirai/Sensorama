if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, # to arrange all stuff and functional programming
               jsonlite, # to read the data with FromJSON
               utils, 
               BiocManager,
               ChemmineR,
               ChemmineOB, 
               grid, 
               ggplotify,
               rcdk,
               #ingerprint,
               furrr)

# Data importing -------------------------------------------------------------------------------
  # In this case the data is already prepared and only required its importation straightforwardly,
  # This data has been provided from the UAB bioinformatics team

odour <- fromJSON("Data/odor_relations.json", flatten = T)
compounds <- read.csv("Data/compounds.csv")

## Data checks --------------------------------------------------------------------------------------
  # Seems to be duplicated data cases, just point to control

# compounds %>%
#   mutate(cid= as.character(cid)) %>%
#   inner_join(odour, by=c("cid"="cid") ) %>%
#   head(2) # Seem to be duplicated data
# 
# compounds %>%
#   mutate(cid= as.character(cid)) %>%
#   inner_join(odour, by=c("cid"="cid") ) %>%
#   group_by_all() %>%
#   tally() %>%
#   as.data.frame()

# Data arranging and nesting -------------------------------------------------------------------
  # The datasets will be worked without thes duplicities

df <- compounds %>%
  mutate(cid= as.character(cid)) %>%
  inner_join(odour, by=c("cid"="cid") ) %>%
  distinct() %>%
  mutate( 
    level = case_when( level == "first"  ~ '1',
                       level == "second" ~ '2',
                       level == "third"  ~ '3',
                       level == "fourth" ~ '4', 
                       level == "fifth"  ~ '5', 
                       level == "sixth"  ~ '6',
                       level == "seventh"~ '7',
                       level == "eighth" ~ '8')) %>%
  mutate(cid= as.integer(cid) ) %>%
  arrange(cid, level) %>%
  mutate(cid= as.character(cid)) %>%
  select(cid, canonical_smiles, xlogp, level, name ) %>%
  filter(!cid %in% c("222","14923", "25519") )

df_nested <- df %>%
  group_by(cid, canonical_smiles, xlogp) %>%
  nest() %>%
  ungroup() %>%
  rename(odour = "data")

# microbenchmark::microbenchmark(
#   df_nested %>%
#     mutate("canonical_sdf" = future_map(canonical_smiles, smiles2sdf ),
#            "parsed_smile"  = future_map(canonical_smiles, parse.smiles)) %>%
#     mutate("descriptors"= future_map(parsed_smile, ~ eval.desc(.x, nombre_metodos  ))) 
#   ,
#   df_nested %>%
#     mutate("canonical_sdf" = map(canonical_smiles, smiles2sdf ),
#            "parsed_smile"  = map(canonical_smiles, parse.smiles)) %>%
#     mutate("descriptors"= map(parsed_smile, ~ eval.desc(.x, nombre_metodos  ))),
#   times= 20
# )

nombre_metodos <- get.desc.names()

future::plan(multisession)

df_nested <- df_nested %>%
  mutate("canonical_sdf" = future_map(canonical_smiles, smiles2sdf, .progress = TRUE ),
         "parsed_smile"  = future_map(canonical_smiles, parse.smiles, .progress = TRUE)) %>%
  mutate("descriptors"= future_map(parsed_smile, ~ eval.desc(.x, nombre_metodos  ),.progress = TRUE)) 

# df_nested %>%
#   select(-c(molecular_weight, canonical_sdf,parsed_smile)) %>% 
#   toJSON() %>%
#   write("Data/Sensorama_JSON.json")

rm(list=setdiff(ls(), "df_nested"))
